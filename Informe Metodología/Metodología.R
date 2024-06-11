library(readxl)
library(tidyverse)

#---------------------------Metodología----------------------------------------

#Tabla mortalidad de la supen

mortalidad <- read_excel("Informe Metodología/tavid2000-2150.xls")
mortalidad$qx <- as.double(mortalidad$qx)
mortalidad <- mortalidad[order(mortalidad$edad),]
mortalidad_mujeres <- subset(mortalidad, sex == 2 & year == 2023,
                             select = c(edad,qx))
mortalidad_hombres <- subset(mortalidad, sex == 1 & year == 2023,
                             select = c(edad,qx))

#Se obtienen las probabilidades de sobrevivencia
px_M <- 1- mortalidad_mujeres$qx
px_H <- 1- mortalidad_hombres$qx

#Se añaden las probabilidades de sobrevivencia a la base datos
mortalidad_mujeres$px <- px_M
mortalidad_hombres$px <- px_H

# Tabla invalidez
invalidez <- read_excel("Informe Metodología/Invalidez.xlsx", 
                        col_types = c("text", "numeric", "numeric"))

#Se obtienen las probabilidades de no ser inválido
p_no_invalidez_M <- 1- invalidez$Mujeres
p_no_invalidez_H <- 1- invalidez$Hombres

invalidez <- cbind(invalidez, p_no_invalidez_H, p_no_invalidez_M)

#extender la base hasta la edad de 115
invalidez <- rbind(invalidez, invalidez[rep(71, 26),])
invalidez$Edad[71:nrow(invalidez)] <- 89:115

#----------- Activos--------|

# Leer la hoja 'Activos'
activos <- read_excel("Informe Metodología/Fondo C.xlsx", sheet = "Activos")
activos <- activos[, -c(1, (ncol(activos)-1):ncol(activos))]

# Calcular la edad al 31 de diciembre de 2023
activos$Fec.Nac <- as.Date(activos$Fec.Nac) # Asegurar que FEC_NAC es de tipo Date
activos$Edad <- as.numeric(format(as.Date("2023-12-31"), "%Y")) - as.numeric(format(activos$Fec.Nac, "%Y"))
# Ordenar la columna 'Edad' de menor a mayor
activos_ordenado <- activos[order(activos$Edad), ]

# Se filtran los activos
activos_filtrados <- activos_ordenado %>%
  filter(rowSums(select(., -c(1:350, 363))) != 0)

# Se cuentan las cotizaciones
num_cotizaciones <- rowSums(activos_filtrados[, -c(1,2,363)] > 0)
activos_filtrados$"Cantidad Cotizaciones" <- num_cotizaciones

activos_sin_cotizacion <- activos_filtrados[, c("Sexo", "Edad", "Cantidad Cotizaciones")]

# Se le agrega el sexo  del cónyuge y la edad el hijo(a). La edad del afiliado
# y cónyuge es la misma por ende no se crea otra columna. El hijo es 25 años
# menor

conyugue <- c()
sexo_afiliado <- activos_sin_cotizacion$Sexo

edad_hijo <- activos_sin_cotizacion$Edad-25

for(i in (1: length(sexo_afiliado))) {
  if (sexo_afiliado[i] == "M"){
    conyugue[i] <- "F"
  }else {
    conyugue[i] <- "M"
}
  if(edad_hijo[i] < 0) {
    edad_hijo[i] = 0
  }
} 
activos_sin_cotizacion <- cbind(activos_sin_cotizacion, conyugue, edad_hijo)


#---------------Simulaciones-------------|

# Se considera 4 estados de decremento: Pensión por sucesión, invalidez,
# vejez, salida del fondo (muere pero no se cumple los requisitos de sucesión,
# se invalida pero no cumple los requisitos para recibir la pensión por invalidez)

#Caso base mujer

#Se simulan diversas trayectorias de una mujer de 20 años
mujer_tablavida <- subset(mortalidad, sex == 2 & ynac == 2003 & edad >= 20,
                          select = c(edad,qx, year))
px_mujer <- 1-mujer_tablavida$qx

set.seed(2901)
iteraciones <- 100
n <- length(px_mujer)
activos_simulados <- list()
pensionados_simulados <- list()

promedio_cotizaciones <- 5
#round(mean(rowSums(activos_filtrados[, -c(1:350,363)] > 0)))
for (i in 1:iteraciones) {
  
  prob_muerte_M <- runif(n) # Se toman como probabilidades de muerte
  prob_invalidez_M <- runif(n) # Se toman como probabilidades de invalidez
  prob_postergar <- runif(1)

  t <- 1
  cont <- 1
  estado <- 0
  edad <- activos_sin_cotizacion$Edad[1]
  cotizaciones <- activos_sin_cotizacion$`Cantidad Cotizaciones`[1] 
  
  while (t == 1) {
        if (prob_muerte_M[cont] < px_mujer[cont]) {
          if(prob_invalidez_M[cont] < invalidez$p_no_invalidez_M[invalidez$Edad == edad]){
            if(edad >=  65 & cotizaciones >= 180 & prob_postergar < 0.9  ){
               estado <- 1
                t <-0
            }else {
               edad <- edad +1
               cont <- cont +1
               cotizaciones <- cotizaciones + promedio_cotizaciones
             }
          }else if(cotizaciones >= 180 )  {
            estado <- 2
            t <- 0
          }else {
            estado <- 4
            t <-0
          }
       }else if(cotizaciones >= 180 ){
        estado <- 3
        t <- 0
       }else {
        estado <-4
        t <- 0
      }
  }
  # Almacenar los resultados de la simulación
  activos_simulados[[i]] <- data.frame(iteracion = i, estado = estado, edad = edad, cotizaciones = cotizaciones)
}
# Convertir la lista de resultados en un data frame
resultados_simulacion <- bind_rows(activos_simulados)

