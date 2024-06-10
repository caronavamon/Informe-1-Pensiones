library(readxl)
library(tidyverse)

#---------------------------Metodología----------------------------------------

#Tabla mortalidad de la supen

mortalidad <- read_excel("Informe Metodología/tavid2000-2150.xls")
mortalidad$qx <- as.double(mortalidad$qx)
mortalidad <- mortalidad[order(mortalidad$edad),]
mortalidad_mujeres <- subset(mortalidad, sex == 2 & year == 2024,
                             select = c(edad,qx))

#Se obtienen las probabilidades de sobrevivencia
px_M <- 1- mortalidad_mujeres$qx
px_H <- 1- mortalidad_hombres$qx

#Se añaden las probabilidades de sobrevivencia a la base datos
mortalidad_mujeres$px <- px_M
mortalidad_mujeres$px <- px_H

mortalidad_hombres <- subset(mortalidad, sex == 1 & year == 2024,
                             select = c(edad,qx))

# Tabla invalidez
invalidez <- read_excel("Informe Metodología/Invalidez.xlsx", 
                        col_types = c("text", "numeric", "numeric"))

#Se obtienen las probabilidades de no ser inválido
p_no_invalidez_M <- 1- invalidez$Mujeres
p_no_invalidez_H <- 1- invalidez$Hombres



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

# Se eliminan las cotizaciones
activos_sin_cotizacion <- activos_filtrados[, c("Sexo", "Edad")]

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

#Se simulan diversas trayectorias de vida de la persona
set.seed(2901)
iteraciones=10^4
n=length(activos_sin_cotizacion)

for (i in 1:iteraciones) {
  
  prob_muerte_M <- list() # Se toman como probabilidades de muerte
  prob_invalidez_M <- list() # Se toman como probabilidades de invalidez
  
  for( j in 1:100) {
    prob_muerte_M[[i]] <- runif(n)
    prob_invalidez_M[[i]] <- runif(n)
  }
  
  t <- 1
  cont <- 1
  estado <- 0
  
  while (t == 1) {
    for(k in 1: n) {
      if(activos_sin_cotizacion$Sexo == "F") {
        if (prob_muerte_M[[cont]] < px_M[cont]) {
          if(prob_invalidez_M[[cont]] < p_no_invalidez_M){
            #if(revisar si cumple las condiciones para jubilarse){
               #estado <- 1
                #t <-0
          }# poner el caso de pensión por invalidez 
        }#poner el caso de sucesión
      }
    } 
  }
    

}

