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

#extender la base hasta la edad de 115
invalidez <- rbind(invalidez, invalidez[rep(71, 26),])
invalidez$Edad[71:nrow(invalidez)] <- 89:115

#separar los datos para hombre y mujeres

invalidez_M <- invalidez[, -2]
invalidez_H <- invalidez[, -3]

#Se obtienen las probabilidades de no ser inválido
invalidez_M$p_no_invalidez <- 1- invalidez_M$Mujeres
invalidez_H$p_no_invalidez <- 1- invalidez_H$Hombres

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


#agregar densidad cotización 
nuevas_cotizaciones <- data.frame(
  edad = 79:115,
  Cotizaciones = rep(10, length(79:115))  # Repetir el valor 10
)
promedios_cotizaciones_edad <- rbind(promedios_cotizaciones_edad, nuevas_cotizaciones)


#---------------Simulaciones-------------|

# Se considera 4 estados de decremento: Pensión por sucesión, invalidez,
# vejez, salida del fondo (muere pero no se cumple los requisitos de sucesión,
# se invalida pero no cumple los requisitos para recibir la pensión por invalidez)


# Función para obtener los estados
proyeccion_demo <- function(edad, sexo, cont, cotizaciones, prob_muerte, prob_invalidez) {
  
  if(sexo == "F") {
    px <- mortalidad_mujeres$px[mortalidad_mujeres$edad == edad] 
    p_no_invalidez <- invalidez_M$p_no_invalidez[invalidez$Edad == edad]
  } else {
    px <- mortalidad_hombres$px[mortalidad_hombres$edad == edad]
    p_no_invalidez <- invalidez_H$p_no_invalidez[invalidez$Edad == edad]
  }
  prob_postergar <- runif(1)
  print(cont)
  
  if (prob_muerte[cont] < px) {
    if(prob_invalidez[cont] < p_no_invalidez){
      if(edad >=  65 & cotizaciones >= 300 & prob_postergar < 0.9  ){
        return(1) #pensión por vejez
      }else {
        return(0) #activo
      }
    }else{
      if(cotizaciones >= 180 )  {
        return(2) #pensión por invalidez
      }else {
        return(4) #salida del fondo
      }
    } 
  }else {
    if(cotizaciones >= 180 ){
      return(3) #pensión por sucesión
    }else {
      return(4) #salida del fondo
    }
  }
}  


# Proyección

set.seed(2901)
iteraciones <- 100

lista_resultados_df_M <-list()
lista_resultados_df_H <-list()

for (i in 1:iteraciones) {
  
  #Mujeres
  tabla_proyeccionesM_activos<- data.frame(
    "Año" = 0:100,
    "Activo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesM_activos$Activo[1] <- sum(activos_sin_cotizacion$Sexo== "F")
  
  #Hombres
  tabla_proyeccionesH_activos<- data.frame(
    "Año" = 0:100,
    "Activo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesH_activos$Activo[1] <- sum(activos_sin_cotizacion$Sexo== "M")
  

  for(j in 1: nrow(activos_sin_cotizacion)) {
    
    cont <- 1
    edad <- activos_sin_cotizacion$Edad[j]
    sexo <- activos_sin_cotizacion$Sexo[j]
    cotizaciones <- activos_sin_cotizacion$`Cantidad Cotizaciones`[j]
    
    n <- 115-edad
    prob_muerte <- runif(n)
    prob_invalidez <- runif(n)
    
    estado <- proyeccion_demo(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    
    while(estado == 0) {
      
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 2] <- tabla_proyeccionesM_activos[cont+1, 2] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 2] <- tabla_proyeccionesH_activos[cont+1, 2] + 1
      }
      
      cont <- cont + 1
      edad <- edad + 1
      cotizaciones <- cotizaciones + 
        promedios_cotizaciones_edad$Cotizaciones[promedios_cotizaciones_edad$edad == edad]
      estado <- proyeccion_demo(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    }
    
    if(estado == 1) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 3] <- tabla_proyeccionesM_activos[cont+1, 3] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 3] <- tabla_proyeccionesH_activos[cont+1, 3] + 1
      }
    }
    if(estado == 2) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 4] <- tabla_proyeccionesM_activos[cont+1, 4] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 4] <- tabla_proyeccionesH_activos[cont+1, 4] + 1
      }
    }
    if(estado == 3) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 5] <- tabla_proyeccionesM_activos[cont+1, 5] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 5] <- tabla_proyeccionesH_activos[cont+1, 5] + 1
      }
    }
    if(estado == 4) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 6] <- tabla_proyeccionesM_activos[cont+1, 6] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 6] <- tabla_proyeccionesH_activos[cont+1, 6] + 1
      }
    }
    
  }
  lista_resultados_df_M[[i]] <- tabla_proyeccionesM_activos
  lista_resultados_df_H[[i]] <- tabla_proyeccionesH_activos
  
}

# Unir los dataframes de las iteraciones en uno solo
combinar_dfs <- function(lista_df) {
  do.call("bind_rows", lista_df)
}

# Combinar los dataframes para mujeres y hombres
df_combinado_M <- combinar_dfs(lista_resultados_df_M)
df_combinado_H <- combinar_dfs(lista_resultados_df_H)

# Calcular los promedios por año para cada estado
calcular_promedios <- function(df) {
  df %>%
    group_by(Año) %>%
    summarise(
      Activo = mean(Activo),
      PJ = mean(PJ),
      PI = mean(PI),
      PS = mean(PS),
      SR = mean(SR)
    )
}

# Calcular promedios para mujeres y hombres
promedios_M <- calcular_promedios(df_combinado_M)
promedios_H <- calcular_promedios(df_combinado_H)
# Visualizar los primeros resultados
head(promedios_M)
head(promedios_H)


lista_resultados_df_M <-list()
lista_resultados_df_H <-list()

for (i in 1:iteraciones) {
  print(i)
  

  #Mujeres
  tabla_proyeccionesM_activos<- data.frame(
    "Año" = 0:100,
    "Activo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesM_activos$Activo[1] <- sum(activos_sin_cotizacion$Sexo== "F")
  
  #Hombres
  tabla_proyeccionesH_activos<- data.frame(
    "Año" = 0:100,
    "Activo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesH_activos$Activo[1] <- sum(activos_sin_cotizacion$Sexo== "M")
  
  for(j in 1: 100) {
    
    cont <- 1
    edad <- activos_sin_cotizacion$Edad[j]
    sexo <- activos_sin_cotizacion$Sexo[j]
    cotizaciones <- activos_sin_cotizacion$`Cantidad Cotizaciones`[j]
    
    n <- 115-edad
    prob_muerte <- runif(n)
    prob_invalidez <- runif(n)
    
    estado <- proyeccion_demo(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    
    while(estado == 0) {
      
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 2] <- tabla_proyeccionesM_activos[cont+1, 2] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 2] <- tabla_proyeccionesH_activos[cont+1, 2] + 1
      }
      
      cont <- cont + 1
      edad <- edad + 1
      cotizaciones <- cotizaciones + 
        promedios_cotizaciones_edad$Cotizaciones[promedios_cotizaciones_edad$edad == edad]
      estado <- proyeccion_demo(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    }
    
    if(estado == 1) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 3] <- tabla_proyeccionesM_activos[cont+1, 3] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 3] <- tabla_proyeccionesH_activos[cont+1, 3] + 1
      }
    } else if(estado == 2) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 4] <- tabla_proyeccionesM_activos[cont+1, 4] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 4] <- tabla_proyeccionesH_activos[cont+1, 4] + 1
      }
    }else if(estado == 3) {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 5] <- tabla_proyeccionesM_activos[cont+1, 5] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 5] <- tabla_proyeccionesH_activos[cont+1, 5] + 1
      }
    }else {
      if(sexo == "F"){
        tabla_proyeccionesM_activos[cont+1, 6] <- tabla_proyeccionesM_activos[cont+1, 6] + 1 
      }else {
        tabla_proyeccionesH_activos[cont+1, 6] <- tabla_proyeccionesH_activos[cont+1, 6] + 1
      }
    }
    
  }
  lista_resultados_df_M[[i]] <- tabla_proyeccionesM_activos
  lista_resultados_df_H[[i]] <- tabla_proyeccionesH_activos
  
  
}

