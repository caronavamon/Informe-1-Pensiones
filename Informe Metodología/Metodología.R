library(readxl)
library(tidyverse)

#---------------------------Metodología----------------------------------------

#Tabla mortalidad de la supen
mortalidad_mujeres <- read_excel("Informe Metodología/TablasMortalidad_edit.xlsx", 
                                                          sheet = "mujeres")
mortalidad_hombres <- read_excel("Informe Metodología/TablasMortalidad_edit.xlsx", 
                                    sheet = "hombres")

#Función que obtiene la tabla de vida de cada edad, mediante la diagonal

tabla_vida_function <- function(tabla, tabla_sexo) {
  
  for(i in 0:115) {
    
    col_inicial <- 23
    col_final <- 138-i #23+115-i
    
    fila_inicial <- i+1 
    fila_final <- 116
    
    datos <- as.matrix(tabla_sexo[fila_inicial: fila_final, col_inicial:col_final])
    
    qx <- diag(datos) #probabilidades de muerte
    px <- 1-qx #probabilidades de sobrevivencia
    Edad <- c(i:115)
    
    tabla_vida <- data.frame("Edad" = Edad, "qx"= qx, "px" = px)
    tabla[[i+1]] <- tabla_vida
  }
  return(tabla)
}

tablas_vida_M <- list()
tablas_vida_H <- list()

tablas_vida_M <- tabla_vida_function(tablas_vida_M, mortalidad_mujeres)
tablas_vida_H <- tabla_vida_function(tablas_vida_H, mortalidad_hombres)


# Tabla invalidez
invalidez <- read_excel("Informe Metodología/Invalidez.xlsx", 
                        col_types = c("text", "numeric", "numeric"))

#extender la base hasta la edad de 115
invalidez <- rbind(invalidez, invalidez[rep(71, 26),])
invalidez$Edad[71:nrow(invalidez)] <- 89:115

#separar los datos para hombres y mujeres

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

# Se le agrega el sexo  del cónyuge y la edad el hijo(a). La edad del afiliado
# y cónyuge es la misma por ende no se crea otra columna. El hijo es 25 años
# menor

conyugue <- c()
sexo_afiliado <- activos_ordenado$Sexo

edad_hijo <- activos_ordenado$Edad-25

for(i in (1: length(sexo_afiliado))) {
  if (sexo_afiliado[i] == "M"){
    conyugue[i] <- "F"
  }else {
    conyugue[i] <- "M"
  }
  if(edad_hijo[i] < 0) {
    edad_hijo[i] = NA
  }
} 
activos_ordenado <- cbind(activos_ordenado, conyugue, edad_hijo)

# Se filtran los activos

activos_ordenado <- activos_ordenado %>%
  mutate(across(-c(1,2, 363:365), ~ ifelse(. < 10000, 0, .)))

# Se cuentan las cotizaciones
num_cotizaciones <- rowSums(activos_ordenado[, -c(1,2,363:365)] > 0)
activos_ordenado$"Cantidad Cotizaciones" <- num_cotizaciones

activos_filtrados <- activos_ordenado %>%
  filter(rowSums(select(., -c(1:350, 363:366))) != 0)

activos_total_cotizacion <- activos_filtrados[, c("Edad", "Sexo", "Cantidad Cotizaciones")]


#agregar más edades a la densidad cotización 
nuevas_cotizaciones <- data.frame(
  edad = 79:115,
  Cotizaciones = rep(10, length(79:115))  # Repetir el valor 10
)
promedios_cotizaciones_edad <- rbind(promedios_cotizaciones_edad, nuevas_cotizaciones)


#--------Inactivos------------

# Se filtran los inactivos
inactivos_filtrados <- activos_ordenado %>%
  filter(rowSums(select(.,-c(1:350, 363:366))) == 0)

inactivos_total_cotizacion <- inactivos_filtrados[, c("Edad", "Sexo", "Cantidad Cotizaciones")]


#-------Pensionados----------

# se cargan los datos de interés
pensionados <- read_excel("Informe Metodología/Fondo C.xlsx", sheet = "Pensionados")
# se eliminan las columnas no necesarias
pensionados <- pensionados[,c(2:6,8)]


# Calcular la edad al 31 de diciembre de 2023
pensionados$FEC_NAC<- as.Date(pensionados$FEC_NAC) # Asegurar que FEC_NAC es de tipo Date
pensionados$Edad <- as.numeric(format(as.Date("2023-12-31"), "%Y")) - as.numeric(format(pensionados$FEC_NAC, "%Y"))

# Se rellenan datos faltantes para pensionados por vejez

# calcula edad de pension
pensionados <- pensionados %>%
  mutate(Edad_Pension = as.numeric(difftime(`Rige de la Pensión`, FEC_NAC, units = "days")) / 365.25) 

#calcula el promedio de edad de pension por tipo y sexo
promedio_edad_pension_tipo<- pensionados %>%
  group_by(COD_TIPO_PENSION, SEXO) %>%
  summarize(promedio_edad_pension = mean(Edad_Pension, na.rm = TRUE)) 

#aquí trabaja solo con los que tiene NA's
pensionados_na <- pensionados %>%
  filter(is.na(`Rige de la Pensión`)) 

# se le agrega la edad de pensión promedio a los que tienen NA's
pensionados_na_con_promedio <- left_join(pensionados_na, promedio_edad_pension_tipo, by = c("SEXO", "COD_TIPO_PENSION"))

# Actualiza la columna 'Edad_Pension' con el promedio de edad de pensión correspondiente
pensionados_na_con_promedio <- pensionados_na_con_promedio %>%
  mutate(Edad_Pension = ifelse(is.na(Edad_Pension), promedio_edad_pension, Edad_Pension))

# Eliminar la columna 'promedio_edad_pension'
pensionados_na_con_promedio <- pensionados_na_con_promedio[, -6]

# pone la fecha en que se pensionarían
pensionados_na_con_promedio <- pensionados_na_con_promedio %>%
  mutate(`Rige de la Pensión` = 
           fecha_pension <- FEC_NAC + as.numeric(Edad_Pension) * 365.25 * 24 * 60 * 60)

# se corrigen años de rige de la pensión mayores a 2023
fecha_inicio_pension <- pensionados_na_con_promedio$`Rige de la Pensión`
for(i in 1 : length(fecha_inicio_pension)) {
  if(year(fecha_inicio_pension[i]) > 2023){
    year(fecha_inicio_pension[i]) <- 2023
  }
}
pensionados_na_con_promedio$`Rige de la Pensión` <- fecha_inicio_pension

# se une todo
pensionados <- bind_rows(pensionados_na_con_promedio, pensionados %>% filter(!is.na(`Rige de la Pensión`))) 

# se extrae  el año de la fecha
pensionados$año <- format(pensionados$`Rige de la Pensión`, "%Y")


#---------------Simulaciones-------------|

# Se considera 4 estados de decremento: Pensión por sucesión, invalidez,
# vejez, salida del fondo (muere pero no se cumple los requisitos de sucesión,
# se invalida pero no cumple los requisitos para recibir la pensión por invalidez)

#-----Activos proyecciones------


# Función para obtener los estados
proyeccion_demo <- function(edad, sexo, cont, cotizaciones, prob_muerte, prob_invalidez) {
  
  if(sexo == "F") {
    tabla_vida <- tablas_vida_M[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad] 
    p_no_invalidez <- invalidez_M$p_no_invalidez[invalidez$Edad == edad]
  } else {
    tabla_vida <- tablas_vida_H[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad]
    p_no_invalidez <- invalidez_H$p_no_invalidez[invalidez$Edad == edad]
  }
  prob_postergar <- runif(1)
  
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
  tabla_proyeccionesM_activos$Activo[1] <- sum(activos_total_cotizacion$Sexo== "F")
  
  #Hombres
  tabla_proyeccionesH_activos<- data.frame(
    "Año" = 0:100,
    "Activo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesH_activos$Activo[1] <- sum(activos_total_cotizacion$Sexo== "M")
  

  for(j in 1: nrow(activos_total_cotizacion)) {
    
    cont <- 1
    edad <- activos_total_cotizacion$Edad[j]
    sexo <- activos_total_cotizacion$Sexo[j]
    cotizaciones <- activos_total_cotizacion$`Cantidad Cotizaciones`[j]
    
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

# Calcular la proyeccioon mediante promedios para mujeres y hombres
proyeccion_activos_M <- calcular_promedios(df_combinado_M)
proyeccion_activos_H <- calcular_promedios(df_combinado_H)

#---------Inactivos--------

# Función para obtener los estados
proyeccion_demo_inactivos <- function(edad, sexo, cont, cotizaciones, prob_muerte, prob_invalidez) {
  
  if(sexo == "F") {
    tabla_vida <- tablas_vida_M[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad] 
    p_no_invalidez <- invalidez_M$p_no_invalidez[invalidez$Edad == edad]
  } else {
    tabla_vida <- tablas_vida_H[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad]
    p_no_invalidez <- invalidez_H$p_no_invalidez[invalidez$Edad == edad]
  }
  
  if (prob_muerte[cont] < px) {
    if(prob_invalidez[cont] < p_no_invalidez){
      if(edad >=  65 & cotizaciones >= 300){
        return(1) #pensión por vejez
      }else {
        return(0) #inactivo
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

lista_resultados_inactivos_df_M <-list()
lista_resultados_inactivos_df_H <-list()

for (i in 1:iteraciones) {
  
  #Mujeres
  tabla_proyeccionesM_inactivos<- data.frame(
    "Año" = 0:100,
    "Inactivo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesM_inactivos$Inactivo[1] <- sum(inactivos_total_cotizacion$Sexo== "F")
  
  #Hombres
  tabla_proyeccionesH_inactivos<- data.frame(
    "Año" = 0:100,
    "Inactivo" = rep(0, 101),
    "PJ" = rep(0, 101),
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  tabla_proyeccionesH_inactivos$Inactivo[1] <- sum(inactivos_total_cotizacion$Sexo== "M")
  
  
  for(j in 1: nrow(inactivos_total_cotizacion)) {
    
    cont <- 1
    edad <- inactivos_total_cotizacion$Edad[j]
    sexo <- inactivos_total_cotizacion$Sexo[j]
    cotizaciones <- inactivos_total_cotizacion$`Cantidad Cotizaciones`[j]
    
    n <- 115-edad
    prob_muerte <- runif(n)
    prob_invalidez <- runif(n)
    
    estado <- proyeccion_demo_inactivos(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    
    while(estado == 0) {
      
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 2] <- tabla_proyeccionesM_inactivos[cont+1, 2] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 2] <- tabla_proyeccionesH_inactivos[cont+1, 2] + 1
      }
      
      cont <- cont + 1
      edad <- edad + 1
      estado <- proyeccion_demo_inactivos(edad, sexo,cont, cotizaciones, prob_muerte, prob_invalidez )
    }
    
    if(estado == 1) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 3] <- tabla_proyeccionesM_inactivos[cont+1, 3] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 3] <- tabla_proyeccionesH_inactivos[cont+1, 3] + 1
      }
    }
    if(estado == 2) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 4] <- tabla_proyeccionesM_inactivos[cont+1, 4] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 4] <- tabla_proyeccionesH_inactivos[cont+1, 4] + 1
      }
    }
    if(estado == 3) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 5] <- tabla_proyeccionesM_inactivos[cont+1, 5] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 5] <- tabla_proyeccionesH_inactivos[cont+1, 5] + 1
      }
    }
    if(estado == 4) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 6] <- tabla_proyeccionesM_inactivos[cont+1, 6] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 6] <- tabla_proyeccionesH_inactivos[cont+1, 6] + 1
      }
    }
    
  }
  lista_resultados_inactivos_df_M[[i]] <- tabla_proyeccionesM_inactivos
  lista_resultados_inactivos_df_H[[i]] <- tabla_proyeccionesH_inactivos
  
}

# Combinar los dataframes para mujeres y hombres
df_combinado_inactivos_M <- combinar_dfs(lista_resultados_inactivos_df_M)
df_combinado_inactivos_H <- combinar_dfs(lista_resultados_inactivos_df_H)

calcular_promedios_inactivos <- function(df) {
  
  df %>%
    group_by(Año) %>%
    summarise(
      Inactivo = mean(Inactivo),
      PJ = mean(PJ),
      PI = mean(PI),
      PS = mean(PS),
      SR = mean(SR)
    )
}



# Calcular la proyeccion mediante promedios para mujeres y hombres
proyeccion_inactivos_M <- calcular_promedios_inactivos(df_combinado_inactivos_M)
proyeccion_inactivos_H <- calcular_promedios_inactivos(df_combinado_inactivos_H)


#----------------Pensionados--------------------------


# Función para obtener los estados
proyeccion_demo_pensionados <- function(edad, sexo, cont, prob_muerte, tipo) {
  
  if(sexo == "F") {
    tabla_vida <- tablas_vida_M[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad] 
  } else {
    tabla_vida <- tablas_vida_H[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad]
  }
  
  if (prob_muerte[cont] < px) {
    return(tipo) #se mantiene la pensión
  } else{
    if(tipo == "Sucesión") {
      return("SR")
    }else{
      return("PS")
    }
  }
}  

proyeccion_beneficiarios <- function(edad, prob_muerte, tipo){
  
  if(sexo == "F") {
    tabla_vida <- tablas_vida_M[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad] 
  } else {
    tabla_vida <- tablas_vida_H[[edad+1]]
    px <- tabla_vida$px[tabla_vida$Edad == edad]
  }
  
  if (prob_muerte[cont] < px) {
    return(tipo) #se mantiene la pensión
  } else{
    return("SR")
  }
}


# Proyección

set.seed(2901)
iteraciones <- 100

lista_resultados_pensionados_df_M <-list()
lista_resultados_pensionados_df_H <-list()

for (i in 1:iteraciones) {
  
  conteo <- pensionados %>%
    group_by(SEXO,COD_TIPO_PENSION) %>%
    summarise(cantidad_personas = n())
  
  
  #Mujeres
  tabla_proyeccionesM_pensionados<- data.frame(
    "Año" = 0:100,
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "PV" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  

  tabla_proyeccionesM_pensionados[1,-c(1,5)]<- conteo$cantidad_personas[1:3]
  
  #Hombres
  tabla_proyeccionesH_pensionados<- data.frame(
    "Año" = 0:100,
    "PI" = rep(0, 101),
    "PS" = rep(0, 101),
    "PV" = rep(0, 101),
    "SR" = rep(0, 101)
  )
  
  tabla_proyeccionesH_pensionados[1,-c(1,5)]<- conteo$cantidad_personas[4:6]
  
  
  for(j in 1: nrow(pensionados)) {
    
    cont <- 1
    edad <- pensionados$Edad[j]
    sexo <- pensionados$SEXO[j]
    tipo <- pensionados$COD_TIPO_PENSION[j]
    
    n <- 115-edad
    prob_muerte <- runif(n)
    
    estado <- proyeccion_demo_pensionados(edad, sexo,cont, prob_muerte,tipo)
    
    while(estado == tipo) {
      
      # Obtener el índice de la columna 'Edad'
      col <- which(colnames(tabla_proyeccionesM_pensionados) == tipo)
      
      if(sexo == "F"){
        tabla_proyeccionesM_pensionados[cont+1, col] <- tabla_proyeccionesM_pensionados[cont+1, col] + 1 
      }else {
        tabla_proyeccionesH_pensionados[cont+1, col] <- tabla_proyeccionesH_pensionados[cont+1, col] + 1
      }
      
      cont <- cont + 1
      edad <- edad + 1
      estado <- proyeccion_demo_pensionados(edad, sexo,cont,prob_muerte,tipo)
    }
    
    
    if(estado == "PS") {
      aux_c <- 1
      aux_h <- 1
      edad_c <- edad
      edad_h <- edad-25
      tipo <- "PS"
      
      n_c <- 115-edad
      prob_muerte_c <- runif(n_c)
    
      if (sexo == "M"){
        sexo_c <- "F"
      }else {
        sexo_c <- "M"
      }
      
      if(edad_h < 0) {
        edad_h <- NA
      }else{
        n_h <- 115-edad_h
        prob_muerte_h <- runif(n_h)
        estado_h <- proyeccion_beneficiarios(edad_h, sexo,aux_h, prob_muerte_h,tipo)
      }
      
      estado_c <- proyeccion_beneficiarios(edad_c, sexo_c,aux_h, prob_muerte_c,tipo)
      
      while(estado == tipo) {
        
        # Obtener el índice de la columna 'Edad'
        col <- which(colnames(tabla_proyeccionesM_pensionados) == tipo)
        
        if(sexo == "F"){
          tabla_proyeccionesM_pensionados[cont+1, col] <- tabla_proyeccionesM_pensionados[cont+1, col] + 1 
        }else {
          tabla_proyeccionesH_pensionados[cont+1, col] <- tabla_proyeccionesH_pensionados[cont+1, col] + 1
        }
        
        cont <- cont + 1
        edad <- edad + 1
        estado <- proyeccion_demo_pensionados(edad, sexo,cont,prob_muerte,tipo)
      }

      
     
    
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 3] <- tabla_proyeccionesM_inactivos[cont+1, 3] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 3] <- tabla_proyeccionesH_inactivos[cont+1, 3] + 1
      }
    }
    
    
    
    if(estado == "SR") {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 4] <- tabla_proyeccionesM_inactivos[cont+1, 4] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 4] <- tabla_proyeccionesH_inactivos[cont+1, 4] + 1
      }
    }
    if(estado == 3) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 5] <- tabla_proyeccionesM_inactivos[cont+1, 5] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 5] <- tabla_proyeccionesH_inactivos[cont+1, 5] + 1
      }
    }
    if(estado == 4) {
      if(sexo == "F"){
        tabla_proyeccionesM_inactivos[cont+1, 6] <- tabla_proyeccionesM_inactivos[cont+1, 6] + 1 
      }else {
        tabla_proyeccionesH_inactivos[cont+1, 6] <- tabla_proyeccionesH_inactivos[cont+1, 6] + 1
      }
    }
    
  }
  lista_resultados_inactivos_df_M[[i]] <- tabla_proyeccionesM_inactivos
  lista_resultados_inactivos_df_H[[i]] <- tabla_proyeccionesH_inactivos
  
}

# Combinar los dataframes para mujeres y hombres
df_combinado_inactivos_M <- combinar_dfs(lista_resultados_inactivos_df_M)
df_combinado_inactivos_H <- combinar_dfs(lista_resultados_inactivos_df_H)

calcular_promedios_inactivos <- function(df) {
  
  df %>%
    group_by(Año) %>%
    summarise(
      Inactivo = mean(Inactivo),
      PJ = mean(PJ),
      PI = mean(PI),
      PS = mean(PS),
      SR = mean(SR)
    )
}



# Calcular la proyeccion mediante promedios para mujeres y hombres
proyeccion_inactivos_M <- calcular_promedios_inactivos(df_combinado_inactivos_M)
proyeccion_inactivos_H <- calcular_promedios_inactivos(df_combinado_inactivos_H)

