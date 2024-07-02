#---------- Cálculo Pensiones -----------------------

#top_300_prom <- function(df) {
#salario_referencia <- data.frame("ID"= character(), "Tipo" = character(),
#"Salario" = numeric())

#for(i in 1: nrow(df)) {

# salarios <- df[i,]

#top <- head(sort(salarios, decreasing = TRUE), 300)

# salario_prom <- mean(top)

#salario_referencia <- rbind(salario_referencia, 
#data.frame(ID = ID,
# Tipo = Tipo,
# Salario = salario_prom))
# }
# return(salario_referencia)
#}

#salario_referencia_activos <- lapply(Mensual_Activos, top_300_prom)
#salario_referencia_inactivos<- lapply(Mensual_Inactivos, top_300_prom)


# Se filtran los datos para que no salgan los SR

lista_pensionados_activos <- lapply(lista_pensionados_activos, function(df) {
  filtered_df <- df[df$COD_TIPO_PENSION != "SR", ]
})

lista_pensionados_inactivos <- lapply(lista_pensionados_inactivos, function(df) {
  filtered_df <- df[df$COD_TIPO_PENSION != "SR", ]
})


top_300_prom <- function(df) {
  salario_referencia <- data.frame("ID"= character(), "Tipo" = character(),
                                   "Salario" = numeric())
  
  for(i in 1: nrow(df)) {
    ID <- df$ID[i]
    
    name <- 2023 + df$cont[i]
    col <- which(colnames(ProyeccionSalarios) == name)
    Tipo <- df$COD_TIPO_PENSION[i]
    parentesco <- df$COD_PARENTESCO[i]
    
    if(is.na(parentesco) == FALSE & parentesco == "H") {
      salarios <- t(ProyeccionSalarios[i-1, 3: col])
    }else{
      salarios <- t(ProyeccionSalarios[i, 3: col])
    }
    
    top <- head(sort(salarios, decreasing = TRUE), 300)
    
    salario_prom <- mean(top)
    
    salario_referencia <- rbind(salario_referencia, 
                                data.frame(ID = ID,
                                           Tipo = Tipo,
                                           Salario = salario_prom))
  }
  return(salario_referencia)
}

salario_referencia_activos <- lapply(lista_pensionados_activos, top_300_prom)
salario_referencia_inactivos<- lapply(lista_pensionados_inactivos, top_300_prom)



Salario_minimo <- 11953.64*12


#----Pensión base-------------
cuantia <- function(df, lista_info_pensionados){
  
  salario_cuantia <-c(0)
  
  for(i in 1:nrow(df)) {
    
    salario <-df$Salario[i]
    
    if(salario < 2*Salario_minimo) {
      pension <- 0.525*salario
    }else if(salario >= 2*Salario_minimo & salario < 3*Salario_minimo) {
      pension <-  0.51*salario
    } else if(salario >= 3*Salario_minimo & salario < 4*Salario_minimo){
      pension <- 0.494*salario
    } else if(salario >= 4*Salario_minimo & salario < 5*Salario_minimo){
      pension <- 0.478*salario
    }else if(salario >= 5*Salario_minimo & salario < 6*Salario_minimo){
      pension <- 0.462*salario
    }else if(salario >= 6*Salario_minimo & salario < 8*Salario_minimo){
      pension <- 0.446*salario
    }else if(salario >= 8*Salario_minimo){
      pension <- 0.43*salario
    }
    
    if(pension > 3500000) {
      pension <- 3500000
    }
    salario_cuantia[i] <- salario
    
    #Tipo[i]<- lista_info_pensionados$Tipo[lista_info_pensionados$ID == df$ID[i]]
  }
  
  df$"Pensión base" <- salario_cuantia
  return(df)
}



pension_base_activos<- lapply(salario_referencia_activos, cuantia)
pension_base_inactivos<- lapply(salario_referencia_inactivos, cuantia)

#-------Adicionales-----------

#Bonificación

bonificacion<- function(df, salario_df){
  
  pension_bonificacion <- rep(NA, nrow(salario_df))
  
  for(i in 1: nrow(df) ){
    tipo <- df$COD_TIPO_PENSION[i]
    cotizaciones <- df$Cotizaciones_principal[i]
    pension  <- salario_df$`Pensión base`[i]
    salario <- salario_df$Salario[i]
    
    cotizaciones_extra <- cotizaciones - 300
    if(cotizaciones_extra > 0) {
      if(tipo == "Vejez" | tipo == "Invalidez") {
        pension_bonif <- pension + salario*0.000833*cotizaciones_extra
      } else {
        pension_bonif <- pension
      }
    }else{
      pension_bonif <- pension
    }
    
    if(pension_bonif > 3500000) {
      pension_bonif <- 3500000
    }
    pension_bonificacion[i] <- pension_bonif
    
  }
  return(pension_bonificacion)
}

for(k in 1: length(pension_base_activos)) {
  pension_base_activos[[k]]$"Pensión con bonificación" <- bonificacion(lista_pensionados_activos[[k]], pension_base_activos[[k]])
  pension_base_inactivos[[k]]$"Pensión con bonificación" <- bonificacion(lista_pensionados_inactivos[[k]], pension_base_inactivos[[k]])
  
}

#Postergación 

postergacion <- function(df, salario_df){
  
  pension_postergacion <- rep(NA, nrow(salario_df))
  
  for(i in 1: nrow(df) ){
    tipo <- df$COD_TIPO_PENSION[i]
    edad <- df$Edad[i]
    pension  <- salario_df$`Pensión con bonificación`[i]
    salario <- salario_df$Salario[i]
    
    postergacion_tiempo <- edad - 65
    
    if(postergacion_tiempo > 0) {
      if(tipo == "Vejez" | tipo == "Invalidez") {
        pension_post <- pension + salario* 0.001333*postergacion_tiempo
      } else{
        pension_post <- pension
      }
    }else {
      pension_post <- pension
    }
    
    if(pension_post > 3500000) {
      pension_post <- 3500000
    }
    pension_postergacion[i] <- pension_post
  }
  return(pension_postergacion)
}

for(k in 1: length(pension_base_activos)) {
  pension_base_activos[[k]]$"Pensión con postergacion" <- postergacion(lista_pensionados_activos[[k]], pension_base_activos[[k]])
  pension_base_inactivos[[k]]$"Pensión con postergacion" <- postergacion(lista_pensionados_inactivos[[k]], pension_base_inactivos[[k]])
  
}

#Sucesión

sucesion <- function(df, salario_df){
  
  pension_sucesion <- rep(NA, nrow(salario_df))
  
  for(i in 1: nrow(df) ){
    tipo <- df$COD_TIPO_PENSION[i]
    edad <- df$Edad[i]
    parentesco <- df$COD_PARENTESCO[i]
    
    if(identical(salario_df, pensionados_data)){
      pension <- pensionados_data$MONTO[i]
    }else{
      pension  <- salario_df$`Pensión con postergacion`[i]
    }
    
    if(tipo == "Sucesión") {
      if(parentesco == "C"){
        if(edad >= 60){
          pension_suc <- pension*0.7 
        }else if(edad >50 & edad < 60) {
          pension_suc <- pension*0.6
        }else if(edad <= 50) {
          pension_suc<- pension*0.5
        }
      } else{
        pension_suc <- pension*0.3
      }
    }else {
      pension_suc <- NA
    }
    
    if(is.na(pension_suc) == FALSE & pension_suc > 3500000) {
      pension_suc <- 3500000
    }
    pension_sucesion[i] <- pension_suc
  }
  return(pension_sucesion)
}


for(k in 1: length(pension_base_activos)) {
  pension_base_activos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_activos[[k]], pension_base_activos[[k]])
  pension_base_inactivos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_inactivos[[k]], pension_base_inactivos[[k]])
}


#Añadir una columna con el contador y el monto de pensión a lista_info_pensionados de activos e inactivos


añadir_columnas <- function(lista_info_pensionados, pension_base, lista_pensionados){
  
  cont <- c()
  pension <- c()
  
  for(i in 1 : nrow(lista_info_pensionados)) {
    Tipo <- lista_info_pensionados$Tipo[i]
    ID <- lista_info_pensionados$ID[i]
    cont[i] <-lista_pensionados$cont[lista_pensionados$ID == ID][1]
    
    if(Tipo == "Sucesión") {
      pension_sucesion  <- pension_base$`Pensión Sucesión`[pension_base$ID == ID]
      if(length(pension_sucesion) == 2) {
        parentesco <- lista_info_pensionados$COD_PARENTESCO[i]
        if(parentesco == "C"){
          pension[i] <- pension_sucesion[1]
        }else{
          pension[i] <- pension_sucesion[2]
        }
      }else{
        pension[i] <- pension_sucesion
      }
    } else{
      pension[i] <- pension_base$"Pensión con postergacion"[pension_base$ID == ID][1]
    }
  }
  
  lista_info_pensionados$cont <- cont
  lista_info_pensionados$MONTO <- pension
  
  return(lista_info_pensionados)
}

for(k in 1: length(lista_info_pensionados_activos)) {
  lista_info_pensionados_activos[[k]] <- añadir_columnas(lista_info_pensionados_activos[[k]],
                                                         pension_base_activos[[k]], 
                                                         lista_pensionados_activos[[k]])
  lista_info_pensionados_inactivos[[k]] <- añadir_columnas(lista_info_pensionados_inactivos[[k]],
                                                           pension_base_inactivos[[k]], 
                                                           lista_pensionados_inactivos[[k]])
}



# Se agrega la columna con la pension para PS

lista_info_pensionados_activos <- lapply(lista_info_pensionados_activos, sucesion_pensionados)
lista_info_pensionados_inactivos <- lapply(lista_info_pensionados_inactivos, sucesion_pensionados)


#------- Inflar pensión de activos e inactivos  -----------

inflar_pension <- function(df) {
  
  años <- as.character(2023:2123)
  ID <- df$ID
  Tipo <- df$Tipo
  
  # Initialize the matrix with the correct dimensions
  num_years <- length(años)
  num_rows <- nrow(df)
  monto_pensionados_matrix <- matrix(NA, nrow = num_rows, ncol = num_years + 2)
  
  # Fill the matrix with data
  monto_pensionados_matrix[, 1] <- ID
  monto_pensionados_matrix[, 2] <- Tipo
  monto_pensionados_matrix[, 3:(2 + num_years)] <- NA  # or other default values for years
  
  # Assign column names
  colnames(monto_pensionados_matrix) <- c("ID", "Tipo", años)
  
  # Convert matrix to data frame
  monto_pensionados <- as.data.frame(monto_pensionados_matrix, stringsAsFactors = FALSE)
  
  # Filtrar las filas donde Tipo no es "SR"
  #df_cont <- df_cont[df_cont$COD_TIPO_PENSION != "SR", ]
  
  for(i in 1: nrow(df)){
    
    año_inicio_pension <- df$cont[i]
    parentesco <- df$COD_PARENTESCO[i]
    duracion_pensionado <- df$Duracion[i]
    
    if (Tipo[i] == "PS"){
      pension <-df$"Pension Sucesion"[i] 
      if(parentesco == "C"){
        año_inicio_pension <- año_inicio_pension + df$Duracion[i-1] + 1
      }else{
        año_inicio_pension <- año_inicio_pension + df$Duracion[i-2] + 1
      }
      año_final_pension <- año_inicio_pension + duracion_pensionado
      t <- año_inicio_pension : año_final_pension
    }else{
      pension <- df$MONTO[i]
      año_final_pension <- año_inicio_pension + duracion_pensionado
      t <- 0 : duracion_pensionado
    }
    
    pension_inflada <- pension*(1+inflacion)^t
    
    
    pension_inflada[pension_inflada > 3500000] <- 3500000
    
    #vector2 <- monto_pensionados[i, (año_inicio + 3):(año_final + 3)]
    #print(vector2)
    
    monto_pensionados[i, (año_inicio_pension + 3):(año_final_pension + 3)] <- pension_inflada
    #print(vector2)
    
    #same_length <- identical(length(pension_inflada), length(vector2))
    
    # Imprimir el resultado
    #print(same_length)
  }
  return(monto_pensionados)  
}


monto_pension_activos <- lapply(lista_info_pensionados_activos, inflar_pension)
monto_pension_inactivos <-lapply(lista_info_pensionados_inactivos, inflar_pension)



#-------Pensionados--------------


asignar_monto <- function(df) {
  monto_seleccionado <- pensionados_data %>% select(ID, MONTO)
  df <- left_join(df, monto_seleccionado, by = "ID")
  return(df)
}

# Aplicar la función a cada dataframe en la lista
lista_info_pensionados_pensionados <- lapply(lista_info_pensionados_pensionados, asignar_monto)

limpiar_columnas_duplicadas <- function(df) {
  # Elimina las columnas duplicadas MONTO.x y MONTO.y si existen
  df <- df %>% select(-matches("MONTO\\.x|MONTO\\.y"))
  return(df)
}

# Aplicar la función de limpieza a cada dataframe en la lista
lista_info_pensionados_pensionados <- lapply(lista_info_pensionados_pensionados, limpiar_columnas_duplicadas)


sucesion_pensionados <- function(df){
  
  pension_sucesion <- rep(NA, nrow(df))
  
  
  for(i in 1: nrow(df)){
    tipo <- df$Tipo[i]
    
    if(tipo != "PS") {
      next
    }
    
    pension <- df$MONTO[i]
    edad <- df$Edad[i]
    duracion <-df$Duracion[i]
    edad_adq_pension <- edad - duracion
    parentesco <- df$COD_PARENTESCO[i]
    
    if(parentesco == "C"){
      if(edad_adq_pension >= 60){
        pension_sucesion[i]<- pension*0.7 
      }else if(edad_adq_pension >50 & edad_adq_pension < 60) {
        pension_sucesion[i]<- pension*0.6
      }else if(edad_adq_pension <= 50) {
        pension_sucesion[i] <- pension*0.5
      }
    } else{
      pension_sucesion[i] <- pension*0.3
    }
  }
  df$"Pension Sucesion" <- pension_sucesion
  return(df)
}


lista_info_pensionados_pensionados <- lapply(lista_info_pensionados_pensionados, sucesion_pensionados)


pension_pensionados <- function(df_pensionados) {
  
  años <- as.character(2023:2123)
  ID <- df_pensionados$ID
  Tipo <- df_pensionados$Tipo
  
  # Initialize the matrix with the correct dimensions
  num_years <- length(años)
  num_rows <- nrow(df_pensionados)
  monto_pensionados_matrix <- matrix(NA, nrow = num_rows, ncol = num_years + 2)
  
  # Fill the matrix with data
  monto_pensionados_matrix[, 1] <- ID
  monto_pensionados_matrix[, 2] <- Tipo
  monto_pensionados_matrix[, 3:(2 + num_years)] <- NA  # or other default values for years
  
  # Assign column names
  colnames(monto_pensionados_matrix) <- c("ID", "Tipo", años)
  
  # Convert matrix to data frame
  monto_pensionados <- as.data.frame(monto_pensionados_matrix, stringsAsFactors = FALSE)
  
  
  for(i in 1: nrow(df_pensionados)){
    duracion <- df_pensionados$Duracion[i]
    parentesco <- df_pensionados$COD_PARENTESCO[i]
    
    
    if(Tipo[i] == "PS") {
      pension <-df_pensionados$"Pension Sucesion"[i]
      if(parentesco == "C"){
        duracion_principal <- df_pensionados$Duracion[i-1] + 1
      }else{
        duracion_principal <- df_pensionados$Duracion[i-2] + 1 
      }
      duracion <- duracion_principal + duracion
      t <- duracion_principal : duracion
    }else{
      pension <- df_pensionados$MONTO[i]
      duracion_principal <- 0
      t <- 0:duracion
    }
    
    pension_inflada <- pension*(1+inflacion)^t
    
    pension_inflada[pension_inflada > 3500000] <- 3500000
    
    #print(pension_inflada)
    
    monto_pensionados[i, (duracion_principal + 3):(duracion+3)] <- pension_inflada
    #print(vector2)
    
    #same_length <- identical(length(pension_inflada), length(vector2))
    
    # Imprimir el resultado
    #print(same_length)
  }
  return(monto_pensionados)  
}

monto_pension_pensionados <- lapply(lista_info_pensionados_pensionados,pension_pensionados)

#pension_pensionados(lista_info_pensionados_pensionados[[1]])


#------------- Valor presente-------------------------------------------

ii <- (1+inflacion)*(1+ 0.07) - 1


process_pension_data <- function(df){
  
  # Convertir datos a formato largo para facilitar el procesamiento
  long_df <- df %>%
    pivot_longer(cols = starts_with("2024"):starts_with("2123"), 
                 names_to = "Año", 
                 values_to = "Monto") %>%
    filter(!is.na(Monto)) # Filtrar valores NA
  
  # Agrupar por Tipo y Año, y sumar los montos
  grouped_df <- long_df %>%
    group_by(Tipo, Año) %>%
    summarise(MontoTotal = sum(as.numeric(Monto), na.rm = TRUE)*13, .groups = 'drop')
  
  # Convertir Year a numérico
  grouped_df$Año <- as.numeric(grouped_df$Año)
  
  # Calcular el valor presente para cada monto al base_year
  grouped_df <- grouped_df %>%
    mutate(PV = MontoTotal / (1 + ii)^(Año - 2024)) %>%
    group_by(Tipo) %>%
    summarise(PV_Total = sum(PV, na.rm = TRUE), .groups = 'drop')
  
  return(grouped_df)
}

valor_presente_pension_pensionados <- lapply(monto_pension_pensionados, process_pension_data)
valor_presente_pension_activos <- lapply(monto_pension_activos, process_pension_data)
valor_presente_pension_inactivos <- lapply(monto_pension_inactivos, process_pension_data)

#Promedio valor presente de curso de pago por tipo 
calcular_promedios_VP_CP <- function(df) {
  
  df_filtrado <- df[df$Tipo != "PS",]
  df_filtrado %>%
    group_by(Tipo) %>%
    summarise(Promedio_PV_Total = mean(PV_Total, na.rm = TRUE))
  
}

combinado_VP_CP <- combinar_dfs(valor_presente_pension_pensionados)
promedio_VP_CP <- calcular_promedios_VP_CP(combinado_VP_CP)

#Pensiones en gen actual

VP_GenActual <- list()

# Iterar sobre los índices de los dataframes
for (i in seq_along(valor_presente_pension_activos)) {
  # Combina el dataframe correspondiente de lista1 y lista2
  df1 <- valor_presente_pension_activos[[i]]
  df2 <- valor_presente_pension_inactivos[[i]]
  
  # Combina ambos dataframes en uno solo
  df_combinado <- rbind(df1, df2)
  
  # Agrupa por la columna 'tipo' y suma las cantidades
  resultado <- aggregate(PV_Total ~ Tipo, data = df_combinado, FUN = sum)
  
  
  df_ps <- valor_presente_pension_pensionados[[i]]
  
  ps_valor <- df_ps$PV_Total[df_ps$Tipo == "PS"]
  
  resultado <- resultado %>%
    dplyr::mutate(PV_Total = ifelse(Tipo == "PS", PV_Total + ps_valor, PV_Total))
  
  # Almacena el resultado en la lista
  VP_GenActual[[i]] <- resultado
}

#Promedio Valor presente de la generacion actual por tipo 
calcular_promedios_VP <- function(df) {
  
  df %>%
    group_by(Tipo) %>%
    summarise(Promedio_PV_Total = mean(PV_Total, na.rm = TRUE))
  
}
combinado_VP_GenActual <- combinar_dfs(VP_GenActual)
promedio_VP_GenActual <- calcular_promedios_VP(combinado_VP_GenActual)

# Total pensiones por año, pensiones en curso de pago

# Función para filtrar y sumar
process_dataframe <- function(df) {
  
  # Eliminar las dos primeras columnas (suponiendo que no son necesarias)
  filtered_df <- df[,-c(1,2)]
  
  # Convertir todas las columnas a numérico
  filtered_df[] <- lapply(filtered_df, function(x) as.numeric(as.character(x)))
  
  # Sumar los elementos de las columnas numéricas
  sums <- colSums(filtered_df, na.rm = TRUE)
  return(sums)
}


# Aplicar la función a cada dataframe en la lista
Pensiones_CP <- lapply(monto_pension_pensionados, function(df) {
  filtered_df <- df[df$Tipo != "PS", ]
  process_dataframe(filtered_df) * 13
})

#Promedio por año CP
combinado_Pensiones_CP <- combinar_dfs(Pensiones_CP)
promedio_Pensiones_CP <- colMeans(combinado_Pensiones_CP)

#Total pensiones por año, pensiones generación actual

Pensiones_GenActual <- list()
GenActual <- list()

# Iterar sobre los índices de los dataframes
for (i in seq_along(monto_pension_activos)) {
  # Combina el dataframe correspondiente de lista1 y lista2
  df1 <- monto_pension_activos[[i]]
  df2 <- monto_pension_inactivos[[i]]
  df3 <- monto_pension_pensionados[[i]][monto_pension_pensionados[[i]]$Tipo == "PS",]
  
  # Combina ambos dataframes en uno solo
  df_combinado <- rbind(df1, df2,df3)
  
  # Agrupa por la columna 'tipo' y suma las cantidades
  resultado <- process_dataframe(df_combinado)
  
  
  # Almacena el resultado en la lista
  Pensiones_GenActual[[i]] <- resultado * 13
  GenActual[[i]] <- df_combinado
}

#Promedio por año Generacion Actual
combinado_Pensiones_GenActual <- combinar_dfs(Pensiones_GenActual)
promedio_Pensiones_GenActual <- colMeans(combinado_Pensiones_GenActual)

# Contribuciones pensiones mayores a dos millones 

#Gen Actual

GenActual <- lapply(GenActual, function(df) {
  df <- df[,-c(1:4)]
})

contribuciones_pensionados_superiores <- function(df) {
  df[] <- lapply(df, function(x) as.numeric(as.character(x)))
  resultado <- df %>%
    mutate(across(everything(), ~ if_else(!is.na(.), ifelse(. > 2000000, . * 0.05, 0), NA)))
  resultado <- resultado*13
}

contri_pensionados_superiores_GA <- lapply(GenActual, 
                                           contribuciones_pensionados_superiores)

#Curso de pago 

Curso_pago <- lapply(monto_pension_pensionados, function(df) {
  filtered_df <- df[df$Tipo != "PS", ]
  filtered_df <- filtered_df[,-c(1:4)]
})
contri_pensionados_superiores_CP <- lapply(Curso_pago, 
                                           contribuciones_pensionados_superiores)

#Valor presente de las contribuciones 
VP_contribuciones <- function(df){
  
  # Convertir datos a formato largo para facilitar el procesamiento
  long_df <- df %>%
    pivot_longer(cols = starts_with("2025"):starts_with("2123"), 
                 names_to = "Año", 
                 values_to = "Monto") %>%
    filter(!is.na(Monto)) # Filtrar valores NA
  
  # Agrupar por Tipo y Año, y sumar los montos
  grouped_df <- long_df %>%
    group_by(Año) %>%
    summarise(MontoTotal = sum(as.numeric(Monto), na.rm = TRUE), .groups = 'drop')
  
  # Convertir Year a numérico
  grouped_df$Año <- as.numeric(grouped_df$Año)
  
  # Calcular el valor presente para cada monto al base_year
  grouped_df <- grouped_df %>%
    mutate(PV = MontoTotal / (1 + ii)^(Año - 2024)) %>%
    summarise(PV_Total = sum(PV, na.rm = TRUE), .groups = 'drop')
  
  return(grouped_df)
}

VP_contribuciones_GA <- lapply(contri_pensionados_superiores_GA, 
                               VP_contribuciones)
VP_contribuciones_CP <- lapply(contri_pensionados_superiores_CP, 
                               VP_contribuciones)

#Promedio contribuciones valor presente
combinado_VP_contribuciones_GA <- combinar_dfs(VP_contribuciones_GA)
promedio_VP_contribuciones_GA <- colMeans(combinado_VP_contribuciones_GA)
combinado_VP_contribuciones_CP <- combinar_dfs(VP_contribuciones_CP)
promedio_VP_contribuciones_CP <- colMeans(combinado_VP_contribuciones_CP)

# SEM Generacion Actual
SEM_GA <- lapply(VP_GenActual, function(df) {
  sum(df[,2])*0.085*12/13
})

#SEM Curso de Pago
SEM_CP <- lapply(valor_presente_pension_pensionados, function(df) {
  sum(df[-2,2])*0.085*12/13
})

#Promedio
promedio_SEM_CP <- mean(unlist(SEM_CP))
promedio_SEM_GA <- mean(unlist(SEM_GA))

#----------------------Balance-------------------------------

PVC <- map(Balances_Activos[1:100], "PVC")
PVC_promedio <- mean(unlist(PVC))


# ACTIVO

#RESERVA PENSIONES EN CURSO DE PAGO
Reserva_PPCP <- 40930473298.35
## Contribuciones pensionados 
contribuciones_pensionados <- promedio_VP_contribuciones_CP

# ACTIVO GENERACIONES ACTUALES
## Reserva beneficios en formación
Reserva_BF <- 250858785692.97

## VPA Cotizaciones
VPA_cotizaciones <- PVC_promedio
## Contribuciones pensionados
contribuciones_pensionados_genActual <- promedio_VP_contribuciones_GA
Total_VPA_ACTIVO_GA <- VPA_cotizaciones + contribuciones_pensionados_genActual 

# PASIVO

# VPA Curso de Pago
## Vejez
Vejez_CP <- promedio_VP_CP$Promedio_PV_Total[3]
## Invalidez
Invalidez_CP <- promedio_VP_CP$Promedio_PV_Total[1]
## Muerte
Muerte_CP <- promedio_VP_CP$Promedio_PV_Total[2]

Total_VPA_CP <- Vejez_CP + Invalidez_CP + Muerte_CP


#VPA Costo Gen Actuales
## Vejez
Vejez_GA <- promedio_VP_GenActual$Promedio_PV_Total[4]
## Invalidez
Invalidez_GA <- promedio_VP_GenActual$Promedio_PV_Total[1]
## Muerte
Muerte_GA <- promedio_VP_GenActual$Promedio_PV_Total[2] + promedio_VP_GenActual$Promedio_PV_Total[3]

Total_VPA_GA <- Vejez_GA + Invalidez_GA + Muerte_GA

# OTROS GASTOS
## Costo Enfermedad y Maternidad (SEM)
### Curso
SEM_CursoPago <- promedio_SEM_CP
### Gen Actuales
SEM_GenActual <- promedio_SEM_GA
Total_Otros_Gastos <- SEM_CursoPago + SEM_GenActual
