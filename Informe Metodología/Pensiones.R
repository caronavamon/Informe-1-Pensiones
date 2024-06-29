#---------- Cálculo Pensiones -----------------------

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
      salario_cuantia[i] <- 0.525*salario
    }else if(salario >= 2*Salario_minimo & salario < 3*Salario_minimo) {
      salario_cuantia[i] <-  0.51*salario
    } else if(salario >= 3*Salario_minimo & salario < 4*Salario_minimo){
      salario_cuantia[i] <- 0.494*salario
    } else if(salario >= 4*Salario_minimo & salario < 5*Salario_minimo){
      salario_cuantia[i] <- 0.478*salario
    }else if(salario >= 5*Salario_minimo & salario < 6*Salario_minimo){
      salario_cuantia[i] <- 0.462*salario
    }else if(salario >= 6*Salario_minimo & salario < 8*Salario_minimo){
      salario_cuantia[i] <- 0.446*salario
    }else if(salario >= 8*Salario_minimo){
      salario_cuantia[i] <- 0.43*salario
    }
    
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
        pension_bonificacion[i] <- pension + salario*0.000833*cotizaciones_extra
      } else {
        pension_bonificacion[i] <- pension
      }
    }else{
      pension_bonificacion[i] <- pension
    }
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
        pension_postergacion[i] <- pension + salario* 0.001333*postergacion_tiempo
      } else{
        pension_postergacion[i] <- pension
      }
    }else {
      pension_postergacion[i] <- pension
    }
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
          pension_sucesion[i] <- pension*0.7 
        }else if(edad >50 & edad < 60) {
          pension_sucesion[i] <- pension*0.6
        }else if(edad <= 50) {
          pension_sucesion[i] <- pension*0.5
        }
      } else{
        pension_sucesion[i] <- pension*0.3
      }
    }else {
      pension_sucesion[i] <- NA
    }
  }
  return(pension_sucesion)
}




for(k in 1: length(pension_base_activos)) {
  pension_base_activos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_activos[[k]], pension_base_activos[[k]])
  pension_base_inactivos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_inactivos[[k]], pension_base_inactivos[[k]])
}


#------- Pension -----------

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
  
  pension_sucesion <- c(NA)
  
  
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
        pension_sucesion[i] <- pension*0.7 
      }else if(edad_adq_pension >50 & edad_adq_pension < 60) {
        pension_sucesion[i] <- pension*0.6
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
    summarise(MontoTotal = sum(as.numeric(Monto), na.rm = TRUE), .groups = 'drop')
  
  # Convertir Year a numérico
  grouped_df$Año <- as.numeric(grouped_df$Año)
  
  # Calcular el valor presente para cada monto al base_year
  grouped_df <- grouped_df %>%
    mutate(PV = MontoTotal / (1 + inflacion)^(Año - 2024)) %>%
    group_by(Tipo) %>%
    summarise(PV_Total = sum(PV, na.rm = TRUE), .groups = 'drop')
  
  return(grouped_df)
}

valor_presente_pension_pensionados <- lapply(monto_pension_pensionados, process_pension_data)



