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




pension_base_pensionados <-  rep(list(pension_sucesion_pensionados),interaciones)

for(k in 1: length(pension_base_activos)) {
  pension_base_activos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_activos[[k]], pension_base_activos[[k]])
  pension_base_inactivos[[k]]$"Pensión Sucesión" <- sucesion(lista_pensionados_inactivos[[k]], pension_base_inactivos[[k]])
  pension_base_pensionados[[k]]$"Pensión Sucesión" <-sucesion(lista_) 
}


#-------Pensionados--------------


sucesion_pensionados <- function(df){
  
  
  pension_sucesion_pensionados <- data.frame("ID"= character(), "Tipo" = character(),
                                             "Monto" = numeric(), "Pensión Sucesión" = numeric())
  for(i in 1: nrow(pensionados_data)){
    
    tipo <- "PS"
    
    duracion <- df$Duracion[df$ID == pensionados_data$ID[i]]
    
    ID <- df$ID[df$ID == pensionados_data$ID[i] & df$Tipo == "PS"]
    
    if(is.vector(ID) == TRUE ){
      parentesco <- df$COD_PARENTESCO[df$ID == pensionados_data$ID[i]][1]
      parentesco_h <- df$COD_PARENTESCO[df$ID == pensionados_data$ID[i]][2]
      ID <- ID[1]
    } 
    
    edad_principal <- pensionados_data$Edad[pensionados_data$ID == ID]
    edad_c <- edad_principal + duracion
    Monto <- pensionados_data$MONTO[pensionados_data$ID == ID]
    parentesco <- df$COD_PARENTESCO[df$ID == pensionados_data$ID[i]]
  

    if(tipo == "Sucesión") {
      if(parentesco == "C"){
        if(edad-c >= 60){
          pension_sucesion <- pension*0.7 
        }else if(edad_c >50 & edad_c < 60) {
          pension_sucesion <- pension*0.6
        }else if(edad_c <= 50) {
          pension_sucesion<- pension*0.5
        }
        
        pension_sucesion_pensionados <- rbind(pension_sucesion_pensionados, 
                                              data.frame(ID = ID, 
                                                         Tipo = Tipo,
                                                         Monto = Monto,
                                                         "Pensión Sucesión" = pension_sucesion))
      } else{
        pension_sucesion <- pension*0.3
        pension_sucesion_pensionados <- rbind(pension_sucesion_pensionados, 
                                              data.frame(ID = ID, 
                                                         Tipo = Tipo,
                                                         Monto = Monto,
                                                         "Pensión Sucesión" = pension_sucesion))
      }
    }else {
      pension_sucesion <- NA
    }
  }
  return(pension_sucesion)
}



pension_sucesion_pensionados <- lapply(lista_info_pensionados, sucesion_pensionados)




pension_pensionados <- function(df_pensionados) {
  
  años <- as.character(2024:2123)
  monto <- pensionados_data$MONTO
  ID <- pensionados_data$ID
  Tipo <- pensionados_data$COD_TIPO_PENSION
  
  # Initialize the matrix with the correct dimensions
  num_years <- length(años)
  num_rows <- length(monto)
  monto_pensionados_matrix <- matrix(NA, nrow = num_rows, ncol = num_years + 3)
  
  # Fill the matrix with data
  monto_pensionados_matrix[, 1] <- ID
  monto_pensionados_matrix[, 2] <- Tipo
  monto_pensionados_matrix[, 3] <- monto
  monto_pensionados_matrix[, 4:(3 + num_years)] <- NA  # or other default values for years
  
  # Assign column names
  colnames(monto_pensionados_matrix) <- c("ID", "Tipo", "Monto", años)
  
  # Convert matrix to data frame
  monto_pensionados <- as.data.frame(monto_pensionados_matrix, stringsAsFactors = FALSE)
  
  
  for(i in 1: nrow(pensionados_data)){
    duracion <- df_pensionados$Duracion[df_pensionados$ID == pensionados_data$ID[i] & 
                                          df_pensionados$Tipo != "PS"]
    t <- 1:duracion
    
    pension <- monto[i]*(1+inflacion)^t
    monto_pensionados[i,4:(duracion+3)] <- pension
    
  }
  return(monto_pensionados)  
}

monto_pension_pensionados <- lapply(lista_info_pensionados,pension_pensionados)



