#---------- Cálculo Pensiones -----------------------

top_300_prom <- function(df) {
  salario_referencia <- data.frame("ID"= character(), "Salario" = numeric())
  
  for(i in 1: nrow(df)) {
    ID <- df$ID[i]
    
    name <- 2023 + df$cont[i]
    col <- which(colnames(ProyeccionSalarios) == name)
    salarios <- t(ProyeccionSalarios[i, 3: col])
    
    top <- head(sort(salarios, decreasing = TRUE), 300)
    
    salario_prom <- mean(top)
    
    salario_referencia <- rbind(salario_referencia, 
                                     data.frame(ID = ID, 
                                                Salario = salario_prom))
  }
  return(salario_referencia)
}

salario_referencia_activos<- lapply(lista_pensionados_activos, top_300_prom)
salario_referencia_inactivos<- lapply(lista_pensionados_inactivos, top_300_prom)


Salario_minimo <- 11953.64*12



#----Pensión base-------------
cuantia <- function(df){
  
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
    pension  <- salario_df$`Pensión con postergacion`[i]
    parentesco <- df$COD_PARENTESCO[i]
    
    
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


#-------Pensionados--------------
