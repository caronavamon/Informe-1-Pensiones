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

