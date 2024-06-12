library(readxl)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

# Inflación

inflacion <- read.csv("Inflacion.csv", sep = ";", dec = ",")
mean(inflacion$Variacion.interanual....)
mean(inflacion$Variacion.mensual....)

# Escala Salarial 

activos <- read_excel("Fondo C.xlsx", sheet = "Activos")
activos <- activos[, -c(364, 365)]
activos_salarios <- activos[, -c(1, 2, 3)]
activos_salarios[activos_salarios <= 10000] <- 0
inflacion <- 0.04
inicio <- as.Date("01/01/1994",format="%d/%m/%Y")
fin <- as.Date("01/12/2023",format="%d/%m/%Y")
fechas <- seq(inicio,fin,by="month")
fechas <- fechas[format(fechas, "%m") != "04"]
activos <- activos %>%
  select(-grep("abr/", names(activos)))
activos_salarios <- activos_salarios %>%
  select(-grep("abr/", names(activos_salarios)))
rownames(activos_salarios) <- activos$ID

# Quitamos los salarios atipicos 
conteo_atipicos <- apply(activos[,-(1:3)], 1, function(row) sum(row > 5000000))
conteo_atipicos <- as.data.frame(cbind(ID = activos$ID, Cantidad = conteo_atipicos))
conteo_atipicos <- conteo_atipicos %>% filter(Cantidad > 0)

atipicos <- conteo_atipicos %>%
  filter(Cantidad <= 3) %>%
  pull(ID)

activos_salarios[rownames(activos_salarios) %in% atipicos, ] <- apply(activos_salarios[rownames(activos_salarios) %in% atipicos, ], 2, function(x) ifelse(x > 5000000, 0, x))


# Crear una matriz para almacenar los resultados
activos_VP <- matrix(0, nrow = nrow(activos_salarios), 
                     ncol = ncol(activos_salarios))

# Calcular el intervalo de meses entre la primera fecha y las demás
meses <- as.numeric(interval(fechas, fechas[330]) %/% months(1))

# Calcular el factor de acumulacion 
factor_acumulacion <- (1 + inflacion / 12) ^ meses

# Aplicar el factor de acumulacion 
activos_VP <- activos_salarios * factor_acumulacion
colnames(activos_VP) <- fechas
activos_VP <- cbind(ID = activos$ID, Fec.Nac = activos$Fec.Nac, activos_VP)

# Calcular cuando la persona inicia a tener un salario
activos_efectivos <- as.matrix(activos_salarios >= 10000, 
                               nrow = nrow(activos_salarios), 
                               ncol = ncol(activos_salarios))
activos_efectivos<- cbind(ID = activos$ID, activos_efectivos)
activos_efectivos<- as.data.frame(activos_efectivos)

años <- year(fechas)
names(activos_efectivos)[-1] <- años

# Transformar de wide a long format
activos_efectivos_long <- pivot_longer(activos_efectivos, cols = -ID, 
                                       names_to = "Año", values_to = "Valor")

# Agrupar por ID y Año, luego resumir para ver si hay algún TRUE en cada año
activos_efectivos_agrupado <- activos_efectivos_long %>%
  group_by(ID, Año) %>%
  summarize(Valor = any(Valor == TRUE), .groups = 'drop')

# Hacer una dataframe con ID y fecha de nacimiento 
nacimiento <- activos[,1:2]
nacimiento$Fec.Nac <- year(nacimiento$Fec.Nac)

# Se encuentran las edades 
edades <- merge(activos_efectivos_agrupado, nacimiento, by = "ID")
edades$Año <- as.numeric(edades$Año)
edades <- edades %>%
  group_by(ID) %>%
  mutate(
    primer_true = cumsum(Valor),
    edad = if_else(primer_true > 0, Año - Fec.Nac, 0)
  )

datos_salarios <- activos_VP %>%
  select(-Fec.Nac) %>%
  pivot_longer(cols = -c(ID), names_to = "Fecha", values_to = "Salario") %>%
  separate(Fecha, into = c("Año", "mes", "dia"), sep = "-") %>%
  group_by(ID, Año) %>%
  summarise(Salario = mean(Salario[Salario != 0]), .groups = "drop")

edades <- edades[,-(3:5)]
salarios_edad <-  merge(datos_salarios, edades, by = c("ID", "Año"))

rango_edades <- 19:78
salarios_promedio <- data.frame(matrix(ncol = length(rango_edades), 
                                       nrow = nrow(activos)))
colnames(salarios_promedio) <- rango_edades
salarios_promedio <- cbind(ID = activos$ID, salarios_promedio)

for (i in 1:nrow(salarios_edad)) {
  # Obtenemos el ID, el salario y la edad correspondiente a la fila actual
  id <- salarios_edad[i, 1]
  salario <- salarios_edad[i, 3]
  edad <- salarios_edad[i, 4]
  
  # Buscamos la columna correspondiente a la edad y asignamos el salario
  nombre_columna <- as.character(edad)
  indice <- which(colnames(salarios_promedio) == nombre_columna)
  salarios_promedio[salarios_promedio$ID == id, indice] <- salario
}

salarios_promedio[is.na(salarios_promedio)] <- 0

promedio <- function(x) {
  # Excluimos los ceros y los valores NA
  x <- x[x != 0 & !is.na(x)]
  # Calculamos el promedio
  if(length(x) == 0) {
    return(NA)  # Si no hay valores, devolvemos NA
  } else {
    return(mean(x))
  }
}

# Aplicamos la función a cada columna del dataframe
promedios_edad <- apply(salarios_promedio[,-1], 2, promedio)
promedios_edad <- as.data.frame(promedios_edad)
promedios_edad <- cbind(edad = rango_edades, promedios_edad)

# Sacamos la variacion 
promedios_edad$Variacion <- c(NA, (promedios_edad$promedios_edad[-1] / promedios_edad$promedios_edad[-nrow(promedios_edad)]) - 1)

# Graficamos 
ggplot(promedios_edad, aes(x = edad, y = Variacion)) +
  geom_line(color = "#2F4F4F") +           
  geom_smooth(method = "loess", se = FALSE, color = "cadetblue3") +
  labs(title = "Variación del Salario Promedio por Edad",
       x = "Edad",
       y = "Variación del Salario Promedio") +
  theme_cowplot() 

ggplot(promedios_edad, aes(x = edad, y = promedios_edad)) +
  geom_bar(stat = "identity", fill = "#2F4F4F", color = "white", alpha = 0.7) +  
  labs(title = "Variación del Salario Promedio por Edad",
       x = "Edad",
       y = "Variación del Salario Promedio") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_cowplot()

table(salarios_edad$edad)

# Densidad de cotización 

activos_efectivos_long$Valor <- as.logical(activos_efectivos_long$Valor)
cotizaciones_año <- activos_efectivos_long %>%
  group_by(ID, Año) %>%
  summarise(Cotizaciones = sum(Valor), .groups = 'drop')

cotizaciones_año <-  merge(cotizaciones_año, edades, by = c("ID", "Año"))

cotizaciones_promedio <- data.frame(matrix(ncol = length(rango_edades), 
                                       nrow = nrow(activos)))
colnames(cotizaciones_promedio) <- rango_edades
cotizaciones_promedio <- cbind(ID = activos$ID, cotizaciones_promedio)

for (i in 1:nrow(cotizaciones_año)) {
  id <- cotizaciones_año[i, 1]
  cantidad <- cotizaciones_año[i, 3]
  edad <- cotizaciones_año[i, 4]
  
  # Buscamos la columna correspondiente a la edad y asignamos la cotizacion
  nombre_columna <- as.character(edad)
  indice <- which(colnames(cotizaciones_promedio) == nombre_columna)
  cotizaciones_promedio[cotizaciones_promedio$ID == id, indice] <- cantidad
}

cotizaciones_promedio[is.na(cotizaciones_promedio)] <- 0

promedios_cotizaciones_edad <- apply(cotizaciones_promedio[,-1], 2, promedio)
promedios_cotizaciones_edad <- as.data.frame(promedios_cotizaciones_edad)
promedios_cotizaciones_edad  <- cbind(edad = rango_edades, promedios_cotizaciones_edad)
names(promedios_cotizaciones_edad)[2] <- "Cotizaciones"
promedios_cotizaciones_edad$Cotizaciones <- round(promedios_cotizaciones_edad$Cotizaciones)

ggplot(promedios_cotizaciones_edad, aes(x = edad, y = Cotizaciones)) +
  geom_bar(stat = "identity", fill = "#2F4F4F", color = "white", alpha = 0.7) +  
  labs(title = "Cotizaciones Promedio por Edad",
       x = "Edad",
       y = "Cotizaciones promedio") +
  theme_cowplot()

# Rendimiento 

# Anual

financiero <- read_excel("Fondo C.xlsx", sheet = "Financiero")

rendimiento <- function(RI, RF, C, P){
  rend <- (RF - RI + P - C)/(RI-((P+C)/2))
  return(rend)
}

financiero <- financiero %>%
  mutate(Año = year(Período))
lista_año <- split(financiero, financiero$Año)
rendimientos_anuales <- c()

for (i in 1:(length(lista_año) - 1)) {
  observaciones <- lista_año[i:(i + 1)]
  RI <- observaciones[[1]][1,2]
  RF <- observaciones[[2]][1,2]
  C <- sum(observaciones[[1]][,4])
  P <- sum(observaciones[[1]][,5])
  rendimientos_anuales[i] <- rendimiento(RI, RF, C, P)
}

rendimientos_anuales <- unlist(rendimientos_anuales)

financiero_rendimiento <- cbind(Año = c(2013:2022), 
                                Rendimientos = rendimientos_anuales)
financiero_rendimiento <- as.data.frame(financiero_rendimiento)
promedio_rendimiento <- mean(financiero_rendimiento$Rendimientos)

ggplot(financiero_rendimiento, aes(x = Año, y = Rendimientos)) +
  geom_line(color = "#2F4F4F") + 
  geom_point(color = "#2F4F4F") +
  geom_hline(yintercept = promedio_rendimiento, linetype = "dashed", 
             color = "cadetblue3") +
  labs(title = "Rendimientos anuales del fondo",
       x = "Año",
       y = "Rendimiento") +
  theme_cowplot()
