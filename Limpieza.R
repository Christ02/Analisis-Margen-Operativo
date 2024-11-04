# Librerias
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)

data <- read_csv("C:/Users/barri/Documents/UFM/2024/Segundo Semestre/Data Wrangling/Laboratorio8/c1.csv")
View(data)

# Limpieza de Datos

# Convertir la columna 'Fecha' al formato de fecha correcto
data$Fecha <- as.Date(data$Fecha, format = "%d-%m-%y")
head(data$Fecha)

# Camion 

data$Camion_5 <- gsub("Q-", "", data$Camion_5)
data$Camion_5 <- gsub("Q", "", data$Camion_5)
data$Camion_5[data$Camion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$Camion_5 <- as.numeric(data$Camion_5)
class(data$Camion_5)
colnames(data)[colnames(data) == "Camion_5"] <- "Camion"

# Pickup

data$Pickup <- gsub("Q-", "", data$Pickup)
data$Pickup <- gsub("Q", "", data$Pickup)
data$Pickup[data$Pickup == ""] <- NA  # Asigna NA a valores vacíos
data$Pickup <- as.numeric(data$Pickup)
class(data$Pickup)

# Moto

data$Moto <- gsub("Q-", "", data$Moto)
data$Moto <- gsub("Q", "", data$Moto)
data$Moto[data$Moto == ""] <- NA  # Asigna NA a valores vacíos
data$Moto <- as.numeric(data$Moto)
class(data$Moto)

# Factura

data$factura <- gsub("Q-", "", data$factura)
data$factura <- gsub("Q", "", data$factura)
data$factura[data$factura == ""] <- NA  # Asigna NA a valores vacíos
data$factura <- as.numeric(data$factura)
class(data$factura)

# directoCamion_5

data$directoCamion_5 <- gsub("Q-", "", data$directoCamion_5)
data$directoCamion_5 <- gsub("Q", "", data$directoCamion_5)
data$directoCamion_5[data$directoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$directoCamion_5 <- as.numeric(data$directoCamion_5)
class(data$directoCamion_5)
colnames(data)[colnames(data) == "directoCamion_5"] <- "directoCamion"

# directoPickup

data$directoPickup <- gsub("Q-", "", data$directoPickup)
data$directoPickup <- gsub("Q", "", data$directoPickup)
data$directoPickup[data$directoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$directoPickup <- as.numeric(data$directoPickup)
class(data$directoPickup)

# directoMoto

data$directoMoto<- gsub("Q-", "", data$directoMoto)
data$directoMoto <- gsub("Q", "", data$directoMoto)
data$directoMoto[data$directoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$directoMoto <- as.numeric(data$directoMoto)
class(data$directoMoto)

# fijoCamion_5

data$fijoCamion_5 <- gsub("Q-", "", data$fijoCamion_5)
data$fijoCamion_5 <- gsub("Q", "", data$fijoCamion_5)
data$fijoCamion_5[data$fijoCamion_5 == ""] <- NA  # Asigna NA a valores vacíos
data$fijoCamion_5 <- as.numeric(data$fijoCamion_5)
class(data$fijoCamion_5)
colnames(data)[colnames(data) == "fijoCamion_5"] <- "fijoCamion"

# fijoPickup

data$fijoPickup<- gsub("Q-", "", data$fijoPickup)
data$fijoPickup <- gsub("Q", "", data$fijoPickup)
data$fijoPickup[data$fijoPickup == ""] <- NA  # Asigna NA a valores vacíos
data$fijoPickup <- as.numeric(data$fijoPickup)
class(data$fijoPickup)

# fijoMoto

data$fijoMoto<- gsub("Q-", "", data$fijoMoto)
data$fijoMoto <- gsub("Q", "", data$fijoMoto)
data$fijoMoto[data$fijoMoto == ""] <- NA  # Asigna NA a valores vacíos
data$fijoMoto <- as.numeric(data$fijoMoto)
class(data$fijoMoto)

# Conversión de X a Booleanos

# 5-30 minutos

data[["5-30"]] <- gsub("x", "TRUE", data[["5-30"]])
data[["5-30"]][is.na(data[["5-30"]]) | data[["5-30"]] == ""] <- "FALSE"

data[["5-30"]] <- as.logical(data[["5-30"]])

head(data[["5-30"]])
class(data[["5-30"]])

# 30 - 45 minutos

data[["30-45"]] <- gsub("x", "TRUE", data[["30-45"]])
data[["30-45"]][is.na(data[["30-45"]]) | data[["30-45"]] == ""] <- "FALSE"
data[["30-45"]] <- as.logical(data[["30-45"]])
head(data[["30-45"]])
class(data[["30-45"]])


# 45 - 75 minutos

data[["45-75"]] <- gsub("x", "TRUE", data[["45-75"]])
data[["45-75"]][is.na(data[["45-75"]]) | data[["45-75"]] == ""] <- "FALSE"
data[["45-75"]] <- as.logical(data[["45-75"]])
head(data[["45-75"]])
class(data[["45-75"]])


# 75 - 120 minutos

data[["75-120"]] <- gsub("x", "TRUE", data[["75-120"]])
data[["75-120"]][is.na(data[["75-120"]]) | data[["75-120"]] == ""] <- "FALSE"
data[["75-120"]] <- as.logical(data[["75-120"]])
head(data[["75-120"]])
class(data[["75-120"]])


# Más de 120 minutos

data[["120+"]] <- gsub("x", "TRUE", data[["120+"]])
data[["120+"]][is.na(data[["120+"]]) | data[["120+"]] == ""] <- "FALSE"
data[["120+"]] <- as.logical(data[["120+"]])
head(data[["120+"]])
class(data[["120+"]])


# Eliminar columnas que no sirven de nada 
data <- data[, !names(data) %in% c("...23", "...24", "...25", "...26", "...27", "...28")]

# Crear la columna 'directos' sumando los costos directos
data$directos <- rowSums(data[, c("directoCamion", "directoPickup", "directoMoto")], na.rm = TRUE)

# Crear la columna 'fijos' sumando los costos fijos
data$fijos <- rowSums(data[, c("fijoCamion", "fijoPickup", "fijoMoto")], na.rm = TRUE)

# Crear la columna 'total_costos' sumando los directos y fijos
data$total_costos <- data$directos + data$fijos

# Crear la columna 'ebitda' restando total_costos de la factura
data$ebitda <- data$factura - data$total_costos



write.csv(data, file = "C:/Users/barri/Documents/UFM/2024/Segundo Semestre/Data Wrangling/Laboratorio8/data.csv", row.names = FALSE)
