# Librerías necesarias
library(dplyr)
library(ggplot2)

# ----*Exploración de Datos*----

# 1. Crear una tabla de frecuencia de los códigos de mantenimiento
frecuencia_cod <- table(data$Cod)
df_frecuencia_cod <- as.data.frame(frecuencia_cod)
colnames(df_frecuencia_cod) <- c("Cod", "Frecuencia")

# Gráfico de barras de la frecuencia de códigos
ggplot(df_frecuencia_cod, aes(x = reorder(Cod, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  xlab("Códigos") +
  ylab("Frecuencia") +
  ggtitle("Frecuencia de Códigos") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Suma de facturas por tipo de mantenimiento
facturas_por_tipo <- aggregate(data$factura, by = list(data$Cod), FUN = sum)
colnames(facturas_por_tipo) <- c("Tipo", "Suma_Facturas")

# 3. Crear una tabla de frecuencia de la variable 'Origen'
frecuencia_origen <- table(data$origen)
df_frecuencia_origen <- as.data.frame(frecuencia_origen)
colnames(df_frecuencia_origen) <- c("Origen", "Frecuencia")

# Gráfico de barras de la frecuencia por origen
ggplot(df_frecuencia_origen, aes(x = Origen, y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Frecuencia por Origen", x = "Origen", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Suma de facturas por ID
suma_facturas_por_ID <- aggregate(data$factura, by = list(data$ID), FUN = sum)
colnames(suma_facturas_por_ID) <- c("ID", "Suma_Facturas")
suma_facturas_por_ID_ordenada <- suma_facturas_por_ID[order(-suma_facturas_por_ID$Suma_Facturas), ]

# Top 5 postes con la suma más alta de facturas
top_5_postes <- head(suma_facturas_por_ID_ordenada, 5)

# 5. Suma de gastos fijos y directos de los top 5 postes
postes_seleccionados <- data[data$ID %in% top_5_postes$ID, ]
suma_gastos_por_poste <- postes_seleccionados %>%
  group_by(ID) %>%
  summarize(
    Suma_Fijo = sum(fijoCamion + fijoPickup + fijoMoto, na.rm = TRUE),
    Suma_Directo = sum(directoCamion + directoPickup + directoMoto, na.rm = TRUE)
  )

suma_gastos_por_poste$total_gastos <- suma_gastos_por_poste$Suma_Fijo + suma_gastos_por_poste$Suma_Directo

# Poste más costoso
poste_mas_costoso <- suma_gastos_por_poste[which.max(suma_gastos_por_poste$total_gastos), ]

# Mostrar resultados
print(poste_mas_costoso)

# 6. Comparación de facturas y gastos de los top 5 postes
comparacion_top_5 <- merge(top_5_postes, suma_gastos_por_poste, by = "ID")
comparacion_top_5$Profit <- comparacion_top_5$Suma_Facturas - comparacion_top_5$total_gastos

# Mostrar la tabla con el profit
print(comparacion_top_5)

# 7. Costos por tipo de vehículo
costos_por_tipo <- data %>%
  group_by(Cod) %>%
  summarize(
    Total_DirectoCamion = sum(directoCamion, na.rm = TRUE),
    Total_DirectoPickup = sum(directoPickup, na.rm = TRUE),
    Total_DirectoMoto = sum(directoMoto, na.rm = TRUE),
    Total_FijoCamion = sum(fijoCamion, na.rm = TRUE),
    Total_FijoPickup = sum(fijoPickup, na.rm = TRUE),
    Total_FijoMoto = sum(fijoMoto, na.rm = TRUE)
  )

costos_por_tipo$Total_Camion <- costos_por_tipo$Total_DirectoCamion + costos_por_tipo$Total_FijoCamion
costos_por_tipo$Total_Pickup <- costos_por_tipo$Total_DirectoPickup + costos_por_tipo$Total_FijoPickup
costos_por_tipo$Total_Moto <- costos_por_tipo$Total_DirectoMoto + costos_por_tipo$Total_FijoMoto

# Gráfico de barras de costos totales por tipo de vehículo
costos_sumarios <- data.frame(
  Tipo = c("Camion", "Pickup", "Moto"),
  Costo_Total = c(
    sum(costos_por_tipo$Total_Camion),
    sum(costos_por_tipo$Total_Pickup),
    sum(costos_por_tipo$Total_Moto)
  )
)

ggplot(costos_sumarios, aes(x = Tipo, y = Costo_Total, fill = Tipo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Costo_Total, 2)), vjust = -0.5, size = 3) +
  labs(title = "Costos Totales por Tipo de Vehículo", y = "Costo Total") +
  scale_fill_manual(values = c("darkblue", "darkgreen", "darkred")) +
  theme_minimal()

# Crear una nueva columna "Tipo" en función de las observaciones de los vehículos
data$Tipo <- ifelse(!is.na(data$Moto) & data$Moto > 0, "Moto", 
                    ifelse(!is.na(data$Camion) & data$Camion > 0, "Camion", 
                           ifelse(!is.na(data$Pickup) & data$Pickup > 0, "Pickup", NA)))

# Verificar la estructura de la nueva columna "Tipo"
table(data$Tipo)

# 8. Calcular y visualizar el profit por tipo de vehículo
facturacion_por_tipo <- data %>%
  group_by(Tipo) %>%
  summarize(
    Suma_Facturacion = sum(factura, na.rm = TRUE),
    Suma_Fijo = sum(fijoCamion + fijoPickup + fijoMoto, na.rm = TRUE),
    Suma_Directo = sum(directoCamion + directoPickup + directoMoto, na.rm = TRUE)
  )

facturacion_por_tipo$Suma_Total_Costos <- facturacion_por_tipo$Suma_Fijo + facturacion_por_tipo$Suma_Directo
facturacion_por_tipo$Profit <- facturacion_por_tipo$Suma_Facturacion - facturacion_por_tipo$Suma_Total_Costos

# Mostrar la tabla con resultados
print(facturacion_por_tipo)

# Crear un gráfico de barras de facturación y costos por tipo de vehículo
ggplot(facturacion_por_tipo, aes(x = Tipo)) +
  geom_bar(aes(y = Suma_Facturacion, fill = "Facturación"), stat = "identity") +
  geom_bar(aes(y = Suma_Total_Costos, fill = "Costos"), stat = "identity") +
  labs(title = "Facturación y Costos por Tipo de Vehículo", y = "Valor") +
  scale_fill_manual(values = c("Facturación" = "blue", "Costos" = "red")) +
  theme_minimal()

