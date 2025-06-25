library(tidyverse)
library(viridis)
library(hms)
library(leaflet)
library(ggcorrplot)

# Cargar la base limpia

base <- read_excel("data/base_agua_limpia.xlsx")
view(base)

# Resumen general de la base de datos

str(base)

# Extra: volver a poner las categóricas como factores y la fecha como Date por que
# al pasarse a excel pierde esta característica.

base$sitio <- as.factor(base$sitio)
base$cuerpo <- as.factor(base$cuerpo)
base$ubi_muestra <- as.factor(base$ubi_muestra)
base$analisis_agua1 <- as.factor(base$analisis_agua1)
base$fecha <- as.Date(base$fecha)
base$hora <- as_hms(base$hora)

summary(base)

# Tablas de frecuencia para variables categóricas

table(base$sitio)
table(base$ubi_muestra)
table(base$cuerpo)
table(base$analisis_agua1)

# Gráficos

### Distribución de la demanda biológica de oxígeno antes del tratamiento

graf_1 <- base %>% ggplot(aes(x=dbof))+
  geom_histogram(binwidth = 5, fill = viridis(1), color = "black", alpha= 0.8)+
  labs(title = "Distribución del DBOF (Demanda biológica de Oxígeno)",
       x = "DBO (mg/L) Antes del Tratamiento",
       y = "Frecuencia") + 
  theme_minimal()

print(graf_1)

### Distribución de la demanda biológica de oxígeno posterior al tratamiento

graf_2 <- base %>% ggplot(aes(x=dboj))+
  geom_histogram(binwidth = 5, fill = viridis(1), color = "black", alpha= 0.8)+
  labs(title = "Distribución del DBOJ (Demanda biológica de Oxígeno)",
       x = "DBO (mg/L) Posterior del Tratamiento",
       y = "Frecuencia") + 
  theme_minimal()

print(graf_2)

### Boxplot de la demanda biológica de oxígeno antes al tratamiento

graf_3 <- base %>% 
  filter(!is.na(cuerpo)) %>% filter(dbof < 50) %>% ggplot(aes(x = cuerpo, y = dbof, fill = cuerpo))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_viridis_d()+
  labs(title = "DBO antes del tratamiento\n según el cuerpo de agua",
       x = "Cuerpo de agua",
       y = "DBO (mg/L)")+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none")

print(graf_3)

### Boxplot de la demanda biológica de oxígeno posterior al tratamiento

graf_4 <- base %>% 
  filter(!is.na(cuerpo)) %>% filter(dboj < 50) %>% ggplot(aes(x = cuerpo, y = dboj, fill = cuerpo))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_viridis_d()+
  labs(title = "DBO posterior del tratamiento\n según el cuerpo de agua",
       x = "Cuerpo de agua",
       y = "DBO (mg/L)")+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none")

print(graf_4)

### Distribución de la demanda química de oxígeno antes del tratamiento

graf_5 <- base %>% ggplot(aes(x=dqof))+
  geom_histogram(binwidth = 65, fill = viridis(1), color = "black", alpha= 0.8)+
  labs(title = "Distribución del DQOF (Demanda química de Oxígeno)",
       x = "DQO (mg/L) Antes del Tratamiento",
       y = "Frecuencia") + 
  theme_minimal()

print(graf_5)

### Distribución de la demanda biológica de oxígeno posterior al tratamiento

graf_6 <- base %>% ggplot(aes(x=dqoj))+
  geom_histogram(binwidth = 150, fill = viridis(1), color = "black", alpha= 0.8)+
  labs(title = "Distribución del DQOJ (Demanda química de Oxígeno)",
       x = "DBO (mg/L) Posterior del Tratamiento",
       y = "Frecuencia") + 
  theme_minimal()

print(graf_6)

### Boxplot de la demanda biológica de oxígeno antes al tratamiento

graf_7 <- base %>% 
  filter(!is.na(cuerpo))  %>% ggplot(aes(x = cuerpo, y = dbof, fill = cuerpo))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_viridis_d()+
  labs(title = "DBO antes del tratamiento\n según el cuerpo de agua",
       x = "Cuerpo de agua",
       y = "DBO (mg/L)")+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none")

print(graf_7)

### Boxplot de la demanda biológica de oxígeno posterior al tratamiento

graf_8 <- base %>% 
  filter(!is.na(cuerpo)) %>% filter(dqoj < 50) %>% ggplot(aes(x = cuerpo, y = dboj, fill = cuerpo))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_viridis_d()+
  labs(title = "DQO posterior del tratamiento\n según el cuerpo de agua",
       x = "Cuerpo de agua",
       y = "DQO (mg/L)")+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none")

print(graf_8)

### Boxplot de DBO según la calidad del agua

graf_9 <- base %>% 
  ggplot(aes(x= oxigeno, y = dqof)) + 
  geom_point(size = 3, alpha = 0.8)+
  scale_color_viridis() + 
  labs(title = "Oxígeno disuelto vs DBO",
       x = "Oxígeno disuelto mg/L", 
       y = "DBO mg/L")+
  theme_minimal(base_size = 15)

print(graf_9)

graf_10 <- base %>% 
  ggplot(aes(x=fecha, y = dbof, color = sitio))+
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  scale_color_viridis_d() +
labs(title = "Evolución del DBO en el tiempo por sitio",
     x = "Fecha",
     y = "DBO mg/L")+
  theme_minimal(base_size = 15)

print(graf_10)

### Gráfico interactivo de mapa

variable_mapa <- "dqof"

datos_mapa <- base %>%
  mutate(
    valor = .data[[variable_mapa]],
    rango_auto = cut(
      valor,
      breaks = pretty(range(valor, na.rm = TRUE), n = 10),
      include.lowest = TRUE,
      dig.lab = 10   )
  )

paleta <- colorFactor(
  palette = "YlOrRd",
  domain = datos_mapa$rango_auto
)

leaflet(datos_mapa) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitud,
    lat = ~latitud,
    color = ~paleta(rango_auto),
    fillColor = ~paleta(rango_auto),
    fillOpacity = 0.85,
    radius = 6,
    stroke = FALSE,
    label = ~paste("Sitio:", sitio,
                   "Cuerpo:", cuerpo,
                   paste0(" ", toupper(variable_mapa), ": ", valor, " mg/L")),
    popup = ~paste0("<strong>Ubicación:</strong> ", sitio,
                    "<strong>Cuerpo:</strong> ", cuerpo,
                    "<strong>", toupper(variable_mapa), ":</strong> ", valor, " mg/L")
  ) %>%
  addLegend(
    "bottomright",
    pal = paleta,
    values = ~rango_auto,
    title = paste("Rangos de", toupper(variable_mapa)),
    opacity = 1
  )

### Heatmap de Correlación

val_num <- base %>% 
  select(where(is.numeric)) 
matriz <- cor(val_num, use = "pairwise.complete.obs")
ggcorrplot(matriz, method = "square", type = "lower",
           lab = FALSE, lab_size = 3,
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Mapa de Calor de Correlaciones para variables numéricas",
           ggtheme = ggplot2::theme_minimal())

## Datos a analizar

# Predecir el valor del DBO y DQO para poder clasificar el agua según su calidad.
# Se pueden hacer algunas pruebas de hipótesis para concluir sobre la independencia de ciertas variables y la distribución de los cuerpos y sitios (se pueden guiar con el heatmap)
# Hay que tomar con pinzas el dqoj, podemos analizar el dqof y el dboj mientras tanto.
# Investigar sobre las épocas de Costa Rica y el año hidrológico (Mar-Set / Oct- Ene Si no me equivoco) y como esto puede afectar el dbo y dqo.
# Buscar exactamente la posición de los puntos de muestreo y ver si hay algo cerca que podría contaminar.
# Investigar sobre las variables.
# Buscar métricas aparte del R2 y las medidas de error para estos modelos específicos.

## MODELOS

# RandomForest de Regresión : Se realizará para predecir el valor del DBO y DQO según las otras variables.
# RandomForest de Clasificación : Se realizará para predecir la calidad del agua según las demás variables.
# K-means : Se usará para clasificar los datos según grupos de similaridad y ver si concuerdan con los sitios además de clasificar la calidad del agua.

## ANÁLISIS DE DATOS

# Buscar más relaciones entre las variables.
# Hacer cuadros resumen con algunas métricas.
# Hacer más gráficos

