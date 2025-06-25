library(tidyverse)

# Cargar la base limpia

base <- read_excel("data/base_agua_limpia.xlsx")
view(base)

# Extra: volver a poner las categóricas como factores y la fecha como Date por que
# al pasarse a excel pierde esta característica.

base$sitio <- as.factor(base$sitio)
base$cuerpo <- as.factor(base$cuerpo)
base$ubi_muestra <- as.factor(base$ubi_muestra)
base$analisis_agua1 <- as.factor(base$analisis_agua1)
base$fecha <- as.Date(base$fecha)
base$hora <- as_hms(base$hora)