library(tidyverse)

# Cargar la base limpia

base <- read_excel("data/base_agua_limpia.xlsx")
view(base)
glimpse(base)

# Extra: volver a poner las categóricas como factores y la fecha como Date por que
# al pasarse a excel pierde esta característica.

base$sitio <- as.factor(base$sitio)
base$ubi_muestra <- as.factor(base$ubi_muestra)
base$cuerpo <- as.factor(base$cuerpo)

