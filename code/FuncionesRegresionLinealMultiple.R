library(tidyverse)



base <- read_excel("data/base_agua_limpia_julio.xlsx")
base$latitud <- as.factor(base$latitud)
base$longitud <- as.factor(base$longitud)
base$sitio <- as.factor(base$sitio)
base$ubi_muestra <- as.factor(base$ubi_muestra)
base$cuerpo <- as.factor(base$cuerpo)
base$horarecolectaj <- as_hms(base$horarecolectaj)

base <- base[!is.na(base$dqoj),]

base <- select(base, where(is.numeric))


round(cor(x=base, method = "pearson"), 3)


library(psych)


view(base)
multi.hist(x = base, dcol = c("red", "blue"), dlty = c("dotted", "solid"),
           main = "")


base_limpia <- na.omit(base)

modelo <- lm(dqoj ~ ., data = base, na.action = na.exclude)
