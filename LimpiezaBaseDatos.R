library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(hms)

# Lectura de la base de datos

base <- read_excel("data/Metabase.xlsx", 
                   sheet = "Datos", 
                   range = "A1:GG93", 
                   na = c('-1',
                          "99/99/99", 
                          '999999', 
                          '00/00/00',
                          '99:99',
                          '99:99:99',
                          '888888',
                          '9999'))
view(base)

# Selección de Columnas para el estudio

base <- base %>% 
  select(
    # Variables Generales 
    sitio, ubi_muestra = ubicación, cuerpo, latitudf, latitudj, longitudf, longitudj,
    # Variables del muestreo
    fecha, fecharecolectaf, fecharecolectaj,
    # Variables del sitio
    profundidad, salinidad, oxigeno, solid_dis = TDS, conduc, resist, presion, sat_oxigen,
    # Variables del agua y residuos en Febrero
    tempaguaf, tempairef, colifecalf, ecolif, enterococof,
    # Variablaes del agua y residuos en Julio
    tempaguaj, tempairej, colifecalj, ecolij, enterococoj,
    # Variables generales del agua y sedimentos
    ph, fosfatos, silicatos, amonio, nitritos, nitratos, chla_agua, faopigmentos, matsuspension, 
    alcali_total, dureza, carbonatos,
    # Variables acerca de la concentración de químicos en el agua
    zinc, cobre, plomo, cadmio, arsenico, manganeso, ca2, mg2, na, k, cl, SO42, 
    # Índices sobre la calidad del agua
    dbof, dboj, dqof, dqoj
  )


# Categóricas

base <- base %>% mutate(
  ubi_muestra = case_when(
    ubi_muestra == "1" ~ "Superficie",
    ubi_muestra == "2" ~ "Fondo",
    TRUE ~ NA
  ),
  cuerpo = case_when(
    cuerpo == "1" ~ "Salado",
    cuerpo == "2" ~ "Dulce",
    TRUE ~ NA
  ),
  fecha = case_when(
    str_detect(fecha, "^99/99") ~ NA,
    fecha == "02dec2019" ~ "12/02/2019",
    fecha == "02nov2019" ~ "11/02/2019",
    fecha == "11feb2019" ~ "02/11/2019",
    TRUE ~ as.character(fecha)
  )
) %>% 
  mutate(fecha = mdy(fecha),
         fecharecolectaj = mdy(fecharecolectaj),
         fecharecolectaf = mdy(fecharecolectaf))

# Asignación de tipo de columna

glimpse(base)

base <- base %>%
  mutate(across(
    c(latitudf, latitudj, longitudf, longitudj,
      profundidad, salinidad, oxigeno, solid_dis, conduc, resist, presion, sat_oxigen,
      tempaguaf, tempairef, colifecalf, ecolif, enterococof,
      tempaguaj, tempairej, colifecalj, ecolij, enterococoj,
      ph, fosfatos, silicatos, amonio, nitritos, nitratos, chla_agua, faopigmentos, matsuspension, 
      alcali_total, dureza, carbonatos,
      zinc, cobre, plomo, cadmio, arsenico, manganeso, ca2, mg2, na, k, cl, SO42, 
      dbof, dboj, dqof, dqoj
    ),
    as.numeric
  )
)


base <- base %>% mutate(across(
    c(sitio, ubi_muestra, cuerpo),
    as.factor
))

base$oxigeno <- round(base$oxigeno, 2)

# Borrar filas sin sitio, cuerpo, ubi_muestra, latitudj, latitudf, longitudj y longitudf

base <- base %>% 
  mutate(latitud = coalesce(latitudf, latitudj))

base <- base %>% 
  mutate(longitud = coalesce(longitudf, longitudj))

base <- base %>% 
  select(-latitudf) %>% 
  select(-latitudj) %>% 
  select(-longitudj) %>% 
  select(-longitudf)

base <- base %>% 
  filter(!is.na(sitio)) %>% 
  filter(!is.na(ubi_muestra) | !is.na(cuerpo)) %>% 
  filter(!is.na(latitud) | !is.na(longitud)) 


# Arreglo de la columna de fecha

base$fecharecolectaf <- ifelse(
  is.na(base$fecharecolectaf) & format(base$fecha, "%m") == "02",
  base$fecha, base$fecharecolectaf
)

base$fecharecolectaf <- as.Date(base$fecharecolectaf)

## Imputación de fechas

set.seed(3736)

rango_feb <- seq(as.Date("2019-02-11"), as.Date("2019-02-19"), by = "day")
rango_jul <- seq(as.Date("2019-07-22"), as.Date("2019-08-04"), by = "day")

base$fecharecolectaf[is.na(base$fecharecolectaf)] <- sample(rango_feb, sum(is.na(base$fecharecolectaf)), replace = TRUE)
base$fecharecolectaj[is.na(base$fecharecolectaj)] <- sample(rango_jul, sum(is.na(base$fecharecolectaj)), replace = TRUE)

base <- base %>% select(-fecha)

# Eliminación de columnas

## Primero calcular la cantidad de NA's en las columnas y el porcentaje sobre el total

### Esto da sobre la columna

resumen_na_por_columna <- base %>% 
  summarise(across(everything(), 
                   ~sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), 
               names_to = "columna", 
               values_to = "cantidad_na") %>%
  mutate(
    total = nrow(base),
    porcentaje_na = (cantidad_na / total) * 100
  )

resumen_na_por_columna <- data.frame(resumen_na_por_columna)

view(resumen_na_por_columna)

cols_mas_75_na <- resumen_na_por_columna %>% 
  filter(porcentaje_na > 75) %>%
  pull(columna)


base <- base %>% select(-all_of(cols_mas_75_na))

# Eliminación de filas

resumen_na_por_filas <- base %>%
  mutate(
    cantidad_na = rowSums(is.na(.)),
    porcentaje_na = (cantidad_na / ncol(.)) * 100
  ) %>%
  select(cantidad_na, porcentaje_na)

resumen_na_por_filas <- data.frame(resumen_na_por_filas)

view(resumen_na_por_filas)

#base <- base %>%
#  mutate(
#    porcentaje_na = rowSums(is.na(.)) / ncol(.) * 100
#  ) %>%
#  filter(porcentaje_na < 85) %>%  # solo filas con menos de 85% NA
#  select(-porcentaje_na)

# Guardar csv con la base limpia

write.xlsx(base, "data/base_agua_limpia.xlsx", na= "NA")
