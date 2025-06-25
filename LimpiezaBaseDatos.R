library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(hms)

# Lectura de la base de datos

base <- read_excel("data/Metabase.xlsx", sheet = "Datos")
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
    ubi_muestra == "-1" ~ NA,
    TRUE ~ NA
  ),
  cuerpo = case_when(
    cuerpo == "1" ~ "Salado",
    cuerpo == "2" ~ "Dulce",
    TRUE ~ NA
  ),
  fecha = case_when(
    str_detect(fecha, "^99/99") ~ NA,
    str_detect(fecha, "-1") ~ NA,
    fecha == "02dec2019" ~ "02/12/2019",
    fecha == "02nov2019" ~ "02/11/2019",
    fecha == "11feb2019" ~ "11/02/2019",
    TRUE ~ as.character(fecha)
  ),
  fecharecolectaf = case_when(
    str_detect(fecharecolectaf, "^99/99") ~ NA,
    str_detect(fecharecolectaf, "00/00/00") ~ NA,
    str_detect(fecharecolectaf, "-1") ~ NA,
    TRUE ~ as.character(fecharecolectaf) 
  ),
  fecharecolectaf = case_when(
    str_detect(fecharecolectaf, "-1") ~ NA,
    TRUE ~ as.character(fecharecolectaf) 
  )
)


# Asignación de tipo de columna

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

base$fecha


base <- base %>%
  mutate(
    # Convertir fechas en formato dd/mm/aaaa
    across(c(fecharecolectaf, fecharecolectaj),
           ~ as.Date(., format = "%d/%m/%Y")),
    
    # Opcional: Forzar NA para fechas inválidas (ej: meses >12)
    across(c(fecharecolectaf, fecharecolectaj),
           ~ ifelse(is.na(as.Date(., format = "%d/%m/%Y")), 
                    NA, 
                    as.Date(., format = "%d/%m/%Y")))
  )

# Asignación de NA's por columna
base <- base %>% mutate(
  across(c(
    latitudf, latitudj, longitudf, longitudj,
    profundidad, salinidad, oxigeno, solid_dis, conduc, resist, presion, sat_oxigen,
    tempaguaf, tempairef, colifecalf, ecolif, enterococof,
    tempaguaj, tempairej, colifecalj, ecolij, enterococoj,
    ph, fosfatos, silicatos, amonio, nitritos, nitratos, chla_agua, faopigmentos, matsuspension, 
    alcali_total, dureza, carbonatos,
    zinc, cobre, plomo, cadmio, arsenico, manganeso, ca2, mg2, na, k, cl, SO42, 
    dbof, dboj, dqof, dqoj
  ), 
  ~na_if(., 1))
)



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

cols_mas_80_na <- resumen_na %>% 
  filter(porcentaje_na > 80) %>%
  pull(columna)


base <- base %>% select(-all_of(cols_mas_80_na))

### Esto da sobre la fila

resumen_na_por_filas <- base %>%
  mutate(
    cantidad_na = rowSums(is.na(.)),
    porcentaje_na = (cantidad_na / ncol(.)) * 100
  ) %>%
  select(cantidad_na, porcentaje_na)

resumen_na_por_filas <- data.frame(resumen_na_por_filas)

view(resumen_na_por_filas)

base <- base %>%
  mutate(
    porcentaje_na = rowSums(is.na(.)) / ncol(.) * 100
  ) %>%
  filter(porcentaje_na < 85) %>%  # solo filas con menos de 85% NA
  select(-porcentaje_na)


# Ordenar el data frame por fecha

base <- base %>% arrange(fecha, hora)

# Para efectos del análisis de datos y el posterior modelo se ordenarán como factor las variables categórcas

base$sitio <- as.factor(base$sitio)
base$ubi_muestra <- as.factor(base$ubi_muestra)
base$cuerpo <- as.factor(base$cuerpo)
base$analisis_agua1 <- as.factor(base$analisis_agua1)

# Extra: Eliminar la observación 55 por falta de fecha

base <- base[-nrow(base),]

# Guardar csv con la base limpia

write.xlsx(base, "data/base_agua_limpia.xlsx", na= "NA")
