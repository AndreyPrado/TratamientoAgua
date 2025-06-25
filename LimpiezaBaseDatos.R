library(readxl)
library(tidyverse)
library(lubridate)

# Lectura de la base de datos

base <- read_excel("Metabase.xlsx", sheet = "Datos")
view(base)

# Selección de Columnas para el estudio

base <- base %>% 
  select(sitio, ubi_muestra = ubicación, cuerpo, fecha, profundidad, secchi, temperatura, salinidad,
         oxigeno, solidos_dis = TDS , conduc, conduc_abs =conduc_ABS,
         resist, PPWO, presion, sat_oxigen, latitud, longitud,
         analisis_agua1, ph, ph1, 
         fosfatos, silicatos, amonio, nitritos, nitratos, filtros_chla,
         chla_agua, faopigmentos, filtros_matsuspension, matsuspension,
         , alcali_parcial, alcali_total, carbonatos, zinc, cobre,  ca2, mg2, na, k,cl,
         SO42, dbof, dboj)

# Limpieza de columnas y asignación de NA's

base <- base %>% 
  mutate(
    ubi_muestra = case_when(
      ubi_muestra == 1 ~ 'superficie',
      ubi_muestra == 2 ~ 'fondo',
      TRUE ~ NA 
    ),
    cuerpo = case_when(
      cuerpo == 1 ~ 'dulce',
      cuerpo == 2 ~ 'salado',
      TRUE ~ NA
    ),
    analisis_agua1 = case_when(
      analisis_agua1 == 0 ~ 'no',
      analisis_agua1 == 1 ~ 'si',
      TRUE ~ NA
    )
  )

# Asignación de tipo de columna

base <- base %>%
  mutate(across(
    c(profundidad, secchi, salinidad, oxigeno,
      conduc, conduc_abs, resist, PPWO, presion, sat_oxigen, ph,
      solidos_dis, latitud, longitud, fosfatos, silicatos, amonio,
      nitritos, nitratos, filtros_chla, chla_agua, faopigmentos,
      filtros_matsuspension, matsuspension, alcali_parcial, alcali_total,
      carbonatos, zinc, cobre, ca2, mg2, na, k, cl, SO42, dbof, dboj),
    as.numeric
  ))

# Asignación de NA's por columna

base <- base %>%
  mutate(
    profundidad = na_if(profundidad, -1),
    secchi = na_if(secchi, -1),
    salinidad = na_if(salinidad, -1),
    oxigeno = na_if(oxigeno, -1),
    conduc = na_if(conduc, -1),
    conduc_abs = na_if(conduc_abs, -1),
    resist = na_if(resist, -1),
    PPWO = na_if(PPWO, -1),
    presion = na_if(presion, -1),
    sat_oxigen = na_if(sat_oxigen, -1),
    ph = na_if(ph, -1),
    solidos_dis = na_if(solidos_dis, -1),
    latitud = na_if(latitud, -1),
    longitud = na_if(longitud, -1),
    fosfatos = na_if(fosfatos, -1),
    silicatos = na_if(silicatos, -1),
    amonio = na_if(amonio, -1),
    nitritos = na_if(nitritos, -1),
    nitratos = na_if(nitratos, -1),
    filtros_chla = na_if(filtros_chla, -1),
    chla_agua = na_if(chla_agua, -1),
    faopigmentos = na_if(faopigmentos, -1),
    filtros_matsuspension = na_if(filtros_matsuspension, -1),
    matsuspension = na_if(matsuspension, -1),
    alcali_parcial = na_if(alcali_parcial, -1),
    alcali_total = na_if(alcali_total, -1),
    carbonatos = na_if(carbonatos, -1),
    zinc = na_if(zinc, -1),
    cobre = na_if(cobre, -1),
    ca2 = na_if(ca2, -1),
    mg2 = na_if(mg2, -1),
    na = na_if(na, -1),
    k = na_if(k, -1),
    cl = na_if(cl, -1),
    SO42 = na_if(SO42, -1),
    dbof = na_if(dbof, -1),
    dboj = na_if(dboj, -1),
    temperatura = na_if(temperatura, -1),
    temperatura = na_if(temperatura, 9999.0),
    longitud = na_if(longitud, 999999.00000),
    ph1 = na_if(ph1, 9999.00),
    faopigmentos = na_if(faopigmentos, 9999.00),
    dboj = na_if(dboj, 999999.0),
    dbof = na_if(dbof, 888888.0)
  )

# Arreglar las fechas

base <- base %>%
  mutate(
    fecha = as.character(fecha),
    fecha = na_if(fecha, "-1"),
    fecha = na_if(fecha, "99/99/99"),
    fecha = trimws(fecha),
    fecha = if_else(grepl("^[0-9]{2}[a-z]{3}[0-9]{4}$", fecha),
                    paste0(substr(fecha, 1, 2), " ", substr(fecha, 3, 5), " ", substr(fecha, 6, 9)),
                    fecha),
    fecha = parse_date_time(fecha, orders = c("mdy", "d b Y"), locale = "en_US"),
    
    fecha = as.Date(fecha)
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
