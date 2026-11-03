library(tidyverse)
library(readxl)
library(writexl)

ruta <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS/Data_Adolescentes_2021_2025_COSTA.xlsx"

datos <- read_excel(ruta)

conteo_cantonal <- datos %>%
  mutate(
    anio = as.integer(anio),
    tipo = str_to_upper(str_squish(tipo)),
    codigo_provincia = str_pad(
      as.character(codigo_provincia), 2, pad = "0"
    ),
    codigo_canton = str_pad(
      as.character(codigo_canton), 4, pad = "0"
    )
  ) %>%
  filter(
    anio %in% 2021:2025,
    tipo == "APREHENDIDO",
    codigo_provincia %in% c("07", "08", "09", "12", "13", "24", "23")
  ) %>%
  count(
    anio,
    codigo_provincia,
    provincia = nombre_provincia,
    codigo_canton,
    canton = nombre_canton,
    name = "total_aprehensiones"
  ) %>%
  arrange(provincia, canton, anio)

View(conteo_cantonal)

write_xlsx(
  conteo_cantonal,
  file.path(dirname(ruta), "Conteo_cantonal_2021_2025.xlsx")
)