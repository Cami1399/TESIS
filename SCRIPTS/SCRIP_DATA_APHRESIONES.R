## LIMPIEZA DE DATOS - DETENIDOS/APREHENDIDOS PM ----
# Fuente: Ministerio del Interior, detenidos/aprehendidos PM 2019-2025.
# Poblacion de analisis: adolescentes (12-17 anios), region Costa, 2021-2025.
#
# ESTRUCTURA DEL SCRIPT
#   SECCION 1: Filtrado secuencial (anio -> edad -> provincias de la Costa)
#   SECCION 2: Exploracion inicial (categorias, graficos, diagnostico)
#   SECCION 3: Clasificacion y transformacion previa de tipos de dato
#   SECCION 4: Limpieza (texto, faltantes, formatos, repetidas, factores)
#   SECCION 5: Verificacion final de tipos


# ============================================================ #
# SECCION 1: FILTRADO SECUENCIAL -----
# ============================================================ #

##  1.1 Librerias -----
library(readxl)
library(dplyr)
library(lubridate)

##  1.2 Carga de datos -----
ruta <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/mdi_detenidosaprehendidos_pm_2019_2025.xlsx"
data_raw <- read_excel(ruta, sheet = "1. Detenidos y Aprehendidos")

##  1.3 Filtro 1: solo anios 2021-2025 -----
# 'anio' se obtiene del anio de la fecha de detencion/aprehension.
Data_2021_2025 <- data_raw %>%
  mutate(anio = year(fecha_detencion_aprehension)) %>%
  filter(anio >= 2021 & anio <= 2025)
# Filas repetidas (identicas en todas las columnas) en este corte; solo se cuentan, no se eliminan
sum(duplicated(Data_2021_2025))

##  1.4 Filtro 2: solo edad 12-17 -----
# 'edad' viene como texto -> se convierte a numerico en la misma columna.
# OJO: si algun valor no es numerico (ej. "SE DESCONOCE") queda NA y el filtro
# lo excluye. Conviene revisar cuantas filas se pierden por este motivo.
Data_2021_2025 <- Data_2021_2025 %>%
  mutate(edad = as.numeric(edad))

Data_Adolescentes_2021_2025 <- Data_2021_2025 %>%
  filter(edad >= 12 & edad <= 17)

##  1.5 Filtro 3: solo provincias de la region Costa -----
# Nombres copiados EXACTOS de 'nombre_provincia' en la base original.
# Si algun conteo sale en 0, revisar sort(unique(data_raw$nombre_provincia)) por tildes/formato.
provincias_costa <- c(
  "ESMERALDAS",
  "MANABÍ",
  "STO DGO DE LOS TSÁCHILAS",   # Santo Domingo de los Tsachilas
  "LOS RÍOS",
  "GUAYAS",
  "SANTA ELENA",
  "EL ORO"
)


Data_Adolescentes_2021_2025_COSTA <- Data_Adolescentes_2021_2025 %>%
  filter(nombre_provincia %in% provincias_costa)
##  1.6 Filtro 4: solo aprehensiones -----
Data_Adolescentes_2021_2025_COSTA <- Data_Adolescentes_2021_2025_COSTA %>%
  filter(toupper(trimws(tipo)) == "APREHENDIDO")

# Verificar el tipo y la cantidad de registros finales
Data_Adolescentes_2021_2025_COSTA %>%
  count(tipo, name = "n_registros")

# ============================================================ #
# SECCION 2: EXPLORACION INICIAL -----
# ============================================================ #

##  2.1 Librerias adicionales -----
library(tidyr)
library(stringr)

##  2.2 Carga de datos anterior (DESACTIVADA) -----
# Data_Adolescentes_2021_2025_COSTA ahora se crea en la SECCION 1 (filtrado desde la base original).
# ruta <- "/cloud/project/DATAS/Data_Adolescentes_2021_2025_COSTA.xlsx"
# Data_Adolescentes_2021_2025_COSTA <- read_excel(ruta)

##  2.3 Vista de la base filtrada -----
Data_Adolescentes_2021_2025_COSTA


##  2.4 Categorias unicas a un archivo .TXT -----
# Sirve para revisar ortografia y categorias repetidas antes de limpiar.

library(dplyr)

# 1. Lista de las 25 variables seleccionadas (sin codigos, coordenadas ni fechas)
vars_interes <- c(
  "anio", "tipo", "estado_civil", "estatus_migratorio", "edad", "sexo", 
  "genero", "nacionalidad", "autoidentificacion_etnica", "nivel_de_instruccion", 
  "condicion", "movilizacion", "tipo_arma", "arma", "lugar", "tipo_lugar", 
  "nombre_zona", "nombre_subzona", "nombre_distrito", "nombre_circuito", 
  "nombre_subcircuito", "nombre_provincia", "nombre_canton", 
  "nombre_parroquia", "presunta_infraccion"
)

# 2. Redirigir la salida hacia el archivo TXT
sink("categorias_unicas_dataset.txt")

# 3. Bucle para extraer e imprimir las categorias de cada variable
for (var in vars_interes) {
  # Verificar si la variable existe en el dataframe antes de procesar
  if (var %in% names(Data_Adolescentes_2021_2025_COSTA)) {
    categorias <- Data_Adolescentes_2021_2025_COSTA[[var]] %>%
      na.omit() %>%
      unique() %>%
      sort()
    
    cat("\n============================================================\n")
    cat("VARIABLE:", toupper(var), "| Total únicas:", length(categorias), "\n")
    cat("============================================================\n")
    
    for (i in seq_along(categorias)) {
      cat(sprintf("  %2d. %s\n", i, categorias[i]))
    }
  }
}

# 4. Cerrar el archivo TXT y restaurar la salida a la consola
sink()

cat("\n[Éxito] Se ha generado el archivo 'categorias_unicas_dataset.txt' en tu directorio de trabajo.\n")


##  2.5 Visualizacion exploratoria inicial completa -----

library(dplyr)
library(ggplot2)
library(forcats)
library(patchwork)
library(stringr)

# 1. Variables a graficar (excluidas latitud y longitud)
vars_numericas <- c("edad", "anio")

vars_descriptivas <- c(
  "tipo", "sexo", "genero", "estado_civil", "estatus_migratorio",
  "nivel_de_instruccion", "movilizacion", "tipo_arma",
  "nombre_provincia", "presunta_infraccion"
)

todas_las_vars <- c(vars_numericas, vars_descriptivas)

# 2. Tema claro sin texto/titulos en el eje de frecuencia
tema_tablero <- theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(size = 12, face = "bold"),
    axis.text          = element_text(size = 9, color = "black"),
    axis.title         = element_blank(), # Elimina titulos de los ejes (incluyendo "Frecuencia")
    axis.text.y        = element_blank(), # Elimina numeros/marcas del eje de frecuencia en verticales
    axis.ticks.y       = element_blank(),
    panel.grid.major.y = element_blank(), # Limpia lineas de fondo innecesarias
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(10, 15, 10, 10)
  )

# 3. Funcion para graficar con numeros exactos en las barras

graficar_barras <- function(var) {
  df <- Data_Adolescentes_2021_2025_COSTA %>% filter(!is.na(.data[[var]]))
  
  if (is.numeric(df[[var]])) {
    ggplot(df, aes(x = factor(.data[[var]]))) +
      geom_bar(fill = "steelblue") +
      # Agregar cantidad exacta arriba de las barras verticales
      geom_text(
        stat = "count", 
        aes(label = after_stat(count)), 
        vjust = -0.5, 
        size = 3.5, 
        fontface = "bold"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + # Margen superior para el numero
      labs(title = paste("Distribución de:", var)) +
      tema_tablero +
      theme(
        axis.text.x  = element_text(size = 9, color = "black"),
        axis.ticks.x = element_line()
      )
    
  } else {
    ggplot(df, aes(x = fct_infreq(.data[[var]]))) +
      geom_bar(fill = "darkorange") +
      coord_flip() +
      # Agregar cantidad exacta al lado de las barras horizontales
      geom_text(
        stat = "count", 
        aes(label = after_stat(count)), 
        hjust = -0.2, 
        size = 3.2, 
        fontface = "bold"
      ) +
      # Ajustar margen derecho para que el numero no salga recortado
      scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
      labs(title = paste("Distribución de:", var)) +
      tema_tablero +
      theme(
        axis.text.y  = element_text(size = 9, color = "black"), # Mantiene etiquetas de categorias
        axis.ticks.y = element_line(),
        axis.text.x  = element_blank(), # Oculta numeros del eje horizontal
        axis.ticks.x = element_blank()
      )
  }
}

# 4. Generar lista de graficos
graficos <- lapply(todas_las_vars, graficar_barras)
names(graficos) <- todas_las_vars


# 5. Guardar una imagen PNG gigante unica
tablero_gigante <- wrap_plots(graficos, ncol = 2) +
  plot_annotation(
    title = "Exploración Completa de Variables - Costa",
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

ggsave(
  filename  = "tablero_gigante_cantidades.png",
  plot      = tablero_gigante,
  width     = 16,
  height    = 30,
  dpi       = 300,
  limitsize = FALSE
)


# 6. Exportar en PDF multipagina (RECOMENDADO)

pdf("tablero_completo_con_cantidades.pdf", width = 11, height = 14)

# Pagina 1
print((graficos[["edad"]] | graficos[["anio"]]) / 
        (graficos[["tipo"]] | graficos[["sexo"]]))

# Pagina 2
print((graficos[["genero"]] | graficos[["estado_civil"]]) / 
        (graficos[["estatus_migratorio"]] | graficos[["nivel_de_instruccion"]]))

# Pagina 3
print((graficos[["movilizacion"]] | graficos[["tipo_arma"]]) / 
        graficos[["nombre_provincia"]])

# Pagina 4: Presunta infraccion completa
print(graficos[["presunta_infraccion"]])

dev.off()


##  2.6 Diagnostico inicial -----
dim(Data_Adolescentes_2021_2025_COSTA)

# NA reales por columna. Ojo: los faltantes tambien pueden venir como texto
# (SIN_DATO, SE_DESCONOCE, etc.); esos se tratan en 4.2.
colSums(is.na(Data_Adolescentes_2021_2025_COSTA))

# Filas repetidas (identicas en todas las columnas)
sum(duplicated(Data_Adolescentes_2021_2025_COSTA))



# ============================================================ #
# SECCION 3: CLASIFICACION Y TRANSFORMACION PREVIA DE TIPOS -----
# ============================================================ #
# Esta seccion va ANTES de la limpieza.
# Latitud y longitud conservan sus valores y tipos originales.
# Los factores se aplican DESPUES de limpiar (4.6).

##  3.1 Copia de trabajo y tipos de dato actuales -----
data_limpia <- Data_Adolescentes_2021_2025_COSTA

table(sapply(data_limpia, function(x) class(x)[1]))
sapply(data_limpia, class)

##  3.2 Naturaleza del tipo de dato -----
# 1. Definir cada grupo
cuant_discretas <- c("edad", "anio")
cuant_continuas <- c("latitud", "longitud", "fecha_detencion_aprehension",
                     "hora_detencion_aprehension")
cual_ordinales  <- c("nivel_de_instruccion")
# Esta clasificacion no modifica los valores ni los tipos de las columnas.

# 2. Tabla de clasificacion
clasificacion <- tibble(variable = names(data_limpia)) %>%
  mutate(
    naturaleza = if_else(variable %in% c(cuant_discretas, cuant_continuas),
                         "Cuantitativa", "Cualitativa"),
    tipo = case_when(
      variable %in% cuant_discretas ~ "Discreta",
      variable %in% cuant_continuas ~ "Continua",
      variable %in% cual_ordinales  ~ "Ordinal",
      TRUE                          ~ "Nominal"
    )
  )

print(clasificacion, n = Inf)

# 3. Resumen: cuantas hay de cada tipo
clasificacion %>% count(naturaleza, tipo)

##  3.3 Transformacion de tipos con objeto separado 'data_tipos' (REFERENCIA) -----
# Esta copia no alimenta la limpieza.
# Latitud y longitud no se convierten a numeric ni a factor.

# 1. Grupos segun la clasificacion
vars_codigo   <- names(data_limpia)[str_starts(names(data_limpia), "codigo_")]
vars_entero   <- c("edad", "anio")
vars_coordenadas <- c("latitud", "longitud")
vars_ordinal  <- "nivel_de_instruccion"
vars_tiempo   <- c("fecha_detencion_aprehension", "hora_detencion_aprehension")
vars_control  <- "iccs_formato_valido"

# Nominales = todas las demas -> factor
# Se excluyen las coordenadas para conservar su formato original.
vars_nominal <- setdiff(names(data_limpia),
                        c(vars_codigo, vars_entero, vars_coordenadas,
                          vars_ordinal, vars_tiempo, vars_control))

# 2. Orden de las categorias del nivel de instruccion
niveles_instruccion <- c("EDUCACIÓN INICIAL",
                         "EDUCACIÓN GENERAL BÁSICA",
                         "BACHILLERATO",
                         "TERCER NIVEL TÉCNICO-TECNOLÓGICO Y DE GRADO",
                         "CUARTO NIVEL O DE POSGRADO")

# 3. Conversion
data_tipos <- data_limpia %>%
  mutate(
    nivel_de_instruccion_txt = nivel_de_instruccion,
    
    across(all_of(vars_codigo), as.character),
    across(all_of(vars_entero), as.integer),
    across(all_of(vars_nominal), as.factor),
    
    nivel_de_instruccion = factor(nivel_de_instruccion,
                                  levels  = niveles_instruccion,
                                  ordered = TRUE)
  )

# 4. Verificacion
data.frame(variable = names(data_tipos),
           clase    = sapply(data_tipos, function(x) paste(class(x), collapse = " / ")),
           row.names = NULL)

levels(data_tipos$nivel_de_instruccion)
table(data_tipos$nivel_de_instruccion, useNA = "ifany")

##  3.4 Transformacion previa de tipos (ANTES de la limpieza) -----
# Codigos -> character; edad/anio -> integer; fecha/hora -> Date/hms.
# Latitud y longitud conservan su formato original.

# 1. Grupos
tp_codigo   <- names(Data_Adolescentes_2021_2025_COSTA)[
  str_starts(names(Data_Adolescentes_2021_2025_COSTA), "codigo_")]
tp_entero   <- c("edad", "anio")

# 2. NA antes de convertir
na_antes <- colSums(is.na(Data_Adolescentes_2021_2025_COSTA[tp_entero]))

# 3. Conversion
Data_Adolescentes_2021_2025_COSTA <- Data_Adolescentes_2021_2025_COSTA %>%
  mutate(
    across(all_of(tp_codigo), as.character),
    across(all_of(tp_entero), as.integer),
    fecha_detencion_aprehension = as.Date(fecha_detencion_aprehension),
    hora_detencion_aprehension  = hms::as_hms(hora_detencion_aprehension)
  )

# 4. Verificacion: NA nuevos creados por la conversion
na_despues <- colSums(is.na(Data_Adolescentes_2021_2025_COSTA[tp_entero]))
na_despues - na_antes

# 5. Copia de trabajo actualizada
data_limpia <- Data_Adolescentes_2021_2025_COSTA


# ============================================================ #
# SECCION 4: LIMPIEZA -----
# ============================================================ #
data_limpia <- Data_Adolescentes_2021_2025_COSTA

## 4.1 Estandarizar texto -----
# Quita espacios sobrantes, pasa a mayusculas y "" -> NA.
# Latitud y longitud quedan excluidas.

data_limpia <- data_limpia %>%
  mutate(across(where(is.character) & !any_of(c("latitud", "longitud")),
                ~ .x %>% str_squish() %>% str_to_upper() %>% na_if("")))

# Error de digitacion detectado en 'lugar'
data_limpia <- data_limpia %>%
  mutate(lugar = recode(lugar, "CENTRO INFANTILE" = "CENTRO INFANTIL"))

## 4.2 Valores faltantes -----
# SIN_DATO y SE_DESCONOCE se unifican en "SE_DESCONOCE".
# NO_APLICA se mantiene como categoria.
# Latitud y longitud quedan excluidas.

marcadores_faltante <- c("SIN_DATO", "SIN DATO", "S/D", "SD",
                         "SE DESCONOCE", "SE_DESCONOCE", "NULL", "NA", "N/A")

data_limpia <- data_limpia %>%
  mutate(across(where(is.character) & !any_of(c("latitud", "longitud")),
                ~ replace(.x, is.na(.x) | .x %in% marcadores_faltante, "SE_DESCONOCE")))

# % de faltantes por variable
data_limpia %>%
  summarise(across(everything(), ~ sum(.x %in% "SE_DESCONOCE"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(pct = round(100 * n_faltantes / nrow(data_limpia), 2)) %>%
  filter(n_faltantes > 0) %>%
  arrange(desc(n_faltantes)) %>%
  print(n = Inf)

## 4.3 Formatos -----
# Latitud y longitud no se transforman.

data_limpia <- data_limpia %>%
  mutate(
    across(starts_with("codigo_"), as.character),
    edad = as.integer(edad),
    anio = as.integer(anio),
    fecha_detencion_aprehension = as.Date(fecha_detencion_aprehension),
    hora_detencion_aprehension  = hms::as_hms(hora_detencion_aprehension)
  )

## 4.4 Filas repetidas -----
# Se eliminan copias extra de filas identicas en TODAS las columnas.

cat("Filas antes:", nrow(data_limpia), "\n")
cat("Copias extra:", nrow(data_limpia) - nrow(distinct(data_limpia)), "\n")

# Para revisar cuales son
filas_repetidas <- data_limpia %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

data_limpia <- data_limpia %>% distinct()

cat("Filas después:", nrow(data_limpia), "\n")

## 4.5 Verificacion de la limpieza -----
anyDuplicated(data_limpia)
sapply(select(data_limpia, where(is.character)), n_distinct)

## 4.6 Factores y ordered factor (DESPUES de la limpieza) -----
# Las coordenadas conservan su formato original y no pasan a factor.

# 1. Grupos
fac_codigo   <- names(data_limpia)[str_starts(names(data_limpia), "codigo_")]
fac_entero   <- c("edad", "anio")
fac_coordenadas <- c("latitud", "longitud")
fac_ordinal  <- "nivel_de_instruccion"
fac_tiempo   <- c("fecha_detencion_aprehension", "hora_detencion_aprehension")
fac_control  <- "iccs_formato_valido"

# Nominales = todas las demas
fac_nominal <- setdiff(names(data_limpia),
                       c(fac_codigo, fac_entero, fac_coordenadas,
                         fac_ordinal, fac_tiempo, fac_control))

# 2. Orden de las categorias del nivel de instruccion
fac_niveles_instruccion <- c("EDUCACIÓN INICIAL",
                             "EDUCACIÓN GENERAL BÁSICA",
                             "BACHILLERATO",
                             "TERCER NIVEL TÉCNICO-TECNOLÓGICO Y DE GRADO",
                             "CUARTO NIVEL O DE POSGRADO")

# 3. Control de categorias no incluidas en los niveles
setdiff(unique(data_limpia$nivel_de_instruccion), fac_niveles_instruccion)

# 4. Conversion
data_limpia <- data_limpia %>%
  mutate(
    nivel_de_instruccion_txt = nivel_de_instruccion,
    
    across(all_of(fac_nominal), as.factor),
    
    nivel_de_instruccion = factor(nivel_de_instruccion,
                                  levels  = fac_niveles_instruccion,
                                  ordered = TRUE)
  )
# 5. Verificacion
data.frame(variable = names(data_limpia),
           clase    = sapply(data_limpia, function(x) paste(class(x), collapse = " / ")),
           row.names = NULL)

levels(data_limpia$nivel_de_instruccion)
table(data_limpia$nivel_de_instruccion, useNA = "ifany")


# ============================================================ #
# SECCION 4: LIMPIEZA -----
# ============================================================ #

data_limpia <- Data_Adolescentes_2021_2025_COSTA

## 4.1 Estandarizar texto -----
# Quita espacios sobrantes (inicio, fin y dobles), pasa a mayusculas y "" -> NA.
# Latitud y longitud quedan excluidas para conservar su formato original.

data_limpia <- data_limpia %>%
  mutate(across(where(is.character) & !any_of(c("latitud", "longitud")),
                ~ .x %>% str_squish() %>% str_to_upper() %>% na_if("")))

# Error de digitacion detectado en 'lugar'

data_limpia <- data_limpia %>%
  mutate(lugar = recode(lugar, "CENTRO INFANTILE" = "CENTRO INFANTIL"))

## 4.2 Valores faltantes -----
# SIN_DATO y SE_DESCONOCE se unifican en "SE_DESCONOCE".
# "NO_APLICA" se mantiene como categoria.
# Latitud y longitud quedan excluidas y conservan sus valores originales.

marcadores_faltante <- c("SIN_DATO", "SIN DATO", "S/D", "SD",
                         "SE DESCONOCE", "SE_DESCONOCE", "NULL", "NA", "N/A")

data_limpia <- data_limpia %>%
  mutate(across(where(is.character) & !any_of(c("latitud", "longitud")),
                ~ replace(.x, is.na(.x) | .x %in% marcadores_faltante, "SE_DESCONOCE")))

# % de faltantes por variable

data_limpia %>%
  summarise(across(everything(), ~ sum(.x %in% "SE_DESCONOCE"))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  mutate(pct = round(100 * n_faltantes / nrow(data_limpia), 2)) %>%
  filter(n_faltantes > 0) %>%
  arrange(desc(n_faltantes)) %>%
  print(n = Inf)

## 4.3 Formatos -----
# Tipos de dato (codigos como texto para no perder ceros).
# Latitud y longitud NO se convierten: conservan su formato original.

data_limpia <- data_limpia %>%
  mutate(
    across(starts_with("codigo_"), as.character),
    edad     = as.integer(edad),
    anio     = as.integer(anio),
    fecha_detencion_aprehension = as.Date(fecha_detencion_aprehension),
    hora_detencion_aprehension  = hms::as_hms(hora_detencion_aprehension)
  )

# Verificar que las coordenadas no cambiaron durante 4.1, 4.2 y 4.3

stopifnot(
  identical(
    data_limpia$latitud,
    Data_Adolescentes_2021_2025_COSTA$latitud
  ),
  identical(
    data_limpia$longitud,
    Data_Adolescentes_2021_2025_COSTA$longitud
  )
)

## 4.4 Filas repetidas -----
# No hay columna ID: repetida = fila identica en TODAS las columnas.
# Se cuentan las copias extra, se guardan las filas repetidas para revision
# y se eliminan las copias extra, conservando una fila de cada combinacion.

cat("Filas antes:", nrow(data_limpia), "\n")
cat("Copias extra:", nrow(data_limpia) - nrow(distinct(data_limpia)), "\n")

# Para revisar cuales son

filas_repetidas <- data_limpia %>%
  group_by(across(everything())) %>%
  filter(n() > 1) %>%
  ungroup()

data_limpia <- data_limpia %>% distinct()

cat("Filas después:", nrow(data_limpia), "\n")

## 4.5 Verificacion de la limpieza -----
# Las categorias unicas se pueden volver a exportar con el bucle sink() de 2.4,
# apuntando a 'data_limpia' en lugar de Data_Adolescentes_2021_2025_COSTA.

anyDuplicated(data_limpia)  # Debe dar 0: ya no hay filas identicas repetidas.
sapply(select(data_limpia, where(is.character)), n_distinct)

## 4.6 Factores y ordered factor (DESPUES de la limpieza) -----
# Con el texto ya limpio:
#   - nominales -> factor
#   - nivel_de_instruccion -> ordered factor
#   - codigos siguen como character; edad/anio integer
#   - latitud/longitud conservan sus valores y tipos originales

# 1. Grupos
fac_codigo   <- names(data_limpia)[str_starts(names(data_limpia), "codigo_")]
fac_entero   <- c("edad", "anio")
fac_coordenadas <- c("latitud", "longitud")
fac_ordinal  <- "nivel_de_instruccion"
fac_tiempo   <- c("fecha_detencion_aprehension", "hora_detencion_aprehension")
fac_control  <- "iccs_formato_valido"

# Nominales = todas las demas
# Latitud y longitud quedan excluidas de la conversion a factor.

fac_nominal <- setdiff(names(data_limpia),
                       c(fac_codigo, fac_entero, fac_coordenadas,
                         fac_ordinal, fac_tiempo, fac_control,
                         "nivel_de_instruccion_txt"))

# 2. Orden de las categorias del nivel de instruccion (de menor a mayor)

fac_niveles_instruccion <- c("EDUCACIÓN INICIAL",
                             "EDUCACIÓN GENERAL BÁSICA",
                             "BACHILLERATO",
                             "TERCER NIVEL TÉCNICO-TECNOLÓGICO Y DE GRADO",
                             "CUARTO NIVEL O DE POSGRADO")

# 3. Control: categorias que no estan en los niveles
# Estas categorias quedaran como NA en el factor ordenado,
# pero se conservaran en nivel_de_instruccion_txt.

setdiff(unique(data_limpia$nivel_de_instruccion), fac_niveles_instruccion)

# 4. Conversion

data_limpia <- data_limpia %>%
  mutate(
    # Conserva el texto previo a la conversion a factor ordenado
    nivel_de_instruccion_txt = as.character(nivel_de_instruccion),
    
    across(all_of(fac_nominal), as.factor),
    
    nivel_de_instruccion = factor(nivel_de_instruccion,
                                  levels  = fac_niveles_instruccion,
                                  ordered = TRUE)
  )

# 5. Verificacion

data.frame(variable = names(data_limpia),
           clase    = sapply(data_limpia, function(x) paste(class(x), collapse = " / ")),
           row.names = NULL)

levels(data_limpia$nivel_de_instruccion)
table(data_limpia$nivel_de_instruccion, useNA = "ifany")

# Revisar tambien las categorias conservadas como texto
table(data_limpia$nivel_de_instruccion_txt, useNA = "ifany")


# ============================================================ #
# SECCION 5: VERIFICACION FINAL DE TIPOS -----
# ============================================================ #
# Resume los tipos finales de 'data_limpia'.
# Si las coordenadas originalmente son texto, no apareceran en
# select(where(is.numeric)), pero seguiran presentes en data_limpia.

## 5.1 Variables numericas -----

# 1. Cuantas hay
data_limpia %>% select(where(is.numeric)) %>% ncol()

# 2. Cuales son
data_limpia %>% select(where(is.numeric)) %>% names()

# 3. Tipo de dato de TODAS las variables (resumen)
sapply(data_limpia, function(x) class(x)[1]) %>% table()

# 4. Tipo de cada variable, una por una
data.frame(variable = names(data_limpia),
           tipo     = sapply(data_limpia, function(x) class(x)[1]),
           row.names = NULL)

# 5. Visualizar las coordenadas conservadas
data_limpia %>%
  select(latitud, longitud) %>%
  head()

# ============================================================ #
# EXPORTAR DATAFRAME COMPLETO A EXCEL (.XLSX) -----

# 1. Instalar el paquete si no lo tienes instalado previamente
if (!require("writexl")) install.packages("writexl")
library(writexl)

# 2. Carpeta de destino (en R se usan "/" en lugar de "\")
carpeta_salida <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"

# 3. Exportar el objeto directamente a un archivo de Excel
write_xlsx(
  x    = data_limpia, 
  path = file.path(carpeta_salida, "Data_Adolescentes_2021_2025_COSTA.xlsx")
)

cat("\n[Éxito] El objeto se ha guardado como 'Data_Adolescentes_2021_2025_COSTA.xlsx' en:", carpeta_salida, "\n")


