
# CV ORIGINAL POR INDICADOR Y ANIO + TABLAS DE MUESTRAS MENORES A 60


# 1. LIBRERIAS Y PARAMETROS -----

## 1.1 Librerias ----
library(tidyverse)   # dplyr, tidyr, ggplot2, stringr, purrr, etc.
library(readxl)      # lectura del Excel (no viene dentro de tidyverse)

## 1.2 Parametros que puedes cambiar ----
carpeta_excel <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"
ruta_excel    <- file.path(carpeta_excel, "comparacion_original_gvf_fay_herriot.xlsx")

# Los graficos y las tablas como imagen van a SCRIPTS/SALIDAS_EXPLORACIONES/salidas_visualizacion.
if (!exists("carpeta_imagenes")) carpeta_imagenes <- "C:/Users/camil/OneDrive/Desktop/TESIS/SCRIPTS/SALIDAS_EXPLORACIONES"
carpeta_graficos <- file.path(carpeta_imagenes, "salidas_visualizacion")
carpeta_tablas   <- carpeta_graficos
marca_muestra    <- 60      # marca INEC de tamano de muestra

dir.create(carpeta_graficos, showWarnings = FALSE, recursive = TRUE)
dir.create(carpeta_tablas, showWarnings = FALSE, recursive = TRUE)

## 1.3 Funcion auxiliar para guardar graficos ----
guardar_png <- function(g, nombre, ancho, alto, dpi = 130, carpeta = carpeta_graficos) {
  ggsave(file.path(carpeta, nombre), g, width = ancho, height = alto, dpi = dpi)
}




# 2. LECTURA Y PREPARACION DE LOS DATOS -----
## 2.1 Lectura del Excel ----
stopifnot(file.exists(ruta_excel))
# Archivo de SCRIP_ESTIMACION_AREAS_PEQUENAS.R.
# Sus columnas se renombran a los nombres que usa este script (suavizado = Fay-Herriot).
est <- read_excel(ruta_excel, sheet = "Comparacion") %>%
  rename(valor_suavizado_pct = valor_fh_pct,
         error_suavizado_pp  = error_fh_pp,
         cv_pct              = cv_original_pct,
         limite_inferior_pct = limite_inferior_original_pct,
         limite_superior_pct = limite_superior_original_pct)

columnas_necesarias <- c("anio", "provincia", "canton", "indicador",
                         "muestra_denominador", "valor_original_pct",
                         "error_original_pp", "limite_inferior_pct",
                         "limite_superior_pct", "error_maximo_tolerable_pp",
                         "error_gvf_pp", "valor_suavizado_pct",
                         "error_suavizado_pp", "peso_dato_original")
stopifnot(all(columnas_necesarias %in% names(est)))

## 2.2 Etiquetas de indicadores y preparacion ----
etiquetas_indicador <- c(
  inasistencia_12_17                        = "Inasistencia 12-17",
  desempleo_general                         = "Desempleo",
  pobreza_ingresos                          = "Pobreza por ingresos",
  adolescentes_nbi_sobre_total_provisional  = "NBI adolescente (sobre población total)",
  monoparental_jefatura_aproximacion        = "Monoparentalidad (proxy)")
indicadores <- intersect(names(etiquetas_indicador), unique(est$indicador))

est <- est %>%
  filter(indicador %in% indicadores) %>%
  mutate(
    provincia = ifelse(str_detect(provincia, "Santo Domingo"), "Santo Domingo", provincia),
    etiqueta  = unname(etiquetas_indicador[indicador]))

## 2.3 Resumen de lo leido ----
print(est %>%
        group_by(Indicador = etiqueta) %>%
        summarise(`Celdas con valor` = sum(!is.na(valor_original_pct)),
                  `Sin dato (muestra 0)` = sum(is.na(valor_original_pct) | muestra_denominador == 0, na.rm = TRUE),
                  `Error original cero` = sum(error_original_pp <= 1e-10, na.rm = TRUE),
                  `Muestra < 60`     = sum(muestra_denominador > 0 & muestra_denominador < marca_muestra,
                                           na.rm = TRUE),
                  .groups = "drop"))


# -----------------------------------------------------------------------------#
# 3. OBJETOS Y TABLAS DE MUESTRA BAJA (MENOR A 60) -----
## Base cantonal de muestra baja (menor a 60) ----
# Un renglon por canton-anio-indicador con muestra entre 1 y 59.
# Los canton-anios con muestra cero no tienen dato y no se incluyen.
base_muestra_baja <- est %>%
  filter(!is.na(muestra_denominador),
         muestra_denominador > 0,
         muestra_denominador < marca_muestra) %>%
  mutate(
    cv_original_pct  = cv_pct,
    cv_gvf_pct       = ifelse(valor_original_pct > 0,
                              error_gvf_pp / valor_original_pct * 100, NA_real_),
    cv_suavizado_pct = ifelse(valor_suavizado_pct > 0,
                              error_suavizado_pp / valor_suavizado_pct * 100, NA_real_)) %>%
  arrange(etiqueta, provincia, canton, anio)


## Objeto 1: lista de canton-anio-indicador con muestra menor a 60 ----
lista_muestra_baja <- base_muestra_baja %>%
  transmute(Indicador = etiqueta,
            Provincia = provincia,
            Canton    = canton,
            Anio      = anio,
            Muestra   = muestra_denominador)

print(lista_muestra_baja, n = Inf, width = Inf)


## Objeto 2: errores y CV en cada etapa (original, GVF, suavizado) ----
# Error estandar y CV: original (antes), con GVF (varianza suavizada) y suavizado final (despues de la estimacion en areas pequenas).
# El modelo usa la varianza suavizada (GVF) en todos los canton-anio.
errores_muestra_baja <- base_muestra_baja %>%
  transmute(Indicador = etiqueta,
            Provincia = provincia,
            Canton    = canton,
            Anio      = anio,
            Muestra   = muestra_denominador,
            `Error estándar original (pp)`   = round(error_original_pp, 2),
            `CV original (%)`                = round(cv_original_pct, 1),
            `Error estándar con GVF (pp)`    = round(error_gvf_pp, 2),
            `CV con GVF (%)`                 = round(cv_gvf_pct, 1),
            `Error estándar suavizado (pp)`  = round(error_suavizado_pp, 2),
            `CV suavizado (%)`               = round(cv_suavizado_pct, 1))

print(errores_muestra_baja, n = Inf, width = Inf)


## Objeto 3: estimacion original vs estimacion suavizada ----
# Valor del indicador (%) antes y despues de la estimacion en areas pequenas.
estimaciones_muestra_baja <- base_muestra_baja %>%
  transmute(Indicador = etiqueta,
            Provincia = provincia,
            Canton    = canton,
            Anio      = anio,
            Muestra   = muestra_denominador,
            `Estimación original (%)`  = round(valor_original_pct, 2),
            `Estimación suavizada (%)` = round(valor_suavizado_pct, 2),
            `Cambio (pp)`              = round(valor_suavizado_pct - valor_original_pct, 2))

print(estimaciones_muestra_baja, n = Inf, width = Inf)


# ------------------------------------------------------------------------------ #
# VISUALIZACION. CV SEGUN EL TAMANO DE MUESTRA: 6 IMAGENES -----

# Tres tipos de CV, cada uno con dos imagenes:
#   GENERAL : los 5 anios juntos.
#   PANEL   : un cuadro por anio (2021 a 2025), en una fila horizontal.
# Tipos de CV:
#   CV original  = error estandar directo / valor original * 100
#   CV con GVF   = error estandar con varianza suavizada por GVF / valor original * 100 (antes de la estimacion en areas pequenas)
#   CV suavizado = error estandar final (Fay-Herriot) / valor suavizado * 100 (despues de la estimacion en areas pequenas)
# Misma escala vertical en las 6 imagenes.

## Anios, etiquetas y paleta ----
anios_referencia <- sort(unique(est$anio))

etiquetas_g7 <- c(
  inasistencia_12_17                       = "Inasistencia escolar (12–17)",
  desempleo_general                        = "Desempleo general",
  adolescentes_nbi_sobre_total_provisional = "Adolescentes NBI / total (provisional)",
  monoparental_jefatura_aproximacion       = "Monoparentalidad (aproximación)",
  pobreza_ingresos                         = "Pobreza por ingresos")
orden_g7 <- intersect(names(etiquetas_g7), indicadores)
relleno_g7 <- c("#222222", "#EEEEEE", "#1F5A80", "#4388B5", "#8FBBD9")
puntos_g7  <- c("#222222", "#9E9E9E", "#1F5A80", "#4388B5", "#8FBBD9")
raya_g7    <- c("white", "black", "white", "white", "black")
names(relleno_g7) <- names(puntos_g7) <- names(raya_g7) <-
  unname(etiquetas_g7[c("inasistencia_12_17", "desempleo_general",
                        "adolescentes_nbi_sobre_total_provisional",
                        "monoparental_jefatura_aproximacion", "pobreza_ingresos")])

## Datos ----
d8 <- est %>%
  filter(anio %in% anios_referencia, indicador %in% orden_g7,
         !is.na(valor_original_pct), muestra_denominador > 0) %>%
  mutate(ind7   = factor(unname(etiquetas_g7[indicador]), levels = unname(etiquetas_g7[orden_g7])),
         rango_muestra = factor(ifelse(muestra_denominador >= marca_muestra, "60 o más", "Menos de 60"),
                                levels = c("Menos de 60", "60 o más")),
         error_cero = error_original_pp <= 1e-10,
         cv_pct_sin_error_cero = ifelse(error_cero, NA_real_, cv_pct),
         cv_gvf_pct       = ifelse(valor_original_pct > 0, error_gvf_pp / valor_original_pct * 100, NA_real_),
         cv_suavizado_pct = ifelse(valor_suavizado_pct > 0, error_suavizado_pp / valor_suavizado_pct * 100, NA_real_))

limite_orig <- max(d8$cv_pct_sin_error_cero, d8$cv_suavizado_pct, na.rm = TRUE)   # misma escala en las 6 imagenes
n_fuera_gvf <- sum(d8$cv_gvf_pct > limite_orig, na.rm = TRUE)

## Funcion del grafico ----
# solo_general = TRUE : un grafico con los 5 anios juntos.
# solo_general = FALSE: panel horizontal, un cuadro por anio en una fila.
grafico_cv_muestra <- function(datos, columna, titulo, eje_y, nota, limite, solo_general = FALSE) {
  datos <- datos %>% mutate(cv = .data[[columna]]) %>% filter(!is.na(cv))
  if (solo_general) {
    datos <- datos %>% mutate(panel = factor("Los 5 años"))
  } else {datos <- datos %>% mutate(panel = factor(as.character(anio), levels = as.character(anios_referencia)))}
  medianas <- datos %>% group_by(panel, rango_muestra, ind7) %>%
    summarise(mediana = median(cv), .groups = "drop") %>%
    mutate(raya = raya_g7[as.character(ind7)])
  ggplot(datos, aes(rango_muestra, cv, fill = ind7)) +
    geom_point(aes(colour = ind7),
               position = position_jitterdodge(jitter.width = .15, dodge.width = .8),
               size = ifelse(solo_general, 1, 0.8), alpha = .35, show.legend = FALSE) +
    geom_boxplot(position = position_dodge(.8), width = .7, colour = "grey15",
                 linewidth = .4, outlier.shape = NA) +
    geom_errorbar(data = medianas, aes(x = rango_muestra, ymin = mediana, ymax = mediana,
                                       group = ind7, colour = raya),
                  position = position_dodge(.8), width = .7, linewidth = .9, inherit.aes = FALSE) +
    {if (!solo_general) facet_wrap(~panel, nrow = 1)} +
    scale_fill_manual(values = relleno_g7, name = NULL) +
    scale_colour_manual(values = c(puntos_g7, white = "white", black = "black"), guide = "none") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = seq(0, 400, 20),
                       expand = expansion(mult = c(0, .05))) +
    coord_cartesian(ylim = c(0, limite)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = titulo, x = "Tamaño de la muestra (registros)", y = eje_y, caption = nota) +
    theme_minimal(base_size = 13) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey92"),
          axis.line = element_line(colour = "grey20"),
          strip.text = element_text(face = "bold", size = 13),
          plot.title = element_text(family = "serif", face = "bold", size = 21),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9.5, lineheight = 1.4, colour = "grey20"),
          plot.caption.position = "plot",
          plot.margin = margin(15, 25, 10, 25), legend.position = "bottom")
}

## Notas al pie ----
nota_c <- paste0(
  "Cada punto es un cantón-año. CV original = error original ÷ dato original × 100. La línea de la caja es la mediana.\n",
  "No se dibujan los cantón-año sin dato, con dato de 0 % o con error original de cero.")

nota_g <- paste0(
  "Cada punto es un cantón-año. CV con GVF = error con la varianza GVF ÷ dato original × 100. La línea de la caja es la mediana.\n",
  "La escala se recorta en ", round(limite_orig), " %; ", n_fuera_gvf, " cantón-año quedan fuera del dibujo, pero cuentan en las cajas.\n",
  "No se dibujan los cantón-año sin dato o con dato de 0 %.")

nota_d <- paste0(
  "Cada punto es un cantón-año. CV Fay-Herriot = error Fay-Herriot ÷ estimación Fay-Herriot × 100. La línea de la caja es la mediana.\n",
  "Solo cantón-año con muestra, agrupados por el tamaño de su muestra.")


# ==============================================================================
# CV ORIGINAL: general y panel por anio -----
# ==============================================================================
g1 <- grafico_cv_muestra(d8, "cv_pct_sin_error_cero",
                         "CV original según el tamaño de muestra, por indicador",
                         "Coeficiente de variación original (CV %)", nota_c, limite_orig, solo_general = TRUE)
guardar_png(g1, "1_cv_original_general.png", 11, 6.2, dpi = 200)

g2 <- grafico_cv_muestra(d8, "cv_pct_sin_error_cero",
                         "CV original según el tamaño de muestra, por indicador y año",
                         "Coeficiente de variación original (CV %)", nota_c, limite_orig)
guardar_png(g2, "2_cv_original_panel_anios.png", 26, 9, dpi = 200)


# CV CON GVF (antes de areas pequenas): general y panel por anio -----

g3 <- grafico_cv_muestra(d8, "cv_gvf_pct",
                         "CV con varianza suavizada (GVF) según el tamaño de muestra",
                         "Coeficiente de variación con GVF (CV %)", nota_g, limite_orig, solo_general = TRUE)
guardar_png(g3, "3_cv_gvf_general.png", 11, 6.2, dpi = 200)

g4 <- grafico_cv_muestra(d8, "cv_gvf_pct",
                         "CV con varianza suavizada (GVF) según el tamaño de muestra, por año",
                         "Coeficiente de variación con GVF (CV %)", nota_g, limite_orig)
guardar_png(g4, "4_cv_gvf_panel_anios.png", 26, 9, dpi = 200)


# CV SUAVIZADO (despues de areas pequenas): general y panel por anio -----

g5 <- grafico_cv_muestra(d8, "cv_suavizado_pct",
                         "CV Fay-Herriot según el tamaño de muestra, por indicador",
                         "Coeficiente de variación Fay-Herriot (CV %)", nota_d, limite_orig, solo_general = TRUE)
guardar_png(g5, "5_cv_suavizado_general.png", 11, 6.2, dpi = 200)

g6 <- grafico_cv_muestra(d8, "cv_suavizado_pct",
                         "CV Fay-Herriot según el tamaño de muestra, por indicador y año",
                         "Coeficiente de variación Fay-Herriot (CV %)", nota_d, limite_orig)
guardar_png(g6, "6_cv_suavizado_panel_anios.png", 26, 9, dpi = 200)



# BASE DE SALTOS ENTRE ANIOS (la usan los ejemplos de abajo) -----


# Funcion auxiliar: maximo que devuelve NA si todo es NA (en vez de -Inf con aviso)
max_seguro <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

base_saltos <- est %>%
  arrange(provincia, canton, indicador, anio) %>%
  group_by(provincia, canton, indicador) %>%
  mutate(salto_original  = abs(valor_original_pct  - lag(valor_original_pct)),
         salto_suavizado = abs(valor_suavizado_pct - lag(valor_suavizado_pct)),
         sin_dato   = is.na(valor_original_pct),
         error_cero = !is.na(error_original_pp) & error_original_pp <= 1e-10) %>%
  ungroup()



# EJEMPLO 1: EL CANTON CON LOS SALTOS MAS ABRUPTOS -----

# 1) Ranking de cantones por saltos entre anios consecutivos (solo cantones con los 25 datos).
ranking_saltos <- base_saltos %>%
  group_by(provincia, canton, indicador) %>%
  summarise(anios = n(),
            max_salto_original  = suppressWarnings(max(salto_original,  na.rm = TRUE)),
            max_salto_suavizado = suppressWarnings(max(salto_suavizado, na.rm = TRUE)),
            .groups = "drop") %>%
  filter(anios == 5) %>%
  group_by(provincia, canton) %>%
  filter(n() == 5) %>%
  summarise(`Suma de saltos máximos original (pp)`  = round(sum(max_salto_original), 1),
            `Suma de saltos máximos suavizado (pp)` = round(sum(max_salto_suavizado), 1),
            .groups = "drop") %>%
  arrange(desc(`Suma de saltos máximos original (pp)`))
print(ranking_saltos, n = 15, width = Inf)

# 2) Canton elegido: el primero del ranking.
# Para fijar otro, escribe su nombre y su provincia en lugar de la seleccion automatica.
canton_saltos    <- ranking_saltos$canton[1]
provincia_saltos <- ranking_saltos$provincia[1]
print(paste("Ejemplo 1:", canton_saltos, "-", provincia_saltos))

# 3) Tabla: valores, saltos y muestra de los 5 indicadores en los 5 anios
tabla_saltos_canton <- base_saltos %>%
  filter(canton == canton_saltos, provincia == provincia_saltos) %>%
  transmute(Indicador = etiqueta, Anio = anio, Muestra = muestra_denominador,
            `Estimación original (%)`  = round(valor_original_pct, 2),
            `Salto original (pp)`      = round(salto_original, 2),
            `Estimación suavizada (%)` = round(valor_suavizado_pct, 2),
            `Salto suavizado (pp)`     = round(salto_suavizado, 2)) %>%
  arrange(Indicador, Anio)
print(tabla_saltos_canton, n = Inf, width = Inf)

# 4) Datos del grafico en formato largo
datos_saltos_largo <- base_saltos %>%
  filter(canton == canton_saltos, provincia == provincia_saltos) %>%
  select(etiqueta, anio, muestra_denominador,
         Original = valor_original_pct, `Fay-Herriot` = valor_suavizado_pct) %>%
  pivot_longer(c(Original, `Fay-Herriot`), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Fay-Herriot")))

# Misma escala vertical en cada indicador, en todas las imagenes
limites_saltos <- datos_saltos_largo %>%
  group_by(etiqueta) %>%
  summarise(lo = 0, hi = max(valor, na.rm = TRUE) * 1.15, .groups = "drop") %>%
  pivot_longer(c(lo, hi), values_to = "y") %>%
  mutate(anio = min(datos_saltos_largo$anio))

# 5) Funcion del grafico: un panel por indicador, los anios en el eje X
grafico_saltos <- function(etapas, titulo) {
  d <- filter(datos_saltos_largo, etapa %in% etapas)
  ggplot(d, aes(anio, valor, colour = etapa, group = etapa)) +
    geom_blank(data = limites_saltos, aes(x = anio, y = y), inherit.aes = FALSE) +
    geom_line(linewidth = .9) +
    geom_point(size = 2.6) +
    geom_text(data = filter(d, etapa == etapas[1]),
              aes(label = paste0("n=", muestra_denominador)),
              vjust = -1, size = 3.4, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 2, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", `Fay-Herriot` = "#C0392B"), name = NULL) +
    scale_x_continuous(breaks = 2021:2025, expand = expansion(mult = 0.13)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros encuestados del indicador en el cantón y el año. ",
                          "Cada recuadro tiene su propia escala.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 10.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}

nombre_cantones <- paste0(canton_saltos, " (", provincia_saltos, ")")
g_saltos_pc_ambos <- grafico_saltos(c("Original", "Fay-Herriot"),
                                    paste0("Dato original y Fay-Herriot: ", nombre_cantones))

print(g_saltos_pc_ambos)
guardar_png(g_saltos_pc_ambos, "ejemplo_1_saltos_ambos.png", 11, 7, dpi = 200)



# EJEMPLO 2: SALTOS ABRUPTOS SIN CEROS NO ESTIMABLES -----


## Parametros de esta seccion (cambialos si quieres) ----
max_error_cero  <- 0    # maximo de cantón-indicador-año con error original cero que se tolera
min_muestra_req <- 30   # muestra minima (en todos los indicadores y años) para entrar al ranking


## 2) Diagnostico de calidad por canton ----
diagnostico_cantones <- base_saltos %>%
  group_by(provincia, canton) %>%
  summarise(datos_validos     = sum(!sin_dato),
            con_NA            = sum(sin_dato),
            errores_cero      = sum(error_cero),
            ceros_exactos     = sum(valor_original_pct == 0, na.rm = TRUE),
            muestra_minima    = suppressWarnings(min(muestra_denominador, na.rm = TRUE)),
            .groups = "drop")


## 3) Ranking de cantones por saltos (solo cantones confiables) ----
saltos_por_indicador <- base_saltos %>%
  group_by(provincia, canton, indicador) %>%
  summarise(max_salto_original  = max_seguro(salto_original),
            max_salto_suavizado = max_seguro(salto_suavizado),
            .groups = "drop")

ranking_saltos <- saltos_por_indicador %>%
  group_by(provincia, canton) %>%
  summarise(suma_original  = sum(max_salto_original,  na.rm = TRUE),
            suma_suavizado = sum(max_salto_suavizado, na.rm = TRUE),
            .groups = "drop") %>%
  inner_join(diagnostico_cantones, by = c("provincia", "canton")) %>%
  filter(datos_validos == 25,                 # sin NA: los 5 indicadores x 5 anios
         errores_cero <= max_error_cero,      # sin ceros no estimables
         muestra_minima >= min_muestra_req) %>%
  transmute(Provincia = provincia,
            Canton    = canton,
            `Suma de saltos máximos original (pp)`  = round(suma_original, 1),
            `Suma de saltos máximos suavizado (pp)` = round(suma_suavizado, 1),
            `Muestra mínima` = muestra_minima) %>%
  arrange(desc(`Suma de saltos máximos original (pp)`))

print(ranking_saltos, n = 15, width = Inf)


## 4) Canton elegido: el primero del ranking ----
canton_saltos    <- ranking_saltos$Canton[1]
provincia_saltos <- ranking_saltos$Provincia[1]
print(paste("Ejemplo 2:", canton_saltos, "-", provincia_saltos))

stopifnot(nrow(filter(base_saltos, canton == canton_saltos,
                      provincia == provincia_saltos)) > 0)

diagnostico_canton <- diagnostico_cantones %>%
  filter(canton == canton_saltos, provincia == provincia_saltos)
print(diagnostico_canton, width = Inf)


## 5) Tabla: valores, saltos y muestra de los 5 indicadores en los 5 anios ----
tabla_saltos_canton <- base_saltos %>%
  filter(canton == canton_saltos, provincia == provincia_saltos) %>%
  transmute(Indicador = etiqueta, Anio = anio, Muestra = muestra_denominador,
            `Estimación original (%)`  = round(valor_original_pct, 2),
            `Error original cero`      = ifelse(error_cero, "Sí", "No"),
            `Salto original (pp)`      = round(salto_original, 2),
            `Estimación suavizada (%)` = round(valor_suavizado_pct, 2),
            `Salto suavizado (pp)`     = round(salto_suavizado, 2)) %>%
  arrange(Indicador, Anio)
print(tabla_saltos_canton, n = Inf, width = Inf)


## 6) Datos del grafico en formato largo ----
datos_saltos_largo <- base_saltos %>%
  filter(canton == canton_saltos, provincia == provincia_saltos) %>%
  select(etiqueta, anio, muestra_denominador, error_cero,
         Original = valor_original_pct, `Fay-Herriot` = valor_suavizado_pct) %>%
  pivot_longer(c(Original, `Fay-Herriot`), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Fay-Herriot")),
         cero_no_estimable = (etapa == "Original") & error_cero)

limites_saltos <- datos_saltos_largo %>%
  group_by(etiqueta) %>%
  summarise(lo = 0, hi = max(valor, na.rm = TRUE) * 1.15, .groups = "drop") %>%
  pivot_longer(c(lo, hi), values_to = "y") %>%
  mutate(anio = min(datos_saltos_largo$anio))


## 7) Funcion del grafico: un panel por indicador, los anios en el eje X ----
grafico_saltos <- function(etapas, titulo) {
  d <- filter(datos_saltos_largo, etapa %in% etapas)
  ggplot(d, aes(anio, valor, colour = etapa, group = etapa)) +
    geom_blank(data = limites_saltos, aes(x = anio, y = y), inherit.aes = FALSE) +
    geom_line(linewidth = .9) +
    geom_point(aes(shape = cero_no_estimable), size = 2.6) +
    geom_text(data = filter(d, etapa == etapas[1]),
              aes(label = paste0("n=", muestra_denominador)),
              vjust = -1, size = 3.4, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 2, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", `Fay-Herriot` = "#C0392B"), name = NULL) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                       labels = c(`FALSE` = "Estimable", `TRUE` = "Error cero (no estimable)"),
                       guide = if (any(datos_saltos_largo$cero_no_estimable)) "legend" else "none",
                       name = NULL) +
    scale_x_continuous(breaks = 2021:2025, expand = expansion(mult = 0.13)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros encuestados del indicador en el cantón y el año. ",
                          "Cada recuadro tiene su propia escala.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 10.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}


## 8) Grafico: original frente a suavizado ----
nombre_cantones <- paste0(canton_saltos, " (", provincia_saltos, ")")
g_saltos_ambos <- grafico_saltos(c("Original", "Fay-Herriot"),
                                 paste0("Dato original y Fay-Herriot: ", nombre_cantones))

print(g_saltos_ambos)
guardar_png(g_saltos_ambos, "ejemplo_2_saltos_ambos.png", 11, 7, dpi = 200)



# EJEMPLO 3: UN CANTON ESTABLE (MUESTRAS GRANDES) -----


## 1) Ranking de estabilidad: donde el suavizado cambia menos ----
ranking_estables <- base_saltos %>%
  mutate(cambio_abs = abs(valor_suavizado_pct - valor_original_pct)) %>%
  group_by(provincia, canton) %>%
  summarise(muestra_minima    = suppressWarnings(min(muestra_denominador, na.rm = TRUE)),
            muestra_mediana   = median(muestra_denominador, na.rm = TRUE),
            cv_mediano        = median(cv_pct, na.rm = TRUE),
            peso_original_med = mean(peso_dato_original, na.rm = TRUE),
            cambio_abs_medio  = mean(cambio_abs, na.rm = TRUE),
            cambio_abs_max    = max_seguro(cambio_abs),
            .groups = "drop") %>%
  inner_join(diagnostico_cantones %>% select(provincia, canton, datos_validos, errores_cero),
             by = c("provincia", "canton")) %>%
  filter(datos_validos == 25, errores_cero == 0) %>%
  arrange(cambio_abs_medio) %>%
  transmute(Provincia = provincia, Canton = canton,
            `Muestra mínima`            = muestra_minima,
            `Muestra mediana`           = muestra_mediana,
            `CV mediano (%)`            = round(cv_mediano, 1),
            `Peso del dato original`    = round(peso_original_med, 2),
            `Cambio absoluto medio (pp)` = round(cambio_abs_medio, 2),
            `Cambio absoluto máximo (pp)` = round(cambio_abs_max, 2))

print(ranking_estables, n = 10, width = Inf)


## 2) Canton elegido: el primero del ranking ----
canton_estable    <- ranking_estables$Canton[1]
provincia_estable <- ranking_estables$Provincia[1]
print(paste("Ejemplo 3:", canton_estable, "-", provincia_estable))

stopifnot(nrow(filter(base_saltos, canton == canton_estable,
                      provincia == provincia_estable)) > 0)

print(diagnostico_cantones %>%
        filter(canton == canton_estable, provincia == provincia_estable), width = Inf)


## 3) Tabla: valores, cambio por suavizado, peso y muestra ----
tabla_estable <- base_saltos %>%
  filter(canton == canton_estable, provincia == provincia_estable) %>%
  transmute(Indicador = etiqueta, Anio = anio, Muestra = muestra_denominador,
            `CV original (%)`          = round(cv_pct, 1),
            `Estimación original (%)`  = round(valor_original_pct, 2),
            `Estimación suavizada (%)` = round(valor_suavizado_pct, 2),
            `Cambio por suavizado (pp)` = round(valor_suavizado_pct - valor_original_pct, 2),
            `Peso del dato original`   = round(peso_dato_original, 2),
            `Salto original (pp)`      = round(salto_original, 2),
            `Salto suavizado (pp)`     = round(salto_suavizado, 2)) %>%
  arrange(Indicador, Anio)
print(tabla_estable, n = Inf, width = Inf)


## 4) Comparacion: canton inestable vs canton estable ----
comparar_cantones <- function(canton, provincia, nombre) {
  base_saltos %>%
    filter(canton == !!canton, provincia == !!provincia) %>%
    summarise(Canton = nombre,
              `Muestra mediana`            = median(muestra_denominador, na.rm = TRUE),
              `CV original mediano (%)`    = round(median(cv_pct, na.rm = TRUE), 1),
              `Peso medio del dato original` = round(mean(peso_dato_original, na.rm = TRUE), 2),
              `Cambio absoluto medio (pp)` = round(mean(abs(valor_suavizado_pct - valor_original_pct), na.rm = TRUE), 2),
              `Cambio absoluto máximo (pp)` = round(max(abs(valor_suavizado_pct - valor_original_pct), na.rm = TRUE), 2))
}

tabla_comparacion <- bind_rows(
  comparar_cantones(canton_saltos,   provincia_saltos,   paste0(canton_saltos, " (muestras chicas)")),
  comparar_cantones(canton_estable,  provincia_estable,  paste0(canton_estable, " (muestras grandes)")))
print(tabla_comparacion, width = Inf)


## 5) Datos del grafico en formato largo ----
datos_estable_largo <- base_saltos %>%
  filter(canton == canton_estable, provincia == provincia_estable) %>%
  select(etiqueta, anio, muestra_denominador,
         Original = valor_original_pct, `Fay-Herriot` = valor_suavizado_pct) %>%
  pivot_longer(c(Original, `Fay-Herriot`), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Fay-Herriot")))

limites_estable <- datos_estable_largo %>%
  group_by(etiqueta) %>%
  summarise(lo = 0, hi = max(valor, na.rm = TRUE) * 1.15, .groups = "drop") %>%
  pivot_longer(c(lo, hi), values_to = "y") %>%
  mutate(anio = min(datos_estable_largo$anio))


## 6) Funcion del grafico ----
grafico_estable <- function(etapas, titulo) {
  d <- filter(datos_estable_largo, etapa %in% etapas)
  ggplot(d, aes(anio, valor, colour = etapa, group = etapa)) +
    geom_blank(data = limites_estable, aes(x = anio, y = y), inherit.aes = FALSE) +
    geom_line(linewidth = .9) +
    geom_point(size = 2.6) +
    geom_text(data = filter(d, etapa == etapas[1]),
              aes(label = paste0("n=", format(muestra_denominador, big.mark = ".", decimal.mark = ","))),
              vjust = -1, size = 3.2, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 2, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", `Fay-Herriot` = "#C0392B"), name = NULL) +
    scale_x_continuous(breaks = 2021:2025, expand = expansion(mult = 0.13)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros encuestados del indicador en el cantón y el año. ",
                          "Cada recuadro tiene su propia escala.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 10.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}


## 7) Grafico: original frente a suavizado ----
nombre_estable <- paste0(canton_estable, " (", provincia_estable, ")")
g_estable_ambos <- grafico_estable(c("Original", "Fay-Herriot"),
                                   paste0("Dato original y Fay-Herriot: ", nombre_estable))

print(g_estable_ambos)
guardar_png(g_estable_ambos, "ejemplo_3_estable_ambos.png", 11, 7, dpi = 200)



# CV ORIGINAL SEGUN LA DENSIDAD MUESTRAL DEL CANTON -----


## 1) Parametros que puedes cambiar ----
anio_fig       <- 2022            # anio del grafico
canton_alta    <- "Guayaquil";  provincia_alta <- "Guayas"   # alta densidad muestral
canton_baja    <- "Bolívar";    provincia_baja <- "Manabí"   # baja densidad muestral
limite_y       <- NA              # NA = escala automatica; o un numero (p. ej. 60) para recortar

## 2) Etiquetas y paleta ----
etiquetas_fig <- c(
  inasistencia_12_17                       = "Inasistencia escolar (12–17)",
  desempleo_general                        = "Desempleo general",
  adolescentes_nbi_sobre_total_provisional = "Adolescentes NBI / total (provisional)",
  monoparental_jefatura_aproximacion       = "Monoparentalidad (aproximación)",
  pobreza_ingresos                         = "Pobreza por ingresos")
paleta_fig <- c("#222222", "#9E9E9E", "#1F5A80", "#4388B5", "#8FBBD9")
names(paleta_fig) <- unname(etiquetas_fig)

## 3) Datos ----
datos_fig <- est %>%
  filter(anio == anio_fig,
         indicador %in% names(etiquetas_fig),
         (canton == canton_alta & provincia == provincia_alta) |
           (canton == canton_baja & provincia == provincia_baja)) %>%
  mutate(grupo = factor(ifelse(canton == canton_alta, "alta", "baja"),
                        levels = c("alta", "baja"),
                        labels = c(paste0("ALTA DENSIDAD MUESTRAL\n(", canton_alta, ")"),
                                   paste0("BAJA DENSIDAD MUESTRAL\n(", canton_baja, ")"))),
         ind = factor(unname(etiquetas_fig[indicador]), levels = unname(etiquetas_fig)),
         cv_original = ifelse(error_original_pp <= 1e-10, NA_real_, cv_pct))

stopifnot(nrow(datos_fig) == 10)   # 2 cantones x 5 indicadores

tabla_fig <- datos_fig %>%
  arrange(grupo, ind) %>%
  transmute(Canton = canton, Indicador = ind, Muestra = muestra_denominador,
            `Valor original (%)` = round(valor_original_pct, 2),
            `Error estándar (pp)` = round(error_original_pp, 2),
            `CV original (%)`    = round(cv_original, 1))
print(tabla_fig, n = Inf, width = Inf)

if (any(is.na(datos_fig$cv_original)))
  warning("Hay CV originales sin calcular (error cero): saldran como 's/d'. Prueba otro anio.")

ymax_fig <- if (is.na(limite_y)) {
  max(datos_fig$cv_original, na.rm = TRUE) * 1.25
} else limite_y

## 4) Funcion del grafico ----
# Sobre cada barra: el CV y, debajo, la muestra (n) del denominador de ese indicador.
grafico_cv_densidad <- function(titulo, nota) {
  d <- datos_fig %>%
    mutate(cv = cv_original,
           cv_barra = pmin(coalesce(cv, 0), ymax_fig),
           etiqueta_cv = ifelse(is.na(cv), "s/d",
                                paste0(format(round(cv, 1), nsmall = 1, decimal.mark = ","), "%")),
           etiqueta = paste0(etiqueta_cv, "\nn = ",
                             formatC(muestra_denominador, format = "d", big.mark = ".", decimal.mark = ",")))
  n_recortadas <- sum(d$cv > ymax_fig, na.rm = TRUE)
  nota_extra <- if (n_recortadas > 0)
    paste0("\nHay ", n_recortadas, " barra(s) recortada(s) en ", round(ymax_fig),
           " %; su valor real está en la etiqueta.") else ""
  
  ggplot(d, aes(grupo, cv_barra, fill = ind)) +
    geom_col(position = position_dodge(.85), width = .8,
             colour = "grey15", linewidth = .3) +
    geom_text(aes(label = etiqueta), position = position_dodge(.85),
              vjust = -.3, size = 3.2, lineheight = .9, family = "serif") +
    scale_fill_manual(values = paleta_fig, name = NULL) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, .02))) +
    coord_cartesian(ylim = c(0, ymax_fig)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = titulo,
         x = "Grupo de cantones", y = "Coeficiente de variación (CV %)",
         caption = paste0(nota, nota_extra)) +
    theme_minimal(base_size = 13) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(colour = "grey92"),
          axis.line = element_line(colour = "grey20"),
          axis.text.x = element_text(face = "bold", size = 11),
          plot.title = element_text(family = "serif", face = "bold", size = 18),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, lineheight = 1.4, colour = "grey20"),
          plot.caption.position = "plot",
          plot.margin = margin(15, 25, 10, 25), legend.position = "bottom")
}

## 5) Grafico ----
nota_orig <- paste0("CV original = error estándar directo ÷ valor original × 100. Año ", anio_fig,
                    ".\nn = muestra (registros del denominador de cada indicador) en el cantón. ",
                    "\ns/d = sin dato (error original cero: el CV no se puede calcular).")

g_figB_original <- grafico_cv_densidad(
  paste0("CV original por densidad muestral del cantón, ", anio_fig), nota_orig)

print(g_figB_original)
guardar_png(g_figB_original, "figB_cv_densidad_original.png", 12, 7.2, dpi = 200)


# CV MAYOR A 30 %: ORIGINAL, GVF Y FAY-HERRIOT ----------------------------------

# CV = error / valor * 100. Solo se calcula si el valor es mayor que 0.
# Un CV original bajo no siempre es precision: con pocas zonas encuestadas el error sale muy bajo o 0.
# Por eso el original se separa con los criterios del INEC (2025, pp. 22-23), muestra >= 60 y grados de libertad >= 9, mas error mayor que 0 (un error de 0 indica falta de variacion en la muestra; Corral et al., 2022, p. 19, advierte que ocurre con poca muestra).
cv30 <- read_excel(ruta_excel, sheet = "Comparacion", guess_max = 5000) %>%
  mutate(
    Indicador = unname(c(
      pobreza_ingresos = "Pobreza",
      adolescentes_nbi_sobre_total_provisional = "NBI adolescente",
      monoparental_jefatura_aproximacion = "Monoparentalidad",
      inasistencia_12_17 = "Inasistencia",
      desempleo_general = "Desempleo")[indicador]),
    grupo = case_when(is.na(valor_original_pct) ~ "Sin muestra",
                      valor_original_pct == 0   ~ "Dato directo 0 %",
                      TRUE                      ~ "Dato mayor que 0"),
    confiable = coalesce(cumple_muestra & cumple_gl & error_original_pp > 1e-10, FALSE))
cv30_dato <- filter(cv30, grupo == "Dato mayor que 0")

# Agrega una fila con el total de los cinco indicadores
con_total <- function(d, f) {
  bind_rows(d %>% group_by(Indicador) %>% f(), d %>% f() %>% mutate(Indicador = "TOTAL"))
}

# Tabla 1. Los tres pasos, en las mismas areas (dato directo mayor que 0)
cv30_pasos <- con_total(cv30_dato, \(x) summarise(x,
  areas = n(),
  original = sum(cv_original_pct > 30), gvf = sum(cv_gvf_pct > 30),
  fay_herriot = sum(cv_fh_pct > 30), .groups = "drop")) %>%
  mutate(original_pct = round(100 * original / areas, 1),
         gvf_pct = round(100 * gvf / areas, 1),
         fay_herriot_pct = round(100 * fay_herriot / areas, 1))

# Tabla 2. El CV original por dentro: cuales son confiables de verdad
cv30_original <- con_total(cv30, \(x) summarise(x,
  total = n(),
  confiable_cv_hasta_30 = sum(confiable & grupo == "Dato mayor que 0" & cv_original_pct <= 30, na.rm = TRUE),
  confiable_cv_mayor_30 = sum(confiable & grupo == "Dato mayor que 0" & cv_original_pct > 30, na.rm = TRUE),
  no_confiable_cv_hasta_30 = sum(!confiable & grupo == "Dato mayor que 0" & cv_original_pct <= 30, na.rm = TRUE),
  no_confiable_cv_mayor_30 = sum(!confiable & grupo == "Dato mayor que 0" & cv_original_pct > 30, na.rm = TRUE),
  dato_0 = sum(grupo == "Dato directo 0 %"),
  sin_muestra = sum(grupo == "Sin muestra"), .groups = "drop"))

# Tabla 3. Fay-Herriot: todas las estimaciones, segun el tipo de caso
cv30_fh <- cv30 %>%
  group_by(Indicador, grupo) %>%
  summarise(casos = n(), fh_mayor_30 = sum(cv_fh_pct > 30), .groups = "drop") %>%
  bind_rows(cv30 %>% group_by(grupo) %>%
              summarise(casos = n(), fh_mayor_30 = sum(cv_fh_pct > 30), .groups = "drop") %>%
              mutate(Indicador = "TOTAL"))

# Tabla 4. Fay-Herriot por anio
cv30_anio <- cv30 %>%
  group_by(Indicador, anio) %>%
  summarise(fh_mayor_30 = sum(cv_fh_pct > 30), .groups = "drop") %>%
  pivot_wider(names_from = anio, values_from = fh_mayor_30)

# Tabla 5. Lista de los casos con CV Fay-Herriot mayor a 30 %
cv30_lista <- cv30 %>%
  filter(cv_fh_pct > 30) %>%
  transmute(Indicador, provincia, canton, anio, grupo, muestra_denominador, grados_libertad,
            valor_original_pct, cv_original_pct, cv_gvf_pct, valor_fh_pct, error_fh_pp, cv_fh_pct) %>%
  arrange(Indicador, desc(cv_fh_pct))

# Tabla 6. El CV original por dentro: por que no es confiable
# Cada caso se cuenta una sola vez, en el primer motivo que cumple.
cv30_motivo <- cv30_dato %>%
  mutate(cv_original = if_else(cv_original_pct <= 30, "CV hasta 30 %", "CV mayor a 30 %"),
         motivo = case_when(
           error_original_pp <= 1e-10 ~ "error_0",
           muestra_denominador < 60 ~ "muestra_menor_60",
           grados_libertad < 9 ~ "gl_menor_9",
           TRUE ~ "cumple_todo"))
contar_motivo <- function(d) {
  summarise(d, casos = n(),
            cumple_todo = sum(motivo == "cumple_todo"),
            no_cumple = sum(motivo != "cumple_todo"),
            error_0 = sum(motivo == "error_0"),
            muestra_menor_60 = sum(motivo == "muestra_menor_60"),
            gl_menor_9 = sum(motivo == "gl_menor_9"), .groups = "drop")
}
cv30_motivo_tabla <- bind_rows(
  cv30_motivo %>% group_by(cv_original, Indicador) %>% contar_motivo(),
  cv30_motivo %>% group_by(cv_original) %>% contar_motivo() %>% mutate(Indicador = "TOTAL")) %>%
  arrange(cv_original, Indicador == "TOTAL")

# Tabla 7. Cuantos incumplen cada criterio (un caso puede estar en varios)
cv30_incumple <- cv30_motivo %>%
  group_by(cv_original) %>%
  summarise(casos = n(),
            con_error_0 = sum(error_original_pp <= 1e-10),
            con_muestra_menor_60 = sum(muestra_denominador < 60),
            con_gl_menor_9 = sum(grados_libertad < 9),
            con_muestra_y_gl = sum(muestra_denominador < 60 & grados_libertad < 9),
            .groups = "drop")

# Tabla 8. Aporte del modelo segun el tamano de muestra
# Error mediano que entra al modelo (GVF) y error mediano final, peso mediano de la encuesta y casos con CV mayor a 30 %.
# Sin muestra solo hay Fay-Herriot.
# El CV original y el de la GVF solo se cuentan con dato directo mayor que 0.
cv30_tramo <- cv30 %>%
  mutate(n_registros = coalesce(muestra_denominador, 0),
         Tramo = cut(n_registros, c(-1, 0, 59, 150, 300, 1000, Inf),
                     labels = c("Sin muestra", "1 a 59", "60 a 150", "151 a 300",
                                "301 a 1000", "Más de 1000")))
resumir_aporte <- function(d) {
  summarise(d, casos = n(),
            con_dato_mayor_0 = sum(grupo == "Dato mayor que 0"),
            original_mayor_30 = sum(cv_original_pct > 30 & grupo == "Dato mayor que 0", na.rm = TRUE),
            gvf_mayor_30 = sum(cv_gvf_pct > 30 & grupo == "Dato mayor que 0", na.rm = TRUE),
            fh_mayor_30_con_dato = sum(cv_fh_pct > 30 & grupo == "Dato mayor que 0"),
            fh_mayor_30_todos = sum(cv_fh_pct > 30),
            error_gvf_mediana_pp = round(median(error_gvf_pp, na.rm = TRUE), 2),
            error_fh_mediana_pp = round(median(error_fh_pp), 2),
            peso_encuesta_mediana = round(median(peso_dato_original), 2), .groups = "drop")
}
cv30_aporte <- cv30_tramo %>% group_by(Tramo) %>% resumir_aporte()
cv30_aporte_indicador <- cv30_tramo %>% group_by(Indicador, Tramo) %>% resumir_aporte()

# Dibuja una tabla como imagen: se ve en Plots y se guarda como PNG.
# La letra "serif" es Times New Roman en Windows.
#   d = tabla, titulo y nota = textos, nombre = archivo, ancho = pulgadas
tabla_imagen <- function(d, titulo, nombre, ancho, nota = NULL) {
  d <- as.data.frame(d)
  textos <- lapply(d, \(x) if (is.numeric(x))
    ifelse(is.na(x), "-", format(x, big.mark = " ", decimal.mark = ",", trim = TRUE,
                                 scientific = FALSE, drop0trailing = TRUE)) else as.character(x))
  encabezado <- stringr::str_wrap(names(d), 12)
  largo <- mapply(\(x, e) max(nchar(x), max(nchar(strsplit(e, "
")[[1]]))), textos, encabezado)
  ancho_col <- (largo + 3) / sum(largo + 3)
  ancho_col <- unname(ancho_col)
  centro <- cumsum(ancho_col) - ancho_col / 2
  n <- nrow(d)
  alto_enc <- 0.5 + 0.55 * max(lengths(strsplit(encabezado, "
")))
  celdas <- do.call(rbind, lapply(seq_along(textos), \(k) data.frame(
    x = centro[k], ancho = ancho_col[k], fila = seq_len(n), texto = textos[[k]],
    row.names = NULL, total = as.character(d[[1]]) == "TOTAL" | (names(d)[min(2, ncol(d))] == "Indicador" &
                                                as.character(d[[min(2, ncol(d))]]) == "TOTAL"))))
  cabecera <- data.frame(x = centro, ancho = ancho_col, texto = encabezado)
  lineas <- c(-0.5 + alto_enc, -0.5, -n - 0.5)
  filas_total <- sort(unique(celdas$fila[celdas$total]))
  inicio_total <- filas_total[!(filas_total - 1) %in% filas_total]
  g <- ggplot() +
    geom_segment(data = data.frame(y = lineas), aes(x = 0, xend = 1, y = y, yend = y),
                 colour = "black", linewidth = 0.6) +
    geom_segment(data = data.frame(y = 0.5 - inicio_total), aes(x = 0, xend = 1, y = y, yend = y),
                 colour = "black", linewidth = 0.25) +
    geom_text(data = cabecera, aes(x, -0.5 + alto_enc / 2, label = texto), colour = "black",
              fontface = "bold", size = 3.7, lineheight = 0.9, family = "serif") +
    geom_text(data = celdas, aes(x, -fila, label = texto, fontface = ifelse(total, "bold", "plain")),
              colour = "black", size = 3.8, family = "serif") +
    scale_x_continuous(limits = c(0, 1), expand = c(0.01, 0.01)) +
    labs(title = titulo, caption = if (is.null(nota)) NULL else stringr::str_wrap(nota, ancho * 13)) +
    theme_void(base_family = "serif") +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = 14, margin = margin(b = 8)),
          plot.caption = element_text(hjust = 0, size = 9, colour = "black", margin = margin(t = 8)),
          plot.margin = margin(12, 12, 12, 12))
  print(g)
  guardar_png(g, nombre, ancho, 1.3 + 0.28 * (n + alto_enc) + if (is.null(nota)) 0 else 0.35, dpi = 200,
              carpeta = carpeta_tablas)
  invisible(g)
}

tabla_imagen(
  cv30_pasos %>% transmute(Indicador, `Áreas` = areas, `Original > 30 %` = original, `Original (%)` = original_pct,
                           `GVF > 30 %` = gvf, `GVF (%)` = gvf_pct,
                           `Fay-Herriot > 30 %` = fay_herriot, `Fay-Herriot (%)` = fay_herriot_pct),
  "CV mayor a 30 % en cada paso", "tabla_1_cv_tres_pasos.png", 10,
  "Solo cantón-año con dato directo mayor que 0. CV = error / valor x 100.")
tabla_imagen(
  cv30_original %>% transmute(Indicador, Total = total,
                              `Confiable, CV hasta 30 %` = confiable_cv_hasta_30,
                              `Confiable, CV mayor a 30 %` = confiable_cv_mayor_30,
                              `No confiable, CV hasta 30 %` = no_confiable_cv_hasta_30,
                              `No confiable, CV mayor a 30 %` = no_confiable_cv_mayor_30,
                              `Dato 0 %` = dato_0, `Sin muestra` = sin_muestra),
  "El dato original por dentro", "tabla_2_original_detalle.png", 10.5,
  "Confiable: muestra de 60 o más registros, 9 o más grados de libertad (INEC, 2025) y error mayor que 0.")
tabla_imagen(
  cv30_motivo_tabla %>% transmute(`CV original` = cv_original, Indicador, Casos = casos,
                                  `Cumple todo` = cumple_todo, `No cumple` = no_cumple, `Error = 0` = error_0,
                                  `Muestra < 60` = muestra_menor_60, `Grados de libertad < 9` = gl_menor_9),
  "Por qué el CV original no es confiable", "tabla_3_original_motivo.png", 10.5,
  "Solo dato directo mayor que 0. Cada caso se cuenta una vez, en el primer motivo que cumple (de izquierda a derecha).")
tabla_imagen(
  cv30_incumple %>% transmute(`CV original` = cv_original, Casos = casos, `Error = 0` = con_error_0,
                              `Muestra < 60` = con_muestra_menor_60, `Grados de libertad < 9` = con_gl_menor_9,
                              `Muestra y grados de libertad` = con_muestra_y_gl),
  "Casos que incumplen cada criterio", "tabla_4_original_incumple.png", 9,
  "Un caso puede estar en varias columnas.")
tabla_imagen(
  cv30_fh %>% transmute(Indicador, Grupo = grupo, Casos = casos, `Fay-Herriot > 30 %` = fh_mayor_30),
  "Fay-Herriot: CV mayor a 30 % según el tipo de caso", "tabla_5_fh_tipo_caso.png", 8)
tabla_imagen(cv30_anio, "Fay-Herriot: CV mayor a 30 % por año", "tabla_6_fh_anio.png", 7.5,
             "De 86 cantones por indicador y año.")
tabla_imagen(
  cv30_aporte %>% transmute(`Registros` = as.character(Tramo), Casos = casos, `Con dato > 0` = con_dato_mayor_0,
                            `Original > 30 %` = original_mayor_30, `GVF > 30 %` = gvf_mayor_30,
                            `Fay-Herriot > 30 % (dato > 0)` = fh_mayor_30_con_dato,
                            `Fay-Herriot > 30 % (todos)` = fh_mayor_30_todos,
                            `Error GVF (pp)` = error_gvf_mediana_pp, `Error Fay-Herriot (pp)` = error_fh_mediana_pp,
                            `Peso de la encuesta` = peso_encuesta_mediana),
  "Aporte del modelo según el tamaño de muestra", "tabla_7_aporte_muestra.png", 12.5,
  "Errores y peso: medianas. Los cinco indicadores juntos.")
tabla_imagen(
  cv30_aporte_indicador %>% transmute(Indicador, Registros = as.character(Tramo), Casos = casos,
                            `Original > 30 %` = original_mayor_30, `GVF > 30 %` = gvf_mayor_30,
                            `Fay-Herriot > 30 % (dato > 0)` = fh_mayor_30_con_dato,
                            `Fay-Herriot > 30 % (todos)` = fh_mayor_30_todos,
                            `Error GVF (pp)` = error_gvf_mediana_pp, `Error Fay-Herriot (pp)` = error_fh_mediana_pp,
                            `Peso de la encuesta` = peso_encuesta_mediana),
  "Aporte del modelo según el tamaño de muestra, por indicador", "tabla_8_aporte_muestra_indicador.png", 12.5,
  "Errores y peso: medianas.")
# Tabla 9. Casos de la encuesta evaluados por indicador y anio
# Registros del denominador de cada indicador en los cantones de la Costa, sin factores de expansion.
# No se suman entre anios (la ENEMDU repite hogares) ni entre indicadores (son las mismas personas).
unidad_indicador <- c(
  "Pobreza" = "Personas con dato de pobreza",
  "NBI adolescente" = "Personas (población total)",
  "Desempleo" = "Personas de la PEA",
  "Inasistencia" = "Adolescentes de 12 a 17",
  "Monoparentalidad" = "Hogares")
casos_por_anio <- cv30 %>%
  group_by(Indicador, anio) %>%
  summarise(casos = sum(muestra_denominador, na.rm = TRUE),
            cantones = sum(muestra_denominador > 0, na.rm = TRUE), .groups = "drop")
tabla_imagen(
  casos_por_anio %>%
    select(Indicador, anio, casos) %>%
    pivot_wider(names_from = anio, values_from = casos) %>%
    mutate(`Qué se cuenta` = unname(unidad_indicador[Indicador]), .after = Indicador),
  "Casos de la encuesta evaluados por indicador y año", "tabla_9_casos_por_anio.png", 10.5,
  paste("Registros encuestados en los cantones de la Costa, sin factores de expansión. No se suman entre",
        "años, porque la ENEMDU repite hogares, ni entre indicadores."))
tabla_imagen(
  casos_por_anio %>%
    select(Indicador, anio, cantones) %>%
    pivot_wider(names_from = anio, values_from = cantones),
  "Cantones con muestra por indicador y año (de 86)", "tabla_10_cantones_con_muestra.png", 7.5)

# Tabla 11. Error original y error con la GVF segun el tamano de muestra
# Solo canton-anio con muestra. El error original de cero no es precision.
tabla_imagen(
  cv30_tramo %>% filter(n_registros > 0) %>% group_by(Tramo) %>%
    summarise(casos = n(), cero = sum(error_original_pp <= 1e-10),
              original = round(median(error_original_pp), 2),
              gvf = round(median(error_gvf_pp), 2),
              sube = sum(error_gvf_pp > error_original_pp), .groups = "drop") %>%
    transmute(Registros = as.character(Tramo), Casos = casos, `Con error original de cero` = cero,
              `Error original (pp)` = original, `Error GVF (pp)` = gvf,
              `Casos en que la GVF es mayor` = sube),
  "Error original y error con la GVF según el tamaño de muestra", "tabla_11_varianza_asignada.png", 9,
  "Errores: medianas en puntos porcentuales. Los cinco indicadores juntos.")
# La lista de los casos con CV Fay-Herriot mayor a 30 % queda en el objeto cv30_lista.

g_cv30 <- cv30_pasos %>%
  filter(Indicador != "TOTAL") %>%
  select(Indicador, Original = original_pct, GVF = gvf_pct, `Fay-Herriot` = fay_herriot_pct) %>%
  pivot_longer(-Indicador, names_to = "etapa", values_to = "pct") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "GVF", "Fay-Herriot"))) %>%
  ggplot(aes(Indicador, pct, fill = etapa)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%")), position = position_dodge(0.8),
            vjust = -0.4, size = 3.2) +
  scale_fill_manual(values = c(Original = "#8A94A6", GVF = "#D9A441", `Fay-Herriot` = "#1F5FA8")) +
  labs(x = NULL, y = "% de áreas con CV mayor a 30 %", fill = NULL,
       title = "Áreas con CV mayor a 30 % en cada paso",
       subtitle = "Solo cantón-año con dato directo mayor que 0") +
  theme_minimal() + theme(legend.position = "top")
print(g_cv30)
guardar_png(g_cv30, "7_cv_mayor_30_tres_pasos.png", 10, 5.5, dpi = 200)

