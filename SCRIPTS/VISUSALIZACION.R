
# CV ORIGINAL POR INDICADOR Y ANIO + TABLAS DE MUESTRAS MENORES A 60


# 1. LIBRERIAS Y PARAMETROS -----

## 1.1 Librerias ----
library(tidyverse)   # dplyr, tidyr, ggplot2, stringr, purrr, etc.
library(readxl)      # lectura del Excel (no viene dentro de tidyverse)
library(writexl)     # exportacion a Excel

## 1.2 Parametros que puedes cambiar ----
carpeta_excel <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"
ruta_excel    <- file.path(carpeta_excel, "estimaciones_original_vs_suavizado.xlsx")

carpeta_graficos <- file.path(carpeta_excel, "graficos_comparativos")
marca_muestra    <- 60      # marca INEC de tamano de muestra

dir.create(carpeta_graficos, showWarnings = FALSE, recursive = TRUE)

## 1.3 Funcion auxiliar para guardar graficos ----
guardar_png <- function(g, nombre, ancho, alto, dpi = 130) {
  ggsave(file.path(carpeta_graficos, nombre), g, width = ancho, height = alto, dpi = dpi)
}




# 2. LECTURA Y PREPARACION DE LOS DATOS -----
## 2.1 Lectura del Excel ----
stopifnot(file.exists(ruta_excel))
est <- read_excel(ruta_excel, sheet = "Estimaciones")

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
# Error estandar y CV: original (antes), con GVF (varianza suavizada) y
# suavizado final (despues de la estimacion en areas pequenas).
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
#   CV con GVF   = error estandar con varianza suavizada por GVF / valor original * 100
#                  (antes de la estimacion en areas pequenas)
#   CV suavizado = error estandar final (Fay-Herriot) / valor suavizado * 100
#                  (despues de la estimacion en areas pequenas)
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
    labs(title = titulo, x = "Categoría poblacional", y = eje_y, caption = nota) +
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
nota_denominador <- paste0(
  "Categoría poblacional = tamaño de la muestra en el cantón. Población (denominador) de cada indicador:\n",
  "Inasistencia 12–17: adolescentes de 12 a 17 años encuestados · Desempleo: personas de la población económicamente activa (PEA)\n",
  "Pobreza por ingresos: personas con condición de pobreza definida · ",
  "NBI: personas encuestadas del cantón-año (población total) · Monoparentalidad: hogares clasificables (no personas).")

nota_c <- paste0(
  "Cada punto representa un cantón-año de un indicador. CV original = error estándar directo ÷ valor original × 100.\n",
  "Línea interior: mediana · Caja: 50 % central · Bigotes: hasta 1,5 veces el rango intercuartílico · Todos los extremos visibles.\n",
  "Se excluyen CV ausentes (valor 0 %), cantón-años sin dato y cantón-años con error original cero (el error no se pudo estimar). Cajas con pocos puntos son poco informativas.\n",
  nota_denominador)

nota_g <- paste0(
  "Cada punto representa un cantón-año de un indicador. CV con GVF = error estándar con la varianza suavizada por GVF ÷ valor original × 100.\n",
  "El valor sigue siendo el original; solo se suavizó la varianza. Línea interior: mediana · Caja: 50 % central · Bigotes: hasta 1,5 veces el rango intercuartílico.\n",
  "Misma escala vertical que los otros gráficos (se recorta en ", round(limite_orig), " %; ", n_fuera_gvf, " cantón-años con CV mayor quedan fuera del dibujo pero cuentan en las cajas).\n",
  "Se excluyen cantón-años sin dato y con valor original igual a cero (el CV no se puede calcular).\n",
  nota_denominador)

nota_d <- paste0(
  "Cada punto representa un cantón-año de un indicador. CV suavizado = error estándar final (Fay–Herriot) ÷ valor suavizado × 100.\n",
  "Línea interior: mediana · Caja: 50 % central · Bigotes: hasta 1,5 veces el rango intercuartílico · Misma escala vertical que el gráfico original.\n",
  "Se excluyen cantón-años sin dato y con valor suavizado igual a cero. La categoría poblacional es la muestra original del cantón-año.\n",
  nota_denominador)


# ==============================================================================
# CV ORIGINAL: general y panel por anio -----
# ==============================================================================
g1 <- grafico_cv_muestra(d8, "cv_pct_sin_error_cero",
                         "CV original según el tamaño de muestra, por indicador",
                         "Coeficiente de variación original (CV %)", nota_c, limite_orig, solo_general = TRUE)
guardar_png(g1, "1_cv_original_general.png", 12, 7.2, dpi = 200)

g2 <- grafico_cv_muestra(d8, "cv_pct_sin_error_cero",
                         "CV original según el tamaño de muestra, por indicador y año",
                         "Coeficiente de variación original (CV %)", nota_c, limite_orig)
guardar_png(g2, "2_cv_original_panel_anios.png", 26, 9, dpi = 200)


# CV CON GVF (antes de areas pequenas): general y panel por anio -----

g3 <- grafico_cv_muestra(d8, "cv_gvf_pct",
                         "CV con varianza suavizada (GVF) según el tamaño de muestra",
                         "Coeficiente de variación con GVF (CV %)", nota_g, limite_orig, solo_general = TRUE)
guardar_png(g3, "3_cv_gvf_general.png", 12, 7.2, dpi = 200)

g4 <- grafico_cv_muestra(d8, "cv_gvf_pct",
                         "CV con varianza suavizada (GVF) según el tamaño de muestra, por año",
                         "Coeficiente de variación con GVF (CV %)", nota_g, limite_orig)
guardar_png(g4, "4_cv_gvf_panel_anios.png", 26, 9, dpi = 200)


# CV SUAVIZADO (despues de areas pequenas): general y panel por anio -----

g5 <- grafico_cv_muestra(d8, "cv_suavizado_pct",
                         "CV suavizado según el tamaño de muestra, por indicador",
                         "Coeficiente de variación suavizado (CV %)", nota_d, limite_orig, solo_general = TRUE)
guardar_png(g5, "5_cv_suavizado_general.png", 12, 7.2, dpi = 200)

g6 <- grafico_cv_muestra(d8, "cv_suavizado_pct",
                         "CV suavizado según el tamaño de muestra, por indicador y año",
                         "Coeficiente de variación suavizado (CV %)", nota_d, limite_orig)
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



# EJEMPLO 1: PEDRO CARBO, UN CANTON CON SALTOS ABRUPTOS -----

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

# 2) Canton elegido (cambialo por otro del ranking si quieres)
canton_saltos    <- "Pedro Carbo"
provincia_saltos <- "Guayas"

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
         Original = valor_original_pct, Suavizado = valor_suavizado_pct) %>%
  pivot_longer(c(Original, Suavizado), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Suavizado")))

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
              vjust = -1, size = 2.9, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", Suavizado = "#C0392B"), name = NULL) +
    scale_x_continuous(breaks = 2021:2025) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros del denominador de cada indicador en el cantón-año. ",
                          "Cada panel tiene su propia escala vertical, igual en todas las imágenes.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}

nombre_cantones <- paste0(canton_saltos, " (", provincia_saltos, ")")
g_saltos_pc_ambos <- grafico_saltos(c("Original", "Suavizado"),
                                    paste0("Original frente a suavizado, 5 indicadores y 5 años: ", nombre_cantones))

print(g_saltos_pc_ambos)
guardar_png(g_saltos_pc_ambos, "ejemplo_saltos_pedro_carbo_ambos.png", 18, 6, dpi = 200)



# EJEMPLO 2: ARENILLAS, UN CANTON CON SALTOS ABRUPTOS Y SIN CEROS NO ESTIMABLES -----


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


## 4) Canton elegido ----
canton_saltos    <- "Arenillas"
provincia_saltos <- "El Oro"

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
         Original = valor_original_pct, Suavizado = valor_suavizado_pct) %>%
  pivot_longer(c(Original, Suavizado), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Suavizado")),
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
              vjust = -1, size = 2.9, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", Suavizado = "#C0392B"), name = NULL) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 4),
                       labels = c(`FALSE` = "Estimable", `TRUE` = "Error cero (no estimable)"),
                       name = NULL) +
    scale_x_continuous(breaks = 2021:2025) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros del denominador de cada indicador en el cantón-año. ",
                          "Cada panel tiene su propia escala vertical, igual en todas las imágenes.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}


## 8) Grafico: original frente a suavizado ----
nombre_cantones <- paste0(canton_saltos, " (", provincia_saltos, ")")
g_saltos_ambos <- grafico_saltos(c("Original", "Suavizado"),
                                 paste0("Original frente a suavizado, 5 indicadores y 5 años: ", nombre_cantones))

print(g_saltos_ambos)
guardar_png(g_saltos_ambos, "ejemplo_saltos_ambos.png", 18, 6, dpi = 200)



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


## 2) Canton elegido ----
canton_estable    <- "Machala"
provincia_estable <- "El Oro"

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
         Original = valor_original_pct, Suavizado = valor_suavizado_pct) %>%
  pivot_longer(c(Original, Suavizado), names_to = "etapa", values_to = "valor") %>%
  mutate(etapa = factor(etapa, levels = c("Original", "Suavizado")))

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
              vjust = -1, size = 2.7, colour = "grey30", show.legend = FALSE) +
    facet_wrap(~ etiqueta, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = c(Original = "#222222", Suavizado = "#C0392B"), name = NULL) +
    scale_x_continuous(breaks = 2021:2025) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, .05))) +
    labs(title = titulo, x = "Año", y = "Estimación del indicador (%)",
         caption = paste0("n = registros del denominador de cada indicador en el cantón-año. ",
                          "Cada panel tiene su propia escala vertical, igual en todas las imágenes.")) +
    theme_minimal(base_size = 12) +
    theme(plot.background = element_rect(fill = "#F8F9FB", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          panel.grid.minor = element_blank(),
          strip.text = element_text(face = "bold", size = 9.5),
          plot.title = element_text(family = "serif", face = "bold", size = 17),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 9, colour = "grey25"),
          plot.caption.position = "plot", legend.position = "bottom")
}


## 7) Grafico: original frente a suavizado ----
nombre_estable <- paste0(canton_estable, " (", provincia_estable, ")")
g_estable_ambos <- grafico_estable(c("Original", "Suavizado"),
                                   paste0("Original frente a suavizado, 5 indicadores y 5 años: ", nombre_estable))

print(g_estable_ambos)
guardar_png(g_estable_ambos, "ejemplo_estable_ambos.png", 18, 6, dpi = 200)



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