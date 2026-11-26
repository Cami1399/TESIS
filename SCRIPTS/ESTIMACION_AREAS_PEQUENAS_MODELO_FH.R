# ESTIMACION EN AREAS PEQUENAS: MODELO FAY-HERRIOT POR CANTON-ANIO, 2021-2025
#
# Combina las estimaciones directas de la ENEMDU con variables auxiliares de registros administrativos y proyecciones de poblacion mediante el modelo de area de Fay y Herriot (1979).
# Cada canton-anio es un area (86 cantones de la Costa x 5 anios).
# Se ajusta un modelo por indicador.



# 0. PAQUETES, RUTAS Y FUNCIONES DE APOYO --------------------------------------

## 0.1 Paquetes ----

library(tidyverse)   # dplyr, tidyr, purrr, ggplot2, stringr, tibble
library(readxl)      # lectura de los Excel de insumo
# writexl::write_xlsx exporta los resultados (se llama con prefijo).
# sae::mseFH ajusta el modelo (Molina y Marhuenda, 2015).
# No se usa library(sae) porque al adjuntarlo carga MASS, cuya funcion select() tapa a la de dplyr; se llama siempre con el prefijo sae::.
if (!requireNamespace("sae", quietly = TRUE)) install.packages("sae")
stopifnot(requireNamespace("sae", quietly = TRUE),
          requireNamespace("writexl", quietly = TRUE))

## 0.2 Rutas y parametros ----

# Se pueden definir antes de source() para usar otras carpetas.
if (!exists("carpeta_datos"))  carpeta_datos  <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"
# Las tres tablas de estimaciones van a DATAS_PROCESADAS (reemplazan las del script anterior).
if (!exists("carpeta_salida")) carpeta_salida <- carpeta_datos
if (!exists("exportar_tabla")) exportar_tabla <- TRUE
# Los graficos y la tabla de resultados van a SCRIPTS/SALIDAS_EXPLORACIONES/salidas_modelo_fh; los resultados y la validacion tambien se muestran en consola.
if (!exists("carpeta_imagenes")) carpeta_imagenes <- "C:/Users/camil/OneDrive/Desktop/TESIS/SCRIPTS/SALIDAS_EXPLORACIONES"
carpeta_img <- file.path(carpeta_imagenes, "salidas_modelo_fh")
if (isTRUE(exportar_tabla)) dir.create(carpeta_img, recursive = TRUE, showWarnings = FALSE)
# En consola solo se muestran los resultados de cada indicador (seccion 5); con mostrar_detalle <- TRUE antes de source() se ven tambien las etapas intermedias.
if (!exists("mostrar_detalle")) mostrar_detalle <- FALSE

ruta_estimaciones <- file.path(carpeta_datos, "estimaciones_originales.xlsx")
ruta_indicadores  <- file.path(carpeta_datos, "construccion_indicadores_factores.xlsx")
ruta_auxiliares   <- file.path(carpeta_datos, "panel_variables_auxiliares_2021_2025.xlsx")
ruta_poblacion    <- file.path(carpeta_datos, "poblacion_cantonal_costa_total_12_17_2021_2025.xlsx")
stopifnot(file.exists(ruta_poblacion))
stopifnot(file.exists(ruta_estimaciones), file.exists(ruta_indicadores),
          file.exists(ruta_auxiliares))

## 0.3 Funciones de apoyo ----

# Muestra en consola un titulo y, si se da, una tabla.
mostrar_tabla <- function(titulo, tabla = NULL) {
  cat("\n", titulo, "\n", sep = "")
  if (!is.null(tabla)) print(as.data.frame(tabla), row.names = FALSE)
  invisible(NULL)
}
# Etapas intermedias: solo se muestran si mostrar_detalle es TRUE.
mostrar_etapa <- function(titulo, tabla = NULL) {
  if (isTRUE(mostrar_detalle)) mostrar_tabla(titulo, tabla)
  invisible(NULL)
}

# Nombre de la columna de cada indicador en la tabla ancha
nombres_indicadores <- c(
  inasistencia_12_17 = "inasistencia_12_17_pct",
  desempleo_general = "desempleo_pct",
  pobreza_ingresos = "pobreza_ingresos_pct",
  adolescentes_nbi_sobre_total_provisional = "adolescentes_nbi_sobre_total_provisional_pct",
  monoparental_jefatura_aproximacion = "monoparental_jefatura_aproximacion_pct")
columnas_indicadores <- unname(nombres_indicadores)
claves_territorio <- c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")

mostrar_etapa("0. Paquetes y rutas", tibble(
  elemento = c("version de sae", "carpeta de insumos", "carpeta de salida"),
  valor = c(as.character(utils::packageVersion("sae")), carpeta_datos, carpeta_salida)))


# 1. LECTURA DE INSUMOS --------------------------------------------------------

## 1.1 Estimaciones directas (INDICADORES.R) ----

# Una fila por indicador, canton y anio: porcentaje, error estandar en puntos porcentuales, IC95, CV, n del denominador, grados de libertad y marcas de precision INEC.
estimadores <- read_excel(ruta_estimaciones) %>%
  mutate(anio = as.integer(anio),
         codigo_canton = str_pad(as.character(codigo_canton), 4, pad = "0"))

columnas_estimaciones <- c("anio", "codigo_provincia", "provincia", "codigo_canton",
                           "canton", "indicador", "valor_pct", "error_estandar_pp",
                           "limite_inferior_pct", "limite_superior_pct", "cv_pct",
                           "muestra_denominador", "grados_libertad",
                           "error_maximo_tolerable_pp", "cumple_muestra",
                           "cumple_gl", "cumple_error", "denominador_expandido",
                           "upm_denominador", "estratos_upm_solitaria")
faltan <- setdiff(columnas_estimaciones, names(estimadores))
if (length(faltan)) stop("Faltan columnas en estimaciones_originales.xlsx (vuelva a correr INDICADORES.R): ",
                         paste(faltan, collapse = ", "))
stopifnot(setequal(unique(estimadores$indicador), names(nombres_indicadores)),
          !anyDuplicated(estimadores[c("anio", "codigo_canton", "indicador")]))

## 1.2 Indicadores en formato ancho (para conservar filas y NA) ----

indicadores_agregados <- read_excel(ruta_indicadores) %>%
  mutate(anio = as.integer(anio),
         codigo_canton = str_pad(as.character(codigo_canton), 4, pad = "0"))
stopifnot(all(c(claves_territorio, columnas_indicadores) %in% names(indicadores_agregados)))

## 1.3 Variables auxiliares  ----

# El panel viene en formato largo; el modelo necesita una columna por variable y un valor en todas las areas: 430 canton-anio y ningun vacio.
auxiliares <- read_excel(ruta_auxiliares) %>%
  mutate(anio = as.integer(anio),
         codigo_canton = str_pad(as.character(codigo_canton), 4, pad = "0")) %>%
  select(anio, codigo_canton, variable, valor) %>%
  pivot_wider(names_from = variable, values_from = valor)
stopifnot(nrow(auxiliares) == 430,
          !anyNA(select(auxiliares, -anio, -codigo_canton)))

## 1.4 Poblacion de cada area (proyecciones INEC, revision 2024) ----

# La tabla de reporte incluye el tamano de la poblacion del area.
# Se usa la poblacion del grupo de referencia de cada indicador: 12 a 17 anios (inasistencia), 15 anios y mas (desempleo) y poblacion total (pobreza, NBI adolescente y monoparentalidad; para esta ultima no hay proyeccion de hogares).
poblacion <- read_excel(ruta_poblacion, sheet = "Total_adolescente") %>%
  rename_with(~ str_squish(stringi::stri_trans_general(.x, "Latin-ASCII"))) %>%
  transmute(anio = as.integer(ANO),
            codigo_canton = str_pad(COD_CANTON, 4, pad = "0"),
            poblacion_12_17 = `POBLACION 12-17`,
            poblacion_15_mas = `POBLACION 15 Y MAS`,
            poblacion_total = `POBLACION TOTAL`)
stopifnot(nrow(poblacion) == 430, !anyNA(poblacion))
poblacion_area <- poblacion %>%
  pivot_longer(starts_with("poblacion_"), names_to = "poblacion_area_grupo",
               values_to = "poblacion_area") %>%
  inner_join(tibble(indicador = names(nombres_indicadores),
                    poblacion_area_grupo = c("poblacion_12_17", "poblacion_15_mas",
                                             "poblacion_total", "poblacion_total",
                                             "poblacion_total")),
             by = "poblacion_area_grupo", relationship = "many-to-many") %>%
  mutate(poblacion_area_grupo = str_remove(poblacion_area_grupo, "poblacion_"))
stopifnot(nrow(poblacion_area) == 430 * 5)

mostrar_etapa("1. Insumos leidos", tibble(
  insumo = c("estimaciones directas", "indicadores (formato ancho)", "variables auxiliares",
             "poblacion por area"),
  filas = c(nrow(estimadores), nrow(indicadores_agregados), nrow(auxiliares), nrow(poblacion)),
  columnas = c(ncol(estimadores), ncol(indicadores_agregados), ncol(auxiliares), ncol(poblacion))))


# 2. SUAVIZADO DE LA VARIANZA DEL ESTIMADOR DIRECTO (GVF) ----------------------

# El Fay-Herriot trata como conocida la varianza de cada estimacion directa.
# En areas con poca muestra esa varianza es cero o inestable, y con varianza cero el modelo no es aplicable salvo que se obtenga con otro metodo.
# Se reemplaza, en todas las areas, por una varianza suavizada con efecto de diseno (funcion generalizada de la varianza, GVF):
#
#   psi_d = Deff * pt * (1 - pt) / n_d
#
# Donde:
#   psi_d = varianza suavizada del canton-anio d, en escala 0-1.
#   pt    = proporcion del indicador en la provincia y el anio del canton, con los factores de expansion de la ENEMDU.
#   n_d   = registros del denominador del indicador (personas, u hogares en monoparentalidad).
#   pt * (1 - pt) / n_d = varianza de una proporcion en un muestreo aleatorio simple de n_d registros.
#   Deff  = efecto de diseno: cuantas veces es mayor la varianza por el muestreo en conglomerados de la ENEMDU. Uno por indicador.
#
# Efecto de diseno del indicador, con las areas ancla:
#   Deff = suma(psi_d^DIR) / suma(pt * (1 - pt) / n_d)
# Area ancla: canton-anio con gl >= 9 (INEC, 2025, p. 22), sin estratos con una sola UPM y con varianza directa mayor que cero.
#   Las areas con varianza directa cero no se usan para calcular el Deff, pero reciben su varianza suavizada y se mantienen en el modelo:
#   - MDSF-CEPAL (2021, p. 41): las comunas con tasa y varianza nulas no entraron al ajuste de su GVF, "pero si se obtuvieron las predicciones de sus varianzas".
#     Alli el motivo es el logaritmo, que aqui no se usa.
#   - Franco (2020, pp. 4 y 20): con tasas no hace falta retirar los ceros, y una varianza cero les da un peso indebido.
#   - Corral et al. (2022, p. 19): la varianza cero aparece en areas con poca muestra y debe obtenerse con otro metodo.
#
# Se usa la proporcion provincial y no la del canton para que la varianza no sea cero cuando la estimacion directa es 0 %.
#
# Fuente: You (2008, p. 2) escribe la varianza de una proporcion como theta_d * (1 - theta_d) * deff_d / n_d, con efecto de diseno suavizado.
#   Son decisiones propias, que no estan en esa fuente: usar la proporcion provincial en lugar de theta_d, un solo Deff por indicador calculado con las areas ancla, y usar gl >= 9 y estratos con mas de una UPM para elegirlas.
calcular_proporcion_provincial <- function(d) {
  d %>%
    filter(is.finite(valor_pct), denominador_expandido > 0) %>%
    group_by(anio, codigo_provincia) %>%
    summarise(prop_provincial = sum(denominador_expandido * valor_pct / 100) / sum(denominador_expandido),
              .groups = "drop")
}

suavizar_varianza <- function(d) {
  d <- filter(d, is.finite(valor_pct)) %>%
    left_join(calcular_proporcion_provincial(d), by = c("anio", "codigo_provincia"))
  stopifnot(all(d$prop_provincial > 0 & d$prop_provincial < 1), all(d$muestra_denominador > 0))
  d <- mutate(d, varianza_aleatoria = prop_provincial * (1 - prop_provincial) / muestra_denominador,
              ancla_gvf = coalesce(cumple_gl & estratos_upm_solitaria == 0 &
                                     error_estandar_pp > 1e-10, FALSE))
  anclas <- filter(d, ancla_gvf)
  stopifnot(nrow(anclas) > 2)
  deff <- sum((anclas$error_estandar_pp / 100)^2) / sum(anclas$varianza_aleatoria)
  d$psi_d <- deff * d$varianza_aleatoria
  stopifnot(all(is.finite(d$psi_d)), all(d$psi_d > 0))
  diagnostico <- tibble(indicador = first(d$indicador), estimaciones_con_valor = nrow(d),
                        areas_ancla = nrow(anclas), deff = deff,
                        ee_dir_mediana_anclas_pp = median(anclas$error_estandar_pp),
                        ee_gvf_mediana_pp = median(100 * sqrt(d$psi_d)))
  list(datos = d, diagnostico = diagnostico)
}

resultado_varianza <- estimadores %>% group_split(indicador) %>% map(suavizar_varianza)
datos_varianza <- resultado_varianza %>% map("datos") %>% list_rbind()
diagnostico_varianza <- resultado_varianza %>% map("diagnostico") %>% list_rbind()
mostrar_etapa("2. Varianza suavizada con efecto de diseno (GVF), por indicador", diagnostico_varianza)


# 3. MODELO FAY-HERRIOT: MARCO GENERAL Y FUNCIONES -----------------------------

## 3.1 El modelo ----

# Modelo de area en dos etapas (Fay y Herriot, 1979):
#
#   Modelo de enlace:   theta_d = x_d'beta + u_d,        u_d ~ N(0, sigma2_u)
#   Modelo de muestreo: theta^_d^DIR = theta_d + e_d,    e_d ~ N(0, psi_d)
#
# Donde:
#   theta_d      = valor verdadero del indicador en el canton-anio d, que se quiere estimar.
#   x_d          = variables auxiliares del indicador, con la constante.
#   beta         = coeficientes: x_d'beta = b0 + b1*x1_d + ... + bk*xk_d.
#   u_d          = efecto de area: lo propio del canton-anio que las variables no explican; su varianza sigma2_u mide la diferencia entre cantones que queda sin explicar.
#   theta^_d^DIR = estimacion directa de la ENEMDU para el canton-anio d.
#   e_d          = error de muestreo de la encuesta; su varianza psi_d es la suavizada (GVF) y se trata como conocida.
#
# Estimacion final (EBLUP):
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * x_d'beta^
#   gamma_d     = sigma2_u / (sigma2_u + psi_d)
# La estimacion final es un promedio ponderado entre el dato directo y el valor sintetico x_d'beta^ (lo que predicen las variables).
# El peso gamma_d del dato directo es alto cuando su varianza psi_d es baja (mucha muestra) y bajo cuando es alta (poca muestra).
#
# Estimacion de beta y sigma2_u por maxima verosimilitud restringida (REML), que corrige el sesgo de la varianza del efecto de area; si no converge, metodo de momentos de Fay y Herriot.
# El error de cada estimacion es la raiz del error cuadratico medio (MSE) de Prasad y Rao, que calcula sae::mseFH (Molina y Marhuenda, 2015).
#
# Decisiones:
#   - Entran todas las estimaciones directas con valor, incluidas las de 0 %, con la varianza suavizada de la GVF con efecto de diseno; el estimador pondera cada una segun su varianza.
#   - Canton-anio sin estimacion directa: reciben el valor sintetico, porque el modelo "can provide estimates for non-sampled areas based on the regression-synthetic component" (Corral et al., 2022, p. 25).
#     Se marcan con tipo_estimacion = "solo_sintetico".
#     Precedente: MDSF (2019, p. 17).
#   - Escala: el indicador entra como proporcion (0-1), sin transformar, y se multiplica por 100 al exportar.
#     Las auxiliares entran en su escala original, sin transformar (MDSF-CEPAL, 2021, p. 50, nota 23).
#   - Variables de cada indicador: relacion teorica con el indicador y seleccion por su significancia.

## 3.2 Validacion ----

# Se revisa despues de ajustar el modelo:
#   a. Significancia de las variables (p < 0,05).
#   b. Ajuste: parte de la variacion entre areas que explican las auxiliares, 1 - sigma2_u(con auxiliares) / sigma2_u(sin auxiliares), y aporte de cada una.
#   c. Colinealidad: factor de inflacion de la varianza (VIF), a revisar si es mayor que 5 (Corral et al., 2022, p. 21).
#   g. Precision: error del Fay-Herriot (raiz del MSE) frente al error del dato directo que entra al modelo (GVF).
#   i. Dispersion de residuos: residuo estandarizado contra el valor sintetico; los puntos deben repartirse alrededor de cero y casi todos dentro de +/- 1,96.
#     Su desviacion estandar debe estar cerca de 1.
#   j. Variacion que las variables no explican, sigma2_u, y peso del dato directo gamma_d (minimo, mediana y maximo).
#   k. Concordancia con la provincia: razon R entre el dato directo de la provincia y el promedio Fay-Herriot de sus cantones.
#   Residuo estandarizado del area d = (theta^_d^DIR - x_d'beta^) / raiz(sigma2_u + psi_d).
#   Efecto de area estimado u^_d     = theta^_d^FH - x_d'beta^.
#   Peso del dato directo gamma_d    = sigma2_u / (sigma2_u + psi_d).


## 3.3 Funcion de ajuste y validacion ----

# Ajusta el Fay-Herriot de un indicador con sus variables auxiliares y devuelve: estimaciones por area, coeficientes, atributos de validacion y residuos por area.
#   indicador_fh   = codigo del indicador en estimaciones_originales.xlsx
#   variables      = nombres de las variables auxiliares en el panel
#   signo_esperado = relacion que se plantea entre cada variable y el indicador


ajustar_fay_herriot <- function(indicador_fh, variables, signo_esperado) {
  stopifnot(length(variables) == length(signo_esperado), all(variables %in% names(auxiliares)))
  d <- datos_varianza %>%
    filter(indicador == indicador_fh) %>%
    left_join(auxiliares, by = c("anio", "codigo_canton"))
  stopifnot(nrow(d) > 0, !anyNA(d[variables]))

  # sae::mseFH toma estas variables del entorno de la funcion.
  prop <- d$valor_pct / 100
  psi_d <- d$psi_d
  X <- as.matrix(d[variables])
  ajuste <- NULL
  for (metodo in c("REML", "FH")) {
    intento <- tryCatch(sae::mseFH(prop ~ X, vardir = psi_d, method = metodo),
                        error = \(e) NULL)
    if (!is.null(intento) && isTRUE(intento$est$fit$convergence)) { ajuste <- intento; break }
  }
  if (is.null(ajuste)) stop("No convergio Fay-Herriot: ", indicador_fh)
  # Modelo sin variables auxiliares, solo para medir el ajuste
  nulo <- sae::mseFH(prop ~ 1, vardir = psi_d, method = metodo)

  beta <- ajuste$est$fit$estcoef$beta
  sigma2_u <- as.numeric(unlist(ajuste$est$fit$refvar))[1]
  sigma2_u_nulo <- as.numeric(unlist(nulo$est$fit$refvar))[1]
  ajuste_global <- ajuste$est$fit$goodness
  sintetico <- as.numeric(cbind(1, X) %*% beta)
  eblup <- as.numeric(unlist(ajuste$est$eblup))
  mse_fh <- pmax(as.numeric(unlist(ajuste$mse)), 0)
  error_fh <- sqrt(mse_fh)
  stopifnot(length(eblup) == nrow(d), length(error_fh) == nrow(d), is.finite(sigma2_u))

  estimaciones <- d %>%
    transmute(anio, codigo_canton, indicador,
              psi_d_pp2 = 1e4 * psi_d,
              ee_gvf_pp = 100 * sqrt(psi_d),
              sintetico_xb_pct = 100 * sintetico,
              gamma_d = sigma2_u / (sigma2_u + psi_d),
              theta_fh_pct = 100 * eblup,
              mse_fh_pp2 = 1e4 * mse_fh,
              rmse_fh_pp = 100 * error_fh,
              ic_inf_fh_pct = theta_fh_pct - 1.96 * rmse_fh_pp,
              ic_sup_fh_pct = theta_fh_pct + 1.96 * rmse_fh_pp,
              fuera_de_rango = as.integer(eblup < 0 | eblup > 1),
              tipo_estimacion = "fay_herriot")

  # Canton-anio sin estimacion directa (sin muestra): reciben solo el valor sintetico, porque sin encuesta el peso del dato directo es cero.
  # Error = raiz(sigma2_u + g2), donde g2 es el error por estimar beta:
  #   g2_d = x_d' (sum_j x_j x_j' / (sigma2_u + psi_j))^(-1) x_d
  faltan <- estimadores %>%
    filter(indicador == indicador_fh, !is.finite(valor_pct)) %>%
    select(anio, codigo_canton, indicador) %>%
    left_join(auxiliares, by = c("anio", "codigo_canton"))
  stopifnot(!anyNA(faltan[variables]))
  if (nrow(faltan) > 0) {
    X1 <- cbind(1, X)
    Q <- solve(crossprod(X1, X1 / (sigma2_u + psi_d)))
    Xf <- cbind(1, as.matrix(faltan[variables]))
    sintetico_f <- as.numeric(Xf %*% beta)
    mse_f <- sigma2_u + rowSums((Xf %*% Q) * Xf)
    error_f <- sqrt(mse_f)
    estimaciones <- bind_rows(estimaciones, faltan %>%
      transmute(anio, codigo_canton, indicador,
                sintetico_xb_pct = 100 * sintetico_f,
                gamma_d = 0,
                theta_fh_pct = 100 * sintetico_f,
                mse_fh_pp2 = 1e4 * mse_f,
                rmse_fh_pp = 100 * error_f,
                ic_inf_fh_pct = theta_fh_pct - 1.96 * rmse_fh_pp,
                ic_sup_fh_pct = theta_fh_pct + 1.96 * rmse_fh_pp,
                fuera_de_rango = as.integer(sintetico_f < 0 | sintetico_f > 1),
                tipo_estimacion = "solo_sintetico"))
  }
  con_muestra <- estimaciones$tipo_estimacion == "fay_herriot"

  coeficientes <- tibble(indicador = indicador_fh,
                         termino = c("(constante)", variables),
                         beta = beta,
                         ee_beta = ajuste$est$fit$estcoef$std.error,
                         t = ajuste$est$fit$estcoef$tvalue,
                         p_valor = ajuste$est$fit$estcoef$pvalue,
                         signo_esperado = c(NA, signo_esperado)) %>%
    mutate(signo_observado = sign(beta),
           signo_coincide = signo_observado == signo_esperado)

  residuo <- (prop - sintetico) / sqrt(sigma2_u + psi_d)
  efecto <- eblup - sintetico
  cook <- cooks.distance(lm(prop ~ X, weights = 1 / (sigma2_u + psi_d)))
  diagnosticos <- d %>%
    transmute(anio, codigo_canton, canton, indicador,
              residuo_estandarizado = residuo, u_d_pp = 100 * efecto,
              distancia_cook = unname(cook), influyente = distancia_cook > 4 / nrow(d))

  vif <- if (ncol(X) < 2) NA_real_ else
    max(map_dbl(seq_len(ncol(X)), \(j) 1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared)))
  error_directo <- sqrt(psi_d)
  cv_directo <- if_else(prop > 0, 100 * error_directo / prop, NA_real_)
  cv_fh <- if_else(eblup > 0, 100 * error_fh / eblup, NA_real_)
  validacion <- tibble(
    indicador = indicador_fh,
    variables = paste(variables, collapse = " + "),
    metodo = metodo,
    areas = nrow(d),
    directos_en_cero = sum(prop == 0),
    sigma2_u_pp2 = 1e4 * sigma2_u,
    loglike = unname(ajuste_global["loglike"]),
    aic = unname(ajuste_global["AIC"]),
    bic = unname(ajuste_global["BIC"]),
    kic = unname(ajuste_global["KIC"]),
    # Modelo sin variables auxiliares (solo la constante), para comparar el ajuste
    parametros_sin = 2,
    parametros_con = ncol(X) + 2,
    loglike_sin = unname(nulo$est$fit$goodness["loglike"]),
    aic_sin = unname(nulo$est$fit$goodness["AIC"]),
    explica_pct = 100 * (1 - sigma2_u / sigma2_u_nulo),
    vif_maximo = vif,
    shapiro_p_residuos = shapiro.test(residuo)$p.value,
    shapiro_p_efectos = shapiro.test(efecto)$p.value,
    areas_influyentes = sum(diagnosticos$influyente),
    pct_areas_influyentes = 100 * mean(diagnosticos$influyente),
    areas_solo_sintetico = sum(!con_muestra),
    fuera_de_rango = sum(estimaciones$fuera_de_rango),
    gamma_d_mediana = median(estimaciones$gamma_d[con_muestra]),
    ee_gvf_mediana_pp = median(100 * error_directo),
    rmse_fh_mediana_pp = median(100 * error_fh),
    areas_con_error_menor = sum(error_fh < error_directo),
    cv_gvf_mediana = median(cv_directo, na.rm = TRUE),
    # Mediana en los mismos canton-anio que el CV directo (dato mayor que 0), para que las dos se puedan comparar
    cv_fh_mediana = median(cv_fh[prop > 0], na.rm = TRUE),
    areas_con_cv_menor = sum(cv_fh < cv_directo, na.rm = TRUE),
    areas_con_cv_gvf = sum(!is.na(cv_directo)),
    rmse_solo_sintetico_mediana_pp = if (any(!con_muestra))
      median(estimaciones$rmse_fh_pp[!con_muestra]) else NA_real_)

  # Datos para el grafico de linealidad: residuo contra cada variable
  linealidad <- d %>%
    transmute(indicador, residuo_estandarizado = residuo, across(all_of(variables))) %>%
    pivot_longer(all_of(variables), names_to = "variable", values_to = "valor_variable")

  list(estimaciones = estimaciones, coeficientes = coeficientes, validacion = validacion,
       diagnosticos = diagnosticos, linealidad = linealidad)
}

## 3.4 Resultados de cada indicador ----

# Despues de ajustar el modelo final de un indicador (seccion 5) se muestran en consola sus resultados y su validacion, y se dibujan y guardan sus graficos:
#   a. Encabezado: areas, variables auxiliares, metodo, nivel de confianza, sigma2_u y gamma media.
#   b. Estimaciones EBLUP por area: directo, su error (GVF), EBLUP, su error e intervalo al 95 % (primeras 25; todas en estimaciones_fay_herriot.xlsx).
#   c. Resumen: medias del directo y del EBLUP, de sus errores, y reduccion media del MSE = 1 - media(MSE) / media(psi_d).
#   d. Pesos de contraccion: gamma_d minima, cuartiles, media y maxima.
#   e. Coeficientes de regresion.
#   f. Bondad de ajuste: log-verosimilitud, AIC, BIC y KIC.
#   g. Validacion: varianza suavizada (GVF); supuestos y precision (normalidad de los residuos, desviacion del residuo estandarizado, estimaciones fuera de 0-100 %, areas donde baja el error y CV mediano); concordancia con la provincia.
#   h. Graficos: estimacion directa contra EBLUP (contraccion) y varianza directa contra MSE del EBLUP.
# La funcion devuelve las tablas c-g del indicador; la seccion 7 las junta con las de los otros indicadores.
# Las medias del resumen usan solo los canton-anio con muestra.
# Los coeficientes y la bondad de ajuste se dan en escala de porcentaje, la misma de las estimaciones, para que beta se lea en puntos porcentuales; el modelo se ajusta en proporcion (0-1).
#   Al pasar de proporcion a porcentaje cada dato se multiplica por 100: beta se multiplica por 100, la log-verosimilitud baja en n * log(100) y AIC, BIC y KIC suben en 2 * n * log(100).
#   El modelo y sus estimaciones no cambian.
# Aporte de cada variable (calcular_aporte; se usa en 6.7, no se muestra por indicador): parte de la diferencia entre areas que explica.
#   Con una variable es todo lo que explica el modelo.
#   Con dos, es el promedio entre lo que explica sola y lo que agrega cuando ya esta la otra: aporte_1 = (explica(1) + explica(1 y 2) - explica(2)) / 2.
#   Los aportes suman lo que explica el modelo.
# Concordancia con la provincia: R = directo de la provincia / promedio Fay-Herriot de sus cantones, con los factores de expansion (ver 6.5); cerca de 1, el modelo concuerda con la provincia.

nivel_confianza <- 0.95
nombre_archivo <- c(inasistencia_12_17 = "inasistencia", desempleo_general = "desempleo",
                    pobreza_ingresos = "pobreza", adolescentes_nbi_sobre_total_provisional = "nbi",
                    monoparental_jefatura_aproximacion = "monoparentalidad")
nombre_largo <- c(inasistencia_12_17 = "Inasistencia de 12 a 17 años", desempleo_general = "Desempleo",
                  pobreza_ingresos = "Pobreza por ingresos",
                  adolescentes_nbi_sobre_total_provisional = "NBI adolescente",
                  monoparental_jefatura_aproximacion = "Hogares monoparentales con menores")
redondear <- function(d, k = 4) mutate(d, across(where(is.double), \(x) round(x, k)))
formato_valor <- function(x, k = 2) if_else(x == round(x), formatC(x, digits = 0, format = "f"),
                                              formatC(x, digits = k, format = "f"))
formato_p <- function(p) if_else(p < 0.0001, "<0.0001", formatC(p, digits = 4, format = "f"))
tema_grafico <- theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white", colour = NA))

explica_modelo <- function(indicador_fh, variables) {
  ajustar_fay_herriot(indicador_fh, variables, rep(1, length(variables)))$validacion$explica_pct
}
calcular_aporte <- function(final) {
  co <- filter(final$coeficientes, termino != "(constante)")
  ind <- first(co$indicador)
  vars <- co$termino
  stopifnot(length(vars) <= 2)
  total <- final$validacion$explica_pct
  aporte <- if (length(vars) == 1) total else {
    solas <- map_dbl(vars, \(v) explica_modelo(ind, v))
    (solas + total - rev(solas)) / 2
  }
  stopifnot(abs(sum(aporte) - total) < 1e-8)
  tibble(indicador = ind, variable = vars, beta = co$beta,
         p_valor = co$p_valor, aporte_pct = aporte, total_pct = total,
         vif = final$validacion$vif_maximo)
}

reportar_indicador <- function(ajuste) {
  v <- ajuste$validacion
  ind <- v$indicador
  linea <- strrep("=", 90)
  e <- estimadores %>%
    filter(indicador == ind) %>%
    select(anio, codigo_provincia, provincia, codigo_canton, canton,
           theta_dir_pct = valor_pct, denominador_expandido) %>%
    left_join(select(ajuste$estimaciones, -indicador), by = c("anio", "codigo_canton")) %>%
    arrange(anio, codigo_provincia, codigo_canton)
  m <- filter(e, tipo_estimacion == "fay_herriot")
  gamma <- m$gamma_d
  gvf <- filter(diagnostico_varianza, indicador == ind)
  provincia <- m %>%
    filter(denominador_expandido > 0) %>%
    group_by(codigo_provincia, anio) %>%
    summarise(R = sum(denominador_expandido * theta_dir_pct) / sum(denominador_expandido * theta_fh_pct),
              diferencia_pp = (sum(denominador_expandido * theta_fh_pct) - sum(denominador_expandido * theta_dir_pct)) /
                sum(denominador_expandido), .groups = "drop")

  # Tablas del indicador (numericas; se guardan en resultados_fay_herriot.xlsx)
  tablas <- list(
    Modelo = tibble(
      `Elemento` = c("Indicador", "Áreas con muestra", "Áreas solo sintético", "Variables auxiliares", "Método",
                     "Nivel de confianza (%)"),
      Valor = c(nombre_largo[[ind]], nrow(m), v$areas_solo_sintetico,
                str_replace_all(v$variables, fixed(" + "), ", "), v$metodo, 100 * nivel_confianza)),
    Resumen = tibble(
      `Métrica` = c("Estimación directa media (%)", "Estimación EBLUP media (%)",
                    "EE directo medio (pp)", "EE EBLUP medio (pp)",
                    "Reducción media del MSE (%)", "σ²_u, varianza entre áreas (pp²)"),
      Valor = c(mean(m$theta_dir_pct), mean(m$theta_fh_pct), mean(m$ee_gvf_pp), mean(m$rmse_fh_pp),
                100 * (1 - mean(m$mse_fh_pp2) / mean(m$psi_d_pp2)), v$sigma2_u_pp2)),
    Pesos = tibble(
      `Métrica` = c("Gamma mínima", "Primer cuartil", "Mediana", "Media", "Tercer cuartil", "Gamma máxima"),
      Valor = c(min(gamma), unname(quantile(gamma, 0.25)), median(gamma), mean(gamma),
                unname(quantile(gamma, 0.75)), max(gamma))),
    Coeficientes = ajuste$coeficientes %>%
      transmute(`Término` = termino, beta = 100 * beta, std.error = 100 * ee_beta,
                tvalue = t, pvalue = p_valor,
                signo = case_when(is.na(signo_coincide) ~ "", signo_coincide ~ "esperado", TRUE ~ "no esperado")),
    Bondad = tibble(
      `Métrica` = c("Parámetros", "Log-verosimilitud", "AIC"),
      `Sin auxiliares` = c(v$parametros_sin, v$loglike_sin - v$areas * log(100), v$aic_sin + 2 * v$areas * log(100)),
      `Con seleccionadas` = c(v$parametros_con, v$loglike - v$areas * log(100), v$aic + 2 * v$areas * log(100))),
    GVF = tibble(
      `Métrica` = c("Estimaciones con valor", "Áreas ancla", "Efecto de diseño (Deff)",
                    "EE directo mediano de las anclas (pp)", "EE GVF mediano (pp)"),
      Valor = c(gvf$estimaciones_con_valor, gvf$areas_ancla, gvf$deff,
                gvf$ee_dir_mediana_anclas_pp, gvf$ee_gvf_mediana_pp)),
    Supuestos = tibble(
      `Revisión` = c("Shapiro-Wilk p, residuos", "Desviación del residuo estandarizado (cerca de 1)",
                     "Estimaciones fuera de 0-100 %", "Áreas con error FH menor que el directo",
                     "CV mediano directo GVF (%)", "CV mediano FH (%)"),
      Valor = c(v$shapiro_p_residuos, sd(ajuste$diagnosticos$residuo_estandarizado),
                v$fuera_de_rango, v$areas_con_error_menor, v$cv_gvf_mediana, v$cv_fh_mediana)),
    Provincia = tibble(
      `Métrica` = c("Provincia-año", "R mínimo", "R mediana", "R máximo", "Diferencia mediana (pp)",
                    "Se alejan más de 10 %", "Se alejan más de 20 %"),
      Valor = c(nrow(provincia), min(provincia$R), median(provincia$R), max(provincia$R),
                median(abs(provincia$diferencia_pp)), sum(abs(provincia$R - 1) > 0.10),
                sum(abs(provincia$R - 1) > 0.20))))
  titulos <- c(Modelo = "Modelo", Resumen = "Resumen", Pesos = "Pesos de contracción",
               Coeficientes = "Coeficientes de regresión (escala %)", Bondad = "Bondad de ajuste (escala %)",
               GVF = "Validación: varianza suavizada con efecto de diseño (GVF)",
               Supuestos = "Validación: supuestos y precisión",
               Provincia = "Validación: concordancia con la provincia (R = directo de la provincia / promedio FH de sus cantones)")

  # Consola
  cat("\n", linea, "\n", "Estimación en áreas pequeñas EBLUP Fay-Herriot: ", nombre_largo[[ind]], "\n",
      "Áreas (m): ", nrow(m), " con muestra y ", v$areas_solo_sintetico, " solo sintético",
      "  ·  Variables auxiliares: ", str_count(v$variables, fixed("+")) + 1,
      " (", str_replace_all(v$variables, fixed(" + "), ", "), ")",
      "  ·  Método: ", v$metodo, "  ·  Nivel de confianza: ", 100 * nivel_confianza, "%",
      "  ·  σ²_u: ", formatC(v$sigma2_u_pp2, digits = 3, format = "f"),
      "  ·  γ_d media: ", formatC(mean(gamma), digits = 4, format = "f"), "\n", linea, "\n", sep = "")
  mostrar_tabla(paste0("Estimaciones EBLUP por área (primeras 25 de ", nrow(e), "; todas en estimaciones_fay_herriot.xlsx)"),
                e %>%
                  transmute(`Cantón` = canton, `Año` = anio,
                            Tipo = if_else(tipo_estimacion == "fay_herriot", "FH", "Sintético"),
                            Directa = theta_dir_pct, EE_directa = ee_gvf_pp,
                            EBLUP = theta_fh_pct, EE_EBLUP = rmse_fh_pp,
                            Inferior = ic_inf_fh_pct, Superior = ic_sup_fh_pct, Gamma = gamma_d) %>%
                  head(25) %>% redondear(3))
  mostrar_tabla(titulos[["Resumen"]], redondear(tablas$Resumen, 3))
  mostrar_tabla(titulos[["Pesos"]], redondear(tablas$Pesos, 4))
  cat("gamma_d = sigma2_u / (sigma2_u + psi_d). Cerca de 1: domina la estimación directa; cerca de 0: domina la regresión.\n")
  mostrar_tabla(titulos[["Coeficientes"]], redondear(tablas$Coeficientes, 4))
  mostrar_tabla(titulos[["Bondad"]], redondear(tablas$Bondad, 3))
  cat("Un AIC más bajo indica mejor ajuste. Diferencia (con − sin): ",
      formatC(tablas$Bondad$`Con seleccionadas`[3] - tablas$Bondad$`Sin auxiliares`[3], digits = 1, format = "f"), "\n", sep = "")
  mostrar_tabla(titulos[["GVF"]], mutate(tablas$GVF, Valor = formato_valor(Valor)))
  mostrar_tabla(titulos[["Supuestos"]], mutate(tablas$Supuestos,
    Valor = if_else(str_detect(`Revisión`, "Shapiro"), formato_p(Valor), formato_valor(Valor))))
  mostrar_tabla(titulos[["Provincia"]], mutate(tablas$Provincia, Valor = formato_valor(Valor)))

  # Graficos: los dos ejes con la misma escala, de 0 al mayor valor de los dos, para que la linea y = x quede a 45 grados
  # y se vea cuanto se aleja cada punto de ella.
  limites_contraccion <- c(0, max(m$theta_dir_pct, m$theta_fh_pct))
  limites_mse <- c(0, max(m$psi_d_pp2, m$mse_fh_pp2))
  grafico_contraccion <- ggplot(m, aes(theta_dir_pct, theta_fh_pct)) +
    geom_abline(linetype = "dashed", colour = "grey45") +
    geom_point(colour = "steelblue", size = 1.6, alpha = 0.75) +
    coord_equal(xlim = limites_contraccion, ylim = limites_contraccion) +
    labs(title = paste("Contracción del EBLUP hacia la regresión:", nombre_largo[[ind]]),
         subtitle = "Cada punto es un cantón-año con muestra. Línea discontinua: EBLUP igual a la estimación directa",
         x = "Estimación directa θ̂ᴰᴵᴿ (%)", y = "Estimación EBLUP θ̂ᶠᴴ (%)") +
    tema_grafico
  grafico_mse <- ggplot(m, aes(psi_d_pp2, mse_fh_pp2)) +
    geom_abline(linetype = "dashed", colour = "grey45") +
    geom_point(colour = "steelblue", size = 1.6, alpha = 0.75) +
    coord_equal(xlim = limites_mse, ylim = limites_mse) +
    labs(title = paste("MSE del EBLUP frente a la varianza directa:", nombre_largo[[ind]]),
         subtitle = "Bajo la línea discontinua (y = x), el EBLUP es más preciso que la estimación directa",
         x = "Varianza de muestreo directa ψ_d (pp²)", y = "MSE del EBLUP (pp²)") +
    tema_grafico
  print(grafico_contraccion)
  print(grafico_mse)
  if (isTRUE(exportar_tabla)) {
    ggsave(file.path(carpeta_img, paste0("contraccion_eblup_", nombre_archivo[[ind]], ".png")),
           grafico_contraccion, width = 7.5, height = 7.5, dpi = 150)
    ggsave(file.path(carpeta_img, paste0("mse_vs_varianza_directa_", nombre_archivo[[ind]], ".png")),
           grafico_mse, width = 7.5, height = 7.5, dpi = 150)
  }
  invisible(list(tablas = tablas, titulos = titulos))
}

mostrar_etapa("3. Funciones del modelo Fay-Herriot definidas")


# 4. MODELO CON LAS VARIABLES AUXILIARES CANDIDATAS ----------------------------

# Para cada indicador se ajusta el modelo con todas sus variables auxiliares candidatas (Corral et al., 2022, p. 21).
# Una candidata es una variable con relacion teorica con el indicador (p. 28).
# Reglas de las candidatas:
#   - BDH general y BDH con componente variable no van en el mismo modelo: el segundo esta contenido en el primero.
#   - La matricula particular solo es candidata de pobreza.
# Criterio: una variable se conserva si su coeficiente es significativo (p < 0,05); las demas se retiran porque "may introduce noise" (p. 104).
# Se revisa tambien el VIF (mayor que 5, pp. 21 y 106).


# Tabla de significancia de un indicador.
tabla_significancia <- function(ajuste) {
  ajuste$coeficientes %>%
    filter(termino != "(constante)") %>%
    transmute(indicador, variable = termino, beta, ee_beta, p_valor,
              significativa = if_else(p_valor < 0.05, "si", "no"),
              signo_esperado, signo_observado)
}

# Muestra la tabla de significancia y los atributos del modelo.
mostrar_candidatas <- function(ajuste, titulo) {
  mostrar_etapa(paste(titulo, "- significancia de las variables candidatas"),
                tabla_significancia(ajuste) %>% select(-indicador) %>%
                  mutate(across(c(beta, ee_beta, p_valor), \(x) signif(x, 4))))
  mostrar_etapa(paste(titulo, "- atributos del modelo con las candidatas"),
                ajuste$validacion %>%
                  transmute(areas, explica_pct = round(explica_pct, 2),
                            vif_maximo = round(vif_maximo, 2),
                            rmse_fh_mediana_pp = round(rmse_fh_mediana_pp, 3),
                            shapiro_p_residuos = signif(shapiro_p_residuos, 3),
                            areas_influyentes))
  invisible(NULL)
}

## 4.1 Inasistencia de 12 a 17 anios ----

#   theta_d = b0 + b1*abandono_escolar_d + b2*bdh_general_d + b3*poblacion_urbana_d + b4*empleo_formal_d + u_d
# Se mantienen abandono_escolar y bdh_general, significativas y con el signo esperado.
# poblacion_urbana y empleo_formal no son significativas y se retiran.
cand_inasistencia <- ajustar_fay_herriot(
  "inasistencia_12_17",
  c("abandono_escolar", "bdh_general", "poblacion_urbana", "empleo_formal"),
  c(1, 1, NA, NA))
mostrar_candidatas(cand_inasistencia, "4.1 Inasistencia 12-17")

## 4.2 Desempleo ----

#   theta_d = b0 + b1*empleo_formal_d + b2*poblacion_urbana_d + u_d
# Se mantiene poblacion_urbana, significativa y con el signo esperado.
# empleo_formal: se esperaba negativo y salio positivo, pero no es significativa y se retira.
cand_desempleo <- ajustar_fay_herriot(
  "desempleo_general",
  c("empleo_formal", "poblacion_urbana"),
  c(-1, 1))
mostrar_candidatas(cand_desempleo, "4.2 Desempleo")

## 4.3 Pobreza por ingresos ----

#   theta_d = b0 + b1*bdh_general_d + b2*salario_basico_d + b3*matricula_particular_d + b4*poblacion_urbana_d + u_d
# Se mantiene bdh_general, significativa y con el signo esperado.
# salario_basico: se esperaba positivo y salio negativo, pero es significativa, por lo que pasa al modelo con las seleccionadas.
# matricula_particular y poblacion_urbana no son significativas y se retiran.
cand_pobreza <- ajustar_fay_herriot(
  "pobreza_ingresos",
  c("bdh_general", "salario_basico", "matricula_particular", "poblacion_urbana"),
  c(1, 1, -1, NA))
mostrar_candidatas(cand_pobreza, "4.3 Pobreza por ingresos")

## 4.4 NBI adolescente ----

#   theta_d = b0 + b1*bdh_variable_d + b2*abandono_escolar_d + b3*poblacion_urbana_d + u_d
# Se mantiene bdh_variable, significativa y con el signo esperado.
# abandono_escolar: se esperaba positivo y salio negativo, pero no es significativa y se retira, igual que poblacion_urbana.
cand_nbi <- ajustar_fay_herriot(
  "adolescentes_nbi_sobre_total_provisional",
  c("bdh_variable", "abandono_escolar", "poblacion_urbana"),
  c(1, 1, NA))
mostrar_candidatas(cand_nbi, "4.4 NBI adolescente")

## 4.5 Hogares monoparentales con menores ----

#   theta_d = b0 + b1*nacidos_sin_pareja_d + b2*poblacion_urbana_d + u_d
# Se mantienen las dos, significativas y con el signo esperado.
cand_monoparentalidad <- ajustar_fay_herriot(
  "monoparental_jefatura_aproximacion",
  c("nacidos_sin_pareja", "poblacion_urbana"),
  c(1, 1))
mostrar_candidatas(cand_monoparentalidad, "4.5 Monoparentalidad")


# 5. MODELO CON LAS VARIABLES AUXILIARES SELECCIONADAS --------------------------

# Se vuelve a ajustar el modelo de cada indicador con las variables significativas (Corral et al., 2022, pp. 21 y 104).
# Un modelo por indicador (pp. 25 y 28).
# Este es el modelo final.

# En las formulas, d = canton-anio.
# Resultados de cada indicador (tablas de la seccion 3.4), para la seccion 7.
resultados_fh <- list()
comprobar_seleccion <- function(final, candidatas) {
  significativas <- tabla_significancia(candidatas) %>%
    filter(significativa == "si") %>% pull(variable)
  usadas <- setdiff(final$coeficientes$termino, "(constante)")
  if (!setequal(significativas, usadas))
    stop("Variables distintas a las significativas en ",
         first(final$coeficientes$indicador), ". Significativas: ",
         paste(significativas, collapse = ", "))
  if (any(tabla_significancia(final)$significativa == "no"))
    stop("Una variable deja de ser significativa en ", first(final$coeficientes$indicador))
  invisible(TRUE)
}

## 5.1 Inasistencia de 12 a 17 anios ----

# Enlace:
#   theta_d = b0 + b1 * abandono_escolar_d + b2 * bdh_general_d + u_d
# Donde:
#   theta_d               = porcentaje verdadero de adolescentes de 12 a 17 anios que no asisten a educacion.
#   b0                    = constante del modelo.
#   b1 * abandono_escolar = aporte de la tasa de abandono escolar de 8.o de EGB a 3.o de Bachillerato (%), multiplicada por su coeficiente.
#   b2 * bdh_general      = aporte de los hogares con BDH por cada 100 habitantes, multiplicados por su coeficiente.
#   u_d                   = efecto de area del canton-anio.
# Estimacion final:
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * (b0 + b1*abandono_d + b2*bdh_d)
# Por que se relacionan:
#   abandono_escolar: quien abandona el grado sale del sistema educativo (Arias et al., 2022, p. 52), por lo que con mas abandono se espera mas adolescentes que no asisten.
#   bdh_general: el bono se dirige a nucleos familiares en pobreza o extrema pobreza (INEC, 2026b, p. 131) y las mayores brechas para terminar la secundaria se dan entre niveles socioeconomicos (Trucco et al., 2026, p. 14), por lo que con mas hogares con bono se espera mas inasistencia.
fh_inasistencia <- ajustar_fay_herriot("inasistencia_12_17",
                                       c("abandono_escolar", "bdh_general"), c(1, 1))
comprobar_seleccion(fh_inasistencia, cand_inasistencia)
resultados_fh$inasistencia <- reportar_indicador(fh_inasistencia)

## 5.2 Desempleo ----

# Enlace:
#   theta_d = b0 + b1 * poblacion_urbana_d + u_d
# Donde:
#   theta_d               = tasa de desempleo verdadera (% de la PEA).
#   b0                    = constante del modelo.
#   b1 * poblacion_urbana = aporte del porcentaje de poblacion que vive en el area urbana, multiplicado por su coeficiente.
#   u_d                   = efecto de area del canton-anio.
# Estimacion final:
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * (b0 + b1*urbana_d)
# Por que se relacionan:
#   poblacion_urbana: en diciembre de 2025 el desempleo fue de 3,2 % en el area urbana y de 1,5 % en la rural (INEC, 2026f), por lo que con mas poblacion urbana se espera mas desempleo.
fh_desempleo <- ajustar_fay_herriot("desempleo_general", "poblacion_urbana", 1)
comprobar_seleccion(fh_desempleo, cand_desempleo)
resultados_fh$desempleo <- reportar_indicador(fh_desempleo)

## 5.3 Pobreza por ingresos ----

# Enlace:
#   theta_d = b0 + b1 * bdh_general_d + b2 * salario_basico_d + u_d
# Donde:
#   theta_d             = tasa de pobreza por ingresos verdadera (% de personas).
#   b0                  = constante del modelo.
#   b1 * bdh_general    = aporte de los hogares con BDH por cada 100 habitantes, multiplicados por su coeficiente.
#   b2 * salario_basico = aporte del porcentaje de trabajadores formales cuyo sueldo a 30 dias es menor o igual al salario basico, multiplicado por su coeficiente.
#   u_d                 = efecto de area del canton-anio.
# Estimacion final:
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * (b0 + b1*bdh_d + b2*salario_d)
# Por que se relacionan:
#   bdh_general: el bono se dirige a nucleos familiares en pobreza o extrema pobreza (INEC, 2026b, p. 131), por lo que con mas hogares con bono se espera mas pobreza.
#   salario_basico: Chile usa en su modelo de pobreza el porcentaje de afiliados con remuneracion inferior al salario minimo (MDSF, 2019, p. 12), por lo que con mas trabajadores en el salario basico se espera mas pobreza.
# Por que se mantiene salario_basico: se esperaba positivo y en los dos modelos salio negativo.
# Se mantiene porque es significativa, aporta a lo que explica el modelo y no repite informacion con bdh_general (VIF), y porque el modelo busca predecir y no determinar causalidad (MDSF, 2019, p. 24).
# En esa misma pagina Chile mantiene el promedio de ingresos, cuyo signo no fue el esperado, porque le interesa la prediccion.
# Su signo no se interpreta como causa.
fh_pobreza <- ajustar_fay_herriot("pobreza_ingresos",
                                  c("bdh_general", "salario_basico"), c(1, 1))
comprobar_seleccion(fh_pobreza, cand_pobreza)
resultados_fh$pobreza <- reportar_indicador(fh_pobreza)

## 5.4 NBI adolescente ----

# Enlace:
#   theta_d = b0 + b1 * bdh_variable_d + u_d
# Donde:
#   theta_d           = porcentaje verdadero de adolescentes de 12 a 17 anios en hogares pobres por NBI, sobre la poblacion total.
#   b0                = constante del modelo.
#   b1 * bdh_variable = aporte de los hogares con BDH con componente variable por cada 100 habitantes, multiplicados por su coeficiente.
#   u_d               = efecto de area del canton-anio.
# Estimacion final:
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * (b0 + b1*bdh_variable_d)
# Por que se relacionan:
#   bdh_variable: este bono se dirige a nucleos familiares en extrema pobreza con hijos menores de 18 anios (INEC, 2026b, p. 132), por lo que con mas hogares con este bono se espera mas adolescentes en hogares con NBI.
# El modelo admite una o varias variables (Corral et al., 2022, p. 27).
fh_nbi <- ajustar_fay_herriot("adolescentes_nbi_sobre_total_provisional",
                              "bdh_variable", 1)
comprobar_seleccion(fh_nbi, cand_nbi)
resultados_fh$nbi <- reportar_indicador(fh_nbi)

## 5.5 Hogares monoparentales con menores ----

# Enlace:
#   theta_d = b0 + b1 * nacidos_sin_pareja_d + b2 * poblacion_urbana_d + u_d
# Donde:
#   theta_d                 = porcentaje verdadero de hogares monoparentales con menores de 18 anios.
#   b0                      = constante del modelo.
#   b1 * nacidos_sin_pareja = aporte del porcentaje de nacidos vivos de madres solteras, divorciadas, separadas o viudas, multiplicado por su coeficiente.
#   b2 * poblacion_urbana   = aporte del porcentaje de poblacion urbana, multiplicado por su coeficiente.
#   u_d                     = efecto de area del canton-anio.
# Estimacion final:
#   theta^_d^FH = gamma_d * theta^_d^DIR + (1 - gamma_d) * (b0 + b1*nacidos_d + b2*urbana_d)
# Por que se relacionan:
#   nacidos_sin_pareja: recoge el estado civil o conyugal de la madre (INEC, 2025b), por lo que con mas nacidos de madres sin pareja se espera mas hogares con un solo jefe e hijos menores.
#   poblacion_urbana: las familias monoparentales pueden resultar de la ruptura de uniones, dentro de cambios familiares que se asocian con la urbanizacion (Ullmann, Maldonado y Rico, 2014, pp. 26 y 30), por lo que con mas poblacion urbana se espera mas monoparentalidad.
fh_monoparentalidad <- ajustar_fay_herriot("monoparental_jefatura_aproximacion",
                                           c("nacidos_sin_pareja", "poblacion_urbana"), c(1, 1))
comprobar_seleccion(fh_monoparentalidad, cand_monoparentalidad)
resultados_fh$monoparentalidad <- reportar_indicador(fh_monoparentalidad)

## 5.6 Comparacion del modelo con las candidatas y con las seleccionadas ----

#   explica_pct = parte de la diferencia entre areas que explican las variables
#   cambio_mediano_pp y cambio_maximo_pp = cuanto cambia la estimacion de los canton-anio con muestra entre un modelo y otro
comparar_modelos <- function(candidatas, final) {
  atributos <- function(a, modelo) a$validacion %>%
    transmute(indicador, modelo, variables, sigma2_u_pp2, aic, explica_pct, vif_maximo, rmse_fh_mediana_pp,
              shapiro_p_residuos, areas_influyentes)
  cambio <- inner_join(
    candidatas$estimaciones %>% filter(tipo_estimacion == "fay_herriot") %>%
      select(anio, codigo_canton, e1 = theta_fh_pct),
    final$estimaciones %>% filter(tipo_estimacion == "fay_herriot") %>%
      select(anio, codigo_canton, e2 = theta_fh_pct), by = c("anio", "codigo_canton"))
  bind_rows(atributos(candidatas, "candidatas"), atributos(final, "seleccionadas")) %>%
    mutate(cambio_mediano_pp = c(NA, median(abs(cambio$e1 - cambio$e2))),
           cambio_maximo_pp = c(NA, max(abs(cambio$e1 - cambio$e2))))
}
modelos_candidatas <- list(cand_inasistencia, cand_desempleo, cand_pobreza, cand_nbi, cand_monoparentalidad)
modelos_seleccionados <- list(fh_inasistencia, fh_desempleo, fh_pobreza, fh_nbi, fh_monoparentalidad)
significancia_candidatas <- modelos_candidatas %>% map(tabla_significancia) %>% list_rbind()
significancia_seleccionadas <- modelos_seleccionados %>% map(tabla_significancia) %>% list_rbind()
comparacion_modelos <- map2(modelos_candidatas, modelos_seleccionados, comparar_modelos) %>% list_rbind()
mostrar_etapa("5.6 Comparacion de los dos modelos",
              comparacion_modelos %>% mutate(across(where(is.double), \(x) signif(x, 3))))


# 6. RESUMEN Y SALIDAS ---------------------------------------------------------

## 6.1 Union de los cinco modelos ----

modelos_fh <- list(fh_inasistencia, fh_desempleo, fh_pobreza, fh_nbi, fh_monoparentalidad)
estimaciones_fh   <- modelos_fh %>% map("estimaciones") %>% list_rbind()
coeficientes_fh   <- modelos_fh %>% map("coeficientes") %>% list_rbind()
validacion_fh     <- modelos_fh %>% map("validacion") %>% list_rbind()
diagnosticos_fh   <- modelos_fh %>% map("diagnosticos") %>% list_rbind()
linealidad_fh     <- modelos_fh %>% map("linealidad") %>% list_rbind()
stopifnot(setequal(unique(estimaciones_fh$indicador), names(nombres_indicadores)))

mostrar_etapa("6.1 Resumen de la validacion de los cinco modelos", validacion_fh %>%
                select(indicador, sigma2_u_pp2, aic, explica_pct, fuera_de_rango, shapiro_p_residuos,
                       pct_areas_influyentes, ee_gvf_mediana_pp, rmse_fh_mediana_pp,
                       areas_con_error_menor, areas,
                       areas_solo_sintetico, rmse_solo_sintetico_mediana_pp) %>%
                mutate(across(where(is.double), \(x) round(x, 3))))
mostrar_etapa("6.1 Coeficientes no significativos (p >= 0,05) o con signo distinto al esperado",
              coeficientes_fh %>%
                filter(termino != "(constante)", p_valor >= 0.05 | !signo_coincide) %>%
                select(indicador, termino, beta, p_valor, signo_esperado, signo_observado))

## 6.2 Estimadores del Fay-Herriot (formato largo) ----

# Misma estructura que estimaciones_originales.xlsx: una fila por indicador, canton y anio.
# Los canton-anio sin estimacion directa reciben el valor
# sintetico (tipo_estimacion = "solo_sintetico").
estimadores_fh <- estimadores %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
         muestra_denominador) %>%
  left_join(estimaciones_fh, by = c("anio", "codigo_canton", "indicador")) %>%
  transmute(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
            theta_fh_pct, mse_fh_pp2, rmse_fh_pp, ic_inf_fh_pct, ic_sup_fh_pct,
            cv_fh_pct = if_else(theta_fh_pct > 0, 100 * rmse_fh_pp / theta_fh_pct, NA_real_),
            muestra_denominador, gamma_d, sintetico_xb_pct, fuera_de_rango,
            tipo_estimacion) %>%
  arrange(anio, codigo_provincia, codigo_canton, indicador)

## 6.3 Indicadores del Fay-Herriot (formato ancho) ----

# Misma estructura que construccion_indicadores_factores.xlsx: una fila por canton-anio y una columna por indicador.
indicadores_fh <- indicadores_agregados %>%
  select(all_of(claves_territorio)) %>%
  left_join(estimadores_fh %>%
              mutate(columna = unname(nombres_indicadores[indicador])) %>%
              select(anio, codigo_canton, columna, theta_fh_pct) %>%
              pivot_wider(names_from = columna, values_from = theta_fh_pct),
            by = c("anio", "codigo_canton")) %>%
  select(all_of(c(claves_territorio, columnas_indicadores)))
stopifnot(nrow(indicadores_fh) == nrow(indicadores_agregados),
          !anyNA(select(indicadores_fh, all_of(columnas_indicadores))))

## 6.4 Comparacion: original, GVF y Fay-Herriot (formato largo) ----

# Una fila por canton, anio e indicador, con las tres etapas una al lado de la otra: estimacion directa, error con la varianza suavizada (GVF) y estimacion del Fay-Herriot.
# Incluye la poblacion del area.
comparacion <- estimadores %>%
  left_join(estimaciones_fh, by = c("anio", "codigo_canton", "indicador")) %>%
  left_join(poblacion_area, by = c("anio", "codigo_canton", "indicador")) %>%
  transmute(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
            poblacion_area, poblacion_area_grupo, muestra_denominador,
            # Estimacion directa (INDICADORES.R)
            theta_dir_pct = valor_pct,
            ee_dir_pp = error_estandar_pp,
            cv_dir_pct = cv_pct,
            ic_inf_dir_pct = limite_inferior_pct,
            ic_sup_dir_pct = limite_superior_pct,
            grados_libertad, error_maximo_tolerable_pp,
            cumple_muestra, cumple_gl, cumple_error,
            # Varianza suavizada (GVF)
            # Es la varianza que entra al modelo
            psi_d_pp2, ee_gvf_pp,
            cv_gvf_pct = if_else(valor_pct > 0, 100 * ee_gvf_pp / valor_pct, NA_real_),
            # Fay-Herriot
            sintetico_xb_pct, gamma_d,
            theta_fh_pct, mse_fh_pp2, rmse_fh_pp,
            cv_fh_pct = if_else(theta_fh_pct > 0, 100 * rmse_fh_pp / theta_fh_pct, NA_real_),
            ic_inf_fh_pct, ic_sup_fh_pct,
            # Comparacion
            cambio_pp = theta_fh_pct - valor_pct,
            error_menor_fh = as.integer(rmse_fh_pp < ee_gvf_pp),
            fuera_de_rango, tipo_estimacion) %>%
  arrange(anio, codigo_provincia, codigo_canton, indicador)
stopifnot(!anyNA(comparacion$poblacion_area))

mostrar_etapa("6.2-6.4 Tablas de salida", tibble(
  tabla = c("estimadores_fh (largo)", "indicadores_fh (ancho)", "comparacion (largo)"),
  filas = c(nrow(estimadores_fh), nrow(indicadores_fh), nrow(comparacion)),
  con_estimacion_fh = c(sum(is.finite(estimadores_fh$theta_fh_pct)),
                        sum(is.finite(as.matrix(select(indicadores_fh, all_of(columnas_indicadores))))),
                        sum(is.finite(comparacion$theta_fh_pct)))))

## 6.5 Comparacion con la provincia (validacion, no corrige) ----

# La ENEMDU da un dato mas confiable para la provincia que para el canton.
# Se compara, por indicador, provincia y anio, el dato directo de la provincia con el promedio de las estimaciones Fay-Herriot de sus cantones:
#
#   R = directo_provincia / promedio_FH
#   directo_provincia = suma(w_d * directo_d) / suma(w_d)
#   promedio_FH       = suma(w_d * FH_d)      / suma(w_d)
#
# w_d es la suma de los factores de expansion del denominador del canton.
# Los dos promedios usan los mismos cantones, los que tienen muestra.
# Si R esta cerca de 1, el modelo concuerda con la provincia (MDSF, 2019, p. 11).
# La razon R es la del ajuste de MDSF-CEPAL (2021, p. 53), que la calcula con poblaciones.
# Aqui R solo se reporta: no se multiplica ninguna estimacion, porque el ajuste no se recomienda salvo que la publicacion lo exija, y las diferencias grandes pueden indicar que no se cumplen los supuestos (Corral et al., 2022, p. 80, en el capitulo de modelos a nivel de unidad).
# El dato de la provincia tambien tiene error de muestreo.
comparacion_provincia <- estimadores %>%
  filter(is.finite(valor_pct), denominador_expandido > 0) %>%
  inner_join(select(estimaciones_fh, anio, codigo_canton, indicador, theta_fh_pct),
             by = c("anio", "codigo_canton", "indicador")) %>%
  group_by(indicador, codigo_provincia, provincia, anio) %>%
  summarise(cantones_con_muestra = n(),
            directo_provincia_pct = sum(denominador_expandido * valor_pct) / sum(denominador_expandido),
            promedio_fh_pct = sum(denominador_expandido * theta_fh_pct) / sum(denominador_expandido),
            .groups = "drop") %>%
  mutate(diferencia_pp = promedio_fh_pct - directo_provincia_pct,
         R = directo_provincia_pct / promedio_fh_pct,
         se_aleja_10 = as.integer(abs(R - 1) > 0.10),
         se_aleja_20 = as.integer(abs(R - 1) > 0.20))
stopifnot(all(is.finite(comparacion_provincia$R)))
resumen_provincia <- comparacion_provincia %>%
  group_by(indicador) %>%
  summarise(provincia_anio = n(), R_minimo = min(R), R_mediana = median(R), R_maximo = max(R),
            diferencia_abs_mediana_pp = median(abs(diferencia_pp)),
            diferencia_abs_maxima_pp = max(abs(diferencia_pp)),
            se_alejan_10 = sum(se_aleja_10), se_alejan_20 = sum(se_aleja_20))
mostrar_etapa("6.5 Comparacion con la provincia (R cerca de 1)",
              resumen_provincia %>% mutate(across(where(is.double), \(x) round(x, 3))))

## 6.6 Exportacion ----

# Archivos en DATAS_PROCESADAS:
#   indicadores_fay_herriot.xlsx               estimaciones del Fay-Herriot, una columna por indicador (ancho)
#   estimaciones_fay_herriot.xlsx              estimadores del Fay-Herriot con su error, IC y CV (largo)
#   comparacion_original_gvf_fay_herriot.xlsx  original, varianza suavizada (GVF) y Fay-Herriot, lado a lado (largo)
# La validacion (validacion_fh, coeficientes_fh, diagnostico_varianza, diagnosticos_fh, resumen_provincia y comparacion_provincia) queda como objeto; la de cada indicador se muestra en consola en la seccion 5.
# Imagenes en SCRIPTS/SALIDAS_EXPLORACIONES/salidas_modelo_fh:
#   validacion_linealidad_fay_herriot.png      residuos contra cada variable
#   validacion_dispersion_residuos_fay_herriot.png  residuos contra valor sintetico
#   comparacion_directo_fay_herriot.png        directo contra Fay-Herriot (figura 3.2 de Corral et al., 2022, p. 24)
#   comparacion_provincia_fay_herriot.png      dato de la provincia contra promedio de sus cantones
#   comparacion_error_fay_herriot.png          error del directo contra error del Fay-Herriot (misma figura)

if (isTRUE(exportar_tabla)) {
  writexl::write_xlsx(list(Indicadores_fay_herriot = indicadores_fh),
                      file.path(carpeta_salida, "indicadores_fay_herriot.xlsx"))
  writexl::write_xlsx(list(Estimaciones_fay_herriot = estimadores_fh),
                      file.path(carpeta_salida, "estimaciones_fay_herriot.xlsx"))
  writexl::write_xlsx(list(Comparacion = comparacion),
                      file.path(carpeta_salida, "comparacion_original_gvf_fay_herriot.xlsx"))
  # Linealidad: curva suavizada (loess) sobre los residuos
  grafico_linealidad <- linealidad_fh %>%
    mutate(panel = paste(indicador, variable, sep = "\n")) %>%
    ggplot(aes(valor_variable, residuo_estandarizado)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_point(colour = "steelblue4", size = 0.7, alpha = 0.6) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "firebrick") +
    facet_wrap(~ panel, scales = "free_x", ncol = 4) +
    labs(title = "Linealidad: residuos estandarizados contra cada variable auxiliar",
         x = "Variable auxiliar", y = "Residuo estandarizado") +
    theme_minimal(base_size = 9)
  ggsave(file.path(carpeta_img, "validacion_linealidad_fay_herriot.png"), grafico_linealidad,
         width = 12, height = 7, dpi = 150)
  # Dispersion de residuos: residuo estandarizado contra el valor sintetico; los puntos deben repartirse sin forma alrededor de cero
  grafico_dispersion <- diagnosticos_fh %>%
    left_join(select(estimaciones_fh, anio, codigo_canton, indicador, sintetico_xb_pct),
              by = c("anio", "codigo_canton", "indicador")) %>%
    mutate(indicador = unname(c(
      pobreza_ingresos = "Pobreza", adolescentes_nbi_sobre_total_provisional = "NBI adolescente",
      monoparental_jefatura_aproximacion = "Monoparentalidad",
      inasistencia_12_17 = "Inasistencia", desempleo_general = "Desempleo")[indicador])) %>%
    ggplot(aes(sintetico_xb_pct, residuo_estandarizado)) +
    geom_hline(yintercept = 0, colour = "grey40") +
    geom_hline(yintercept = c(-1.96, 1.96), colour = "grey40", linetype = "dashed") +
    geom_point(colour = "steelblue4", size = 0.7, alpha = 0.6) +
    facet_wrap(~ indicador, scales = "free_x", ncol = 3) +
    labs(title = "Residuo estandarizado y valor que predicen las variables auxiliares",
         x = "Valor sintético (%)", y = "Residuo estandarizado") +
    theme_minimal(base_size = 9)
  ggsave(file.path(carpeta_img, "validacion_dispersion_residuos_fay_herriot.png"),
         grafico_dispersion, width = 9, height = 5.5, dpi = 150)
  # Figura 3.2 de Corral et al. (2022, p. 24): estimaciones y errores
  datos_figura <- comparacion %>% filter(tipo_estimacion == "fay_herriot")
  grafico_directo_fh <- ggplot(datos_figura, aes(theta_dir_pct, theta_fh_pct)) +
    geom_abline(colour = "grey40") +
    geom_point(colour = "steelblue4", size = 0.7, alpha = 0.6) +
    facet_wrap(~ indicador, scales = "free", ncol = 5) +
    labs(title = "Estimacion directa contra Fay-Herriot (linea: iguales)",
         x = "Directo (%)", y = "Fay-Herriot (%)") +
    theme_minimal(base_size = 9)
  ggsave(file.path(carpeta_img, "comparacion_directo_fay_herriot.png"), grafico_directo_fh,
         width = 14, height = 3.5, dpi = 150)
  # Dato directo de la provincia contra el promedio Fay-Herriot
  grafico_provincia_fh <- ggplot(comparacion_provincia, aes(directo_provincia_pct, promedio_fh_pct)) +
    geom_abline(linetype = 2, colour = "grey50") +
    geom_point(aes(colour = provincia), size = 1.8, alpha = 0.8) +
    facet_wrap(~ indicador, scales = "free") +
    labs(x = "Dato directo de la provincia (%)", y = "Promedio Fay-Herriot de sus cantones (%)",
         colour = NULL, title = "Concordancia con la provincia, 2021-2025",
         subtitle = "Cada punto es una provincia y un anio. Sobre la linea, los dos datos coinciden") +
    theme_minimal() + theme(legend.position = "bottom")
  ggsave(file.path(carpeta_img, "comparacion_provincia_fay_herriot.png"), grafico_provincia_fh,
         width = 11, height = 7, dpi = 150)
  grafico_error_fh <- ggplot(datos_figura, aes(ee_gvf_pp, rmse_fh_pp)) +
    geom_abline(colour = "grey40") +
    geom_point(colour = "steelblue4", size = 0.7, alpha = 0.6) +
    facet_wrap(~ indicador, scales = "free", ncol = 5) +
    labs(title = "Error del directo (el usado en el modelo) contra error del Fay-Herriot (bajo la linea: el Fay-Herriot es mas preciso)",
         x = "Error del directo (pp)", y = "Error del Fay-Herriot (pp)") +
    theme_minimal(base_size = 9)
  ggsave(file.path(carpeta_img, "comparacion_error_fay_herriot.png"), grafico_error_fh,
         width = 14, height = 3.5, dpi = 150)
  mostrar_etapa("6.6 Archivos guardados", tibble(
    archivo = c("indicadores_fay_herriot.xlsx", "estimaciones_fay_herriot.xlsx",
                "comparacion_original_gvf_fay_herriot.xlsx",
                "validacion_linealidad_fay_herriot.png",
                "validacion_dispersion_residuos_fay_herriot.png",
                "comparacion_directo_fay_herriot.png", "comparacion_provincia_fay_herriot.png",
                "comparacion_error_fay_herriot.png"),
    filas = c(nrow(indicadores_fh), nrow(estimadores_fh), nrow(comparacion), NA, NA, NA, NA, NA)))
}

## 6.7 Tablas de resultados ----

# Resultados de los cinco indicadores juntos, como objetos (con mostrar_detalle <- TRUE tambien en consola).
# Aporte de cada variable: ver 3.4.
nombre_corto <- c(inasistencia_12_17 = "Inasistencia", desempleo_general = "Desempleo",
                  pobreza_ingresos = "Pobreza", adolescentes_nbi_sobre_total_provisional = "NBI",
                  monoparental_jefatura_aproximacion = "Monoparentalidad")
corto <- function(d) mutate(d, indicador = unname(nombre_corto[indicador]))
numero <- function(x, k = 4) formatC(x, digits = k, format = "f")
p_texto <- function(p) if_else(p < 0.0001, "<0.0001", formatC(p, digits = 4, format = "f"))

mostrar_etapa("6.7 Efecto de diseno por indicador", diagnostico_varianza %>% corto() %>%
  transmute(indicador, estimaciones = estimaciones_con_valor, areas_ancla, deff = numero(deff, 2),
            ee_dir_anclas = numero(ee_dir_mediana_anclas_pp, 2),
            ee_gvf_todas = numero(ee_gvf_mediana_pp, 2)))
aporte_variables <- modelos_seleccionados %>% map(calcular_aporte) %>% list_rbind()
mostrar_etapa("6.7 Variables de cada modelo", aporte_variables %>% corto() %>%
  transmute(indicador, variable, beta = numero(beta, 5), p_valor = p_texto(p_valor),
            aporte_pct = numero(aporte_pct, 1), total_pct = numero(total_pct, 1),
            vif = if_else(is.na(vif), "-", numero(vif, 2))))

# sigma2_u = varianza de los efectos de area: la variacion entre areas que las variables no explican.
atributos_fh <- estimaciones_fh %>%
  filter(tipo_estimacion == "fay_herriot") %>%
  group_by(indicador) %>%
  summarise(areas = as.character(n()),
            gamma_d_minimo = numero(min(gamma_d), 3),
            gamma_d_mediano = numero(median(gamma_d), 2),
            gamma_d_maximo = numero(max(gamma_d), 2),
            ee_gvf_mediano_pp = numero(median(ee_gvf_pp), 2),
            rmse_fh_mediano_pp = numero(median(rmse_fh_pp), 2)) %>%
  left_join(validacion_fh %>% transmute(indicador, sigma2_u_pp2 = numero(sigma2_u_pp2, 3),
                                        aic = numero(aic, 1)), by = "indicador") %>%
  relocate(sigma2_u_pp2, .after = areas) %>%
  left_join(diagnosticos_fh %>% group_by(indicador) %>%
              summarise(desviacion_residuo = numero(sd(residuo_estandarizado), 2)), by = "indicador") %>%
  relocate(desviacion_residuo, .after = gamma_d_maximo)
mostrar_etapa("6.7 Atributos del Fay-Herriot", atributos_fh %>%
  corto() %>%
  pivot_longer(-indicador, names_to = "atributo") %>%
  pivot_wider(names_from = indicador, values_from = value) %>%
  select(atributo, Inasistencia, Desempleo, Pobreza, NBI, Monoparentalidad))
mostrar_etapa("6.7 Concordancia con la provincia", resumen_provincia %>% corto() %>%
  transmute(indicador, casos = provincia_anio, R_minimo = numero(R_minimo, 2), R_mediana = numero(R_mediana, 2),
            R_maximo = numero(R_maximo, 2), diferencia_mediana_pp = numero(diferencia_abs_mediana_pp, 2),
            mas_de_10 = se_alejan_10, mas_de_20 = se_alejan_20))


# 7. RESUMEN GENERAL DE LOS CINCO INDICADORES -----------------------------------

# Seccion aparte: se puede correr sola despues del resto del script.
# Junta, en tablas con los cinco indicadores, los resultados de cada uno (seccion 3.4), las muestra en consola
# y las guarda en SCRIPTS/SALIDAS_EXPLORACIONES/salidas_modelo_fh/resultados_fay_herriot.xlsx, en una sola hoja, una debajo de otra:
#   1. Modelo                      5. Bondad de ajuste (sin y con variables auxiliares)
#   2. Resumen y precision          6. Varianza suavizada con efecto de diseno (GVF)
#   3. Pesos de contraccion         7. Supuestos y precision
#   4. Coeficientes de regresion    8. Concordancia con la provincia
# Formato de tabla sin colores: titulo en negrita, cuadro alrededor, linea debajo del encabezado y linea despues de la primera columna; todos los decimales.
# Las estimaciones por area no van aqui porque estan en estimaciones_fay_herriot.xlsx.
stopifnot(requireNamespace("openxlsx", quietly = TRUE), length(resultados_fh) == 5)

## 7.1 Tablas generales ----

# Pasa una tabla de dos columnas (nombre y valor) de cada indicador a una fila por indicador.
a_lo_ancho <- function(tabla) {
  map(resultados_fh, \(r) {
    d <- r$tablas[[tabla]]
    tibble(Indicador = r$tablas$Modelo$Valor[1], nombre = d[[1]], valor = d[[2]])
  }) %>%
    list_rbind() %>%
    filter(nombre != "Indicador") %>%
    pivot_wider(names_from = nombre, values_from = valor)
}
validacion_orden <- validacion_fh %>% arrange(match(indicador, names(nombre_largo)))

tablas_generales <- list(
  `Tabla 1. Modelo de cada indicador` = a_lo_ancho("Modelo") %>%
    mutate(across(c(`Áreas con muestra`, `Áreas solo sintético`, `Nivel de confianza (%)`), as.numeric)) %>%
    select(Indicador, `Variables auxiliares`, `Áreas con muestra`, `Áreas solo sintético`, `Método`,
           `Nivel de confianza (%)`),
  `Tabla 2. Resumen y precisión` = a_lo_ancho("Resumen"),
  `Tabla 3. Pesos de contracción (γ_d)` = a_lo_ancho("Pesos"),
  `Tabla 4. Coeficientes de regresión (escala %)` = map(resultados_fh, \(r)
    mutate(r$tablas$Coeficientes, Indicador = r$tablas$Modelo$Valor[1], .before = 1)) %>% list_rbind(),
  `Tabla 5. Bondad de ajuste sin y con variables auxiliares (escala %)` = validacion_orden %>%
    transmute(Indicador = unname(nombre_largo[indicador]),
              `Parámetros sin auxiliares` = parametros_sin,
              `Parámetros con seleccionadas` = parametros_con,
              `Log-verosimilitud sin auxiliares` = loglike_sin - areas * log(100),
              `Log-verosimilitud con seleccionadas` = loglike - areas * log(100),
              `AIC sin auxiliares` = aic_sin + 2 * areas * log(100),
              `AIC con seleccionadas` = aic + 2 * areas * log(100),
              `Diferencia AIC (con − sin)` = aic - aic_sin),
  `Tabla 6. Varianza suavizada con efecto de diseño (GVF)` = a_lo_ancho("GVF"),
  `Tabla 7. Supuestos y precisión` = a_lo_ancho("Supuestos"),
  `Tabla 8. Concordancia con la provincia (R = directo de la provincia / promedio FH de sus cantones)` = a_lo_ancho("Provincia"))

## 7.2 Consola ----

cat("\n", strrep("=", 90), "\nResumen general de los cinco indicadores\n", strrep("=", 90), "\n", sep = "")
for (k in names(tablas_generales)) mostrar_tabla(k, redondear(tablas_generales[[k]], 4))

## 7.3 Excel ----

# Escribe las tablas una debajo de otra en una hoja.
escribir_tablas <- function(wb, hoja, tablas) {
  openxlsx::addWorksheet(wb, hoja, gridLines = FALSE)
  estilo_titulo <- openxlsx::createStyle(textDecoration = "bold")
  estilo_encabezado <- openxlsx::createStyle(textDecoration = "bold", halign = "center", valign = "center", wrapText = TRUE)
  borde <- function(lado) openxlsx::createStyle(border = lado, borderStyle = "thin", borderColour = "#000000")
  fila <- 1
  for (k in names(tablas)) {
    d <- tablas[[k]]
    n <- nrow(d); m <- ncol(d)
    openxlsx::writeData(wb, hoja, k, startRow = fila, startCol = 1)
    openxlsx::addStyle(wb, hoja, estilo_titulo, rows = fila, cols = 1)
    openxlsx::writeData(wb, hoja, d, startRow = fila + 1, startCol = 1, headerStyle = estilo_encabezado)
    cab <- fila + 1
    cuerpo <- cab + seq_len(n)
    todas <- c(cab, cuerpo)
    openxlsx::setRowHeights(wb, hoja, rows = cab, heights = 45)
    # Numeros: enteros sin decimales; el resto con todos sus decimales (formato General)
    for (j in seq_len(m)) if (is.numeric(d[[j]])) for (i in seq_len(n)) {
      x <- d[[j]][i]
      if (!is.na(x)) openxlsx::addStyle(wb, hoja, openxlsx::createStyle(numFmt = if (x == round(x)) "0" else "General"),
                                        rows = cab + i, cols = j, stack = TRUE)
    }
    # Cuadro, linea debajo del encabezado y linea despues de la primera columna
    openxlsx::addStyle(wb, hoja, borde("top"), rows = cab, cols = 1:m, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, hoja, borde("bottom"), rows = cab, cols = 1:m, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, hoja, borde("bottom"), rows = max(todas), cols = 1:m, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, hoja, borde("left"), rows = todas, cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, hoja, borde("right"), rows = todas, cols = m, gridExpand = TRUE, stack = TRUE)
    openxlsx::addStyle(wb, hoja, borde("right"), rows = todas, cols = 1, gridExpand = TRUE, stack = TRUE)
    fila <- fila + n + 4
  }
  # La segunda columna es mas ancha para que quepan las variables auxiliares de la tabla 1
  openxlsx::setColWidths(wb, hoja, cols = 1:10, widths = c(36, 37, rep(17, 8)))
}
dir.create(carpeta_img, recursive = TRUE, showWarnings = FALSE)
libro_resultados <- openxlsx::createWorkbook()
escribir_tablas(libro_resultados, "Resultados", tablas_generales)
openxlsx::saveWorkbook(libro_resultados, file.path(carpeta_img, "resultados_fay_herriot.xlsx"), overwrite = TRUE)
