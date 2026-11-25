# ESTIMACION EN AREAS PEQUENAS: MODELO FAY-HERRIOT POR CANTON-ANIO, 2021-2025
#
# Combina las estimaciones directas de la ENEMDU con variables auxiliares de registros administrativos y proyecciones de poblacion mediante el modelo de area de Fay y Herriot (1979).
# Cada canton-anio es un area (86 cantones de la Costa x 5 anios).
# Se ajusta un modelo por indicador.
#
#

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
if (!exists("carpeta_salida")) carpeta_salida <- carpeta_datos
if (!exists("exportar_tabla")) exportar_tabla <- TRUE
# Los Excel van a carpeta_salida; las imagenes, a carpeta_img.
if (!exists("carpeta_imagenes")) carpeta_imagenes <- "C:/Users/camil/OneDrive/Desktop/TESIS/SCRIPTS/SALIDAS_EXPLORACIONES"
carpeta_img <- file.path(carpeta_imagenes, "salidas_estimacion_areas_pequenas")
if (isTRUE(exportar_tabla)) dir.create(carpeta_img, recursive = TRUE, showWarnings = FALSE)

ruta_estimaciones <- file.path(carpeta_datos, "estimaciones_originales.xlsx")
ruta_indicadores  <- file.path(carpeta_datos, "construccion_indicadores_factores.xlsx")
ruta_auxiliares   <- file.path(carpeta_datos, "panel_variables_auxiliares_2021_2025.xlsx")
ruta_poblacion    <- file.path(carpeta_datos, "poblacion_cantonal_costa_total_12_17_2021_2025.xlsx")
stopifnot(file.exists(ruta_poblacion))
stopifnot(file.exists(ruta_estimaciones), file.exists(ruta_indicadores),
          file.exists(ruta_auxiliares))

## 0.3 Funciones de apoyo ----

# Muestra en consola el titulo de cada etapa y, si se da, una tabla.
mostrar_etapa <- function(titulo, tabla = NULL) {
  print(paste0("--- ", titulo, " ---"))
  if (!is.null(tabla)) print(as.data.frame(tabla), row.names = FALSE)
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
#   V_i = Deff * pt * (1 - pt) / n_i
#
# Donde:
#   V_i   = varianza suavizada del canton-anio i, en escala 0-1.
#   pt    = proporcion del indicador en la provincia y el anio del canton, con los factores de expansion de la ENEMDU.
#   n_i   = registros del denominador del indicador (personas, u hogares en monoparentalidad).
#   pt * (1 - pt) / n_i = varianza de una proporcion en un muestreo aleatorio simple de n_i registros.
#   Deff  = efecto de diseno: cuantas veces es mayor la varianza por el muestreo en conglomerados de la ENEMDU. Uno por indicador.
#
# Efecto de diseno del indicador, con las areas ancla:
#   Deff = suma(V_directa_i) / suma(pt * (1 - pt) / n_i)
# Area ancla: canton-anio con gl >= 9 (INEC, 2025, p. 22), sin estratos con una sola UPM y con varianza directa mayor que cero.
#   Las areas con varianza directa cero no se usan para calcular el Deff, pero reciben su varianza suavizada y se mantienen en el modelo:
#   - MDSF-CEPAL (2021, p. 41): las comunas con tasa y varianza nulas no entraron al ajuste de su GVF, "pero si se obtuvieron las predicciones de sus varianzas".
#     Alli el motivo es el logaritmo, que aqui no se usa.
#   - Franco (2020, pp. 4 y 20): con tasas no hace falta retirar los ceros, y una varianza cero les da un peso indebido.
#   - Corral et al. (2022, p. 19): la varianza cero aparece en areas con poca muestra y debe obtenerse con otro metodo.
#
# Se usa la proporcion provincial y no la del canton para que la varianza no sea cero cuando la estimacion directa es 0 %.
#
# Fuente: You (2008, p. 2) escribe la varianza de una proporcion como theta_i * (1 - theta_i) * deff_i / n_i, con efecto de diseno suavizado.
#   Son decisiones propias, que no estan en esa fuente: usar la proporcion provincial en lugar de theta_i, un solo Deff por indicador calculado con las areas ancla, y usar gl >= 9 y estratos con mas de una UPM para elegirlas.
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
  d$vardir_gvf <- deff * d$varianza_aleatoria
  d$vardir <- d$vardir_gvf
  stopifnot(all(is.finite(d$vardir)), all(d$vardir > 0))
  diagnostico <- tibble(indicador = first(d$indicador), estimaciones_con_valor = nrow(d),
                        areas_ancla = nrow(anclas), deff = deff,
                        error_directo_mediana_anclas_pp = median(anclas$error_estandar_pp),
                        error_gvf_mediana_pp = median(100 * sqrt(d$vardir_gvf)))
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
#   Etapa 1, enlace:    theta_i = b0 + b1*x1_i + ... + bk*xk_i + v_i,   v_i ~ N(0, s2_v)
#   Etapa 2, muestreo:  y_i = theta_i + e_i,                e_i ~ N(0, V_i)
#
# Donde:
#   theta_i  = valor verdadero del indicador, que se quiere estimar.
#   x1...xk  = variables auxiliares del indicador.
#   b0...bk  = coeficientes: cada variable se multiplica por su coeficiente y los productos se suman.
#   v_i      = efecto propio del canton-anio que las variables no explican; su varianza s2_v mide la diferencia entre cantones que queda sin explicar.
#   y_i      = estimacion directa de la ENEMDU para el canton-anio i.
#   e_i      = error de muestreo de la encuesta; su varianza V_i es la suavizada (GVF) y se trata como conocida.
#
# Estimacion final (EBLUP):
#   estimacion_i = g_i * y_i + (1 - g_i) * (b0 + b1*x1_i + ... + bk*xk_i)
#   g_i = s2_v / (s2_v + V_i)
# La estimacion final es un promedio ponderado entre el dato directo y el valor sintetico (lo que predicen las variables).
# El peso g_i del dato directo es alto cuando su varianza V_i es baja (mucha muestra) y bajo cuando es alta (poca muestra).
#
# Estimacion de b y s2_v por maxima verosimilitud restringida (REML), que corrige el sesgo de la varianza del efecto de area; si no converge, metodo de momentos de Fay y Herriot.
# El error de cada estimacion es la raiz del error cuadratico medio (ECM) de Prasad y Rao, que calcula sae::mseFH (Molina y Marhuenda, 2015).
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
#   b. Ajuste: parte de la variacion entre areas que explican las auxiliares, 1 - s2_v(con auxiliares) / s2_v(sin auxiliares), y aporte de cada una.
#   c. Colinealidad: factor de inflacion de la varianza (VIF), a revisar si es mayor que 5 (Corral et al., 2022, p. 21).
#   g. Precision: error del Fay-Herriot (raiz del ECM) frente al error del dato directo que entra al modelo (GVF).
#   i. Dispersion de residuos: residuo estandarizado contra el valor sintetico; los puntos deben repartirse alrededor de cero y casi todos dentro de +/- 1,96.
#     Su desviacion estandar debe estar cerca de 1.
#   j. Variacion que las variables no explican, sigma_u = raiz(s2_v), y peso del dato directo (minimo, mediana y maximo).
#   k. Concordancia con la provincia: razon R entre el dato directo de la provincia y el promedio Fay-Herriot de sus cantones.
#   Residuo estandarizado del area i = (y_i - sintetico_i) / raiz(s2_v + V_i).
#   Efecto de area estimado        = estimacion_i - sintetico_i.
#   Peso del dato directo          = s2_v / (s2_v + V_i).


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
  vardir <- d$vardir
  X <- as.matrix(d[variables])
  ajuste <- NULL
  for (metodo in c("REML", "FH")) {
    intento <- tryCatch(sae::mseFH(prop ~ X, vardir = vardir, method = metodo),
                        error = \(e) NULL)
    if (!is.null(intento) && isTRUE(intento$est$fit$convergence)) { ajuste <- intento; break }
  }
  if (is.null(ajuste)) stop("No convergio Fay-Herriot: ", indicador_fh)
  # Modelo sin variables auxiliares, solo para medir el ajuste
  nulo <- sae::mseFH(prop ~ 1, vardir = vardir, method = metodo)

  beta <- ajuste$est$fit$estcoef$beta
  s2v <- as.numeric(unlist(ajuste$est$fit$refvar))[1]
  s2v_nulo <- as.numeric(unlist(nulo$est$fit$refvar))[1]
  sintetico <- as.numeric(cbind(1, X) %*% beta)
  eblup <- as.numeric(unlist(ajuste$est$eblup))
  error_fh <- sqrt(pmax(as.numeric(unlist(ajuste$mse)), 0))
  stopifnot(length(eblup) == nrow(d), length(error_fh) == nrow(d), is.finite(s2v))

  estimaciones <- d %>%
    transmute(anio, codigo_canton, indicador,
              error_gvf_pp = 100 * sqrt(vardir_gvf),
              error_modelo_pp = 100 * sqrt(vardir),
              valor_sintetico_pct = 100 * sintetico,
              peso_dato_original = s2v / (s2v + vardir),
              valor_fh_pct = 100 * eblup,
              error_fh_pp = 100 * error_fh,
              limite_inferior_fh_pct = valor_fh_pct - 1.96 * error_fh_pp,
              limite_superior_fh_pct = valor_fh_pct + 1.96 * error_fh_pp,
              fuera_de_rango = as.integer(eblup < 0 | eblup > 1),
              tipo_estimacion = "fay_herriot")

  # Canton-anio sin estimacion directa (sin muestra): reciben solo el valor sintetico, porque sin encuesta el peso del dato directo es cero.
  # Error = raiz(s2_v + g2), donde g2 es el error por estimar b:
  #   g2_i = x_i' (sum_j x_j x_j' / (s2_v + V_j))^(-1) x_i
  faltan <- estimadores %>%
    filter(indicador == indicador_fh, !is.finite(valor_pct)) %>%
    select(anio, codigo_canton, indicador) %>%
    left_join(auxiliares, by = c("anio", "codigo_canton"))
  stopifnot(!anyNA(faltan[variables]))
  if (nrow(faltan) > 0) {
    X1 <- cbind(1, X)
    Q <- solve(crossprod(X1, X1 / (s2v + vardir)))
    Xf <- cbind(1, as.matrix(faltan[variables]))
    sintetico_f <- as.numeric(Xf %*% beta)
    error_f <- sqrt(s2v + rowSums((Xf %*% Q) * Xf))
    estimaciones <- bind_rows(estimaciones, faltan %>%
      transmute(anio, codigo_canton, indicador,
                valor_sintetico_pct = 100 * sintetico_f,
                peso_dato_original = 0,
                valor_fh_pct = 100 * sintetico_f,
                error_fh_pp = 100 * error_f,
                limite_inferior_fh_pct = valor_fh_pct - 1.96 * error_fh_pp,
                limite_superior_fh_pct = valor_fh_pct + 1.96 * error_fh_pp,
                fuera_de_rango = as.integer(sintetico_f < 0 | sintetico_f > 1),
                tipo_estimacion = "solo_sintetico"))
  }
  con_muestra <- estimaciones$tipo_estimacion == "fay_herriot"

  coeficientes <- tibble(indicador = indicador_fh,
                         termino = c("(constante)", variables),
                         coeficiente = beta,
                         error_estandar = ajuste$est$fit$estcoef$std.error,
                         t = ajuste$est$fit$estcoef$tvalue,
                         p_valor = ajuste$est$fit$estcoef$pvalue,
                         signo_esperado = c(NA, signo_esperado)) %>%
    mutate(signo_observado = sign(coeficiente),
           signo_coincide = signo_observado == signo_esperado)

  residuo <- (prop - sintetico) / sqrt(s2v + vardir)
  efecto <- eblup - sintetico
  cook <- cooks.distance(lm(prop ~ X, weights = 1 / (s2v + vardir)))
  diagnosticos <- d %>%
    transmute(anio, codigo_canton, canton, indicador,
              residuo_estandarizado = residuo, efecto_area = efecto,
              distancia_cook = unname(cook), influyente = distancia_cook > 4 / nrow(d))

  vif <- if (ncol(X) < 2) NA_real_ else
    max(map_dbl(seq_len(ncol(X)), \(j) 1 / (1 - summary(lm(X[, j] ~ X[, -j]))$r.squared)))
  error_directo <- sqrt(vardir)
  cv_directo <- if_else(prop > 0, 100 * error_directo / prop, NA_real_)
  cv_fh <- if_else(eblup > 0, 100 * error_fh / eblup, NA_real_)
  validacion <- tibble(
    indicador = indicador_fh,
    variables = paste(variables, collapse = " + "),
    metodo = metodo,
    areas = nrow(d),
    directos_en_cero = sum(prop == 0),
    explica_pct = 100 * (1 - s2v / s2v_nulo),
    vif_maximo = vif,
    shapiro_p_residuos = shapiro.test(residuo)$p.value,
    shapiro_p_efectos = shapiro.test(efecto)$p.value,
    areas_influyentes = sum(diagnosticos$influyente),
    pct_areas_influyentes = 100 * mean(diagnosticos$influyente),
    areas_solo_sintetico = sum(!con_muestra),
    fuera_de_rango = sum(estimaciones$fuera_de_rango),
    peso_directo_mediana = median(estimaciones$peso_dato_original[con_muestra]),
    error_directo_mediana_pp = median(100 * error_directo),
    error_fh_mediana_pp = median(100 * error_fh),
    areas_con_error_menor = sum(error_fh < error_directo),
    cv_directo_mediana = median(cv_directo, na.rm = TRUE),
    cv_fh_mediana = median(cv_fh, na.rm = TRUE),
    areas_con_cv_menor = sum(cv_fh < cv_directo, na.rm = TRUE),
    areas_con_cv_directo = sum(!is.na(cv_directo)),
    error_solo_sintetico_mediana_pp = if (any(!con_muestra))
      median(estimaciones$error_fh_pp[!con_muestra]) else NA_real_)

  # Datos para el grafico de linealidad: residuo contra cada variable
  linealidad <- d %>%
    transmute(indicador, residuo_estandarizado = residuo, across(all_of(variables))) %>%
    pivot_longer(all_of(variables), names_to = "variable", values_to = "valor_variable")

  list(estimaciones = estimaciones, coeficientes = coeficientes, validacion = validacion,
       diagnosticos = diagnosticos, linealidad = linealidad)
}

# Imprime en consola los resultados de un indicador.
mostrar_resultados <- function(ajuste, titulo) {
  mostrar_etapa(paste(titulo, "- coeficientes (escala 0-1)"),
                ajuste$coeficientes %>% mutate(across(where(is.double), \(x) signif(x, 4))))
  mostrar_etapa(paste(titulo, "- validacion"),
                ajuste$validacion %>%
                  mutate(across(where(is.double), \(x) round(x, 3))) %>%
                  pivot_longer(-c(indicador, variables, metodo), names_to = "revision",
                               values_to = "valor", values_transform = as.character) %>%
                  select(revision, valor))
  invisible(NULL)
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
    transmute(indicador, variable = termino, coeficiente, error_estandar, p_valor,
              significativa = if_else(p_valor < 0.05, "si", "no"),
              signo_esperado, signo_observado)
}

# Muestra la tabla de significancia y los atributos del modelo.
mostrar_candidatas <- function(ajuste, titulo) {
  mostrar_etapa(paste(titulo, "- significancia de las variables candidatas"),
                tabla_significancia(ajuste) %>% select(-indicador) %>%
                  mutate(across(c(coeficiente, error_estandar, p_valor), \(x) signif(x, 4))))
  mostrar_etapa(paste(titulo, "- atributos del modelo con las candidatas"),
                ajuste$validacion %>%
                  transmute(areas, explica_pct = round(explica_pct, 2),
                            vif_maximo = round(vif_maximo, 2),
                            error_fh_mediana_pp = round(error_fh_mediana_pp, 3),
                            shapiro_p_residuos = signif(shapiro_p_residuos, 3),
                            areas_influyentes))
  invisible(NULL)
}

## 4.1 Inasistencia de 12 a 17 anios ----

#   theta_i = b0 + b1*abandono_escolar_i + b2*bdh_general_i + b3*poblacion_urbana_i + b4*empleo_formal_i + v_i
# Se mantienen abandono_escolar y bdh_general, significativas y con el signo esperado.
# poblacion_urbana y empleo_formal no son significativas y se retiran.
cand_inasistencia <- ajustar_fay_herriot(
  "inasistencia_12_17",
  c("abandono_escolar", "bdh_general", "poblacion_urbana", "empleo_formal"),
  c(1, 1, NA, NA))
mostrar_candidatas(cand_inasistencia, "4.1 Inasistencia 12-17")

## 4.2 Desempleo ----

#   theta_i = b0 + b1*empleo_formal_i + b2*poblacion_urbana_i + v_i
# Se mantiene poblacion_urbana, significativa y con el signo esperado.
# empleo_formal: se esperaba negativo y salio positivo, pero no es significativa y se retira.
cand_desempleo <- ajustar_fay_herriot(
  "desempleo_general",
  c("empleo_formal", "poblacion_urbana"),
  c(-1, 1))
mostrar_candidatas(cand_desempleo, "4.2 Desempleo")

## 4.3 Pobreza por ingresos ----

#   theta_i = b0 + b1*bdh_general_i + b2*salario_basico_i + b3*matricula_particular_i + b4*poblacion_urbana_i + v_i
# Se mantiene bdh_general, significativa y con el signo esperado.
# salario_basico: se esperaba positivo y salio negativo, pero es significativa, por lo que pasa al modelo con las seleccionadas.
# matricula_particular y poblacion_urbana no son significativas y se retiran.
cand_pobreza <- ajustar_fay_herriot(
  "pobreza_ingresos",
  c("bdh_general", "salario_basico", "matricula_particular", "poblacion_urbana"),
  c(1, 1, -1, NA))
mostrar_candidatas(cand_pobreza, "4.3 Pobreza por ingresos")

## 4.4 NBI adolescente ----

#   theta_i = b0 + b1*bdh_variable_i + b2*abandono_escolar_i + b3*poblacion_urbana_i + v_i
# Se mantiene bdh_variable, significativa y con el signo esperado.
# abandono_escolar: se esperaba positivo y salio negativo, pero no es significativa y se retira, igual que poblacion_urbana.
cand_nbi <- ajustar_fay_herriot(
  "adolescentes_nbi_sobre_total_provisional",
  c("bdh_variable", "abandono_escolar", "poblacion_urbana"),
  c(1, 1, NA))
mostrar_candidatas(cand_nbi, "4.4 NBI adolescente")

## 4.5 Hogares monoparentales con menores ----

#   theta_i = b0 + b1*nacidos_sin_pareja_i + b2*poblacion_urbana_i + v_i
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

# En las formulas, i = canton-anio.
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
#   theta_i = b0 + b1 * abandono_escolar_i + b2 * bdh_general_i + v_i
# Donde:
#   theta_i               = porcentaje verdadero de adolescentes de 12 a 17 anios que no asisten a educacion.
#   b0                    = constante del modelo.
#   b1 * abandono_escolar = aporte de la tasa de abandono escolar de 8.o de EGB a 3.o de Bachillerato (%), multiplicada por su coeficiente.
#   b2 * bdh_general      = aporte de los hogares con BDH por cada 100 habitantes, multiplicados por su coeficiente.
#   v_i                   = efecto propio del canton-anio.
# Estimacion final:
#   estimacion_i = g_i * directo_i + (1 - g_i) * (b0 + b1*abandono_i + b2*bdh_i)
# Por que se relacionan:
#   abandono_escolar: quien abandona el grado sale del sistema educativo (Arias et al., 2022, p. 52), por lo que con mas abandono se espera mas adolescentes que no asisten.
#   bdh_general: el bono se dirige a nucleos familiares en pobreza o extrema pobreza (INEC, 2026b, p. 131) y las mayores brechas para terminar la secundaria se dan entre niveles socioeconomicos (Trucco et al., 2026, p. 14), por lo que con mas hogares con bono se espera mas inasistencia.
fh_inasistencia <- ajustar_fay_herriot("inasistencia_12_17",
                                       c("abandono_escolar", "bdh_general"), c(1, 1))
comprobar_seleccion(fh_inasistencia, cand_inasistencia)
mostrar_resultados(fh_inasistencia, "5.1 Inasistencia 12-17")

## 5.2 Desempleo ----

# Enlace:
#   theta_i = b0 + b1 * poblacion_urbana_i + v_i
# Donde:
#   theta_i               = tasa de desempleo verdadera (% de la PEA).
#   b0                    = constante del modelo.
#   b1 * poblacion_urbana = aporte del porcentaje de poblacion que vive en el area urbana, multiplicado por su coeficiente.
#   v_i                   = efecto propio del canton-anio.
# Estimacion final:
#   estimacion_i = g_i * directo_i + (1 - g_i) * (b0 + b1*urbana_i)
# Por que se relacionan:
#   poblacion_urbana: en diciembre de 2025 el desempleo fue de 3,2 % en el area urbana y de 1,5 % en la rural (INEC, 2026f), por lo que con mas poblacion urbana se espera mas desempleo.
fh_desempleo <- ajustar_fay_herriot("desempleo_general", "poblacion_urbana", 1)
comprobar_seleccion(fh_desempleo, cand_desempleo)
mostrar_resultados(fh_desempleo, "5.2 Desempleo")

## 5.3 Pobreza por ingresos ----

# Enlace:
#   theta_i = b0 + b1 * bdh_general_i + b2 * salario_basico_i + v_i
# Donde:
#   theta_i             = tasa de pobreza por ingresos verdadera (% de personas).
#   b0                  = constante del modelo.
#   b1 * bdh_general    = aporte de los hogares con BDH por cada 100 habitantes, multiplicados por su coeficiente.
#   b2 * salario_basico = aporte del porcentaje de trabajadores formales cuyo sueldo a 30 dias es menor o igual al salario basico, multiplicado por su coeficiente.
#   v_i                 = efecto propio del canton-anio.
# Estimacion final:
#   estimacion_i = g_i * directo_i + (1 - g_i) * (b0 + b1*bdh_i + b2*salario_i)
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
mostrar_resultados(fh_pobreza, "5.3 Pobreza por ingresos")

## 5.4 NBI adolescente ----

# Enlace:
#   theta_i = b0 + b1 * bdh_variable_i + v_i
# Donde:
#   theta_i           = porcentaje verdadero de adolescentes de 12 a 17 anios en hogares pobres por NBI, sobre la poblacion total.
#   b0                = constante del modelo.
#   b1 * bdh_variable = aporte de los hogares con BDH con componente variable por cada 100 habitantes, multiplicados por su coeficiente.
#   v_i               = efecto propio del canton-anio.
# Estimacion final:
#   estimacion_i = g_i * directo_i + (1 - g_i) * (b0 + b1*bdh_variable_i)
# Por que se relacionan:
#   bdh_variable: este bono se dirige a nucleos familiares en extrema pobreza con hijos menores de 18 anios (INEC, 2026b, p. 132), por lo que con mas hogares con este bono se espera mas adolescentes en hogares con NBI.
# El modelo admite una o varias variables (Corral et al., 2022, p. 27).
fh_nbi <- ajustar_fay_herriot("adolescentes_nbi_sobre_total_provisional",
                              "bdh_variable", 1)
comprobar_seleccion(fh_nbi, cand_nbi)
mostrar_resultados(fh_nbi, "5.4 NBI adolescente")

## 5.5 Hogares monoparentales con menores ----

# Enlace:
#   theta_i = b0 + b1 * nacidos_sin_pareja_i + b2 * poblacion_urbana_i + v_i
# Donde:
#   theta_i                 = porcentaje verdadero de hogares monoparentales con menores de 18 anios.
#   b0                      = constante del modelo.
#   b1 * nacidos_sin_pareja = aporte del porcentaje de nacidos vivos de madres solteras, divorciadas, separadas o viudas, multiplicado por su coeficiente.
#   b2 * poblacion_urbana   = aporte del porcentaje de poblacion urbana, multiplicado por su coeficiente.
#   v_i                     = efecto propio del canton-anio.
# Estimacion final:
#   estimacion_i = g_i * directo_i + (1 - g_i) * (b0 + b1*nacidos_i + b2*urbana_i)
# Por que se relacionan:
#   nacidos_sin_pareja: recoge el estado civil o conyugal de la madre (INEC, 2025b), por lo que con mas nacidos de madres sin pareja se espera mas hogares con un solo jefe e hijos menores.
#   poblacion_urbana: las familias monoparentales pueden resultar de la ruptura de uniones, dentro de cambios familiares que se asocian con la urbanizacion (Ullmann, Maldonado y Rico, 2014, pp. 26 y 30), por lo que con mas poblacion urbana se espera mas monoparentalidad.
fh_monoparentalidad <- ajustar_fay_herriot("monoparental_jefatura_aproximacion",
                                           c("nacidos_sin_pareja", "poblacion_urbana"), c(1, 1))
comprobar_seleccion(fh_monoparentalidad, cand_monoparentalidad)
mostrar_resultados(fh_monoparentalidad, "5.5 Monoparentalidad")

## 5.6 Comparacion del modelo con las candidatas y con las seleccionadas ----

#   explica_pct = parte de la diferencia entre areas que explican las variables
#   cambio_mediano_pp y cambio_maximo_pp = cuanto cambia la estimacion de los canton-anio con muestra entre un modelo y otro
comparar_modelos <- function(candidatas, final) {
  atributos <- function(a, modelo) a$validacion %>%
    transmute(indicador, modelo, variables, explica_pct, vif_maximo, error_fh_mediana_pp,
              shapiro_p_residuos, areas_influyentes)
  cambio <- inner_join(
    candidatas$estimaciones %>% filter(tipo_estimacion == "fay_herriot") %>%
      select(anio, codigo_canton, e1 = valor_fh_pct),
    final$estimaciones %>% filter(tipo_estimacion == "fay_herriot") %>%
      select(anio, codigo_canton, e2 = valor_fh_pct), by = c("anio", "codigo_canton"))
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
                select(indicador, explica_pct, fuera_de_rango, shapiro_p_residuos,
                       pct_areas_influyentes, error_directo_mediana_pp, error_fh_mediana_pp,
                       areas_con_error_menor, areas,
                       areas_solo_sintetico, error_solo_sintetico_mediana_pp) %>%
                mutate(across(where(is.double), \(x) round(x, 3))))
mostrar_etapa("6.1 Coeficientes no significativos (p >= 0,05) o con signo distinto al esperado",
              coeficientes_fh %>%
                filter(termino != "(constante)", p_valor >= 0.05 | !signo_coincide) %>%
                select(indicador, termino, coeficiente, p_valor, signo_esperado, signo_observado))

## 6.2 Estimadores del Fay-Herriot (formato largo) ----

# Misma estructura que estimaciones_originales.xlsx: una fila por indicador, canton y anio.
# Los canton-anio sin estimacion directa reciben el valor
# sintetico (tipo_estimacion = "solo_sintetico").
estimadores_fh <- estimadores %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
         muestra_denominador) %>%
  left_join(estimaciones_fh, by = c("anio", "codigo_canton", "indicador")) %>%
  transmute(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
            valor_pct = valor_fh_pct,
            ecm_pp2 = error_fh_pp^2,
            error_estandar_pp = error_fh_pp,
            limite_inferior_pct = limite_inferior_fh_pct,
            limite_superior_pct = limite_superior_fh_pct,
            cv_pct = if_else(valor_fh_pct > 0, 100 * error_fh_pp / valor_fh_pct, NA_real_),
            muestra_denominador, peso_dato_original, valor_sintetico_pct, fuera_de_rango,
            tipo_estimacion) %>%
  arrange(anio, codigo_provincia, codigo_canton, indicador)

## 6.3 Indicadores del Fay-Herriot (formato ancho) ----

# Misma estructura que construccion_indicadores_factores.xlsx: una fila por canton-anio y una columna por indicador.
indicadores_fh <- indicadores_agregados %>%
  select(all_of(claves_territorio)) %>%
  left_join(estimadores_fh %>%
              mutate(columna = unname(nombres_indicadores[indicador])) %>%
              select(anio, codigo_canton, columna, valor_pct) %>%
              pivot_wider(names_from = columna, values_from = valor_pct),
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
            valor_original_pct = valor_pct,
            error_original_pp = error_estandar_pp,
            cv_original_pct = cv_pct,
            limite_inferior_original_pct = limite_inferior_pct,
            limite_superior_original_pct = limite_superior_pct,
            grados_libertad, error_maximo_tolerable_pp,
            cumple_muestra, cumple_gl, cumple_error,
            # Varianza suavizada (GVF)
            error_gvf_pp,
            cv_gvf_pct = if_else(valor_pct > 0, 100 * error_gvf_pp / valor_pct, NA_real_),
            # Error que entra al modelo: el de la GVF
            error_modelo_pp,
            # Fay-Herriot
            valor_sintetico_pct, peso_dato_original,
            valor_fh_pct,
            ecm_fh_pp2 = error_fh_pp^2,
            error_fh_pp,
            cv_fh_pct = if_else(valor_fh_pct > 0, 100 * error_fh_pp / valor_fh_pct, NA_real_),
            limite_inferior_fh_pct, limite_superior_fh_pct,
            # Comparacion
            cambio_pp = valor_fh_pct - valor_pct,
            error_menor_fh = as.integer(error_fh_pp < error_modelo_pp),
            fuera_de_rango, tipo_estimacion) %>%
  arrange(anio, codigo_provincia, codigo_canton, indicador)
stopifnot(!anyNA(comparacion$poblacion_area))

mostrar_etapa("6.2-6.4 Tablas de salida", tibble(
  tabla = c("estimadores_fh (largo)", "indicadores_fh (ancho)", "comparacion (largo)"),
  filas = c(nrow(estimadores_fh), nrow(indicadores_fh), nrow(comparacion)),
  con_estimacion_fh = c(sum(is.finite(estimadores_fh$valor_pct)),
                        sum(is.finite(as.matrix(select(indicadores_fh, all_of(columnas_indicadores))))),
                        sum(is.finite(comparacion$valor_fh_pct)))))

## 6.5 Comparacion con la provincia (validacion, no corrige) ----

# La ENEMDU da un dato mas confiable para la provincia que para el canton.
# Se compara, por indicador, provincia y anio, el dato directo de la provincia con el promedio de las estimaciones Fay-Herriot de sus cantones:
#
#   R = directo_provincia / promedio_FH
#   directo_provincia = suma(w_i * directo_i) / suma(w_i)
#   promedio_FH       = suma(w_i * FH_i)      / suma(w_i)
#
# w_i es la suma de los factores de expansion del denominador del canton.
# Los dos promedios usan los mismos cantones, los que tienen muestra.
# Si R esta cerca de 1, el modelo concuerda con la provincia (MDSF, 2019, p. 11).
# La razon R es la del ajuste de MDSF-CEPAL (2021, p. 53), que la calcula con poblaciones.
# Aqui R solo se reporta: no se multiplica ninguna estimacion, porque el ajuste no se recomienda salvo que la publicacion lo exija, y las diferencias grandes pueden indicar que no se cumplen los supuestos (Corral et al., 2022, p. 80, en el capitulo de modelos a nivel de unidad).
# El dato de la provincia tambien tiene error de muestreo.
comparacion_provincia <- estimadores %>%
  filter(is.finite(valor_pct), denominador_expandido > 0) %>%
  inner_join(select(estimaciones_fh, anio, codigo_canton, indicador, valor_fh_pct),
             by = c("anio", "codigo_canton", "indicador")) %>%
  group_by(indicador, codigo_provincia, provincia, anio) %>%
  summarise(cantones_con_muestra = n(),
            directo_provincia_pct = sum(denominador_expandido * valor_pct) / sum(denominador_expandido),
            promedio_fh_pct = sum(denominador_expandido * valor_fh_pct) / sum(denominador_expandido),
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
#   indicadores_fay_herriot.xlsx               indicadores del Fay-Herriot (ancho)
#   estimaciones_fay_herriot.xlsx              estimadores del Fay-Herriot (largo)
#   comparacion_original_gvf_fay_herriot.xlsx  original, GVF y Fay-Herriot (largo)
#   validacion_fay_herriot.xlsx                resumen, coeficientes, GVF, diagnosticos por area y comparacion con la provincia
# Imagenes en SCRIPTS/SALIDAS_EXPLORACIONES/salidas_estimacion_areas_pequenas:
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
  writexl::write_xlsx(list(Resumen = validacion_fh,
                           Coeficientes = coeficientes_fh,
                           Varianza_GVF = diagnostico_varianza,
                           Diagnosticos_area = diagnosticos_fh,
                           Provincia_resumen = resumen_provincia,
                           Provincia_detalle = comparacion_provincia),
                      file.path(carpeta_salida, "validacion_fay_herriot.xlsx"))
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
    left_join(select(estimaciones_fh, anio, codigo_canton, indicador, valor_sintetico_pct),
              by = c("anio", "codigo_canton", "indicador")) %>%
    mutate(indicador = unname(c(
      pobreza_ingresos = "Pobreza", adolescentes_nbi_sobre_total_provisional = "NBI adolescente",
      monoparental_jefatura_aproximacion = "Monoparentalidad",
      inasistencia_12_17 = "Inasistencia", desempleo_general = "Desempleo")[indicador])) %>%
    ggplot(aes(valor_sintetico_pct, residuo_estandarizado)) +
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
  grafico_directo_fh <- ggplot(datos_figura, aes(valor_original_pct, valor_fh_pct)) +
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
  grafico_error_fh <- ggplot(datos_figura, aes(error_modelo_pp, error_fh_pp)) +
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
                "comparacion_original_gvf_fay_herriot.xlsx", "validacion_fay_herriot.xlsx",
                "validacion_linealidad_fay_herriot.png",
                "validacion_dispersion_residuos_fay_herriot.png",
                "comparacion_directo_fay_herriot.png", "comparacion_provincia_fay_herriot.png",
                "comparacion_error_fay_herriot.png"),
    filas = c(nrow(indicadores_fh), nrow(estimadores_fh), nrow(comparacion),
              nrow(validacion_fh), NA, NA, NA, NA, NA)))
}

## 6.7 Tablas de resultados ----

# Resultados de los cinco indicadores juntos.
# Aporte de cada variable: parte de la diferencia entre areas que explica.
# Con una variable es todo lo que explica el modelo.
# Con dos, es el promedio entre lo que explica sola y lo que agrega cuando ya esta la otra:
#   aporte_1 = (explica(1) + explica(1 y 2) - explica(2)) / 2
# Los aportes suman lo que explica el modelo.
nombre_corto <- c(inasistencia_12_17 = "Inasistencia", desempleo_general = "Desempleo",
                  pobreza_ingresos = "Pobreza", adolescentes_nbi_sobre_total_provisional = "NBI",
                  monoparental_jefatura_aproximacion = "Monoparentalidad")
corto <- function(d) mutate(d, indicador = unname(nombre_corto[indicador]))
numero <- function(x, k = 4) formatC(x, digits = k, format = "f")
p_texto <- function(p) if_else(p < 0.0001, "<0.0001", formatC(p, digits = 4, format = "f"))

mostrar_etapa("6.7 Efecto de diseno por indicador", diagnostico_varianza %>% corto() %>%
  transmute(indicador, estimaciones = estimaciones_con_valor, areas_ancla, deff = numero(deff, 2),
            error_original_anclas = numero(error_directo_mediana_anclas_pp, 2),
            error_gvf_todas = numero(error_gvf_mediana_pp, 2)))
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
  tibble(indicador = ind, variable = vars, coeficiente = co$coeficiente,
         p_valor = co$p_valor, aporte_pct = aporte, total_pct = total,
         vif = final$validacion$vif_maximo)
}
aporte_variables <- modelos_seleccionados %>% map(calcular_aporte) %>% list_rbind()
mostrar_etapa("6.7 Variables de cada modelo", aporte_variables %>% corto() %>%
  transmute(indicador, variable, coeficiente = numero(coeficiente, 5), p_valor = p_texto(p_valor),
            aporte_pct = numero(aporte_pct, 1), total_pct = numero(total_pct, 1),
            vif = if_else(is.na(vif), "-", numero(vif, 2))))

# sigma_u = raiz de s2_v, la variacion entre areas que las variables no explican.
# Se obtiene del peso: s2_v = g * V / (1 - g).
atributos_fh <- estimaciones_fh %>%
  filter(tipo_estimacion == "fay_herriot") %>%
  group_by(indicador) %>%
  summarise(areas = as.character(n()),
            sigma_u_pp = numero(median(error_modelo_pp * sqrt(peso_dato_original / (1 - peso_dato_original))), 2),
            peso_minimo = numero(min(peso_dato_original), 3),
            peso_mediano = numero(median(peso_dato_original), 2),
            peso_maximo = numero(max(peso_dato_original), 2),
            error_gvf_mediano_pp = numero(median(error_gvf_pp), 2),
            error_fh_mediano_pp = numero(median(error_fh_pp), 2)) %>%
  left_join(diagnosticos_fh %>% group_by(indicador) %>%
              summarise(desviacion_residuo = numero(sd(residuo_estandarizado), 2)), by = "indicador") %>%
  relocate(desviacion_residuo, .after = peso_maximo)
mostrar_etapa("6.7 Atributos del Fay-Herriot", atributos_fh %>%
  corto() %>%
  pivot_longer(-indicador, names_to = "atributo") %>%
  pivot_wider(names_from = indicador, values_from = value) %>%
  select(atributo, Inasistencia, Desempleo, Pobreza, NBI, Monoparentalidad))
mostrar_etapa("6.7 Concordancia con la provincia", resumen_provincia %>% corto() %>%
  transmute(indicador, casos = provincia_anio, R_minimo = numero(R_minimo, 2), R_mediana = numero(R_mediana, 2),
            R_maximo = numero(R_maximo, 2), diferencia_mediana_pp = numero(diferencia_abs_mediana_pp, 2),
            mas_de_10 = se_alejan_10, mas_de_20 = se_alejan_20))
