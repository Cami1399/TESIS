# ============================================================================
# PANEL CANTONAL 2021-2025: MUESTRA GENERAL Y PANEL DESBALANCEADO
# ============================================================================
# Autor del paquete: yerovi84/enemduR. Version de referencia: v0.1.0-alpha.2.
# Las funciones sin prefijo enemdu_ que aparecen abajo son adaptaciones propias.
# Este script construye indicadores; ejecutar Data_Maestra.R despues.
# Si enemduR no esta instalado, necesita el repositorio local o acceso a GitHub.
# Resultados exploratorios: no se afirma validacion oficial del INEC.
#
# USO EN R/RSTUDIO:
# Abra este archivo en su proyecto de RStudio y ejecute Source.
# Para cambiar anios: definir anios_panel <- 2025 antes de source().
# Para instalar/cargar y definir funciones sin calcular: solo_preparar <- TRUE.
# Para volver a calcular despues: solo_preparar <- FALSE; ejecutar source().
#
# Retirar solamente los objetos de avisos de ejecuciones anteriores.
rm(list = intersect(
  c("advertencias_R", "aviso", "avisos_por_anio", "avisos_actuales"),
  ls(envir = environment(), all.names = TRUE)),
  envir = environment())

# Salidas de seguimiento: solo consola; no altera ni filtra datos.
mostrar_etapa <- function(titulo, tabla = NULL) {
  cat("\n", titulo, "\n", sep = "")
  if (!is.null(tabla)) print(as.data.frame(tabla), row.names = FALSE)
  invisible(NULL)
}
resumir_indicadores_consola <- function(tabla, columnas) {
  data.frame(indicador = columnas, filas = nrow(tabla),
             disponibles = vapply(tabla[, columnas, drop = FALSE], function(x) sum(is.finite(x)), integer(1)),
             faltantes_o_no_finitos = vapply(tabla[, columnas, drop = FALSE], function(x) sum(!is.finite(x)), integer(1)),
             ceros = vapply(tabla[, columnas, drop = FALSE], function(x) sum(is.finite(x) & x == 0), integer(1)))
}
resumir_filas_consola <- function(tabla) {
  anios <- sort(unique(tabla$anio))
  do.call(rbind, lapply(anios, function(a) {
    x <- tabla[tabla$anio == a, , drop = FALSE]
    data.frame(anio = a, filas = nrow(x), cantones = length(unique(x$codigo_canton)),
               provincias = length(unique(x$codigo_provincia)))
  }))
}

# 1. RUTAS DE TESIS Y BIBLIOTECA PERSONAL DE R ---------------------------------
# No cambia la carpeta de trabajo del proyecto abierto en RStudio.
# Las bases originales de TESIS_CODE se abren en lectura y no se modifican.
raiz_tesis <- "C:/Users/camil/OneDrive/Desktop/TESIS"
if (!dir.exists(raiz_tesis)) stop("No existe la carpeta TESIS; revise raiz_tesis.")
mostrar_etapa("1. Rutas del procesamiento", data.frame(
  uso = c("Microdatos", "Resultados"),
  carpeta = file.path(raiz_tesis, c("DATAS_ORIGEN", "DATAS_PROCESADAS"))))
version_biblioteca <- paste(R.version$major,
                            strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1], sep = ".")
biblioteca_usuario <- file.path(Sys.getenv("LOCALAPPDATA"), "R", "win-library", version_biblioteca)
biblioteca_usuario_real <- file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library", version_biblioteca)
biblioteca_local <- biblioteca_usuario_real
dir.create(biblioteca_local, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(biblioteca_usuario_real, biblioteca_usuario, .libPaths()))
if (getRversion() < "4.2.0") stop("enemduR requiere R >= 4.2.0.")

# 2. INSTALAR CADA DEPENDENCIA SI FALTA ----------------------------------------
# No se reinstalan librerias ya disponibles y no se agregan dependencias al
# DESCRIPTION del paquete. La primera descarga requiere internet.
# glue: mensajes parametrizados del paquete.
# haven: lectura SPSS/Stata utilizada internamente por enemduR.
# janitor: normalizacion de nombres utilizada por enemduR.
# readr: lectura de CSV y conversion numerica con coma decimal.
# rlang: manejo de errores del paquete.
# survey: estimacion con UPM, estrato y pesos; errores e intervalos.
# tibble: tablas de salida del paquete.
# readxl: lectura de los dos Excel cantonales y de diccionarios.
# writexl: exportacion de la tabla final como Excel, en una sola hoja.
dependencias <- c("glue", "haven", "janitor", "readr", "rlang", "survey", "tibble", "readxl", "writexl", "dplyr")
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 600)
for (paquete in dependencias) {
  if (!requireNamespace(paquete, quietly = TRUE)) {
    install.packages(paquete, lib = biblioteca_local, dependencies = NA)
  }
  if (!requireNamespace(paquete, quietly = TRUE)) stop("No se pudo instalar: ", paquete)
}

mostrar_etapa("2. Dependencias disponibles", data.frame(paquete = dependencias,
                                                        version = vapply(dependencias, function(x) as.character(utils::packageVersion(x)), character(1))))

# 3. INSTALAR Y CARGAR enemduR ------------------------------------------------
# Usa el paquete instalado. Si falta, instala la etiqueta concreta de GitHub
# en la biblioteca personal de R; no busca un repositorio en otra carpeta.
if (!requireNamespace("enemduR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", lib = biblioteca_local, dependencies = NA)
  }
  remotes::install_github("yerovi84/enemduR@v0.1.0-alpha.2",
                          lib = biblioteca_local, dependencies = NA, upgrade = "never", build_vignettes = FALSE)
}
if (!requireNamespace("enemduR", quietly = TRUE)) stop("No se pudo instalar enemduR.")
library(enemduR)
library(readxl)
library(readr)
library(survey)
library(tibble)
library(writexl)
# glue, haven, janitor y rlang son importaciones internas del paquete. No hace
# falta adjuntarlas con library(); se comprobaron e instalaron en la seccion 2.

mostrar_etapa("3. Paquete analitico cargado", data.frame(paquete = "enemduR",
                                                         version_instalada = as.character(utils::packageVersion("enemduR"))))

# 4. FORMULAS Y DIFERENCIA ENTRE PAQUETE Y ADAPTACION --------------------------
# Notacion: w_i=fexp; I(condicion)=1 si se cumple, 0 si no. Las sumas se toman
# para el canton y anio. Las estimaciones usan todas las filas del diseno
# nacional antes de estimar subpoblaciones. No son porcentajes sin ponderar.
#
# 4.1 DESEMPLEO GENERAL: REGLAS YA IMPLEMENTADAS EN enemduR
#   100 * sum(w_i * I(condact en 7,8)) / sum(w_i * I(condact en 1,...,8)).
#   condact 1:6=ocupados; 7:8=desempleados; 1:8=PEA.
#   El registro del paquete fija el universo laboral de 15 anios o mas.
#   Se usa enemdu_build_labor_flags(), la misma clasificacion que utiliza
#   enemdu_kpi_employment(). Luego se estima su proporcion en la PEA con
#   enemdu_survey_proportion() para no calcular otras tasas innecesarias.
#   No se reconstruye condact desde preguntas crudas.
#
# 4.2 POBREZA POR INGRESOS: CLASIFICACION DEL PAQUETE Y RUTA DE ESTE ANALISIS
#   El paquete define pobre = I(ingreso per capita < linea de pobreza explicita).
#   Funciones: enemdu_build_poverty_flags() y enemdu_kpi_income_poverty().
#   No generan por si solas la linea oficial ni el IPC.
#   En estas bases ya existe pobreza, entregada por INEC: 1=pobre, 0=no pobre.
#   ESTE SCRIPT usa esa variable, no recalcula lineas ni el ingreso:
#   100 * sum(w_i * pobreza_i) / sum(w_i), en registros con pobreza 0/1.
#   Esta ruta evita aplicar una unica linea de diciembre a toda la base anual.
#   El denominador se estima con ENEMDU; no con la proyeccion externa del Excel.
#
# 4.3 INASISTENCIA 12-17: ADAPTACION PROPIA CON ESTIMADOR DEL PAQUETE
#   100 * sum(w_i * I(12<=edad<=17 y p07=2)) /
#         sum(w_i * I(12<=edad<=17 y p07 en 1,2)).
#   Equivale a (total adolescente - asistentes)/total adolescente *100 cuando
#   todas las respuestas son conocidas. NA no se convierte en no asistencia.
#   No es 100 menos tasa neta de asistencia a secundaria: no exigimos nivel.
#   Edad=p03; asistencia=p07; codigos 1=si y 2=no, contrastados con formulario.
#
# 4.4 NBI ADOLESCENTE: CONSTRUCCION DEL PAQUETE + SUBPOBLACION PROPIA
#   El perfil del paquete construye comp1...comp5:
#     comp1: piso en 7,8 o paredes en 6,7 (calidad de vivienda).
#     comp2: 0 dormitorios o miembros/dormitorios >3 (hacinamiento).
#     comp3: privacion de saneamiento o agua, segun reglas del perfil:
#       vi09 en 3,4,5; agua aceptable vi10 en 1,3 y vi10a en 1,2,3.
#     comp4: presencia de un nino de 6-12 que no asiste a clases.
#     comp5: jefatura con <=2 anios de escolaridad aproximada y
#       miembros/ocupados >3. Sin ocupados, el cociente se trata como infinito.
#   El paquete suma knbi=comp1+...+comp5 y define nbi=I(knbi>=1).
#   Si falta un componente, enemdu_build_nbi_flags() deja NBI como NA.
#   Luego nuestra adaptacion estima:
#   100 * sum(w_i * I(12<=edad<=17) * nbi_i) /
#         sum(w_i), sobre toda la poblacion del canton-anio.
#   Se usan enemdu_join_nbi_sources(), enemdu_build_nbi_components() y
#   enemdu_build_nbi_flags(). El perfil disponible es enemdu_2025_anual.
#   IMPORTANTE: la escolaridad de la jefatura es una aproximacion.
#   El helper de esta version asigna 0 si p10a=1; usa p10b si p10a<=2,
#   6+p10b si p10a=3 y 12+p10b si p10a>=4 (con entradas disponibles).
#   Esa aproximacion debe contrastarse con los niveles reales del cuestionario.
#   Trasladar el perfil a 2021-2024 no esta validado. Se etiqueta PROVISIONAL.
#   Estas son reglas implementadas en esta version, no certificacion INEC.
#
# 4.5 MONOPARENTALIDAD: FORMULA EXTERNA + APROXIMACION PROPIA
#   Referencia publicada CDC/ATSDR, SVI 2018, pagina 15, EP_SNGPNT:
#   100 * hogares monoparentales con hijos <18 / total de hogares.
#   https://www.atsdr.cdc.gov/place-health/media/pdfs/2024/08/SVI2018Documentation_01192022_1.pdf#page=15
#   enemduR NO implementa esa clasificacion. Nuestro proxy observable usa:
#   jefatura (p04=1), ningun conyuge residente (p04=2) y algun hijo/a (p04=3)
#   menor de 18. Se permiten otros convivientes. Una fila y un peso por hogar.
#   100 * sum(w_h * proxy_h) / sum(w_h), en hogares clasificables.
#   No identifica cuidado efectivo ni todos los nucleos secundarios. No debe
#   confundirse con la medicion completa de un solo cuidador que se solicito.
#
# 4.7 PRECISION Y SALIDA
#   enemdu_survey_proportion() utiliza survey y enemdu_evaluate_precision().
#   Conservamos SE, IC95, CV, n muestral, n expandida y diagnostico.
#   CV = SE/abs(estimacion); si estimacion=0, no se interpreta un CV definido.
#   El paquete aproxima n efectiva con n/deff; si falta deff, utiliza n.
#   Los intervalos son Wald; pueden salir de 0-100 y no se recortan a escondidas.
#   Una n expandida grande no reemplaza una muestra suficiente.
#   Los cantones no son dominios de diseno garantizados: todo es exploratorio.
#   Ceros/unos muestrales se marcan sin_variacion_muestral, no certeza real.
#   Fuente del codigo del paquete:
#   https://github.com/yerovi84/enemduR/tree/v0.1.0-alpha.2/R
#
# 5. FUNCIONES PROPIAS DE LECTURA, VALIDACION Y CALCULO ------------------------

# Adaptaciones del proyecto. No modifica funciones ni registros de enemduR.
# 5.1 Combina canton y anio para unir tablas sin depender del orden.
clave_panel <- function(x) paste(x$codigo_canton, x$anio, sep = "_")
# 5.2 Valida codigos y conserva ceros iniciales.
codigo_dpa <- function(x, ancho) {
  x <- trimws(as.character(x))
  if (anyNA(x) || any(!grepl("^[0-9]+$", x)) || any(nchar(x) > ancho)) {
    stop("Codigo geografico ausente o invalido.")
  }
  sprintf(paste0("%0", ancho, "d"), as.integer(x))
}
# 5.3 Rechaza claves duplicadas y anios fuera del intervalo.
validar_claves <- function(x) {
  if (anyNA(x$anio) || anyDuplicated(clave_panel(x))) stop("Clave canton-anio ausente o duplicada.")
  if (any(!x$anio %in% 2021:2025)) stop("Anio fuera del intervalo solicitado.")
  invisible(TRUE)
}
# Reconocer encabezados sin exigir un orden ni columnas auxiliares de edad.
leer_poblacion_panel <- function(fp) {
  p <- as.data.frame(readxl::read_excel(fp, sheet = "Total_adolescente"))
  originales <- names(p)
  n <- tolower(iconv(originales, from = "", to = "ASCII//TRANSLIT", sub = ""))
  n <- gsub("[^a-z0-9]", "", n)
  alias <- list(anio = c("ano", "anio", "ao"),
                codigo_provincia = c("codprovincia", "codigoprovincia"),
                provincia = "provincia", codigo_canton = c("codcanton", "codigocanton"),
                canton = "canton", poblacion_12_17 = "poblacion1217",
                poblacion_12_14 = "poblacion1214", poblacion_15_17 = "poblacion1517")
  for (destino in names(alias)) {
    ix <- which(n %in% alias[[destino]])
    if (length(ix) > 1L) stop("Encabezado ambiguo: ", destino)
    if (length(ix)) names(p)[ix] <- destino
  }
  requeridas <- c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton", "poblacion_12_17")
  faltan <- setdiff(requeridas, names(p))
  if (length(faltan)) stop("Columnas ausentes: ", paste(faltan, collapse = ", "),
                           ". Encabezados encontrados: ", paste(originales, collapse = " | "))
  p$anio <- as.integer(p$anio)
  p$codigo_canton <- codigo_dpa(p$codigo_canton, 4)
  p$codigo_provincia <- codigo_dpa(p$codigo_provincia, 2)
  validar_claves(p)
  stopifnot(is.numeric(p$poblacion_12_17),
            all(is.finite(p$poblacion_12_17) & p$poblacion_12_17 > 0),
            all(substr(p$codigo_canton, 1, 2) == p$codigo_provincia))
  # Comprobar la suma solo si el archivo incluye ambos grupos auxiliares.
  if (all(c("poblacion_12_14", "poblacion_15_17") %in% names(p))) {
    stopifnot(all(is.finite(p$poblacion_12_14)), all(is.finite(p$poblacion_15_17)),
              all(abs(p$poblacion_12_17 - p$poblacion_12_14 - p$poblacion_15_17) < 1e-6))
  }
  p
}

# 5.4 Lee el catalogo territorial; no incorpora poblacion ni conteos a los indicadores.
leer_territorios <- function(raiz) {
  fp <- file.path(raiz, "DATAS_PROCESADAS", "poblacion_cantonal_costa_total_12_17_2021_2025.xlsx")
  p <- leer_poblacion_panel(fp)
  as.data.frame(p[, c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")])
}
# 5.5 Encuentra cada base anual sin mezclar copias de otras carpetas.
localizar_archivos_anio <- function(raiz, anio) {
  carpetas <- list.dirs(file.path(raiz, "DATAS_ORIGEN"), recursive = FALSE)
  carpeta <- carpetas[grepl(as.character(anio), basename(carpetas))]
  if (length(carpeta) != 1L) stop("Carpeta anual ausente o ambigua: ", anio)
  archivos <- list.files(carpeta, full.names = TRUE)
  for (tipo in c("personas", "vivienda")) {
    if (sum(grepl(paste0("^BDDenemdu_", tipo, ".*csv$"), basename(archivos))) != 1L) {
      stop("Base anual ausente o ambigua: ", tipo)
    }
  }
  archivos
}
# 5.6 Lee CSV con enemdu_read_data y convierte numeros con coma decimal.
leer_csv_panel <- function(f, anio, numericas) {
  x <- enemdu_read_data(f, "anual", period = as.character(anio), csv_delim = ";",
                        standardize_names = FALSE, inform_scope = FALSE,
                        col_types = readr::cols(.default = readr::col_character()), na = c("", " "), trim_ws = TRUE)
  if (nrow(readr::problems(x))) stop("Problemas de lectura: ", basename(f))
  for (v in intersect(numericas, names(x))) {
    z <- readr::parse_double(x[[v]], locale = readr::locale(decimal_mark = ","), na = c("", "NA"))
    if (nrow(readr::problems(z))) stop("Problemas numericos: ", v)
    x[[v]] <- z
  }
  x
}
# 5.7 Extrae columnas originales, valida y une vivienda; aun no calcula indicadores.
cargar_anio_panel <- function(raiz, anio) {
  fs <- localizar_archivos_anio(raiz, anio)
  p <- leer_csv_panel(fs[grepl("^BDDenemdu_personas.*csv$", basename(fs))], anio,
                      c("p03", "p04", "p06", "p07", "p10a", "p10b", "condact", "empleo", "secemp", "fexp", "pobreza"))
  v <- leer_csv_panel(fs[grepl("^BDDenemdu_vivienda.*csv$", basename(fs))], anio,
                      c("vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a", "fexp"))
  # Extraer unicamente las columnas originales necesarias; todavia sin indicadores.
  columnas_persona <- c("ciudad", "prov", "area", "id_hogar", "id_persona", "p01",
                        "p03", "p04", "p06", "p07", "p10a", "p10b", "condact", "empleo", "secemp",
                        "pobreza", "fexp", "upm", "estrato", "mes")
  columnas_vivienda <- c("id_hogar", "vi04a", "vi05a", "vi07", "vi09", "vi10", "vi10a", "fexp")
  stopifnot(all(columnas_persona %in% names(p)), all(columnas_vivienda %in% names(v)))
  p <- as.data.frame(p[, columnas_persona])
  v <- as.data.frame(v[, columnas_vivienda])
  req <- c("ciudad", "prov", "id_hogar", "id_persona", "p01", "p03", "p04", "p06", "p07", "p10a", "p10b", "condact", "empleo", "pobreza")
  revision <- enemdu_validate_structure(p, required_vars = req)
  if (any(revision$status == "fail")) stop("Variables requeridas ausentes en ", anio)
  stopifnot(!anyNA(p$id_persona), !anyDuplicated(p$id_persona),
            !anyNA(p$id_hogar), !anyNA(v$id_hogar), !anyDuplicated(v$id_hogar),
            all(is.finite(p$fexp) & p$fexp > 0), all(is.finite(v$fexp) & v$fexp > 0),
            !anyNA(p$upm), !anyNA(p$estrato))
  p$ciudad <- codigo_dpa(p$ciudad, 6)
  p$codigo_canton <- substr(p$ciudad, 1, 4)
  stopifnot(all(substr(p$codigo_canton, 1, 2) == codigo_dpa(p$prov, 2)))
  p <- enemdu_join_nbi_sources(p, v, strict = TRUE)
  stopifnot(nrow(p) == length(p$id_persona))
  list(personas = p, vivienda = v, estructura = revision)
}
# 5.8 Construye una fila por hogar y el proxy monoparental explicado en 4.5.
clasificar_jefatura <- function(p) {
  # Aproximacion observable: jefatura sin conyuge residente y con hijo/a <18.
  # No identifica cuidado efectivo ni nucleos secundarios.
  j <- which(p$p04 == 1)
  h <- as.data.frame(p[j, c("id_hogar", "codigo_canton", "upm", "estrato", "fexp")])
  stopifnot(!anyDuplicated(h$id_hogar), length(unique(p$id_hogar)) == nrow(h))
  m <- match(p$id_hogar, h$id_hogar)
  stopifnot(!anyNA(m), all(p$codigo_canton == h$codigo_canton[m]),
            all(p$upm == h$upm[m]), all(p$estrato == h$estrato[m]),
            all(abs(p$fexp - h$fexp[m]) < 1e-7))
  indicadores <- cbind(conyuge = as.integer(p$p04 %in% 2),
                       hijo_menor = as.integer(p$p04 %in% 3 & !is.na(p$p03) & p$p03 < 18),
                       desconocido = as.integer(is.na(p$p04) | (p$p04 %in% 3 & is.na(p$p03))),
                       menor = as.integer(!is.na(p$p03) & p$p03 < 18),
                       otros = as.integer(p$p04 %in% c(4, 5, 7, 0)))
  sumas <- rowsum(indicadores, p$id_hogar, reorder = FALSE)
  sumas <- sumas[match(h$id_hogar, rownames(sumas)), , drop = FALSE]
  h$mono <- ifelse(sumas[, "desconocido"] == 0,
                   as.integer(sumas[, "conyuge"] == 0 & sumas[, "hijo_menor"] > 0), NA_integer_)
  h$menor_presente <- sumas[, "menor"] > 0
  h$otros_nucleos_posibles <- sumas[, "otros"] > 0
  h
}
# 5.9 Etiqueta la precision y los ceros/unos observados; no cambia estimaciones.
clasificar_precision_panel <- function(r) {
  ifelse(is.na(r$estimate), "no_estimable",
         ifelse(r$estimate <= 1e-12 | r$estimate >= 1 - 1e-12, "sin_variacion_muestral",
                ifelse(r$quality_flag == "reliable", "precision_favorable_alcance_cantonal_no_garantizado", "precision_insuficiente_o_reducida")))
}
# 5.10 Estima con enemdu_survey_proportion y verifica el cociente ponderado.
estimar_panel <- function(d, valor, indicador, objetivo, anio, estado_metodo) {
  d$dominio_panel <- ifelse(d$codigo_canton %in% objetivo$codigo_canton, d$codigo_canton, "FUERA_AMBITO")
  columnas <- c("dominio_panel", "upm", "estrato", "fexp", valor)
  # Diseno nacional completo. Ninguna fila se filtra por canton o edad antes.
  r <- enemdu_survey_proportion(d[, columnas], value = valor,
                                group_vars = "dominio_panel", survey_type = "anual", indicator_id = indicador)
  r <- as.data.frame(r[r$dominio_panel != "FUERA_AMBITO", ])
  names(r)[names(r) == "dominio_panel"] <- "codigo_canton"
  salida <- merge(data.frame(codigo_canton = objetivo$codigo_canton), r, by = "codigo_canton", all.x = TRUE, sort = FALSE)
  salida$indicator_id <- indicador
  salida$anio <- anio
  salida$estado_metodo <- estado_metodo
  salida$alcance <- "canton_fuera_dominios_garantizados_diseno"
  # Inicializar por fila aunque el paquete no devuelva estas columnas.
  for (campo in c("decision", "quality_flag", "failed_reasons")) {
    if (!campo %in% names(salida)) salida[[campo]] <- rep(NA_character_, nrow(salida))
  }
  # Reglas previas al filtrado de NA: no equivalen a un minimo muestral.
  # Desempleo: sin PEA clasificable no existe denominador para estimar la tasa.
  # Inasistencia: sin adolescentes 12-17 con p07=1 o 2 no hay denominador.
  # Pobreza por ingresos: sin personas con pobreza=0 o 1 no hay denominador.
  # Monoparentalidad: sin hogares clasificables por el proxy no hay denominador.
  # La ausencia de registros no demuestra ausencia del fenomeno en el canton.
  # NBI conserva su tratamiento anterior; no se agrega aqui una regla para NBI.
  revisar_denominador <- indicador %in% c("desempleo_general",
                                          "inasistencia_12_17", "pobreza_ingresos", "monoparental_jefatura_aproximacion")
  for (k in seq_len(nrow(salida))) {
    ix <- d$codigo_canton == salida$codigo_canton[k] &
      is.finite(d[[valor]]) & is.finite(d$fexp) & d$fexp > 0
    n_validas <- sum(ix)
    denominador <- sum(d$fexp[ix])
    if (revisar_denominador && (n_validas == 0 || denominador <= 0)) {
      # Indefinido, nunca 0 %. Conservar NA tambien en las medidas de precision.
      for (v in intersect(c("estimate", "standard_error", "cv", "ci_lower",
                            "ci_upper", "deff", "effective_n", "degrees_freedom"), names(salida))) {
        salida[[v]][k] <- NA_real_
      }
      salida$unweighted_n[k] <- n_validas
      salida$weighted_n[k] <- denominador
      salida$decision[k] <- "no_recommended_inference"
      salida$quality_flag[k] <- "no_valid_observations"
      salida$failed_reasons[k] <- "sin_denominador_valido"
    }
    # Con denominador positivo, numerador 0 produce un cero que se conserva.
    if (n_validas > 0 && denominador > 0 && is.finite(salida$estimate[k])) {
      control <- sum(d$fexp[ix] * d[[valor]][ix]) / denominador
      if (abs(control - salida$estimate[k]) > 1e-8) stop("No coincide cociente ponderado: ", indicador)
    }
  }
  salida$porcentaje_exploratorio <- 100 * salida$estimate
  salida$error_estandar_pp <- 100 * salida$standard_error
  salida$ic95_inferior <- 100 * salida$ci_lower
  salida$ic95_superior <- 100 * salida$ci_upper
  salida$estado_revision <- clasificar_precision_panel(salida)
  salida
}
# 5.11 Agrega columnas calculadas; devuelve personas, hogares e indicadores sin exportar.
calcular_anio_panel <- function(fuente, objetivo, anio) {
  p <- fuente$personas
  p <- enemdu_build_labor_flags(p)
  edad_ok <- !is.na(p$p03) & p$p03 >= 12 & p$p03 <= 17
  p$inasistencia <- ifelse(edad_ok & p$p07 %in% c(1, 2), as.integer(p$p07 == 2), NA_integer_)
  p$desempleo_panel <- ifelse(p$labor_pea == 1, p$labor_desempleo, NA_integer_)
  # Utiliza la clasificacion entregada por INEC. No reconstruye ingreso/lineas.
  p$pobreza_panel <- ifelse(p$pobreza %in% c(0, 1), p$pobreza, NA_real_)
  # Candidato de NBI del paquete: no se certifica su perfil ni su traslado historico.
  p <- enemdu_build_nbi_components(p, profile = "enemdu_2025_anual", strict = TRUE)
  p <- enemdu_build_nbi_flags(p)
  p$nbi_adolescente <- ifelse(is.na(p$p03), NA_integer_, ifelse(edad_ok, p$nbi, 0L))
  # Denominador total: no reducirlo silenciosamente si falta clasificar un caso.
  cantones_nbi_incompleto <- unique(p$codigo_canton[is.na(p$nbi_adolescente)])
  p$nbi_adolescente[p$codigo_canton %in% cantones_nbi_incompleto] <- NA_integer_
  h <- clasificar_jefatura(p)
  bloques <- list(
    estimar_panel(p, "inasistencia", "inasistencia_12_17", objetivo, anio, "derivado_p03_p07_codigos_1_2"),
    estimar_panel(p, "desempleo_panel", "desempleo_general", objetivo, anio, "flags_laborales_enemduR_condact"),
    estimar_panel(p, "pobreza_panel", "pobreza_ingresos", objetivo, anio, "clasificacion_pobreza_entregada_INEC_no_recalculada"),
    estimar_panel(p, "nbi_adolescente", "adolescentes_nbi_sobre_total_provisional", objetivo, anio,
                  if (anio == 2025) "perfil_paquete_2025_aproximacion_escolaridad_pendiente_auditoria" else "perfil_2025_trasladado_sin_validacion_historica"),
    estimar_panel(h, "mono", "monoparental_jefatura_aproximacion", objetivo, anio, "proxy_jefatura_no_cuidado_efectivo_ni_nucleos_secundarios")
  )
  # Descripcion de muestra: no excluye cantones ni modifica estimaciones.
  muestra <- do.call(rbind, lapply(objetivo$codigo_canton, function(cod) {
    ix <- p$codigo_canton == cod
    hx <- h$codigo_canton == cod
    data.frame(anio = anio, codigo_canton = cod,
               registros_personas = sum(ix),
               registros_adolescentes = sum(ix & edad_ok),
               registros_pea = sum(ix & p$labor_pea == 1, na.rm = TRUE),
               registros_hogares = sum(hx),
               upm_observadas = length(unique(p$upm[ix])),
               adolescentes_asistencia_faltante = sum(ix & edad_ok & is.na(p$inasistencia)),
               adolescentes_nbi_faltante = sum(ix & edad_ok & is.na(p$nbi)),
               personas_pobreza_faltante = sum(ix & is.na(p$pobreza_panel)))
  }))
  # Conservar variables originales y columnas calculadas de interes en memoria.
  originales <- names(fuente$personas)
  derivadas <- c("labor_pea", "labor_desempleo", "inasistencia", "desempleo_panel",
                 "pobreza_panel", paste0("comp", 1:5), "knbi", "nbi", "nbi_adolescente")
  p <- as.data.frame(p[, unique(c(originales, derivadas))])
  list(indicadores = do.call(rbind, bloques), muestra = muestra,
       personas_calculadas = p, hogares_calculados = h)
}
# 6. PARAMETROS ---------------------------------------------------------------
# Estos parametros se pueden definir antes de source().
if (!exists("solo_preparar")) solo_preparar <- FALSE
if (!isTRUE(solo_preparar)) {
  # raiz_tesis se define en la seccion 1, sin depender de ejecuciones anteriores.
  if (!exists("anios_panel")) anios_panel <- 2021:2025
  # Filtro general por canton-anio; incluye exactamente 60 registros.
  # No reemplaza la evaluacion de muestra/precision de cada indicador.
  muestra_general_minima <- 60L
  dir.create(file.path(raiz_tesis, "DATAS_PROCESADAS"), recursive = TRUE, showWarnings = FALSE)
  # Dos exportaciones solicitadas: tabla completa y tabla limpia. FALSE permite revisar objetos sin escribir.
  if (!exists("exportar_tabla")) exportar_tabla <- TRUE
  stopifnot(all(anios_panel %in% 2021:2025))
  opciones_encuesta <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
  tryCatch({
    
    # 7. EXTRAER COLUMNAS ORIGINALES Y CREAR LOS DATA FRAMES --------------------
    # Cada fila sigue siendo una observacion de persona, no un canton.
    # Se conserva el pais completo para declarar correctamente el diseno.
    # Solo despues de estimar se seleccionan los cantones del panel.
    territorios <- leer_territorios(raiz_tesis)
    poblacion_cantonal <- territorios[territorios$anio %in% anios_panel, ]
    mostrar_etapa("6. Periodo y catalogo territorial", resumir_filas_consola(poblacion_cantonal))
    datos_por_anio <- setNames(lapply(anios_panel, function(anio) {
      mostrar_etapa(paste("7. Lectura y validacion de microdatos del anio", anio))
      fuente <- cargar_anio_panel(raiz_tesis, anio)
      mostrar_etapa("Registros leidos y claves verificadas", data.frame(
        base = c("Personas", "Vivienda"), filas = c(nrow(fuente$personas), nrow(fuente$vivienda)),
        columnas = c(ncol(fuente$personas), ncol(fuente$vivienda))))
      fuente
    }), as.character(anios_panel))
    agregar_anio <- function(d, anio) { d$anio <- as.integer(anio); as.data.frame(d) }
    datos_extraidos <- do.call(rbind, Map(function(d, a) agregar_anio(d$personas, a),
                                          datos_por_anio, anios_panel))
    vivienda_extraida <- do.call(rbind, Map(function(d, a) agregar_anio(d$vivienda, a),
                                            datos_por_anio, anios_panel))
    rownames(datos_extraidos) <- NULL
    rownames(vivienda_extraida) <- NULL
    print(data.frame(objeto = c("datos_extraidos", "vivienda_extraida"),
                     filas = c(nrow(datos_extraidos), nrow(vivienda_extraida)),
                     columnas = c(ncol(datos_extraidos), ncol(vivienda_extraida))))
    
    # 7.1. REVISAR MUESTRA GENERAL ANTES DE CONSTRUIR INDICADORES ---------------
    # Partimos de TODOS los cantones de los insumos, sin excluir un canton
    # completo por tener anios pequenos. Contamos filas de personas de todas
    # las edades, sin sumar pesos ni agregar filas de vivienda.
    muestra_general_R <- poblacion_cantonal[, c("anio", "codigo_provincia",
                                                "provincia", "codigo_canton", "canton")]
    stopifnot(!anyDuplicated(clave_panel(muestra_general_R)))
    muestra_general_R$registros_personas <- vapply(
      seq_len(nrow(muestra_general_R)), function(i) {
        p <- datos_por_anio[[as.character(muestra_general_R$anio[i])]]$personas
        sum(p$codigo_canton == muestra_general_R$codigo_canton[i], na.rm = TRUE)
      }, numeric(1))
    muestra_general_R$cumple_muestra_general <-
      muestra_general_R$registros_personas >= muestra_general_minima
    mostrar_etapa("7.1 Muestra general por canton y anio", muestra_general_R)
    resumen_muestra_R <- do.call(rbind, lapply(sort(unique(muestra_general_R$anio)), function(a) {
      x <- muestra_general_R[muestra_general_R$anio == a, ]
      data.frame(anio = a, canton_anio_evaluados = nrow(x),
                 cumplen_al_menos60 = sum(x$cumple_muestra_general),
                 excluidos_menor60 = sum(!x$cumple_muestra_general),
                 sin_personas_observadas = sum(x$registros_personas == 0))
    }))
    mostrar_etapa("Resultado de la seleccion inicial por muestra", resumen_muestra_R)
    # Seleccion inicial del ambito ANTES de construir indicadores.
    # Se seleccionan cantones-anio, no personas del diseno nacional.
    revision_muestra_inicial_R <- muestra_general_R
    territorios_seleccionados_R <- muestra_general_R[
      muestra_general_R$cumple_muestra_general,
      c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton"),
      drop = FALSE]
    if (!nrow(territorios_seleccionados_R)) stop("Ningun canton-anio cumple la muestra general de 60 registros.")
    poblacion_cantonal <- territorios_seleccionados_R
    # Los microdatos nacionales siguen completos para estimar subpoblaciones.
    
    # 8. CONSTRUIR COLUMNAS CALCULADAS EN OTROS OBJETOS -------------------------
    # Conceptos, variables, formulas y comandos estan explicados en 4.1-4.7.
    # calcular_anio_panel() agrega los flags y componentes y llama los
    # estimadores de enemduR. No escribe archivos.
    # Un resultado estadistico por indicador, canton y anio, sin guardar avisos.
    resultados_por_anio <- list()
    anios_con_muestra <- sort(unique(poblacion_cantonal$anio))
    for (anio_actual in anios_con_muestra) {
      mostrar_etapa(paste("8. Construccion y estimacion de indicadores del anio", anio_actual))
      resultados_por_anio[[as.character(anio_actual)]] <- suppressWarnings(
        calcular_anio_panel(datos_por_anio[[as.character(anio_actual)]],
                            poblacion_cantonal[poblacion_cantonal$anio == anio_actual, ], anio_actual))
      r_anio <- resultados_por_anio[[as.character(anio_actual)]]$indicadores
      mostrar_etapa("Resultados por indicador", do.call(rbind, lapply(unique(r_anio$indicator_id), function(id) {
        x <- r_anio[r_anio$indicator_id == id, ]
        data.frame(indicador = id, cantones = nrow(x),
                   disponibles = sum(is.finite(x$porcentaje_exploratorio)),
                   faltantes = sum(!is.finite(x$porcentaje_exploratorio)),
                   ceros = sum(x$porcentaje_exploratorio == 0, na.rm = TRUE))
      })))
    }
    datos_con_calculos <- do.call(rbind, Map(function(d, a) agregar_anio(d$personas_calculadas, a),
                                             resultados_por_anio, anios_con_muestra))
    hogares_con_calculos <- do.call(rbind, Map(function(d, a) agregar_anio(d$hogares_calculados, a),
                                               resultados_por_anio, anios_con_muestra))
    # Estimadores publicados en el tabulado INEC, aplicados a nuestros indicadores.
    # Una fila por indicador/canton/anio, incluyendo los casos sin estimacion.
    # Porcentajes e intervalos en escala 0-100; SE en puntos porcentuales.
    # CV en porcentaje (100 * SE / abs(estimacion)); indefinido si valor = 0.
    estimadores <- do.call(rbind, lapply(resultados_por_anio, `[[`, "indicadores"))
    j_estimadores <- match(clave_panel(estimadores), clave_panel(poblacion_cantonal))
    stopifnot(!anyNA(j_estimadores))
    estimadores <- data.frame(
      anio = estimadores$anio,
      codigo_provincia = poblacion_cantonal$codigo_provincia[j_estimadores],
      provincia = poblacion_cantonal$provincia[j_estimadores],
      codigo_canton = estimadores$codigo_canton,
      canton = poblacion_cantonal$canton[j_estimadores],
      indicador = estimadores$indicator_id,
      valor_pct = estimadores$porcentaje_exploratorio,
      error_estandar_pp = estimadores$error_estandar_pp,
      limite_inferior_pct = estimadores$ic95_inferior,
      limite_superior_pct = estimadores$ic95_superior,
      cv_pct = ifelse(is.finite(estimadores$estimate) & estimadores$estimate != 0,
                      100 * estimadores$standard_error / abs(estimadores$estimate), NA_real_),
      stringsAsFactors = FALSE)
    rownames(estimadores) <- NULL
    muestra_R <- do.call(rbind, lapply(resultados_por_anio, `[[`, "muestra"))
    
    # 9. DEJAR SOLO INDICADORES AGREGADOS EN UN DATA FRAME CANTON-ANIO -----------
    # El objeto conserva las claves y cinco porcentajes, no microdatos.
    # Los nombres provisional/aproximacion evitan atribuir validacion pendiente.
    indicadores_agregados <- poblacion_cantonal[, c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")]
    nombres_indicadores <- c(
      inasistencia_12_17 = "inasistencia_12_17_pct",
      desempleo_general = "desempleo_pct",
      pobreza_ingresos = "pobreza_ingresos_pct",
      adolescentes_nbi_sobre_total_provisional = "adolescentes_nbi_sobre_total_provisional_pct",
      monoparental_jefatura_aproximacion = "monoparental_jefatura_aproximacion_pct")
    for (id in names(nombres_indicadores)) {
      estimacion <- estimadores[estimadores$indicador == id, ]
      stopifnot(!anyDuplicated(clave_panel(estimacion)))
      j <- match(clave_panel(indicadores_agregados), clave_panel(estimacion))
      indicadores_agregados[[nombres_indicadores[[id]]]] <- estimacion$valor_pct[j]
    }
    
    mostrar_etapa("9. Indicadores agregados por anio", resumir_filas_consola(indicadores_agregados))
    mostrar_etapa("Disponibilidad y ceros por indicador", resumir_indicadores_consola(indicadores_agregados, unname(nombres_indicadores)))
    mostrar_etapa("Primeras filas de los indicadores", utils::head(indicadores_agregados, 6))
    # 10. DOS TABLAS: INDICADORES CON Y SIN FALTANTES --------------------------
    tabla_indicadores_sin_filtrar <- indicadores_agregados
    columnas_indicadores <- unname(nombres_indicadores)
    conservar_indicadores <- Reduce(`&`, lapply(
      indicadores_agregados[, columnas_indicadores, drop = FALSE], is.finite))
    tabla_indicadores_limpia <- indicadores_agregados[conservar_indicadores, , drop = FALSE]
    indicadores_descartados_R <- indicadores_agregados[!conservar_indicadores, , drop = FALSE]
    stopifnot(setequal(clave_panel(indicadores_agregados), clave_panel(territorios_seleccionados_R)))
    mostrar_etapa("10. Limpieza de indicadores", data.frame(
      filas_antes = nrow(tabla_indicadores_sin_filtrar),
      filas_con_algun_faltante = nrow(indicadores_descartados_R),
      filas_conservadas = nrow(tabla_indicadores_limpia),
      cantones_conservados = length(unique(tabla_indicadores_limpia$codigo_canton))))
    mostrar_etapa("Filas conservadas por anio", resumir_filas_consola(tabla_indicadores_limpia))
    mostrar_etapa("Ceros conservados y disponibilidad final", resumir_indicadores_consola(tabla_indicadores_limpia, columnas_indicadores))
    cat("\nResumen estadistico de los indicadores limpios\n")
    print(summary(tabla_indicadores_limpia))
    # 11. EXPORTAR LOS DOS EXCEL DE FACTORES ----------------------------------
    if (isTRUE(exportar_tabla)) {
      writexl::write_xlsx(list(Indicadores_sin_filtrar = tabla_indicadores_sin_filtrar),
                          file.path(raiz_tesis, "DATAS_PROCESADAS", "construccion_indicadores_factores_sin_filtrar.xlsx"))
      writexl::write_xlsx(list(Indicadores_limpios = tabla_indicadores_limpia),
                          file.path(raiz_tesis, "DATAS_PROCESADAS", "construccion_indicadores_factores.xlsx"))
      mostrar_etapa("11. Excel de indicadores guardados", data.frame(
        archivo = c("construccion_indicadores_factores_sin_filtrar.xlsx", "construccion_indicadores_factores.xlsx"),
        filas = c(nrow(tabla_indicadores_sin_filtrar), nrow(tabla_indicadores_limpia))))
    }
  }, finally = options(opciones_encuesta))
}

# 12. ESTIMADORES Y RESUMEN DEL ARCHIVO LIMPIO DE FACTORES --------------------
if (!isTRUE(solo_preparar)) {
  # 1. LIBRERÍAS ---------------------------------------------------------------
  library(dplyr)
  
  # 2. RECUPERAR MUESTRA Y GRADOS DE LIBERTAD DE CADA ESTIMACIÓN ------------------
  # unweighted_n: registros que participan en el denominador del indicador.
  # degrees_freedom: grados de libertad del diseño para esa subpoblación.
  # No se sustituye esta muestra por el total general de personas del cantón.
  
  stopifnot(
    exists("resultados_por_anio"),
    exists("estimadores"),
    exists("tabla_indicadores_limpia")
  )
  
  normalizar_canton <- function(x) {
    sprintf("%04d", as.integer(as.character(x)))
  }
  
  detalle_diseno <- bind_rows(
    lapply(resultados_por_anio, function(x) x$indicadores)
  )
  
  stopifnot(all(
    c("anio", "codigo_canton", "indicator_id",
      "unweighted_n", "degrees_freedom") %in% names(detalle_diseno)
  ))
  
  detalle_diseno <- detalle_diseno |>
    transmute(
      anio = as.integer(anio),
      codigo_canton = normalizar_canton(codigo_canton),
      indicador = indicator_id,
      muestra_denominador = unweighted_n,
      grados_libertad = degrees_freedom
    )
  
  stopifnot(!anyDuplicated(
    detalle_diseno[, c("anio", "codigo_canton", "indicador")]
  ))
  
  # 3. SELECCIONAR LAS MISMAS FILAS DE LA TABLA LIMPIA ----------------------------
  claves_finales <- tabla_indicadores_limpia |>
    transmute(
      anio = as.integer(anio),
      codigo_canton = normalizar_canton(codigo_canton)
    ) |>
    distinct()
  
  estimaciones_clasificadas <- estimadores |>
    mutate(
      anio = as.integer(anio),
      codigo_canton = normalizar_canton(codigo_canton)
    ) |>
    semi_join(
      claves_finales,
      by = c("anio", "codigo_canton")
    ) |>
    left_join(
      detalle_diseno,
      by = c("anio", "codigo_canton", "indicador")
    )
  
  stopifnot(
    nrow(estimaciones_clasificadas) == nrow(claves_finales) * 5L,
    !anyDuplicated(
      estimaciones_clasificadas[
        , c("anio", "codigo_canton", "indicador")
      ]
    )
  )
  
  mostrar_etapa("12. Estimadores del archivo limpio de factores", data.frame(
    filas_de_factores = nrow(tabla_indicadores_limpia),
    estimaciones = nrow(estimaciones_clasificadas),
    indicadores = length(unique(estimaciones_clasificadas$indicador))))
  
  # 4. APLICAR LOS CRITERIOS EN ORDEN -------------------------------------------
  # Todos nuestros indicadores son proporciones.
  #
  # Criterios del documento INEC:
  # 1) Al menos 60 observaciones del denominador.
  # 2) Al menos 9 grados de libertad.
  # 3) Comparar el error estándar con el máximo tolerable para la proporción:
  #
  #    sqrt(p^(2/3) / 9),       cuando p < 0.5
  #    sqrt((1-p)^(2/3) / 9),   cuando p >= 0.5
  #
  # p y su error deben estar en escala 0–1, no 0–100.
  #
  # Control adicional de esta revisión:
  # Los casos extremos (0 % o 100 %) y errores casi nulos que superen
  # los dos primeros criterios se dejan pendientes de revisión.
  # Esta categoría adicional NO es una categoría oficial del INEC.
  
  estimaciones_clasificadas <- estimaciones_clasificadas |>
    mutate(
      proporcion = valor_pct / 100,
      error_estandar_proporcion = error_estandar_pp / 100,
      
      datos_disponibles = is.finite(proporcion) &
        proporcion >= 0 & proporcion <= 1 &
        is.finite(muestra_denominador) &
        muestra_denominador > 0,
      
      cumple_muestra = is.finite(muestra_denominador) &
        muestra_denominador >= 60,
      
      cumple_gl = is.finite(grados_libertad) &
        grados_libertad >= 9,
      
      error_maximo_tolerable = case_when(
        is.finite(proporcion) &
          proporcion >= 0 & proporcion < 0.5 ~
          sqrt(proporcion^(2 / 3) / 9),
        
        is.finite(proporcion) &
          proporcion >= 0.5 & proporcion <= 1 ~
          sqrt((1 - proporcion)^(2 / 3) / 9),
        
        TRUE ~ NA_real_
      ),
      
      # Columna descriptiva independiente de la clasificación.
      tipo_resultado = case_when(
        !is.finite(valor_pct) ~ "Indicador faltante",
        valor_pct == 0 ~ "Indicador igual a 0 %",
        valor_pct == 100 ~ "Indicador igual a 100 %",
        
        !is.finite(error_estandar_pp) |
          error_estandar_pp < 0 ~ "Error no disponible o invalido",
        
        error_estandar_pp < 1e-10 ~
          "Indicador positivo con error casi cero",
        
        TRUE ~ "Indicador positivo con error mayor al umbral"
      ),
      
      clasificacion = case_when(
        !datos_disponibles ~ "Sin evaluacion",
        
        !cumple_muestra ~ "No recomendada para inferencia",
        
        !is.finite(grados_libertad) ~ "Sin evaluacion",
        
        !cumple_gl ~ "No recomendada para inferencia",
        
        !is.finite(error_estandar_proporcion) |
          error_estandar_proporcion < 0 ~ "Sin evaluacion",
        
        proporcion %in% c(0, 1) |
          error_estandar_pp < 1e-10 ~ "Pendiente de revision",
        
        error_estandar_proporcion <= error_maximo_tolerable ~
          "Fiable segun criterios evaluados",
        
        TRUE ~ "Precision reducida"
      ),
      
      motivo = case_when(
        !datos_disponibles ~
          "Falta estimacion valida o informacion del denominador",
        
        !cumple_muestra ~
          "Menos de 60 observaciones en el denominador",
        
        !is.finite(grados_libertad) ~
          "Grados de libertad no disponibles",
        
        !cumple_gl ~
          "Menos de 9 grados de libertad",
        
        !is.finite(error_estandar_proporcion) |
          error_estandar_proporcion < 0 ~
          "Error estandar no disponible o invalido",
        
        proporcion %in% c(0, 1) ~
          "Proporcion extrema: revisar variabilidad e intervalo",
        
        error_estandar_pp < 1e-10 ~
          "Error casi nulo: revisar soporte muestral",
        
        error_estandar_proporcion <= error_maximo_tolerable ~
          "Cumple muestra, grados de libertad y error tolerable",
        
        TRUE ~ "Error estandar superior al maximo tolerable"
      ),
      
      error_maximo_tolerable_pp = 100 * error_maximo_tolerable
    ) |>
    arrange(anio, codigo_provincia, codigo_canton, indicador)
  
  # 5. RESUMEN POR AÑO E INDICADOR ----------------------------------------------
  # Los pasos son secuenciales: solo pasa al siguiente quien cumple el anterior.
  
  resumen_evaluacion <- estimaciones_clasificadas |>
    group_by(anio, indicador) |>
    summarise(
      estimaciones_totales = n(),
      
      paso1_datos_disponibles = sum(datos_disponibles),
      
      paso2_muestra_al_menos60 = sum(
        datos_disponibles & cumple_muestra
      ),
      
      paso3_gl_al_menos9 = sum(
        datos_disponibles & cumple_muestra & cumple_gl
      ),
      
      fiables = sum(
        clasificacion == "Fiable segun criterios evaluados"
      ),
      
      precision_reducida = sum(
        clasificacion == "Precision reducida"
      ),
      
      no_recomendadas = sum(
        clasificacion == "No recomendada para inferencia"
      ),
      
      pendientes_revision = sum(
        clasificacion == "Pendiente de revision"
      ),
      
      sin_evaluacion = sum(
        clasificacion == "Sin evaluacion"
      ),
      
      # Estos conteos describen los resultados; no se suman a las categorías.
      indicadores_igual_cero = sum(
        tipo_resultado == "Indicador igual a 0 %"
      ),
      
      positivos_error_casi_cero = sum(
        tipo_resultado == "Indicador positivo con error casi cero"
      ),
      
      .groups = "drop"
    ) |>
    mutate(
      porcentaje_fiables = 100 * fiables / estimaciones_totales,
      porcentaje_precision_reducida =
        100 * precision_reducida / estimaciones_totales,
      porcentaje_no_recomendadas =
        100 * no_recomendadas / estimaciones_totales
    ) |>
    arrange(anio, indicador)
  
  # Comprobar que cada estimación tiene una sola clasificación.
  stopifnot(all(
    with(
      resumen_evaluacion,
      estimaciones_totales ==
        fiables + precision_reducida + no_recomendadas +
        pendientes_revision + sin_evaluacion
    )
  ))
  
  mostrar_etapa("Clasificacion de los estimadores", as.data.frame(table(
    clasificacion = estimaciones_clasificadas$clasificacion)))
  cat("\nResumen de estimadores por anio e indicador\n")
  print(resumen_evaluacion, n = Inf, width = Inf)
  if (interactive()) View(resumen_evaluacion)
  
  # 6. EXPORTAR EL MISMO EXCEL CON DOS HOJAS ------------------------------------
  # Cerrar el archivo en Excel antes de ejecutar esta sección.
  # Reemplaza estimadores_finales.xlsx; no modifica la tabla maestra.
  
  if (isTRUE(exportar_tabla)) {
    writexl::write_xlsx(
      list(
        Estimaciones_generales = estimaciones_clasificadas,
        Resumen_anual = resumen_evaluacion
      ),
      path = paste0(
        "C:/Users/camil/OneDrive/Desktop/TESIS/",
        "DATAS_PROCESADAS/estimadores_finales.xlsx"
      )
    )
    mostrar_etapa("Excel de estimadores guardado", data.frame(
      archivo = "estimadores_finales.xlsx", estimaciones = nrow(estimaciones_clasificadas),
      filas_resumen = nrow(resumen_evaluacion)))
    
    
    
  }
  
}
