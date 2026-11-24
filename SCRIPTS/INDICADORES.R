# INDICADORES CANTONALES 2021-2025: CONSTRUCCION DE LAS ESTIMACIONES DIRECTAS

# Paquete: yerovi84/enemduR (v0.1.0-alpha.2). Las funciones sin prefijo enemdu_
# son adaptaciones propias.
<<<<<<< HEAD
# Parte 1 (secciones 1-10): construye los indicadores originales con los pesos
#   originales de la ENEMDU. Salidas: construccion_indicadores_factores.xlsx y
#   estimaciones_originales.xlsx; objeto en R: estimadores.
# Parte 2 (secciones 11-15): estimacion en areas pequenas sobre esos objetos.
#   Salidas: estimaciones_original_vs_suavizado.xlsx e indicadores_suavizados.xlsx.
=======
# Construye los indicadores originales (estimaciones directas) con los pesos
# originales de la ENEMDU y marca su precision con los criterios del INEC.
# Salidas: construccion_indicadores_factores.xlsx y estimaciones_originales.xlsx;
# objeto en R: estimadores.
# Orden de los scripts de la tesis:
#   1. INDICADORES.R                          -> estimaciones directas (este script)
#   2. SCRIP_VARIABLES_AUXILIARES.R           -> variables auxiliares
#   3. SCRIP_ESTIMACION_AREAS_PEQUENAS.R      -> varianza, seleccion de variables, Fay-Herriot y validacion
#   4. VISUSALIZACION.R                       -> graficos y tablas
# Para cambiar anios: definir anios_panel antes de source().
>>>>>>> 8461c56 (actualizacion 24/08)

# Salidas de seguimiento: solo consola; no altera ni filtra datos.
mostrar_etapa <- function(titulo, tabla = NULL) {
  print(paste0("--- ", titulo, " ---"))
  if (!is.null(tabla)) print(as.data.frame(tabla), row.names = FALSE)
  invisible(NULL)
}
# Cuenta, por indicador, cuantas celdas tienen valor, cuantas faltan y cuantas son cero.
resumir_indicadores_consola <- function(tabla, columnas) {
  tabla %>%
    summarise(across(all_of(columnas), list(
      disponibles = \(x) sum(is.finite(x)),
      faltantes = \(x) sum(!is.finite(x)),
      ceros = \(x) sum(is.finite(x) & x == 0)), .names = "{.col}|{.fn}")) %>%
    pivot_longer(everything(), names_to = c("indicador", ".value"), names_sep = "\\|") %>%
    mutate(filas = nrow(tabla), .after = indicador)
}
# Cuenta filas, cantones y provincias por anio en una tabla canton-anio.
resumir_filas_consola <- function(tabla) {
  tabla %>%
    group_by(anio) %>%
    summarise(filas = n(), cantones = n_distinct(codigo_canton),
              provincias = n_distinct(codigo_provincia))
}


# CONSTRUCCION DE INDICADORES ORIGINALES

# 1. RUTAS DE TESIS Y BIBLIOTECA PERSONAL DE R ---------------------------------

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
# writexl: exportacion de las tablas finales como Excel.
# dplyr, tidyr, purrr: manipulacion de tablas con el pipe %>%.
dependencias <- c("glue", "haven", "janitor", "readr", "rlang", "survey", "tibble", "readxl", "writexl", "dplyr", "tidyr", "purrr")
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

if (!requireNamespace("enemduR", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", lib = biblioteca_local, dependencies = NA)
  }
  remotes::install_github("yerovi84/enemduR@v0.1.0-alpha.2",
                          lib = biblioteca_local, dependencies = NA, upgrade = "never", build_vignettes = FALSE)
}
if (!requireNamespace("enemduR", quietly = TRUE)) stop("No se pudo instalar enemduR.")
library(enemduR)  # calcula el indicador ponderado con el diseno de la ENEMDU
library(readxl)   # lee los Excel cantonales
library(readr)    # lee los CSV de las bases anuales
library(survey)   # diseno muestral: errores estandar e intervalos
library(tibble)   # tablas de consola
library(writexl)  # exporta los Excel de salida
library(dplyr)    # manipulacion de tablas y pipe %>%
library(tidyr)    # formato largo a ancho
library(purrr)    # map() por anio e indicador
# glue, haven, janitor y rlang son importaciones internas del paquete. No hace
# falta adjuntarlas con library(); se comprobaron e instalaron en la seccion 2.

mostrar_etapa("3. Paquete analitico cargado", data.frame(paquete = "enemduR",
                                                         version_instalada = as.character(utils::packageVersion("enemduR"))))

# 4. FORMULAS Y DIFERENCIA ENTRE PAQUETE Y ADAPTACION --------------------------
# Notacion: w_i=fexp; I(condicion)=1 si se cumple, 0 si no. Las sumas se toman
# para el canton y anio. Las estimaciones usan todas las filas del diseno
# nacional antes de estimar subpoblaciones. No son porcentajes sin ponderar
#
# 4.1 DESEMPLEO GENERAL: REGLAS YA IMPLEMENTADAS EN enemduR
#   100 * sum(w_i * I(condact en 7,8)) / sum(w_i * I(condact en 1,...,8))
#   condact 1:6=ocupados; 7:8=desempleados; 1:8=PEA
#   El registro del paquete fija el universo laboral de 15 anios o mas
#   Se usa enemdu_build_labor_flags(), la misma clasificacion que utiliza
#   enemdu_kpi_employment(). Luego se estima su proporcion en la PEA con
#   enemdu_survey_proportion() para no calcular otras tasas innecesarias
#   No se reconstruye condact desde preguntas crudas
#
# 4.2 POBREZA POR INGRESOS: CLASIFICACION DEL PAQUETE Y RUTA DE ESTE ANALISIS
#   El paquete define pobre = I(ingreso per capita < linea de pobreza explicita)
#   Funciones: enemdu_build_poverty_flags() y enemdu_kpi_income_poverty()
#   En estas bases ya existe pobreza, entregada por INEC: 1=pobre, 0=no pobre
#   ESTE SCRIPT usa esa variable, no recalcula lineas ni el ingreso:
#   100 * sum(w_i * pobreza_i) / sum(w_i), en registros con pobreza 0/1
#   El denominador se estima con la ENEMDU
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
#     comp1: piso en 7,8 o paredes en 6,7 (calidad de vivienda)
#     comp2: 0 dormitorios o miembros/dormitorios >3 (hacinamiento)
#     comp3: privacion de saneamiento o agua, segun reglas del perfil:
#       vi09 en 3,4,5; agua aceptable vi10 en 1,3 y vi10a en 1,2,3
#     comp4: presencia de un nino de 6-12 que no asiste a clases
#     comp5: jefatura con <=2 anios de escolaridad aproximada y
#       miembros/ocupados >3. Sin ocupados, el cociente se trata como infinito.
#   El paquete suma knbi=comp1+...+comp5 y define nbi=I(knbi>=1)
#   Si falta un componente, enemdu_build_nbi_flags() deja NBI como NA
#   Luego nuestra adaptacion estima:
#   100 * sum(w_i * I(12<=edad<=17) * nbi_i) /
#         sum(w_i), sobre toda la poblacion del canton-anio
#   Se usan enemdu_join_nbi_sources(), enemdu_build_nbi_components() y
<<<<<<< HEAD
#   enemdu_build_nbi_flags(). El perfil disponible es enemdu_2025_anual
#   IMPORTANTE: la escolaridad de la jefatura es una aproximacion
#   El helper de esta version asigna 0 si p10a=1; usa p10b si p10a<=2,
#   6+p10b si p10a=3 y 12+p10b si p10a>=4 (con entradas disponibles).
#   Esa aproximacion debe contrastarse con los niveles reales del cuestionario.
#   Trasladar el perfil a 2021-2024 no esta validado. Se etiqueta PROVISIONAL.
=======
#   enemdu_build_nbi_flags(). El perfil disponible es enemdu_2025_anual.
#   Escolaridad de la jefatura (comp5): correccion propia. El paquete asigna
#   6+p10b si p10a=3 y 12+p10b si p10a>=4, pero en el cuestionario 4 es
#   Primaria y 5 Educacion Basica. Se recalcula con los niveles reales de la
#   pregunta de nivel de instruccion:
#     1 Ninguno -> 0                 2 Centro de alfabetizacion -> p10b
#     3 Jardin de infantes -> 0      4 Primaria -> p10b
#     5 Educacion Basica -> p10b     6 Secundaria -> 6 + p10b
#     7 Media/Bachillerato -> 10 + p10b     8, 9 y 10 -> 12 + p10b
#   comp5 = 1 si la jefatura tiene 2 o menos anios y hay mas de 3 personas
#   por ocupado. La conversion a anios es elaboracion propia; la regla es la
#   del INEC (2025b, Anexo 2, Tabla 1).
#   El perfil de 2025 se aplica a 2021-2024. Los codigos se compararon con los
#   formularios de dic-2021, abr-2022, abr-2023, feb-2024, dic-2024 y oct-2025
#   (en lo que se pudo leer automaticamente). Esto no valida las cifras de NBI:
#   se mantiene la etiqueta PROVISIONAL.
>>>>>>> 8461c56 (actualizacion 24/08)
#   Estas son reglas implementadas en esta version, no certificacion INEC.
#   El adolescente con NBI desconocido queda NA y se omite del calculo; el canton se conserva
#   El denominador es la poblacion total del canton-anio.
#
# 4.5 MONOPARENTALIDAD: FORMULA EXTERNA + APROXIMACION PROPIA
#   Referencia publicada CDC/ATSDR, SVI 2018, pagina 15, EP_SNGPNT:
#   100 * hogares monoparentales con hijos <18 / total de hogares.
#   https://www.atsdr.cdc.gov/place-health/media/pdfs/2024/08/SVI2018Documentation_01192022_1.pdf#page=15
#   enemduR NO implementa esa clasificacion. Nuestro proxy observable usa:
#   jefatura (p04=1), ningun conyuge residente (p04=2) y algun hijo/a (p04=3)
#   menor de 18. Se permiten otros convivientes. Una fila y un peso por hogar.
#   100 * sum(w_h * proxy_h) / sum(w_h), en hogares clasificables.

#
# 4.7 PRECISION Y SALIDA
#   enemdu_survey_proportion() utiliza survey y enemdu_evaluate_precision().
#   Conservamos SE, IC95, CV, n muestral, n expandida y diagnostico.
#   CV = SE/abs(estimacion); si estimacion=0, no se interpreta un CV definido.
#   El paquete aproxima n efectiva con n/deff; si falta deff, utiliza n.
#   Los intervalos son Wald; pueden salir de 0-100 y no se recortan a escondidas.
#   Una n expandida grande no reemplaza una muestra suficiente.
#   Los cantones no son dominios de diseno garantizados.
#   Ceros/unos muestrales se marcan sin_variacion_muestral, no certeza real.
#   Fuente del codigo del paquete:
#   https://github.com/yerovi84/enemduR/tree/v0.1.0-alpha.2/R
#
# 5. FUNCIONES PROPIAS DE LECTURA, VALIDACION Y CALCULO ------------------------

# Adaptaciones del proyecto. No modifica funciones de enemduR.
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
  # Datos del diseno que usa la GVF (SCRIP_ESTIMACION_AREAS_PEQUENAS.R): suma de
  # factores de expansion del denominador, numero de UPM y numero de estratos
  # con una sola UPM, contados en los registros validos del canton.
  salida$denominador_expandido <- NA_real_
  salida$upm_denominador <- NA_integer_
  salida$estratos_upm_solitaria <- NA_integer_
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
  # NBI: no lleva regla de denominador aqui; el adolescente sin dato se omite.
  revisar_denominador <- indicador %in% c("desempleo_general",
                                          "inasistencia_12_17", "pobreza_ingresos", "monoparental_jefatura_aproximacion")
  for (k in seq_len(nrow(salida))) {
    ix <- d$codigo_canton == salida$codigo_canton[k] &
      is.finite(d[[valor]]) & is.finite(d$fexp) & d$fexp > 0
    n_validas <- sum(ix)
    denominador <- sum(d$fexp[ix])
    upm_por_estrato <- tapply(as.character(d$upm[ix]), as.character(d$estrato[ix]), function(x) length(unique(x)))
    salida$denominador_expandido[k] <- denominador
    salida$upm_denominador[k] <- length(unique(d$upm[ix]))
    salida$estratos_upm_solitaria[k] <- sum(upm_por_estrato == 1)
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
  # comp5 con la escolaridad corregida (seccion 4.4).
  anios_estudio <- ifelse(p$p10a %in% c(1, 3), 0,
                   ifelse(p$p10a %in% c(2, 4, 5), p$p10b,
                   ifelse(p$p10a == 6, 6 + p$p10b,
                   ifelse(p$p10a == 7, 10 + p$p10b, 12 + p$p10b))))
  jefes <- which(p$p04 == 1)
  jefes <- jefes[!duplicated(p$id_hogar[jefes])]
  anios_jefatura <- anios_estudio[jefes][match(p$id_hogar, p$id_hogar[jefes])]
  ocupados_hogar <- ave(as.numeric(p$empleo %in% 1), p$id_hogar, FUN = sum)
  personas_por_ocupado <- ifelse(ocupados_hogar == 0, Inf, p$hsize / ocupados_hogar)
  stopifnot(!anyNA(anios_jefatura), !anyNA(personas_por_ocupado))
  p$comp5 <- as.integer(anios_jefatura <= 2 & personas_por_ocupado > 3)
  p <- enemdu_build_nbi_flags(p)
  p$nbi_adolescente <- ifelse(is.na(p$p03), NA_integer_, ifelse(edad_ok, p$nbi, 0L))
  # El adolescente sin NBI queda NA y se omite del calculo (denominador total).
  # El canton se conserva; su faltante se informa en muestra_R.
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
  # Conservar variables originales
  originales <- names(fuente$personas)
  derivadas <- c("labor_pea", "labor_desempleo", "inasistencia", "desempleo_panel",
                 "pobreza_panel", paste0("comp", 1:5), "knbi", "nbi", "nbi_adolescente")
  p <- as.data.frame(p[, unique(c(originales, derivadas))])
  list(indicadores = do.call(rbind, bloques), muestra = muestra,
       personas_calculadas = p, hogares_calculados = h)
}
# 6. PARAMETROS ---------------------------------------------------------------
if (!exists("anios_panel")) anios_panel <- 2021:2025
if (!exists("exportar_tabla")) exportar_tabla <- TRUE
stopifnot(all(anios_panel %in% 2021:2025))
carpeta_salida <- file.path(raiz_tesis, "DATAS_PROCESADAS")
dir.create(carpeta_salida, recursive = TRUE, showWarnings = FALSE)
opciones_encuesta <- options(survey.lonely.psu = "adjust", survey.adjust.domain.lonely = TRUE)
poblacion_cantonal <- leer_territorios(raiz_tesis) %>%
  filter(anio %in% anios_panel) %>%
  mutate(anio = as.integer(anio), codigo_canton = as.character(codigo_canton))
mostrar_etapa("6. Periodo y catalogo territorial", resumir_filas_consola(poblacion_cantonal))

# 7. LECTURA DE MICRODATOS ----------------------------------------------------
# Cada fila es una persona (o vivienda)
datos_por_anio <- anios_panel %>%
  set_names() %>%
  map(\(anio) {
    fuente <- cargar_anio_panel(raiz_tesis, anio)
    mostrar_etapa(paste("7. Microdatos", anio), tibble(
      base = c("Personas", "Vivienda"),
      filas = c(nrow(fuente$personas), nrow(fuente$vivienda)),
      columnas = c(ncol(fuente$personas), ncol(fuente$vivienda))))
    fuente
  })
# Agrega la columna anio a las bases de personas y hogares antes de unirlas.
agregar_anio <- function(d, anio) mutate(as.data.frame(d), anio = as.integer(anio))
datos_extraidos <- do.call(rbind, Map(\(d, a) agregar_anio(d$personas, a), datos_por_anio, anios_panel))
vivienda_extraida <- do.call(rbind, Map(\(d, a) agregar_anio(d$vivienda, a), datos_por_anio, anios_panel))

# 8. CONSTRUCCION Y ESTIMACION DE INDICADORES ---------------------------------
# Conceptos, variables y formulas: ver 4.1-4.7. Sin filtro de tamano de muestra:
# todo canton-anio queda en la tabla; si no hay denominador, el indicador es NA.
resultados_por_anio <- anios_panel %>%
  set_names() %>%
  map(\(anio_actual) {
    resultado <- suppressWarnings(calcular_anio_panel(
      datos_por_anio[[as.character(anio_actual)]],
      filter(poblacion_cantonal, anio == anio_actual), anio_actual))
    mostrar_etapa(paste("8. Indicadores", anio_actual), resultado$indicadores %>%
                    group_by(indicador = indicator_id) %>%
                    summarise(cantones = n(),
                              disponibles = sum(is.finite(porcentaje_exploratorio)),
                              faltantes = sum(!is.finite(porcentaje_exploratorio)),
                              ceros = sum(porcentaje_exploratorio == 0, na.rm = TRUE)))
    resultado
  })
options(opciones_encuesta)
datos_con_calculos <- do.call(rbind, Map(\(d, a) agregar_anio(d$personas_calculadas, a), resultados_por_anio, anios_panel))
hogares_con_calculos <- do.call(rbind, Map(\(d, a) agregar_anio(d$hogares_calculados, a), resultados_por_anio, anios_panel))
muestra_R <- resultados_por_anio %>% map(\(x) x$muestra) %>% list_rbind()

# Hogares y personas encuestados por provincia, canton y anio.
muestra_por_canton <- datos_con_calculos %>%
  group_by(anio, codigo_canton) %>%
  summarise(hogares = n_distinct(id_hogar), personas = n(), .groups = "drop") %>%
  left_join(distinct(poblacion_cantonal, codigo_canton, provincia, canton), by = "codigo_canton")
mostrar_etapa("8. Totales por provincia y anio", muestra_por_canton %>%
                group_by(provincia, anio) %>%
                summarise(cantones = n(), hogares = sum(hogares), personas = sum(personas), .groups = "drop"))
mostrar_etapa("8. Hogares por canton y anio", muestra_por_canton %>%
                select(provincia, canton, anio, hogares) %>%
                pivot_wider(names_from = anio, values_from = hogares, names_prefix = "hogares_") %>%
                arrange(provincia, canton))
mostrar_etapa("8. Personas por canton y anio", muestra_por_canton %>%
                select(provincia, canton, anio, personas) %>%
                pivot_wider(names_from = anio, values_from = personas, names_prefix = "personas_") %>%
                arrange(provincia, canton))

# Estimadores originales: una fila por indicador, canton y anio.
# Porcentajes e intervalos en 0-100; error estandar en puntos porcentuales;
# CV en porcentaje (100 * SE / estimacion), indefinido si la estimacion es 0.
# muestra_denominador = registros validos del denominador de cada indicador:
#   PERSONAS en inasistencia (adolescentes 12-17 con p07 = 1 o 2), desempleo (PEA),
#   pobreza (personas con pobreza 0 o 1) y NBI (adolescentes del cantón y año analizados con dato válido de NBI),
#   HOGARES en monoparentalidad (una fila por hogar clasificable).
estimadores <- resultados_por_anio %>%
  map(\(x) x$indicadores %>%
        as_tibble() %>%
        select(anio, codigo_canton, indicator_id, porcentaje_exploratorio, error_estandar_pp,
               ic95_inferior, ic95_superior, estimate, standard_error,
               unweighted_n, degrees_freedom, denominador_expandido,
               upm_denominador, estratos_upm_solitaria)) %>%
  list_rbind() %>%
  mutate(anio = as.integer(anio), codigo_canton = as.character(codigo_canton)) %>%
  left_join(select(poblacion_cantonal, anio, codigo_provincia, provincia, codigo_canton, canton),
            by = c("anio", "codigo_canton")) %>%
  transmute(anio, codigo_provincia, provincia, codigo_canton, canton,
            indicador = indicator_id,
            valor_pct = porcentaje_exploratorio,
            error_estandar_pp,
            limite_inferior_pct = ic95_inferior,
            limite_superior_pct = ic95_superior,
            cv_pct = if_else(is.finite(estimate) & estimate != 0,
                             100 * standard_error / abs(estimate), NA_real_),
            muestra_denominador = unweighted_n,
            grados_libertad = degrees_freedom,
            denominador_expandido, upm_denominador, estratos_upm_solitaria)
stopifnot(!anyNA(estimadores$canton),
          !anyDuplicated(estimadores[, c("anio", "codigo_canton", "indicador")]))
mostrar_etapa("8. Estimadores originales", estimadores %>%
                summarise(filas = n(), indicadores = n_distinct(indicador),
                          con_valor = sum(is.finite(valor_pct)),
                          con_error_cero = sum(error_estandar_pp == 0, na.rm = TRUE)))

# 8.1 CRITERIOS DE PRECISION INEC (marcas en el objeto estimadores)
# Se evalua la calidad de cada estimacion directa canton-anio. Criterios:
# denominador >= 60, grados de libertad >= 9 y error estandar menor o igual al
# maximo tolerable, con p en escala 0-1:
#   raiz_cubica(p^2) / 9 si p < 0.5;  raiz_cubica((1 - p)^2) / 9 si p >= 0.5.
# Fuente: INEC (2025), Aproximacion metodologica para la evaluacion de la precision
# de la inferencia estadistica en encuestas de hogares con disenos complejos,
# pp. 22-23.
# Un error estandar de cero (menor a 1e-10 pp) no cumple: significa falta de
# variacion en la muestra, no precision. Solo marcan; no excluyen filas.
# En la estimacion en areas pequenas entran todas las estimaciones con valor:
# el estimador Fay-Herriot pondera cada una segun su varianza (Corral et al.,
# 2022, p. 21); estas marcas sirven para describir la calidad del dato directo.
# En SCRIP_ESTIMACION_AREAS_PEQUENAS.R (seccion 2) la marca de grados de
# libertad ayuda a elegir las areas con las que se calcula el efecto de diseno
# de la GVF. No excluye ningun canton-anio.
estimadores <- estimadores %>%
  mutate(error_maximo_tolerable_pp =
           100 * pmin(valor_pct / 100, 1 - valor_pct / 100)^(2 / 3) / 9,
         cumple_muestra = muestra_denominador >= 60,
         cumple_gl = grados_libertad >= 9,
         cumple_error = error_estandar_pp > 1e-10 &
           error_estandar_pp <= error_maximo_tolerable_pp)
resumen_precision <- estimadores %>%
  filter(is.finite(valor_pct)) %>%
  group_by(indicador) %>%
  summarise(estimaciones = n(),
            error_cero = sum(error_estandar_pp <= 1e-10, na.rm = TRUE),
            muestra_60 = sum(cumple_muestra, na.rm = TRUE),
            gl_9 = sum(cumple_gl, na.rm = TRUE),
            error_tolerable = sum(cumple_error, na.rm = TRUE),
            cumplen_los_tres = sum(cumple_muestra & cumple_gl & cumple_error, na.rm = TRUE))
mostrar_etapa("8.1 Criterios de precision INEC", resumen_precision)

# 9. INDICADORES ORIGINALES POR CANTON-ANIO -----------------------------------
# Cinco porcentajes por canton-anio, con sus ceros y sus NA.
nombres_indicadores <- c(
  inasistencia_12_17 = "inasistencia_12_17_pct",
  desempleo_general = "desempleo_pct",
  pobreza_ingresos = "pobreza_ingresos_pct",
  adolescentes_nbi_sobre_total_provisional = "adolescentes_nbi_sobre_total_provisional_pct",
  monoparental_jefatura_aproximacion = "monoparental_jefatura_aproximacion_pct")
columnas_indicadores <- unname(nombres_indicadores)
claves_territorio <- c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")
# Convierte las estimaciones (una fila por indicador) a la tabla de indicadores (una columna por indicador).
a_formato_ancho <- function(tabla, valor) {
  tabla %>%
    mutate(columna = unname(nombres_indicadores[indicador])) %>%
    select(anio, codigo_canton, columna, all_of(valor)) %>%
    pivot_wider(names_from = columna, values_from = all_of(valor))
}
indicadores_agregados <- poblacion_cantonal %>%
  select(all_of(claves_territorio)) %>%
  left_join(a_formato_ancho(estimadores, "valor_pct"), by = c("anio", "codigo_canton")) %>%
  select(all_of(c(claves_territorio, columnas_indicadores)))
mostrar_etapa("9. Indicadores por anio", resumir_filas_consola(indicadores_agregados))
mostrar_etapa("Disponibilidad y ceros por indicador",
              resumir_indicadores_consola(indicadores_agregados, columnas_indicadores))
if (isTRUE(exportar_tabla)) {
  writexl::write_xlsx(list(Indicadores = indicadores_agregados),
                      file.path(carpeta_salida, "construccion_indicadores_factores.xlsx"))
  mostrar_etapa("Excel guardado", tibble(
    archivo = "construccion_indicadores_factores.xlsx", filas = nrow(indicadores_agregados)))
}

# 10. ESTIMACIONES ORIGINALES EN EXCEL ----------------------------------------
# Una fila por indicador, canton y anio: porcentaje, error estandar, IC95, CV,
<<<<<<< HEAD
# n del denominador y grados de libertad. Sin suavizar. 
=======
# n del denominador, grados de libertad, datos del diseno (factores de
# expansion, UPM y estratos con una sola UPM) y marcas de precision INEC (8.1).
# Sin suavizar. Es el insumo de SCRIP_ESTIMACION_AREAS_PEQUENAS.R.
>>>>>>> 8461c56 (actualizacion 24/08)
if (isTRUE(exportar_tabla)) {
  writexl::write_xlsx(list(Estimaciones_originales = estimadores),
                      file.path(carpeta_salida, "estimaciones_originales.xlsx"))
  mostrar_etapa("Excel guardado", tibble(
    archivo = "estimaciones_originales.xlsx", filas = nrow(estimadores)))
}
<<<<<<< HEAD


# PARTE 2: ESTIMACION EN AREAS PEQUENAS (SUAVIZADO)
#   estimadores           porcentaje, error estandar, IC, n y grados de libertad
#                         por indicador, canton y anio (sin suavizar).
#   indicadores_agregados indicadores originales (para conservar columnas y NA).
#   nombres_indicadores, columnas_indicadores, claves_territorio, a_formato_ancho,
#   mostrar_etapa, resumir_indicadores_consola, biblioteca_local, carpeta_salida
#   y exportar_tabla.
# Los pesos originales de la ENEMDU no se modifican: no hay calibracion. Los
# cantones no son dominios de diseno y sus estimaciones directas son imprecisas;
# por eso se aplica estimacion en areas pequenas sobre los indicadores ya
# construidos. Auxiliares del modelo: solo provincia y anio.
# Orden: (12) marcas de precision, (13) suavizado del error estandar con una
# funcion generalizada de la varianza (GVF), (14) Fay-Herriot, (15) salidas.
# Fuentes verificadas de esta parte:
#   INEC (2018). Estimacion en areas pequenas de la desnutricion cronica infantil
#     en Ecuador [Presentacion]. CEPAL. (Fay-Herriot con GVF a nivel cantonal con
#     encuestas de representatividad provincial; trata ECV y ENSANUT, no ENEMDU.)
#   INEC (2025). Aproximacion metodologica para la evaluacion de la precision de
#     la inferencia estadistica en encuestas de hogares con disenos complejos.
#   You y Hidiroglou (2024). Application of sampling variance smoothing methods
#     for small area proportion estimation. Statistics Canada.
#   You (2021). Survey Methodology, 47(2).
#   Fay y Herriot (1979). Journal of the American Statistical Association, 74(366).
#   Molina y Marhuenda (2015). The R Journal, 7(1).
stopifnot(exists("estimadores"), exists("indicadores_agregados"),
          exists("a_formato_ancho"), exists("nombres_indicadores"),
          exists("biblioteca_local"), exists("carpeta_salida"))
stopifnot(all(c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton",
                "indicador", "valor_pct", "error_estandar_pp", "limite_inferior_pct",
                "limite_superior_pct", "cv_pct", "muestra_denominador",
                "grados_libertad") %in% names(estimadores)))
mostrar_etapa("Parte 2. Objetos recibidos de la Parte 1", tibble(
  objeto = c("estimadores", "indicadores_agregados"),
  filas = c(nrow(estimadores), nrow(indicadores_agregados))))

# 11. LIBRERIA DE AREAS PEQUENAS -----------------------------------------------
# sae: ajuste del modelo Fay-Herriot (sae::mseFH, REML) y error cuadratico medio
# del valor suavizado. Fuente: Molina y Marhuenda (2015), The R Journal, 7(1).
Se llama siempre con el prefijo sae::.
if (!requireNamespace("sae", quietly = TRUE)) {
  install.packages("sae", lib = biblioteca_local, dependencies = NA)
}
if (!requireNamespace("sae", quietly = TRUE)) stop("No se pudo instalar: sae")
mostrar_etapa("11. Paquete de areas pequenas cargado", data.frame(
  paquete = "sae", version = as.character(utils::packageVersion("sae"))))

# 12. CRITERIOS DE PRECISION INEC (marcas en el objeto estimadores) -------------
# Criterios: denominador >= 60, grados de libertad >= 9 y error estandar menor o
# igual al maximo tolerable, con p en escala 0-1:
#   raiz_cubica(p^2) / 9 si p < 0.5;  raiz_cubica((1 - p)^2) / 9 si p >= 0.5.
# Fuente: INEC (2025), Aproximacion metodologica para la evaluacion de la precision
# de la inferencia estadistica en encuestas de hogares con disenos complejos,
# pp. 22-23.
# Un error estandar de cero (menor a 1e-10 pp) no cumple: significa falta de
# variacion en la muestra, no precision. Solo marcan; no excluyen filas.
estimadores <- estimadores %>%
  mutate(error_maximo_tolerable_pp =
           100 * pmin(valor_pct / 100, 1 - valor_pct / 100)^(2 / 3) / 9,
         cumple_muestra = muestra_denominador >= 60,
         cumple_gl = grados_libertad >= 9,
         cumple_error = error_estandar_pp > 1e-10 &
           error_estandar_pp <= error_maximo_tolerable_pp)
resumen_precision <- estimadores %>%
  filter(is.finite(valor_pct)) %>%
  group_by(indicador) %>%
  summarise(estimaciones = n(),
            error_cero = sum(error_estandar_pp <= 1e-10, na.rm = TRUE),
            muestra_60 = sum(cumple_muestra, na.rm = TRUE),
            gl_9 = sum(cumple_gl, na.rm = TRUE),
            error_tolerable = sum(cumple_error, na.rm = TRUE),
            cumplen_los_tres = sum(cumple_muestra & cumple_gl & cumple_error, na.rm = TRUE))
mostrar_etapa("12. Criterios de precision INEC", resumen_precision)

# 13. SUAVIZADO DEL ERROR ESTANDAR (GVF) ---------------------------------------
# Con pocas personas encuestadas el error estandar directo sale en cero o muy
# inestable, y Fay-Herriot lo trata como conocido. Se reemplaza por el error
# esperado segun el tamano de muestra, con una funcion generalizada de la varianza:
#   log(V_i) = b0 + b1 log(n_i) + e_i,  e_i ~ N(0, tau2)
# V_i = (error estandar)^2 en escala 0-1; n_i = registros del denominador del
# indicador (muestra_denominador: personas, u hogares en monoparentalidad).
# Al volver de la escala logaritmica se multiplica por exp(tau2 / 2) (correccion
# de Rivest y Belmonte) para no subestimar la varianza promedio.
# Fuente: You y Hidiroglou (2024), ecuaciones 4, 6 y 7; forma log-lineal tambien
# en You (2021), seccion 3.
# El GVF se ajusta solo con V_i > 0 (el log de 0 no existe; errores menores a
# 1e-10 pp son cero numerico) y predice V_i en todas las celdas con valor.
# Los NA no entran al calculo y se reponen como NA; ninguna fila se elimina.
suavizar_varianza <- function(d) {
  d <- filter(d, is.finite(valor_pct))
  usa <- is.finite(d$error_estandar_pp) & d$error_estandar_pp > 1e-10 & d$muestra_denominador > 0
  stopifnot(sum(usa) > 2)
  gvf <- lm(log((error_estandar_pp / 100)^2) ~ log(muestra_denominador), data = d[usa, ])
  tau2 <- summary(gvf)$sigma^2
  d$vardir <- as.numeric(exp(predict(gvf, d) + tau2 / 2))
  d$usa_gvf <- usa
  stopifnot(all(is.finite(d$vardir)), all(d$vardir > 0))
  diagnostico <- tibble(indicador = first(d$indicador), celdas_con_valor = nrow(d),
                        celdas_gvf = sum(usa), b0 = unname(coef(gvf)[1]),
                        b1 = unname(coef(gvf)[2]), tau2 = tau2, factor_rb = exp(tau2 / 2))
  list(datos = d, diagnostico = diagnostico)
}
resultado_varianza <- estimadores %>% group_split(indicador) %>% map(suavizar_varianza)
datos_varianza <- resultado_varianza %>% map("datos") %>% list_rbind()
diagnostico_varianza <- resultado_varianza %>% map("diagnostico") %>% list_rbind()
mostrar_etapa("13. Suavizado del error estandar (GVF)", diagnostico_varianza)

# 14. FAY-HERRIOT POR INDICADOR ------------------------------------------------
# Modelo de area, cada canton-anio es un area:
#   y_i = x_i'b + v_i + e_i,  v_i ~ N(0, s2_v),  e_i con varianza V_i conocida
# (la V_i suavizada de la seccion 13). Valor suavizado (EBLUP):
#   g_i * y_i + (1 - g_i) * x_i'b,  g_i = s2_v / (s2_v + V_i)
# Si el dato del canton es confiable (V_i baja), g_i es alto y el resultado queda
# cerca del dato original; si tiene poca muestra (V_i alta), se acerca al valor
# esperado de su provincia y anio. x_i = provincia + anio. Estimacion REML con
# sae::mseFH; si no converge, metodo FH. 
# Fuentes: Fay y Herriot (1979), el modelo; You (2021), por que suavizar la
# varianza antes del modelo; Molina y Marhuenda (2015), sae::mseFH.
ajustar_fay_herriot <- function(d) {
  prop <- d$valor_pct / 100
  vardir <- d$vardir
  # sae::mseFH toma estas variables del entorno de la funcion.
  prov_f <- factor(d$codigo_provincia)
  anio_f <- factor(d$anio)
  ajuste <- NULL
  for (metodo in c("REML", "FH")) {
    intento <- tryCatch(sae::mseFH(prop ~ prov_f + anio_f, vardir = vardir, method = metodo),
                        error = \(e) NULL)
    if (!is.null(intento) && isTRUE(intento$est$fit$convergence)) { ajuste <- intento; break }
  }
  if (is.null(ajuste)) stop("No convergio Fay-Herriot: ", unique(d$indicador))
  eblup <- as.numeric(unlist(ajuste$est$eblup))
  mse <- as.numeric(unlist(ajuste$mse))
  refvar <- as.numeric(unlist(ajuste$est$fit$refvar))[1]
  stopifnot(length(eblup) == nrow(d), length(mse) == nrow(d), is.finite(refvar))
  
  datos <- d %>%
    transmute(anio, codigo_canton, indicador,
              error_gvf_pp = 100 * sqrt(vardir),
              valor_suavizado_pct = 100 * eblup,
              error_suavizado_pp = 100 * sqrt(pmax(mse, 0)),
              peso_dato_original = refvar / (refvar + vardir),
              fuera_de_rango = as.integer(eblup < 0 | eblup > 1),
              cambio_abs_pp = abs(valor_suavizado_pct - valor_pct))
  diagnostico <- datos %>%
    summarise(indicador = first(indicador),
              varianza_entre_cantones_pp2 = 1e4 * refvar,
              peso_medio = mean(peso_dato_original), fuera_de_rango = sum(fuera_de_rango),
              cambio_medio_pp = mean(cambio_abs_pp), cambio_maximo_pp = max(cambio_abs_pp),
              metodo = metodo)
  list(datos = select(datos, -cambio_abs_pp), diagnostico = diagnostico)
}
resultado_fh <- datos_varianza %>% group_split(indicador) %>% map(ajustar_fay_herriot)
suavizado <- resultado_fh %>% map("datos") %>% list_rbind()
diagnostico_suavizado <- diagnostico_varianza %>%
  left_join(resultado_fh %>% map("diagnostico") %>% list_rbind(), by = "indicador")
mostrar_etapa("14. Fay-Herriot por indicador", diagnostico_suavizado)

# 15. TABLAS DE SALIDA ---------------------------------------------------------
estimaciones_comparacion <- estimadores %>%
  left_join(suavizado, by = c("anio", "codigo_canton", "indicador")) %>%
  transmute(anio, codigo_provincia, provincia, codigo_canton, canton, indicador,
            muestra_denominador, grados_libertad,
            valor_original_pct = valor_pct,
            error_original_pp = error_estandar_pp,
            error_cero_numerico = as.integer(error_estandar_pp < 1e-10),
            limite_inferior_pct, limite_superior_pct, cv_pct, error_maximo_tolerable_pp,
            error_gvf_pp, valor_suavizado_pct, error_suavizado_pp,
            peso_dato_original, fuera_de_rango,
            cambio_pp = valor_suavizado_pct - valor_pct) %>%
  arrange(anio, codigo_provincia, codigo_canton, indicador)

# Indicadores suavizados: mismas columnas y filas que los originales; NA se mantienen.
indicadores_suavizados <- indicadores_agregados %>%
  select(all_of(claves_territorio)) %>%
  left_join(a_formato_ancho(estimaciones_comparacion, "valor_suavizado_pct"),
            by = c("anio", "codigo_canton")) %>%
  select(all_of(names(indicadores_agregados)))
stopifnot(nrow(indicadores_suavizados) == nrow(indicadores_agregados),
          all(is.na(as.matrix(select(indicadores_suavizados, all_of(columnas_indicadores)))) ==
                is.na(as.matrix(select(indicadores_agregados, all_of(columnas_indicadores))))))
mostrar_etapa("15. Disponibilidad de indicadores suavizados",
              resumir_indicadores_consola(indicadores_suavizados, columnas_indicadores))
if (isTRUE(exportar_tabla)) {
  writexl::write_xlsx(list(Estimaciones = estimaciones_comparacion),
                      file.path(carpeta_salida, "estimaciones_original_vs_suavizado.xlsx"))
  writexl::write_xlsx(list(Indicadores_suavizados = indicadores_suavizados),
                      file.path(carpeta_salida, "indicadores_suavizados.xlsx"))
  mostrar_etapa("Excel guardados", tibble(
    archivo = c("estimaciones_original_vs_suavizado.xlsx", "indicadores_suavizados.xlsx"),
    filas = c(nrow(estimaciones_comparacion), nrow(indicadores_suavizados))))
}
=======
>>>>>>> 8461c56 (actualizacion 24/08)
