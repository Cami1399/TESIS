# ============================================================================
# DATA MAESTRA: UNION DE FACTORES LIMPIOS, POBLACION Y CONTEO
# Ejecutar despues de 01_Construccion_Indicadores.R.
# No lee microdatos, no recalcula indicadores ni estimadores.
# ============================================================================
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

raiz_tesis <- "C:/Users/camil/OneDrive/Desktop/TESIS"
carpeta_datos <- file.path(raiz_tesis, "DATAS_PROCESADAS")
for (paquete in c("readxl", "writexl")) {
  if (!requireNamespace(paquete, quietly = TRUE)) stop("Instale el paquete: ", paquete)
}
if (!exists("exportar_tabla")) exportar_tabla <- TRUE
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

leer_insumos_cantonales <- function(raiz) {
  p <- leer_poblacion_panel(file.path(raiz, "DATAS_PROCESADAS",
                                      "poblacion_cantonal_costa_total_12_17_2021_2025.xlsx"))
  c <- as.data.frame(readxl::read_excel(file.path(raiz, "DATAS_PROCESADAS",
                                                  "Conteo_cantonal_2021_2025.xlsx"), sheet = "Sheet1"))
  requeridas <- c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton", "total_aprehensiones")
  if (!all(requeridas %in% names(c))) stop("Faltan columnas del conteo: ", paste(setdiff(requeridas, names(c)), collapse = ", "))
  c$anio <- as.integer(c$anio)
  c$codigo_canton <- codigo_dpa(c$codigo_canton, 4)
  c$codigo_provincia <- codigo_dpa(c$codigo_provincia, 2)
  validar_claves(c)
  stopifnot(all(is.finite(c$total_aprehensiones) & c$total_aprehensiones >= 0),
            all(c$total_aprehensiones == floor(c$total_aprehensiones)))
  list(poblacion = p, conteos = c)
}

# 1. LEER SOLO EL EXCEL LIMPIO DE FACTORES ------------------------------------
archivo_factores <- file.path(carpeta_datos, "construccion_indicadores_factores.xlsx")
if (!file.exists(archivo_factores)) stop("Ejecute primero 01_Construccion_Indicadores.R.")
factores <- as.data.frame(readxl::read_excel(archivo_factores, sheet = "Indicadores_limpios"))
factores$codigo_canton <- codigo_dpa(factores$codigo_canton, 4)
factores$codigo_provincia <- codigo_dpa(factores$codigo_provincia, 2)
validar_claves(factores)
columnas_indicadores <- c("inasistencia_12_17_pct", "desempleo_pct", "pobreza_ingresos_pct",
                          "adolescentes_nbi_sobre_total_provisional_pct", "monoparental_jefatura_aproximacion_pct")
stopifnot(all(columnas_indicadores %in% names(factores)),
          all(vapply(factores[, columnas_indicadores, drop = FALSE], function(x) all(is.finite(x)), logical(1))))
mostrar_etapa("1. Factores limpios leidos", data.frame(
  filas = nrow(factores), columnas = ncol(factores),
  cantones = length(unique(factores$codigo_canton)), provincias = length(unique(factores$codigo_provincia))))
mostrar_etapa("Factores disponibles por anio", resumir_filas_consola(factores))
insumos <- leer_insumos_cantonales(raiz_tesis)
mostrar_etapa("Insumos externos leidos y claves comprobadas", data.frame(
  base = c("Poblacion", "Conteos"), filas = c(nrow(insumos$poblacion), nrow(insumos$conteos))))

# 2. EMPAREJAR POR CANTON Y ANIO ---------------------------------------------
# Conservar como respaldo todas las filas del Excel de factores limpios.
# No se introducen territorios adicionales desde poblacion ni conteo.
j_poblacion <- match(clave_panel(factores), clave_panel(insumos$poblacion))
j_conteo <- match(clave_panel(factores), clave_panel(insumos$conteos))
tabla_maestra_sin_filtrar <- factores[, c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")]
tabla_maestra_sin_filtrar$poblacion_12_17 <- insumos$poblacion$poblacion_12_17[j_poblacion]
tabla_maestra_sin_filtrar$total_aprehensiones <- insumos$conteos$total_aprehensiones[j_conteo]
tabla_maestra_sin_filtrar$aprehensiones_por_1000_adolescentes <-
  1000 * tabla_maestra_sin_filtrar$total_aprehensiones / tabla_maestra_sin_filtrar$poblacion_12_17
for (v in columnas_indicadores) tabla_maestra_sin_filtrar[[v]] <- factores[[v]]

mostrar_etapa("2. Resultado de la union antes de excluir", data.frame(
  filas = nrow(tabla_maestra_sin_filtrar),
  sin_coincidencia_poblacion = sum(is.na(j_poblacion)),
  sin_coincidencia_conteo = sum(is.na(j_conteo)),
  conteos_cero_registrados = sum(tabla_maestra_sin_filtrar$total_aprehensiones == 0, na.rm = TRUE)))
mostrar_etapa("Primeras filas de la union", utils::head(tabla_maestra_sin_filtrar, 6))
# 3. REVISAR COINCIDENCIAS: SIN NUEVO FILTRO MUESTRAL -------------------------
# Un cero registrado en conteo es valido; una clave no encontrada queda NA.
revision_integracion_R <- factores[, c("anio", "codigo_provincia", "provincia", "codigo_canton", "canton")]
revision_integracion_R$coincide_poblacion <- !is.na(j_poblacion)
revision_integracion_R$coincide_conteo <- !is.na(j_conteo)
conservar_fila <- !is.na(j_poblacion) & !is.na(j_conteo)
revision_integracion_R$conservar <- conservar_fila
revision_integracion_R$motivo <- ifelse(conservar_fila, "conservado",
                                        ifelse(is.na(j_poblacion) & is.na(j_conteo), "sin_poblacion_y_conteo",
                                               ifelse(is.na(j_poblacion), "sin_coincidencia_poblacion", "sin_coincidencia_conteo")))
filas_sin_coincidencia_R <- tabla_maestra_sin_filtrar[!conservar_fila, , drop = FALSE]
tabla_maestra_limpia <- tabla_maestra_sin_filtrar[conservar_fila, , drop = FALSE]
rownames(tabla_maestra_limpia) <- NULL
tabla_maestra_cantonal <- tabla_maestra_limpia
stopifnot(nrow(tabla_maestra_sin_filtrar) == nrow(tabla_maestra_limpia) + nrow(filas_sin_coincidencia_R),
          !anyDuplicated(clave_panel(tabla_maestra_limpia)), !anyNA(tabla_maestra_limpia))

mostrar_etapa("3. Conservacion y exclusiones por coincidencia", as.data.frame(table(
  resultado = revision_integracion_R$motivo)))
mostrar_etapa("Filas finales por anio", resumir_filas_consola(tabla_maestra_limpia))
mostrar_etapa("Indicadores de la tabla maestra", resumir_indicadores_consola(tabla_maestra_limpia, columnas_indicadores))


# 4. RESUMENES EN R ---------------------------------------------------------
resumen_integracion_R <- data.frame(
  filas_factores = nrow(factores),
  filas_sin_coincidencia = sum(!conservar_fila),
  filas_finales = nrow(tabla_maestra_limpia))
print(resumen_integracion_R, row.names = FALSE)
print(summary(tabla_maestra_limpia))

cantones_finales_R <- unique(tabla_maestra_limpia[
  , c("codigo_provincia", "provincia", "codigo_canton"), drop = FALSE])

resumen_provincias_R <- aggregate(codigo_canton ~ codigo_provincia + provincia,
                                  data = cantones_finales_R, FUN = length)
names(resumen_provincias_R)[3] <- "numero_cantones"
print(resumen_provincias_R, row.names = FALSE)

resumen_cantones_R <- aggregate(anio ~ codigo_provincia + provincia + codigo_canton + canton,
                                data = tabla_maestra_limpia, FUN = length)
names(resumen_cantones_R)[5] <- "registros_canton_anio"
print(resumen_cantones_R, row.names = FALSE)

# EXPORTAR UNICAMENTE LA DATA MAESTRA FILTRADA -------------------------------
archivo_salida <- file.path(carpeta_datos, "Data_maestra_cantonal.xlsx")
writexl::write_xlsx(list(Data_maestra_cantonal = tabla_maestra_limpia),
                    path = archivo_salida)
mostrar_etapa("Data maestra cantonal exportada", data.frame(
  archivo = archivo_salida, filas = nrow(tabla_maestra_limpia),
  columnas = ncol(tabla_maestra_limpia)))