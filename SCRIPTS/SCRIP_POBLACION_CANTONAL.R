#SCRIP _POBLACION CANTONAL

# =============================================================================
# Población por cantón de la Costa del Ecuador, 2021-2025.
#   - Población de 12 a 17 años (adolescente)
#   - Población de 15 años y más (edad de trabajar, INEC 2023, p. 22)
#   - Población total (todas las edades)
# Fuente: INEC, Estimaciones y Proyecciones, Revisión 2024, hoja 3.1.
# Unidad: personas; sexo: Total (hombres y mujeres).
# Fecha de referencia: 30 de junio de cada año (nota de la hoja 3.1).
# Se mantiene la carpeta de origen y el destino del script provincial.
# El archivo cantonal tiene un nombre distinto al archivo provincial anterior.
# =============================================================================

# Ejecutar una sola vez si faltan paquetes:
# install.packages(c("readxl", "dplyr", "tidyr", "openxlsx"))
paquetes <- c("readxl", "dplyr", "tidyr", "openxlsx")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) stop("Instala primero: install.packages(c(",
                         paste(sprintf('"%s"', faltan), collapse = ", "), "))", call. = FALSE)

ruta_origen <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/Tabulados_Estimacion_Proyeccion_v2.xlsx"
ruta_destino <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"
hoja <- "3.1"
anios_interes <- 2021:2025
edades_interes <- 12:17          # población adolescente
edad_minima_trabajo <- 15        # población en edad de trabajar: 15 años y más
edades_todas <- 0:100            # 0 a "100 y más" (100 = 100 y más)
# Clasificación Costa: El Oro, Esmeraldas, Guayas, Los Ríos, Manabí,
# Santo Domingo de los Tsáchilas y Santa Elena. Se usan los códigos de la
# fuente, conservando su división territorial, sin imponer el catálogo 2026.
codigos_costa <- c("07", "08", "09", "12", "13", "23", "24")

if (!file.exists(ruta_origen)) stop("No existe el archivo: ", ruta_origen)
if (!hoja %in% readxl::excel_sheets(ruta_origen)) stop("No existe la hoja 3.1")

# Anclar en A1 evita que readxl recorte las filas/columnas vacías iniciales.
crudos <- readxl::read_excel(ruta_origen, sheet = hoja, range = readxl::cell_limits(c(1, 1), c(NA, NA)),
                             col_names = FALSE, col_types = "text", .name_repair = "minimal")
m <- as.matrix(crudos)
m[] <- trimws(m)
buscar_columna <- function(fila, etiqueta) {
  p <- which(!is.na(fila) & fila == etiqueta)
  if (length(p) != 1L) stop("Columna ausente o ambigua: ", etiqueta)
  p
}
fila_variables <- which(apply(m, 1, function(x)
  all(c("Código Provincia", "Provincia", "Código Cantón", "Cantón", "Edad") %in% x)))
if (length(fila_variables) != 1L) stop("No se identifica un encabezado cantonal único")
fv <- fila_variables[1]
cp <- buscar_columna(m[fv, ], "Código Provincia")
np <- buscar_columna(m[fv, ], "Provincia")
cc <- buscar_columna(m[fv, ], "Código Cantón")
nc <- buscar_columna(m[fv, ], "Cantón")
ce <- buscar_columna(m[fv, ], "Edad")

# En 3.1 cada año ocupa tres columnas: Hombre, Mujer y Total.
# Los años están en celdas combinadas: propagar a la derecha, no hacia abajo.
fila_anios <- which(apply(m, 1, function(x) all(as.character(anios_interes) %in% x)))
if (length(fila_anios) != 1L) stop("No se identifica una fila única con todos los años")
fa <- fila_anios[1]
anios <- m[fa, ]
ultimo <- NA_character_
for (j in seq_along(anios)) {
  if (!is.na(anios[j]) && grepl("^[12][0-9]{3}$", anios[j])) ultimo <- anios[j]
  anios[j] <- ultimo
}
fs <- fa + 1L
col_anios <- vapply(anios_interes, function(a) {
  p <- which(!is.na(anios) & anios == as.character(a) &
               !is.na(m[fs, ]) & m[fs, ] == "Total")
  if (length(p) != 1L) stop("Columna Total ausente o ambigua para ", a)
  p
}, integer(1))

# Se leen todas las edades simples (0 a "100 y más"); la fila "Total" de cada
# cantón no entra en el detalle y se usa solo como control.
# No sumar hombres, mujeres y Total juntos: únicamente se extrae la columna
# Total de cada año.
edad <- suppressWarnings(as.integer(m[, ce]))
edad[!is.na(m[, ce]) & grepl("^100", m[, ce])] <- 100L   # "100 y más" -> 100
filas <- which(seq_len(nrow(m)) > fs & m[, cp] %in% codigos_costa & edad %in% edades_todas)
if (!length(filas)) stop("No se encontraron edades en los cantones de la Costa")

# Filas "Total" de cada cantón (control de la suma de edades)
filas_total <- which(seq_len(nrow(m)) > fs & m[, cp] %in% codigos_costa & m[, ce] == "Total")
control_total <- dplyr::bind_rows(lapply(seq_along(anios_interes), function(k) {
  data.frame(ANIO = anios_interes[k], COD_CANTON = m[filas_total, cc],
             TOTAL_FUENTE = as.numeric(m[filas_total, col_anios[k]]),
             stringsAsFactors = FALSE)
}))
detalle <- dplyr::bind_rows(lapply(seq_along(anios_interes), function(k) {
  data.frame(ANIO = anios_interes[k], COD_PROVINCIA = m[filas, cp],
             PROVINCIA = m[filas, np], COD_CANTON = m[filas, cc], CANTON = m[filas, nc],
             EDADES = edad[filas], NUMERO_POBLACION = suppressWarnings(as.numeric(m[filas, col_anios[k]])),
             stringsAsFactors = FALSE)
}))
detalle <- dplyr::arrange(detalle, COD_PROVINCIA, COD_CANTON, ANIO, EDADES)

# Controles: no convertir faltantes a cero ni ocultarlos mediante na.rm=TRUE.
if (anyNA(detalle) || any(!is.finite(detalle$NUMERO_POBLACION)) ||
    any(detalle$NUMERO_POBLACION < 0) ||
    any(detalle$NUMERO_POBLACION != floor(detalle$NUMERO_POBLACION))) {
  stop("Hay valores faltantes, no numéricos, negativos o no enteros")
}
if (any(substr(detalle$COD_CANTON, 1, 2) != detalle$COD_PROVINCIA))
  stop("Código cantonal inconsistente con la provincia")
if (anyDuplicated(detalle[c("ANIO", "COD_CANTON", "EDADES")]))
  stop("Hay duplicados de cantón-año-edad")
cantones <- dplyr::distinct(detalle, COD_PROVINCIA, PROVINCIA, COD_CANTON, CANTON)
esperado <- tidyr::crossing(cantones, ANIO = anios_interes, EDADES = edades_todas)
if (nrow(dplyr::anti_join(esperado, detalle,
                          by = c("COD_PROVINCIA", "PROVINCIA", "COD_CANTON", "CANTON", "ANIO", "EDADES"))))
  stop("Faltan combinaciones de cantón, año o edad")
if (!setequal(unique(detalle$COD_PROVINCIA), codigos_costa))
  stop("No están presentes las siete provincias")

totales <- detalle |>
  dplyr::group_by(ANIO, COD_PROVINCIA, PROVINCIA, COD_CANTON, CANTON) |>
  dplyr::summarise(
    POBLACION_12_17  = sum(NUMERO_POBLACION[EDADES %in% edades_interes]),
    POBLACION_15_MAS = sum(NUMERO_POBLACION[EDADES >= edad_minima_trabajo]),
    POBLACION_TOTAL  = sum(NUMERO_POBLACION),
    .groups = "drop") |>
  dplyr::arrange(COD_PROVINCIA, COD_CANTON, ANIO)
stopifnot(nrow(detalle) == nrow(cantones) * length(anios_interes) * length(edades_todas),
          nrow(totales) == nrow(cantones) * length(anios_interes))

# Control: la suma de edades debe ser igual a la fila "Total" de la fuente
diferencias <- dplyr::left_join(totales, control_total, by = c("ANIO", "COD_CANTON")) |>
  dplyr::filter(is.na(TOTAL_FUENTE) | POBLACION_TOTAL != TOTAL_FUENTE)
if (nrow(diferencias)) {
  print(diferencias)
  stop("La suma de edades no coincide con la fila Total de la fuente")
}

# Un solo archivo de salida: población de 12 a 17, de 15 años y más y total,
# por cantón y año. Se conservan el nombre del archivo, la hoja
# "Total_adolescente" y la columna "POBLACION 12-17", porque INDICADORES.R y
# SCRIP_DATA_MAESTRA.R lo leen así (las columnas nuevas no les afectan).
totales_export <- dplyr::rename(
  dplyr::select(totales, ANIO, COD_PROVINCIA, PROVINCIA, COD_CANTON, CANTON,
                POBLACION_12_17, POBLACION_15_MAS, POBLACION_TOTAL),
  `AÑO` = ANIO, `POBLACION 12-17` = POBLACION_12_17,
  `POBLACION 15 Y MAS` = POBLACION_15_MAS, `POBLACION TOTAL` = POBLACION_TOTAL)
dir.create(ruta_destino, recursive = TRUE, showWarnings = FALSE)
exportar <- function(tabla, nombre, hoja_salida, columnas_poblacion) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, hoja_salida, gridLines = FALSE)
  openxlsx::writeData(wb, hoja_salida, tabla, withFilter = FALSE)
  openxlsx::freezePane(wb, hoja_salida, firstRow = TRUE)
  openxlsx::addStyle(wb, hoja_salida, openxlsx::createStyle(numFmt = "#,##0"),
                     rows = 2:(nrow(tabla) + 1), cols = columnas_poblacion, gridExpand = TRUE)
  # Códigos conservados como texto: 07, 0701, etc.
  openxlsx::setColWidths(wb, hoja_salida, cols = seq_len(ncol(tabla)), widths = c(10, 19, 35, 17, 30, rep(23, ncol(tabla)-5)))
  salida <- file.path(ruta_destino, nombre)
  openxlsx::saveWorkbook(wb, salida, overwrite = TRUE)
  message("Guardado: ", salida)
}
exportar(totales_export, "poblacion_cantonal_costa_total_12_17_2021_2025.xlsx", "Total_adolescente", 6:8)
message("Cantones: ", nrow(cantones), "; registros cantón-año: ", nrow(totales))
print(dplyr::count(cantones, PROVINCIA, name = "CANTONES"))
