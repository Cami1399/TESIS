# VARIABLES AUXILIARES PARA EL MODELO FAY-HERRIOT CANTONAL
# Documento: Construccion_variables_auxiliares.docx
# Ambito: 86 cantones de las 7 provincias de la Costa, anios 2021-2025



# 0. PAQUETES, LIBRERIAS Y PARAMETROS COMUNES -----------------------------

## 0.1 Paquetes y librerias ----

library(tidyverse)   # dplyr, tidyr, stringr, purrr, readr
library(readxl)      # lectura de archivos .xlsx
library(haven)       # lectura de archivos .sav (SPSS)
# data.table::fread se usa para leer los CSV del REESS (se llama con data.table:: para no cargar el paquete y evitar choques con dplyr)


## 0.2 Parametros comunes ----

# Provincias de la Costa (codigo DPA): El Oro, Esmeraldas, Guayas, Los Rios, Manabi, Santo Domingo de los Tsachilas, Santa Elena
provincias_costa <- c("07", "08", "09", "12", "13", "23", "24")


## 0.3 Poblacion cantonal (INEC, proyecciones revision 2024) ----

# Archivo generado por SCRIP_POBLACION_CANTONAL.R (hoja 3.1 del INEC, cifras al 30 de junio).
# Una fila por canton y anio (86 x 5 = 430).
# Columnas que se usan como denominador:
#   - POBLACION TOTAL    -> tasas de BDH (seccion 2)
#   - POBLACION 15 Y MAS -> tasa de empleo formal (REESS)
ruta_poblacion <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS/poblacion_cantonal_costa_total_12_17_2021_2025.xlsx"

poblacion_cantonal <- read_excel(ruta_poblacion, sheet = "Total_adolescente") %>%
  rename_with(~ .x %>%
                stringi::stri_trans_general("Latin-ASCII") %>%
                str_squish()) %>%
  transmute(
    anio             = as.integer(ANO),
    codigo_provincia = str_pad(COD_PROVINCIA, 2, pad = "0"),
    provincia        = PROVINCIA,
    codigo_canton    = str_pad(COD_CANTON, 4, pad = "0"),
    canton           = CANTON,
    poblacion_12_17  = `POBLACION 12-17`,
    poblacion_15_mas = `POBLACION 15 Y MAS`,
    poblacion_total  = `POBLACION TOTAL`
  )

# Control: 86 cantones x 5 anios
stopifnot(nrow(poblacion_cantonal) == 430,
          n_distinct(poblacion_cantonal$codigo_canton) == 86)


# 1. MINEDEC - MINISTERIO DE EDUCACION ------------------------------------

# Fuente: Descomposicion de la matricula, levantamiento de fin de periodo (registros administrativos AMIE).
# Variables: 1.2.1 Tasa de abandono escolar; 1.2.2 % de matricula particular.

## 1.1 Carga y lectura de archivos ----

# Estructura de los archivos (revisada antes de programar):
#   - Un archivo por periodo lectivo, hoja "Tabla n_01", una fila por institucion educativa (aprox. 16 000 filas a nivel nacional).
#   - 2020-2021 a 2023-2024: encabezado en la fila 17, 250 columnas.
#     Estados por grado: Promovidos, NoPromovidos, Abandono, NoActualizados.
#     Grados: OctavoAnoEGB ... DecimoAnoEGB, PrimerAnoBACH ... TercerAnoBACH.
#   - 2024-2025: encabezado en la fila 16, 176 columnas.
#     Estados: Promovido, NoPromovido, Abandono (ya no trae NoActualizados).
#     Grados: 8voanoBasica ... 10moanoBasica, 1erAnoBachillerato ... 3er...
#   - En 2023-2024 los ceros vienen como "-"; se leen como texto y se convierten a 0.
#   - El orden de las columnas cambia entre anios, por eso se seleccionan por NOMBRE y no por posicion.
#   - Asignacion de anio: el periodo se asigna al anio en que termina (2020-2021 -> 2021; ... ; 2024-2025 -> 2025).
#     Seccion 1.1 del documento.
#   - Grados: solo 8.o EGB a 3.o Bachillerato (edad oficial 12 a 17 anios).

### 1.1.1 Rutas y archivos ----

ruta_minedec <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/DATAS_VARIABLES_AUXILIARES/BD_MATRICULACION_FIN"

# Tabla de control: archivo, anio de referencia y fila del encabezado
archivos_minedec <- tribble(
  ~anio, ~archivo,                                                 ~fila_encabezado,
  2021,  "MINEDUC_RegistrosAdministrativos_2020-2021-Fin.xlsx",    17,
  2022,  "MINEDUC_RegistrosAdministrativos_2021-2022-Fin.xlsx",    17,
  2023,  "MINEDUC_RegistrosAdministrativos_2022-2023-Fin.xlsx",    17,
  2024,  "MINEDUC_RegistrosAdministrativos_2023-2024-Fin.xlsx",    17,
  2025,  "1MINEDEC_RegistrosAdministrativos_2024-2025-Fin.xlsx",   16
)

### 1.1.2 Funcion de lectura ----

# Pasos:
#   1. Lee todo como texto (para manejar el "-").
#   2. Quita tildes y espacios de los nombres de columna (Cod_Cantón -> Cod_Canton, "Tipo de Educación" -> Tipo de Educacion, etc.).
#   3. Deja solo las columnas de identificacion y las de estado por grado.
#   4. Pasa a formato largo: una fila por institucion x grado x sexo x estado.
leer_minedec <- function(archivo, anio, fila_encabezado) {

  datos <- read_excel(
    file.path(ruta_minedec, archivo),
    sheet     = 1,
    skip      = fila_encabezado - 1,
    col_types = "text"
  ) %>%
    rename_with(~ .x %>%
                  stringi::stri_trans_general("Latin-ASCII") %>%
                  str_squish())

  # Nombres de identificacion que cambian entre anios
  datos <- datos %>%
    rename(tipo_educacion = matches("^Tipo (de )?Educacion$"))

  datos %>%
    filter(!is.na(Cod_Canton)) %>%
    transmute(
      anio             = anio,
      codigo_provincia = str_pad(Cod_Provincia, 2, pad = "0"),
      provincia        = str_squish(Provincia),
      codigo_canton    = str_pad(Cod_Canton, 4, pad = "0"),
      canton           = str_squish(Canton),
      amie             = AMIE,
      tipo_educacion   = str_squish(tipo_educacion),
      sostenimiento    = str_squish(Sostenimiento),
      across(matches(
        paste0(
          "(Promovid|Abandono|NoActualizad)",
          ".*(OctavoAnoEGB|NovenoAnoEGB|DecimoAnoEGB|",
          "PrimerAnoBACH|SegundoAnoBACH|TercerAnoBACH|",
          "8voanoBasica|9noanoBasica|10moanoBasica|",
          "1erAnoBachillerato|2doAnoBachillerato|3erAnoBachillerato)$"
        ),
        ignore.case = TRUE
      ))
    ) %>%
    pivot_longer(
      cols      = starts_with("Estudiantes"),
      names_to  = "columna",
      values_to = "estudiantes"
    ) %>%
    mutate(
      # "-" equivale a cero (seccion 3.1 del documento)
      estudiantes = if_else(estudiantes == "-" | is.na(estudiantes),
                            "0", estudiantes),
      estudiantes = as.numeric(estudiantes),
      # Estado: el orden importa ("NoPromovido" contiene "Promovido")
      estado = case_when(
        str_detect(columna, "NoPromovid")   ~ "no_promovidos",
        str_detect(columna, "NoActualizad") ~ "no_actualizados",
        str_detect(columna, "Promovid")     ~ "promovidos",
        str_detect(columna, "Abandono")     ~ "abandono"
      ),
      grado = case_when(
        str_detect(columna, "Octavo|8vo")   ~ "08_EGB",
        str_detect(columna, "Noveno|9no")   ~ "09_EGB",
        str_detect(columna, "Decimo|10mo")  ~ "10_EGB",
        str_detect(columna, "Primer|1er")   ~ "1_BACH",
        str_detect(columna, "Segundo|2do")  ~ "2_BACH",
        str_detect(columna, "Tercer|3er")   ~ "3_BACH"
      )
    ) %>%
    select(-columna)
}

### 1.1.3 Carga de los 5 periodos y filtro Costa ----

minedec_largo <- archivos_minedec %>%
  pmap(\(anio, archivo, fila_encabezado)
       leer_minedec(archivo, anio, fila_encabezado)) %>%
  list_rbind()

# Filtro territorial: solo cantones de las 7 provincias de la Costa
minedec_costa <- minedec_largo %>%
  filter(codigo_provincia %in% provincias_costa)

# Catalogo de cantones (nombre del periodo mas reciente para cada codigo)
cantones_costa <- minedec_costa %>%
  distinct(anio, codigo_provincia, provincia, codigo_canton, canton) %>%
  slice_max(anio, by = codigo_canton, with_ties = FALSE) %>%
  select(codigo_provincia, provincia, codigo_canton, canton) %>%
  arrange(codigo_canton)

### 1.1.4 Control de lectura ----

# Esperado: 48 columnas de estado hasta 2024 (6 grados x 2 sexos x 4 estados), 36 en 2025 (6 grados x 2 sexos x 3 estados) y 86 cantones por anio.
control_lectura <- minedec_costa %>%
  summarise(
    columnas_estado = n_distinct(grado, estado) * 2,
    cantones        = n_distinct(codigo_canton),
    instituciones   = n_distinct(amie),
    .by = anio
  )

print(control_lectura)
print(nrow(cantones_costa))


## 1.2 Construccion de variables ----

# Armado de cada variable:
#
#   1.2.1 Tasa de abandono escolar (TAB) - Indicador oficial
#         numerador   = suma de Abandono
#         denominador = suma de (Promovidos + No promovidos + Abandono)
#         valor       = numerador / denominador * 100
#         Se excluye "No actualizados" del denominador.
#         Todos los sostenimientos y todos los tipos de educacion.
#         Por que: quien abandona el grado sale del sistema educativo (Arias et al., 2022, p. 52), por lo que se relaciona con la inasistencia.
#
#   1.2.2 Porcentaje de matricula particular (%MP) - Creacion propia
#         estudiantes = Promovidos + No promovidos + Abandono
#         numerador   = estudiantes en Sostenimiento == "Particular"
#         denominador = estudiantes en Fiscal + Fiscomisional + Municipal + Particular
#         valor       = numerador / denominador * 100
#         Sin "No actualizados".
#         Por que: se toma como aproximacion a la capacidad de pago de los hogares, por analogia con el porcentaje de afiliados al seguro privado de salud que usa Chile en su modelo de pobreza (MDSF, 2019, p. 12).

### 1.2.0 Base comun MinEdec ----

# Suma por canton, anio y sostenimiento de cada estado (8.o EGB a 3.o Bachillerato, ambos sexos, todos los tipos de educacion)
minedec_canton <- minedec_costa %>%
  summarise(
    estudiantes = sum(estudiantes, na.rm = TRUE),
    .by = c(anio, codigo_canton, sostenimiento, estado)
  ) %>%
  pivot_wider(
    names_from  = estado,
    values_from = estudiantes,
    values_fill = 0
  ) %>%
  mutate(
    con_descomposicion = promovidos + no_promovidos + abandono
  )

### 1.2.1 Tasa de abandono escolar ----

# TAB(c,t) = sum(Abandono) / sum(Promovidos + No promovidos + Abandono) * 100

var_abandono_escolar <- minedec_canton %>%
  summarise(
    numerador   = sum(abandono),
    denominador = sum(con_descomposicion),
    .by = c(anio, codigo_canton)
  ) %>%
  mutate(valor = numerador / denominador * 100) %>%
  left_join(cantones_costa, by = "codigo_canton") %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         numerador, denominador, valor) %>%
  arrange(codigo_canton, anio)

print(var_abandono_escolar)
summary(var_abandono_escolar$valor)

### 1.2.2 Porcentaje de matricula particular ----

# %MP(c,t) = sum(Est. Particular) / sum(Est. todos los sostenimientos) * 100
# Est. = Promovidos + No promovidos + Abandono

var_matricula_particular <- minedec_canton %>%
  summarise(
    numerador   = sum(con_descomposicion[sostenimiento == "Particular"]),
    denominador = sum(con_descomposicion),
    .by = c(anio, codigo_canton)
  ) %>%
  mutate(valor = numerador / denominador * 100) %>%
  left_join(cantones_costa, by = "codigo_canton") %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         numerador, denominador, valor) %>%
  arrange(codigo_canton, anio)

print(var_matricula_particular)
summary(var_matricula_particular$valor)


# 2. MIES - BONOS Y PENSIONES (MINISTERIO DE DESARROLLO HUMANO) -----------

# Fuente: Bases de Aseguramiento No Contributivo, corte a diciembre de cada anio (cierre anual), 2021-2025.
# Variables: 2.2.1 Tasa de hogares con BDH (general);
#            2.2.2 Tasa de hogares con BDH con componente variable.

## 2.1 Carga y lectura de archivos ----

# Estructura de los archivos (revisada antes de programar):
#   - Un archivo .sav (SPSS) por anio, una fila por beneficiario (aprox. 1,4 a 1,6 millones de filas a nivel nacional; 300-480 MB).
#   - Mismas columnas los 5 anios.
#     Se leen solo las necesarias (col_select) para no cargar todo el archivo en memoria:
#       idcant17      = codigo de canton DPA, numerico (701 -> "0701")
#       TIPOBENEFICIO = nombre del subsidio (texto)
#   - TIPOBENEFICIO trae el mismo texto todos los anios:
#       "BONO DE DESARROLLO HUMANO"
#       "BONO DE DESARROLLO HUMANO CON COMPONENTE VARIABLE"
#     Las demas categorias (pensiones, Joaquin Gallegos Lara, Bono 1000 Dias) no se usan.
#   - La provincia se toma de los dos primeros digitos del codigo de canton (en 2021 hay 2 registros del canton 0702 con provincia "GUAYAS").
#   - Los nombres de canton cambian entre anios (p. ej. "YAGUACHI", "...(JUJAN)"); por eso se une por codigo y los nombres se toman del archivo de poblacion (seccion 0.3).

### 2.1.1 Rutas y archivos ----

ruta_mies <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/DATAS_VARIABLES_AUXILIARES/MIES-BONOS/SAV"

archivos_mies <- tribble(
  ~anio, ~archivo,
  2021,  "BONOS_PENSIONES_NACIONAL_DICIEMBRE2021_SPSS.sav",
  2022,  "BONOS_PENSIONES_NACIONAL_DICIEMBRE2022_SPSS.sav",
  2023,  "BONOS_PENSIONES_NACIONAL_DICIEMBRE2023_SPSS.sav",
  2024,  "BONOS_PENSIONES_NACIONAL_DICIEMBRE2024_SPSS.sav",
  2025,  "BONOS_PENSIONES_NACIONAL_DICIEMBRE2025_SPSS.sav"
)

# Subcategorias del BDH (MDH, 2025a, p. 9)
bdh_general  <- "BONO DE DESARROLLO HUMANO"
bdh_variable <- "BONO DE DESARROLLO HUMANO CON COMPONENTE VARIABLE"

### 2.1.2 Funcion de lectura ----

# Pasos:
#   1. Lee solo idcant17 y TIPOBENEFICIO.
#   2. Completa el codigo de canton a 4 digitos y obtiene la provincia.
#   3. Filtra la Costa y las dos subcategorias del BDH.
#   4. Cuenta beneficiarios por canton y subcategoria (una fila = un usuario habilitado, aprox. un hogar).
leer_mies <- function(archivo, anio) {

  read_sav(
    file.path(ruta_mies, archivo),
    col_select = c(idcant17, TIPOBENEFICIO)
  ) %>%
    zap_labels() %>%
    mutate(
      codigo_canton    = sprintf("%04d", as.integer(idcant17)),
      codigo_provincia = str_sub(codigo_canton, 1, 2),
      tipo_beneficio   = str_squish(TIPOBENEFICIO)
    ) %>%
    filter(
      codigo_provincia %in% provincias_costa,
      tipo_beneficio %in% c(bdh_general, bdh_variable)
    ) %>%
    count(codigo_canton, tipo_beneficio, name = "beneficiarios") %>%
    mutate(anio = anio, .before = 1)
}

### 2.1.3 Carga de los 5 anios ----

mies_bdh <- archivos_mies %>%
  pmap(\(anio, archivo) leer_mies(archivo, anio)) %>%
  list_rbind()

### 2.1.4 Control de lectura ----

# Esperado: 86 cantones por anio en cada subcategoria
control_lectura_mies <- mies_bdh %>%
  summarise(
    cantones      = n_distinct(codigo_canton),
    beneficiarios = sum(beneficiarios),
    .by = c(anio, tipo_beneficio)
  ) %>%
  arrange(anio, tipo_beneficio)

print(control_lectura_mies)

# Todos los codigos del MIES deben existir en el archivo de poblacion
stopifnot(all(mies_bdh$codigo_canton %in% poblacion_cantonal$codigo_canton))


## 2.2 Construccion de variables ----

# Armado de cada variable:
#
#   2.2.1 Tasa de hogares con BDH (general) - Creacion propia
#         numerador   = beneficiarios BDH + BDH con componente variable
#         denominador = poblacion total del canton (seccion 0.3)
#         valor       = numerador / denominador * 100
#         Adaptada de la cobertura relativa de la CEPAL (Cecchini y Atuesta, 2017, pp. 21 y 55).
#         Por que: el bono se dirige a nucleos familiares en pobreza o extrema pobreza (INEC, 2026b, p. 131), por lo que se relaciona con la pobreza y con la inasistencia, ya que las mayores brechas para terminar la secundaria se dan entre niveles socioeconomicos (Trucco et al., 2026, p. 14).
#
#   2.2.2 Tasa de hogares con BDH con componente variable - Creacion propia
#         numerador   = beneficiarios BDH con componente variable
#         denominador = poblacion total del canton (seccion 0.3)
#         valor       = numerador / denominador * 100
#         No entra en el mismo modelo que 2.2.1 (esta contenida en ella).
#         Por que: se dirige a nucleos familiares en extrema pobreza con hijos menores de 18 anios (INEC, 2026b, p. 132), por lo que se relaciona con los adolescentes en hogares con NBI.

### 2.2.0 Base comun MIES ----

# Una fila por canton y anio (86 x 5) con los beneficiarios de cada subcategoria y la poblacion total.
# Si un canton no tuviera beneficiarios en un anio, queda en 0.
mies_canton <- poblacion_cantonal %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         poblacion_total) %>%
  left_join(
    mies_bdh %>%
      pivot_wider(
        names_from  = tipo_beneficio,
        values_from = beneficiarios,
        values_fill = 0
      ) %>%
      rename(
        bdh_general  = all_of(bdh_general),
        bdh_variable = all_of(bdh_variable)
      ),
    by = c("anio", "codigo_canton")
  ) %>%
  mutate(across(c(bdh_general, bdh_variable), ~ replace_na(.x, 0)))

### 2.2.1 Tasa de hogares con BDH (general) ----

# TBDH(c,a) = (Beneficiarios BDH + BDH con componente variable) / Poblacion total * 100

var_bdh_general <- mies_canton %>%
  transmute(
    anio, codigo_provincia, provincia, codigo_canton, canton,
    numerador   = bdh_general + bdh_variable,
    denominador = poblacion_total,
    valor       = numerador / denominador * 100
  ) %>%
  arrange(codigo_canton, anio)

print(var_bdh_general)
summary(var_bdh_general$valor)

### 2.2.2 Tasa de hogares con BDH con componente variable ----

# TBDHV(c,a) = Beneficiarios BDH con componente variable / Poblacion total * 100

var_bdh_variable <- mies_canton %>%
  transmute(
    anio, codigo_provincia, provincia, codigo_canton, canton,
    numerador   = bdh_variable,
    denominador = poblacion_total,
    valor       = numerador / denominador * 100
  ) %>%
  arrange(codigo_canton, anio)

print(var_bdh_variable)
summary(var_bdh_variable$valor)


# 3. REESS - EMPLEO EN LA SEGURIDAD SOCIAL (INEC) -------------------------

# Fuente: Registro Estadistico de Empleo en la Seguridad Social (REESS), microdatos mensuales 2021-2025 (definitivas hasta 2023, semidefinitivas 2024 y enero 2025, provisionales febrero-diciembre 2025).
# Variables: 3.2.1 Tasa de empleo formal;
#            3.2.2 % de trabajadores que ganan el salario basico.

## 3.1 Carga y lectura de archivos ----

# Estructura de los archivos (revisada antes de programar):
#   - Un CSV por mes, "BDD_REESS_<anio>_<mes>.csv", repartidos en tres subcarpetas (definitivas, semidefinitivas, provisionales).
#     Se buscan por nombre en toda la carpeta RESS.
#   - Aprox. 3,6 a 3,9 millones de filas por mes a nivel nacional; mismo encabezado en los 60 meses.
#     Se leen solo las columnas necesarias:
#       empleo            = 1 en la afiliacion de mayor sueldo de cada persona (cuenta una vez a cada persona); NA en el resto
#       sector_afiliacion = 1 Privado, 2 Publico (solo estos tienen canton)
#       canton            = codigo DPA numerico (701 -> "0701"); es el domicilio fiscal de la empresa
#       sueldo, dias      = sueldo del mes y dias trabajados
#       sbu               = salario basico unificado del anio
#       s_atip            = 1 no atipico; NA atipico
#   - Cada mes se resume por canton al leerlo, para no cargar en memoria los 60 meses completos.
#   - Nota: en 2025 (datos provisionales) aparecen registros privados con canton "X99" (no identificado: 0799, 0999, ...), como maximo 512 personas al mes (< 0,04 % del empleo de la Costa).
#     Se excluyen.

### 3.1.1 Rutas y archivos ----

ruta_reess <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/DATAS_VARIABLES_AUXILIARES/RESS"

# Lista de los 60 archivos mensuales 2021-2025 (anio y mes desde el nombre)
archivos_reess <- tibble(
  ruta = list.files(ruta_reess, pattern = "^BDD_REESS_[0-9]{4}_[0-9]{1,2}[.]csv$",
                    recursive = TRUE, full.names = TRUE)
) %>%
  mutate(
    anio = as.integer(str_match(basename(ruta), "_([0-9]{4})_")[, 2]),
    mes  = as.integer(str_match(basename(ruta), "_([0-9]{1,2})[.]csv$")[, 2])
  ) %>%
  filter(anio %in% 2021:2025) %>%
  arrange(anio, mes)

# Control: 60 meses, ninguno repetido
stopifnot(nrow(archivos_reess) == 60,
          nrow(distinct(archivos_reess, anio, mes)) == 60)

### 3.1.2 Funcion de lectura ----

# Pasos:
#   1. Lee solo las columnas necesarias.
#   2. Filtra empleo == 1 y sectores 1 (privado) y 2 (publico).
#   3. Completa el codigo de canton y deja la Costa.
#   4. Resume por canton:
#        empleo_registrado = personas (base de la 3.2.1)
#        trabajadores      = personas con s_atip == 1 y dias > 0 (denominador de la 3.2.2)
#        trabajadores_sbu  = de ellas, sueldo / dias * 30 <= sbu (numerador de la 3.2.2)
leer_reess <- function(ruta, anio, mes) {

  data.table::fread(
    ruta,
    select       = c("empleo", "sector_afiliacion", "canton",
                     "sueldo", "dias", "sbu", "s_atip"),
    showProgress = FALSE
  ) %>%
    as_tibble() %>%
    filter(empleo == 1, sector_afiliacion %in% c(1, 2)) %>%
    mutate(
      codigo_canton    = sprintf("%04d", as.integer(canton)),
      codigo_provincia = str_sub(codigo_canton, 1, 2),
      valido_sbu       = s_atip %in% 1 & dias > 0,
      sueldo_30_dias   = if_else(valido_sbu, sueldo / dias * 30, NA_real_)
    ) %>%
    filter(codigo_provincia %in% provincias_costa) %>%
    summarise(
      empleo_registrado = n(),
      trabajadores      = sum(valido_sbu),
      trabajadores_sbu  = sum(valido_sbu & sueldo_30_dias <= sbu, na.rm = TRUE),
      .by = codigo_canton
    ) %>%
    mutate(anio = anio, mes = mes, .before = 1)
}

### 3.1.3 Carga de los 60 meses ----

# Tarda unos minutos (60 archivos de aprox. 370 MB)
reess_mensual <- archivos_reess %>%
  pmap(\(ruta, anio, mes) leer_reess(ruta, anio, mes)) %>%
  list_rbind()

### 3.1.4 Control de lectura y exclusion de cantones X99 ----

# Registros con canton no identificado (X99), excluidos
control_x99 <- reess_mensual %>%
  filter(!codigo_canton %in% poblacion_cantonal$codigo_canton) %>%
  summarise(empleo_excluido = sum(empleo_registrado), .by = c(anio, mes))

print(control_x99, n = Inf)

reess_mensual <- reess_mensual %>%
  filter(codigo_canton %in% poblacion_cantonal$codigo_canton)

# Esperado: 86 cantones en cada uno de los 60 meses
control_lectura_reess <- reess_mensual %>%
  summarise(
    cantones          = n_distinct(codigo_canton),
    empleo_registrado = sum(empleo_registrado),
    .by = c(anio, mes)
  )

print(control_lectura_reess, n = Inf)
stopifnot(all(control_lectura_reess$cantones == 86))


## 3.2 Construccion de variables ----

# Armado de cada variable:
#
#   3.2.1 Tasa de empleo formal (TEF) - Numerador oficial, tasa propia
#         E(c,m)      = personas con empleo == 1, sectores 1 y 2
#         numerador   = promedio de los 12 meses de E(c,m)
#         denominador = poblacion de 15 anios y mas (seccion 0.3)
#         valor       = numerador / denominador * 100
#         Por que: el empleo registrado se parece al empleo del sector formal (INEC, 2023, p. 28), por lo que con mas empleo formal se espera menos desempleo.
#
#   3.2.2 % de trabajadores que ganan el salario basico (PSBU) - Creacion propia
#         filtros     = empleo == 1, sectores 1 y 2, s_atip == 1, dias > 0
#         numerador   = suma de los 12 meses de trabajadores con sueldo / dias * 30 <= SBU
#         denominador = suma de los 12 meses de trabajadores
#         valor       = numerador / denominador * 100
#         Por que: Chile usa en su modelo de pobreza el porcentaje de afiliados con remuneracion inferior al salario minimo (MDSF, 2019, p. 12), por lo que se relaciona con la pobreza.

### 3.2.0 Base comun REESS ----

# Una fila por canton y anio: promedio y sumas de los 12 meses
reess_canton <- reess_mensual %>%
  summarise(
    meses                   = n_distinct(mes),
    empleo_promedio         = sum(empleo_registrado) / 12,
    trabajadores_anual      = sum(trabajadores),
    trabajadores_sbu_anual  = sum(trabajadores_sbu),
    .by = c(anio, codigo_canton)
  )

# Control: 12 meses en cada canton y anio
stopifnot(all(reess_canton$meses == 12))

### 3.2.1 Tasa de empleo formal ----

# TEF(c,a) = [ (1/12) sum_m E(c,m) ] / PET(c,a) * 100

var_empleo_formal <- poblacion_cantonal %>%
  left_join(reess_canton, by = c("anio", "codigo_canton")) %>%
  transmute(
    anio, codigo_provincia, provincia, codigo_canton, canton,
    numerador   = empleo_promedio,
    denominador = poblacion_15_mas,
    valor       = numerador / denominador * 100
  ) %>%
  arrange(codigo_canton, anio)

print(var_empleo_formal)
summary(var_empleo_formal$valor)

### 3.2.2 Porcentaje de trabajadores que ganan el salario basico ----

# PSBU(c,a) = sum_m trabajadores con (sueldo / dias * 30) <= SBU / sum_m trabajadores * 100

var_salario_basico <- poblacion_cantonal %>%
  left_join(reess_canton, by = c("anio", "codigo_canton")) %>%
  transmute(
    anio, codigo_provincia, provincia, codigo_canton, canton,
    numerador   = trabajadores_sbu_anual,
    denominador = trabajadores_anual,
    valor       = numerador / denominador * 100
  ) %>%
  arrange(codigo_canton, anio)

print(var_salario_basico)
summary(var_salario_basico$valor)


# 4. NACIDOS VIVOS (INEC) -------------------------------------------------

# Fuente: Registro Estadistico de Nacidos Vivos (ENV), INEC, 2021-2025 (2025 provisional).
# Variable: 4.2.1 % de nacidos vivos de madres sin pareja.

## 4.1 Carga y lectura de archivos ----

# Estructura de los archivos (revisada antes de programar):
#   - Un CSV por anio (separador ";", UTF-8), una fila por nacido vivo.
#     Los nombres de archivo y carpeta cambian entre anios (ENV_2021.csv, BDD_ENV_2023.csv, ...); se buscan por nombre en toda la carpeta.
#     Los archivos EDF (defunciones fetales) no se usan.
#   - El orden de las columnas cambia entre anios y 2025 tiene menos columnas; las que se usan estan en todos y se leen por nombre:
#       anio_nac           = anio de nacimiento
#       est_civil          = estado civil o conyugal de la madre (texto)
#       prov_res, cant_res = provincia y canton de residencia habitual de la madre (NOMBRES, no codigos)
#   - Cada base trae nacimientos de anios anteriores inscritos tarde; se deja solo anio_nac = anio de la base (2025: 208 985, igual al tabulado oficial 3.1.24).
#   - El canton se asigna por emparejamiento de texto (provincia + canton) con el catalogo de la seccion 0.3, despues de quitar tildes, pasar a mayusculas y quitar espacios dobles.
#     Dos cantones se escriben distinto y van en una tabla de equivalencias.

### 4.1.1 Rutas y archivos ----

ruta_nacidos <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/DATAS_VARIABLES_AUXILIARES/INEC_NACIDOS_DIVORCIOS/NACIDOS_SOLTERAS"

archivos_nacidos <- tibble(
  ruta = list.files(ruta_nacidos, pattern = "ENV_[0-9]{4}[.]csv$",
                    recursive = TRUE, full.names = TRUE)
) %>%
  mutate(anio = as.integer(str_match(basename(ruta), "ENV_([0-9]{4})")[, 2])) %>%
  filter(anio %in% 2021:2025) %>%
  arrange(anio)

# Control: 5 anios, ninguno repetido
stopifnot(nrow(archivos_nacidos) == 5, n_distinct(archivos_nacidos$anio) == 5)

### 4.1.2 Catalogo de cantones para el emparejamiento por texto ----

# Normaliza nombres: sin tildes, mayusculas, sin espacios dobles
normalizar_texto <- function(x) {
  x %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_to_upper() %>%
    str_squish()
}

# Equivalencias: nombre en nacidos vivos -> nombre en el catalogo
equivalencias_canton <- tribble(
  ~provincia_n, ~canton_fuente,                       ~canton_n,
  "GUAYAS",     "SALITRE (URBINA JADO)",              "SALITRE",
  "GUAYAS",     "GENERAL ANTONIO ELIZALDE (BUCAY)",   "GENERAL ANTONIO ELIZALDE"
)

catalogo_texto <- poblacion_cantonal %>%
  distinct(codigo_canton, provincia, canton) %>%
  mutate(provincia_n = normalizar_texto(provincia),
         canton_n    = normalizar_texto(canton)) %>%
  select(codigo_canton, provincia_n, canton_n)

### 4.1.3 Funcion de lectura ----

# Pasos:
#   1. Lee solo anio_nac, est_civil, prov_res y cant_res.
#   2. Deja los nacidos en el anio de la base.
#   3. Normaliza provincia y canton, aplica equivalencias y deja la Costa.
#   4. Clasifica el estado civil de la madre.
leer_nacidos <- function(ruta, anio) {

  read_delim(
    ruta,
    delim      = ";",
    col_types  = cols(.default = col_character()),
    col_select = c(anio_nac, est_civil, prov_res, cant_res),
    progress   = FALSE
  ) %>%
    filter(str_squish(anio_nac) == as.character(anio)) %>%
    mutate(
      anio        = anio,
      provincia_n = normalizar_texto(prov_res),
      canton_n    = normalizar_texto(cant_res),
      est_civil   = normalizar_texto(est_civil)
    ) %>%
    filter(provincia_n %in% catalogo_texto$provincia_n) %>%
    left_join(equivalencias_canton %>% rename(canton_eq = canton_n),
              by = c("provincia_n", "canton_n" = "canton_fuente")) %>%
    mutate(canton_n = coalesce(canton_eq, canton_n)) %>%
    left_join(catalogo_texto, by = c("provincia_n", "canton_n")) %>%
    select(anio, codigo_canton, provincia_n, canton_n, est_civil)
}

### 4.1.4 Carga de los 5 anios ----

nacidos_costa <- archivos_nacidos %>%
  pmap(\(ruta, anio) leer_nacidos(ruta, anio)) %>%
  list_rbind()

### 4.1.5 Control de lectura ----

# Nacidos de la Costa que no se emparejaron con un canton (esperado: 0)
nacidos_sin_canton <- nacidos_costa %>%
  filter(is.na(codigo_canton)) %>%
  count(anio, provincia_n, canton_n)

print(nacidos_sin_canton)
stopifnot(nrow(nacidos_sin_canton) == 0)

# Categorias de estado civil por anio
control_lectura_nacidos <- nacidos_costa %>%
  count(anio, est_civil) %>%
  pivot_wider(names_from = anio, values_from = n)

print(control_lectura_nacidos)


## 4.2 Construccion de variables ----

# Armado de la variable:
#
#   4.2.1 % de nacidos vivos de madres sin pareja (NVSP) - Conteos oficiales, porcentaje y agrupacion propios
#         sin pareja  = Soltera + Divorciada + Separada + Viuda
#         con pareja  = Casada + Unida + Union de hecho
#         numerador   = nacidos de madres sin pareja
#         denominador = total de nacidos, sin "Sin informacion"
#         valor       = numerador / denominador * 100
#         Canton de residencia habitual de la madre.
#         Por que: recoge el estado civil o conyugal de la madre (INEC, 2025b), por lo que se relaciona con los hogares con un solo jefe e hijos menores.
#         Verificacion: en 2025 numerador y denominador son identicos al tabulado oficial 3.1.24 del INEC en los 86 cantones.
#         Limitacion: est_civil mezcla estado civil legal y situacion conyugal; a nivel nacional "Soltera" sube de 23 % (2014) a 63 % (2025) y "Unida" baja (serie 1.3.10), lo que infla el nivel de la variable.
#           Control interno con el Censo 2022: correlacion de 0,35 con % de hogares pobres por NBI, y el cambio de registro no se relaciona con la pobreza (-0,04).

### 4.2.1 Porcentaje de nacidos vivos de madres sin pareja ----

# NVSP(c,a) = Nacidos de madres solteras, divorciadas, separadas o viudas / Total de nacidos vivos (sin "Sin informacion") * 100

estado_sin_pareja <- c("SOLTERA", "DIVORCIADA", "SEPARADA", "VIUDA")
estado_con_pareja <- c("CASADA", "UNIDA", "UNION DE HECHO")

# Control: todas las categorias, salvo "Sin informacion", estan clasificadas
stopifnot(all(setdiff(unique(nacidos_costa$est_civil), "SIN INFORMACION") %in%
                c(estado_sin_pareja, estado_con_pareja)))

var_nacidos_sin_pareja <- nacidos_costa %>%
  filter(est_civil %in% c(estado_sin_pareja, estado_con_pareja)) %>%
  summarise(
    numerador   = sum(est_civil %in% estado_sin_pareja),
    denominador = n(),
    .by = c(anio, codigo_canton)
  ) %>%
  right_join(
    poblacion_cantonal %>%
      select(anio, codigo_provincia, provincia, codigo_canton, canton),
    by = c("anio", "codigo_canton")
  ) %>%
  mutate(valor = numerador / denominador * 100) %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         numerador, denominador, valor) %>%
  arrange(codigo_canton, anio)

print(var_nacidos_sin_pareja)
summary(var_nacidos_sin_pareja$valor)


# 5. POBLACION URBANA (INEC, PROYECCIONES REVISION 2024) -------------------

# Fuente: Estimaciones y proyecciones de poblacion, revision 2024 (INEC), hoja 3.5 "Grupos quinquenales por area geografica y sexo", nivel cantonal, cifras al 30 de junio.
#   Mismo archivo que la seccion 0.3 (hoja 3.1).
# Variable: 5.2.1 % de poblacion urbana.
# Indicadores: desempleo y % de hogares monoparentales con menores.

## 5.1 Carga y lectura ----

# Estructura de la hoja 3.5 (revisada antes de programar):
#   - Fila de encabezado con "Codigo Provincia", "Codigo Canton" y "Edad - Grupos Quinquenales"; debajo, una fila con los anios (celdas combinadas), otra con el area (Urbano / Rural) y otra con el sexo (Hombre / Mujer / Total).
#   - Una fila por canton y grupo de edad (0-4 ... 100 y mas).
#     Se usa solo la columna Total de cada area y anio; no se suman hombres, mujeres y Total juntos.

### 5.1.1 Ruta ----

ruta_proyecciones <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_ORIGEN/Tabulados_Estimacion_Proyeccion_v2.xlsx"

### 5.1.2 Lectura de la hoja 3.5 ----

hoja_area <- read_excel(ruta_proyecciones, sheet = "3.5",
                        range = readxl::cell_limits(c(1, 1), c(NA, NA)),
                        col_names = FALSE, col_types = "text",
                        .name_repair = "minimal") %>%
  as.matrix()
hoja_area[] <- trimws(hoja_area)

# Filas de encabezado: variables, anios, area y sexo
fila_variables <- which(apply(hoja_area, 1, function(x)
  all(c("Código Provincia", "Código Cantón", "Edad - Grupos Quinquenales") %in% x)))
stopifnot(length(fila_variables) == 1)
fila_anio <- fila_variables + 1
fila_area <- fila_variables + 2
fila_sexo <- fila_variables + 3

# Propaga a la derecha los anios y las areas (celdas combinadas)
propagar <- function(x, valido) {
  ultimo <- NA_character_
  for (j in seq_along(x)) {
    if (!is.na(x[j]) && valido(x[j])) ultimo <- x[j]
    x[j] <- ultimo
  }
  x
}
anio_col <- propagar(hoja_area[fila_anio, ], \(v) grepl("^[12][0-9]{3}$", v))
area_col <- propagar(hoja_area[fila_area, ], \(v) v %in% c("Urbano", "Rural"))

col_prov   <- which(hoja_area[fila_variables, ] == "Código Provincia")
col_canton <- which(hoja_area[fila_variables, ] == "Código Cantón")
col_edad   <- which(hoja_area[fila_variables, ] == "Edad - Grupos Quinquenales")

# Filas de la Costa con grupo de edad (sin filas "Total")
filas_area <- which(seq_len(nrow(hoja_area)) > fila_sexo &
                      hoja_area[, col_prov] %in% provincias_costa &
                      !is.na(hoja_area[, col_edad]) &
                      hoja_area[, col_edad] != "Total")

poblacion_area <- map(2021:2025, \(a) {
  col_u <- which(anio_col == a & area_col == "Urbano" & hoja_area[fila_sexo, ] == "Total")
  col_r <- which(anio_col == a & area_col == "Rural"  & hoja_area[fila_sexo, ] == "Total")
  stopifnot(length(col_u) == 1, length(col_r) == 1)
  tibble(
    anio          = a,
    codigo_canton = hoja_area[filas_area, col_canton],
    urbana        = as.numeric(hoja_area[filas_area, col_u]),
    rural         = as.numeric(hoja_area[filas_area, col_r])
  )
}) %>%
  list_rbind() %>%
  summarise(urbana = sum(urbana), rural = sum(rural), .by = c(anio, codigo_canton))

### 5.1.3 Control de lectura ----

# Urbana + rural debe ser igual a la poblacion total de la seccion 0.3
control_area <- poblacion_area %>%
  inner_join(poblacion_cantonal %>% select(anio, codigo_canton, poblacion_total),
             by = c("anio", "codigo_canton")) %>%
  mutate(diferencia = urbana + rural - poblacion_total)

stopifnot(nrow(control_area) == 430, all(control_area$diferencia == 0))


## 5.2 Construccion de variables ----

# Armado de la variable:
#
#   5.2.1 % de poblacion urbana (PU) - Datos oficiales, porcentaje propio
#         numerador   = poblacion del area urbana del canton
#         denominador = poblacion urbana + rural (= poblacion total)
#         valor       = numerador / denominador * 100
#         Por que: en Ecuador el desempleo es mayor en el area urbana (3,2 %) que en la rural (1,5 %) (INEC, 2026f, diciembre de 2025); los cambios familiares que aumentan los hogares monoparentales se asocian con la urbanizacion (Ullmann, Maldonado y Rico, 2014, pp. 26 y 30).

### 5.2.1 Porcentaje de poblacion urbana ----

# PU(c,a) = Poblacion urbana / Poblacion total * 100

var_poblacion_urbana <- poblacion_area %>%
  right_join(
    poblacion_cantonal %>%
      select(anio, codigo_provincia, provincia, codigo_canton, canton),
    by = c("anio", "codigo_canton")
  ) %>%
  mutate(
    numerador   = urbana,
    denominador = urbana + rural,
    valor       = numerador / denominador * 100
  ) %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         numerador, denominador, valor) %>%
  arrange(codigo_canton, anio)

stopifnot(nrow(var_poblacion_urbana) == 430,
          !anyNA(var_poblacion_urbana$valor))

print(var_poblacion_urbana)
summary(var_poblacion_urbana$valor)


# 6. EXPORTACION ----------------------------------------------------------

# Panel largo: una fila por canton x anio x variable auxiliar (86 cantones x 5 anios x 8 variables = 3 440 filas).
# Columnas: anio, codigo_provincia, provincia, codigo_canton, canton, variable, valor Los nombres de provincia y canton se toman del archivo de poblacion (seccion 0.3) para que sean iguales en todas las variables.

## 6.1 Union de las variables en un panel largo ----

panel_variables_auxiliares <- list(
  abandono_escolar     = var_abandono_escolar,
  matricula_particular = var_matricula_particular,
  bdh_general          = var_bdh_general,
  bdh_variable         = var_bdh_variable,
  empleo_formal        = var_empleo_formal,
  salario_basico       = var_salario_basico,
  nacidos_sin_pareja   = var_nacidos_sin_pareja,
  poblacion_urbana     = var_poblacion_urbana
) %>%
  map(~ .x %>%
        mutate(anio = as.integer(anio)) %>%
        select(anio, codigo_canton, valor)) %>%
  list_rbind(names_to = "variable") %>%
  left_join(
    poblacion_cantonal %>%
      select(anio, codigo_provincia, provincia, codigo_canton, canton),
    by = c("anio", "codigo_canton")
  ) %>%
  select(anio, codigo_provincia, provincia, codigo_canton, canton,
         variable, valor) %>%
  arrange(variable, codigo_canton, anio)

## 6.2 Control del panel ----

# Esperado: 3 440 filas, 430 por variable, sin duplicados y sin NA
control_panel <- panel_variables_auxiliares %>%
  summarise(filas = n(), na_valor = sum(is.na(valor)), .by = variable)

print(control_panel)
stopifnot(
  nrow(panel_variables_auxiliares) == 86 * 5 * 8,
  all(control_panel$filas == 430),
  !anyDuplicated(panel_variables_auxiliares[c("anio", "codigo_canton", "variable")]),
  !anyNA(panel_variables_auxiliares$canton),
  !anyNA(panel_variables_auxiliares$valor)
)

## 6.3 Exportacion a DATAS_PROCESADAS ----

ruta_exportacion <- "C:/Users/camil/OneDrive/Desktop/TESIS/DATAS_PROCESADAS"

writexl::write_xlsx(
  list(panel_largo = panel_variables_auxiliares),
  file.path(ruta_exportacion, "panel_variables_auxiliares_2021_2025.xlsx")
)


# 7. DIAGNOSTICO ----------------------------------------------------------

# Revision de las variables auxiliares antes del modelo Fay-Herriot:
#   7.1 distribucion de cada variable (graficos).
# Los graficos quedan como objetos en R; no se exporta nada en esta seccion.

## 7.1 Distribucion de las variables auxiliares ----

# Graficos en escala original (porcentaje):
#   7.1.1 Caja por anio, uno por variable: un punto = un canton en ese anio (86 por caja); el color indica la provincia.
#   7.1.2 Panel con las 8 cajas de la 7.1.1 juntas.
#   7.1.3 Grilla de distribucion: filas = provincias, columnas = variables; en cada recuadro, la curva de los 5 anios juntos de esa provincia.
# El panel no tiene NA (seccion 6.2).

variables_auxiliares <- c("abandono_escolar", "matricula_particular",
                          "bdh_general", "bdh_variable",
                          "empleo_formal", "salario_basico",
                          "nacidos_sin_pareja", "poblacion_urbana")

datos_distribucion <- panel_variables_auxiliares %>%
  filter(!is.na(valor)) %>%
  mutate(anio = factor(anio))

### 7.1.1 Caja por anio (un punto por canton) ----

# Los puntos se separan un poco en horizontal para que no se tapen; la semilla fija esa separacion para que el grafico salga igual cada vez.
# outlier.shape = NA evita dibujar dos veces los atipicos: ya estan como puntos de canton (son los que quedan fuera de los bigotes).
graficar_caja_anio <- function(var) {
  datos_distribucion %>%
    filter(variable == var) %>%
    ggplot(aes(x = anio, y = valor)) +
    geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey40") +
    geom_point(aes(colour = provincia),
               position = position_jitter(width = 0.2, height = 0, seed = 123),
               size = 1.8, alpha = 0.8) +
    scale_colour_brewer(palette = "Dark2") +
    labs(title = var,
         subtitle = "Caja por anio; un punto por canton",
         x = "Anio", y = "Valor (%)", colour = "Provincia") +
    theme_minimal()
}

graficos_caja_anio <- map(variables_auxiliares, graficar_caja_anio) %>%
  set_names(variables_auxiliares)

walk(graficos_caja_anio, print)

### 7.1.2 Panel de cajas: las 8 variables juntas ----

# Mismo grafico de la 7.1.1, con un recuadro por variable.
# Cada recuadro tiene su propia escala vertical porque los rangos son muy distintos.
grafico_panel_cajas <- datos_distribucion %>%
  mutate(variable = factor(variable, levels = variables_auxiliares,
                           labels = c("Abandono escolar", "Matrícula particular", "BDH general",
                                      "BDH variable", "Empleo formal", "Salario básico",
                                      "Nacidos sin pareja", "Población urbana"))) %>%
  ggplot(aes(x = anio, y = valor)) +
  geom_boxplot(outlier.shape = NA, fill = "grey95", colour = "grey40") +
  geom_point(aes(colour = provincia),
             position = position_jitter(width = 0.2, height = 0, seed = 123),
             size = 0.9, alpha = 0.7) +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~ variable, ncol = 4, scales = "free_y") +
  labs(title = "Variables auxiliares por año",
       subtitle = "Una caja por año y un punto por cantón; cada recuadro tiene su propia escala",
       x = "Año", y = "Valor (%)", colour = "Provincia") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_panel_cajas)

### 7.1.3 Grilla de distribucion: provincias x variables ----

# Cada recuadro es una provincia (fila) en una variable (columna); la curva junta los 5 anios de los cantones de esa provincia.
# Las rayitas de abajo son los canton-anio.
# - Eje X: cada columna tiene su propia escala; dentro de una columna todas las provincias comparten la escala y se pueden comparar.
# - Altura: cada curva se ajusta para llenar su recuadro (after_stat(scaled)), asi que se compara la forma, no la altura entre recuadros.
# - Santo Domingo (2 cantones) y Santa Elena (3) tienen 10 y 15 valores; su curva se lee con cuidado.
grafico_grilla_distribucion <- datos_distribucion %>%
  mutate(
    variable  = factor(variable, levels = variables_auxiliares,
                       labels = str_replace_all(variables_auxiliares, "_", "\n")),
    provincia = str_replace(provincia, "^Santo Domingo.*", "Santo Domingo")
  ) %>%
  ggplot(aes(x = valor)) +
  geom_density(aes(y = after_stat(scaled)),
               fill = "steelblue", alpha = 0.35, colour = "steelblue4") +
  geom_rug(alpha = 0.5, length = unit(0.06, "npc"), colour = "steelblue4") +
  facet_grid(provincia ~ variable, scales = "free_x") +
  labs(title = "Distribucion de las variables auxiliares por provincia (2021-2025 juntos)",
       subtitle = "Filas = provincias; columnas = variables; rayitas = canton-anio",
       x = "Valor (%)", y = "Densidad (escalada a 1 en cada recuadro)") +
  theme_minimal(base_size = 9) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0),
        axis.text.y  = element_blank(),
        panel.border = element_rect(fill = NA, colour = "grey85"))

print(grafico_grilla_distribucion)


# GUARDAR IMAGENES EN LA CARPETA DE SCRIPTS ------------------------------------

# Las imagenes de este script van a SCRIPTS/SALIDAS_EXPLORACIONES/salidas_variables_auxiliares.
if (!exists("carpeta_imagenes")) carpeta_imagenes <- "C:/Users/camil/OneDrive/Desktop/TESIS/SCRIPTS/SALIDAS_EXPLORACIONES"
carpeta_img <- file.path(carpeta_imagenes, "salidas_variables_auxiliares")
dir.create(carpeta_img, recursive = TRUE, showWarnings = FALSE)
iwalk(graficos_caja_anio, \(g, n) ggsave(file.path(carpeta_img, paste0("caja_anio_", n, ".png")), g,
                                         width = 9, height = 6, dpi = 150))
ggsave(file.path(carpeta_img, "panel_cajas.png"), grafico_panel_cajas, width = 11.5, height = 7.2, dpi = 150)
ggsave(file.path(carpeta_img, "grilla_distribucion.png"), grafico_grilla_distribucion,
       width = 14, height = 9, dpi = 150)
