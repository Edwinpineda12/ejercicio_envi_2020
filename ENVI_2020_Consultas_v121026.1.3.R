#==================================================================
# Este script es un ejercicio de tratamiento de información contenida
# en las tablas que componen la ENVI 2020 del INEGI. Con esa información
# se trata de responder a la pregunta: 
# ¿Qué características deberían considerarse para identificar y dimensionar 
# a la población potencialmente susceptible de ser atendida por un programa 
# público de vivienda como el programa de Vivienda para el Bienestar?
# 
# Las clasificaciones son tomadas del catalogo oficial
# 
# La versión de este scrip es la:
# v121026.1.3
#
#=================================================================

#=================================================================
# 1. Librerias a utilizar
#=================================================================

librerias <- c("DBI", "duckdb", "dplyr", "ggplot2", "readr", "tibble", "survey")

lapply (librerias, library, character.only = TRUE)

#=================================================================
# 2. Crear una conexión en donde se van a cargar los datos para no saturar la RAM
#=================================================================

con <- dbConnect(
  duckdb(),
  dbdir = "envi_2020.duckdb"
)

#=================================================================
# 3. Crear vector al zip en donde se tiene la información
#=================================================================

envi <- "C:\\Users\\EdwinHéctorPinedaLóp\\OneDrive - Centro Federal de Conciliación y Registro Laboral\\Escritorio\\Analista_A\\conjunto_de_datos_envi_2020_csv.zip"

#=================================================================
# 2. FUNCIONES AUXILIARES PARA SQL
#=================================================================

citar_identificador <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

citar_cadena_sql <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}


#=================================================================
# 3. COMPROBAR EL ARCHIVO ZIP
#=================================================================

if (!exists("envi") || !file.exists(envi)) {
  stop("Define envi con la ruta válida del ZIP de la ENVI 2020.")
}

if (!DBI::dbIsValid(con)) {
  stop("La conexión con DuckDB no es válida.")
}


#=================================================================
# 4. IDENTIFICAR LOS CSV CON INFORMACIÓN SUSTANCIAL
#=================================================================

archivos <- unzip(envi, list = TRUE)$Name

csv_datos <- archivos[
  grepl(
    "(^|/)conjunto_de_datos/conjunto_de_datos.*\\.csv$",
    archivos,
    ignore.case = TRUE
  )
]

if (length(csv_datos) == 0) {
  stop("No se encontraron CSV de conjuntos de datos en el ZIP.")
}

message("Archivos CSV identificados: ", length(csv_datos))


#=================================================================
# 5. EXTRAER LOS CSV SELECCIONADOS
#=================================================================

carpeta <- "datos_envi"

dir.create(
  carpeta,
  showWarnings = FALSE,
  recursive = TRUE
)

unzip(
  envi,
  files = csv_datos,
  exdir = carpeta
)


#=================================================================
# 6. CARGAR LOS CSV EN DUCKDB
#=================================================================

for (archivo in csv_datos) {
  
  nombre_tabla <- tools::file_path_sans_ext(
    basename(archivo)
  )
  
  ruta_csv <- normalizePath(
    file.path(carpeta, archivo),
    winslash = "/",
    mustWork = TRUE
  )
  
  sql <- sprintf(
    "CREATE OR REPLACE TABLE %s AS
     SELECT *
     FROM read_csv_auto(%s, header = true)",
    citar_identificador(nombre_tabla),
    citar_cadena_sql(ruta_csv)
  )
  
  DBI::dbExecute(con, sql)
  
  message("Tabla cargada: ", nombre_tabla)
}

tablas <- DBI::dbListTables(con)
print(tablas)

#=================================================================
# 7. CREAR VECTORES DE TABLAS DE ANALISIS
#=================================================================

tabla_carac <- "conjunto_de_datos_tvivienda_envi_2020"
tabla_demanda <- "conjunto_de_datos_tdemanda_envi_2020"
tabla_pobl <- "conjunto_de_datos_tsdem_envi_2020"

#=================================================================
# 8. EXTRAER LAS VARIABLES DEL DISEÑO MUESTRAL PARA SU USO PONDERADO
#=================================================================

columnas_carac <- DBI::dbGetQuery(
  con,
  sprintf(
    "DESCRIBE SELECT * FROM %s",
    citar_identificador(tabla_carac)
  )
)

print(columnas_carac)

variables_diseno <- c(
  "ENT",
  "TLOC",
  "EST_DIS",
  "UPM_DIS",
  "FACTOR"
)

faltantes_diseno <- setdiff(
  variables_diseno,
  columnas_carac$column_name
)

if (length(faltantes_diseno) > 0) {
  warning(
    "Revisa las variables de diseño faltantes: ",
    paste(faltantes_diseno, collapse = ", ")
  )
}


#=================================================================
# 9. EXTRAER LAS VARIABLES DE VIVIENDA
#=================================================================

sql_vivienda <- sprintf(
  '
  SELECT
    "FOLIO" AS Folio,
    "ENT" AS Entidad,
    "TLOC" AS Tam_Loc,
    "EST_DIS" AS Estrato_diseno,
    "UPM_DIS" AS UPM,
    "VIV_SEL" AS vivienda,

    "P1_1" AS Personas,
    "P1_2" AS gasto_comun,
    "P1_3" AS Hogares,

    "P4_3" AS clase_vivienda,
    "P4_4" AS paredes,
    "P4_5" AS techo,
    "P4_6" AS piso,
    "P4_9" AS cocina,
    "P4_10" AS Cuartos,
    "P4_11" AS sanitario,
    "P4_12" AS baño_agua,
    "P4_13" AS agua_ent,
    "P4_14" AS abast_agua,
    "P4_15" AS drenaje,
    "P4_16" AS electricidad,
    "P4_17" AS comb_cocinar,
    "P4_18" AS leña_carbon,

    "P4_20_1" AS Metros_Conocidos,
    "P4_20_2" AS Metros_Aproximados,

    "P4_21_1" AS Construccion_Conocida,
    "P4_21_2" AS Construccion_Aproximada,
    
    "P4_25_1" AS grietas,
    "P4_25_2" AS pandeos,
    "P4_25_3" AS levantamientos,
    "P4_25_4" AS humedad,
    "P4_25_5" AS fracturas,
    "P4_25_6" AS electricos,
    "P4_25_7" AS tuberias,

    "FACTOR" AS Factor

  FROM %s
  ',
  citar_identificador(tabla_carac)
)

datos_vivienda <- DBI::dbGetQuery(
  con,
  sql_vivienda
)

sql_ingreso <- sprintf(
  '
  SELECT
    "P3_4" AS ingreso,
    "P3_4A" AS periodicidad,
    "FOLIO",	
    "VIV_SEL",	
    "HOGAR", 
    "ENT",
    "TLOC",
    "EST_DIS",
    "UPM_DIS",	
    "FACTOR"

FROM %s
  ',
  citar_identificador(tabla_pobl)
)

datos_ingreso <- DBI::dbGetQuery(
  con,
  sql_ingreso
)

#=================================================================
# 10. PREPARAR VARIABLES 
#=================================================================

variables_numericas <- c(
  "Personas",
  "Cuartos",
  "paredes",
  "techo",
  "piso",
  "sanitario",
  "clase_vivienda",
  "cocina",
  "baño_agua",
  "agua_ent",
  "abast_agua",
  "drenaje",
  "electricidad",
  "comb_cocinar",
  "leña_carbon",
  "Metros_Conocidos",
  "Metros_Aproximados",
  "Construccion_Conocida",
  "Construccion_Aproximada",
  "Factor"
)

datos_vivienda <- datos_vivienda %>%
  mutate(
    across(
      all_of(variables_numericas),
      ~ suppressWarnings(
        as.numeric(trimws(as.character(.x)))
      )
    ),
    
    Entidad = trimws(as.character(Entidad)),
    Tam_Loc = trimws(as.character(Tam_Loc)),
    Estrato_diseno = trimws(as.character(Estrato_diseno)),
    UPM = trimws(as.character(UPM)),
    Folio = trimws(as.character(Folio)),
    vivienda = trimws(as.character(vivienda))
  )

#----------------------------------------------------------------
# Crear vector que contenga las variables analíticas
#----------------------------------------------------------------

mat_vivienda <- datos_vivienda

message(
  "Registros extraídos: ",
  nrow(mat_vivienda)
)


#=================================================================
# 11. CREAR CATALOGO DE ENTIDADES DE ACUERDO AL INEGI
#=================================================================

catalogo_entidades <- c(
  "01" = "Aguascalientes",
  "02" = "Baja California",
  "03" = "Baja California Sur",
  "04" = "Campeche",
  "05" = "Coahuila de Zaragoza",
  "06" = "Colima",
  "07" = "Chiapas",
  "08" = "Chihuahua",
  "09" = "Ciudad de México",
  "10" = "Durango",
  "11" = "Guanajuato",
  "12" = "Guerrero",
  "13" = "Hidalgo",
  "14" = "Jalisco",
  "15" = "México",
  "16" = "Michoacán de Ocampo",
  "17" = "Morelos",
  "18" = "Nayarit",
  "19" = "Nuevo León",
  "20" = "Oaxaca",
  "21" = "Puebla",
  "22" = "Querétaro",
  "23" = "Quintana Roo",
  "24" = "San Luis Potosí",
  "25" = "Sinaloa",
  "26" = "Sonora",
  "27" = "Tabasco",
  "28" = "Tamaulipas",
  "29" = "Tlaxcala",
  "30" = "Veracruz de Ignacio de la Llave",
  "31" = "Yucatán",
  "32" = "Zacatecas"
)

mat_vivienda <- mat_vivienda %>%
  mutate(
    Entidad_nombre = unname(
      catalogo_entidades[Entidad]
    ),
    Entidad_nombre = coalesce(
      Entidad_nombre,
      Entidad
    )
  )

# Comprobar códigos geográficos.
print(mat_vivienda %>% count(Entidad, Entidad_nombre))
print(mat_vivienda %>% count(Tam_Loc, sort = TRUE))
print(mat_vivienda %>% count(Estrato_diseno, sort = TRUE))

#=================================================================
# 12. FUNCIONES AUXILIARES
#=================================================================

mediana_segura <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  
  if (length(x) == 0) {
    return(NA_real_)
  }
  
  median(x)
}

proporcion_ponderada <- function(indicador, peso) {
  
  validos <- !is.na(indicador) &
    !is.na(peso) &
    is.finite(peso) &
    peso > 0
  
  if (!any(validos)) {
    return(NA_real_)
  }
  
  weighted.mean(
    as.numeric(indicador[validos]),
    w = peso[validos]
  ) * 100
}


#=================================================================
# 13. INDICADORES DE HACINAMIENTO Y SUPERFICIE
#=================================================================

mat_vivienda <- mat_vivienda %>%
  mutate(
    res_cuarto = case_when(
      is.na(Cuartos) ~ NA_real_,
      is.na(Personas) ~ NA_real_,
      Cuartos <= 0 ~ NA_real_,
      TRUE ~ Personas / Cuartos
    ),
    
    Ind_hacinamiento = res_cuarto,
    
    hacin = case_when(
      is.na(Cuartos) | is.na(Personas) ~ NA_integer_,
      Cuartos <= 0 ~ NA_integer_,
      res_cuarto > 2.5 ~ 1L,
      TRUE ~ 0L
    ),
    
    Nivel_hacinamiento = case_when(
      is.na(hacin) ~ "No clasificable",
      hacin == 0L ~ "Sin hacinamiento",
      hacin == 1L ~ "Hacinamiento"
    ),
    
    #Esto nos puede ayudar a futuras construcciones de indicadores
    # que tomen en cuenta no solo la cantidad de cuartos sino la 
    # cantidad de M2 por persona
    
    Construccion_M2 = coalesce(
      if_else(
        !is.na(Construccion_Conocida) &
          is.finite(Construccion_Conocida) &
          Construccion_Conocida >= 0,
        Construccion_Conocida,
        NA_real_
      ),
      if_else(
        !is.na(Construccion_Aproximada) &
          is.finite(Construccion_Aproximada) &
          Construccion_Aproximada >= 0,
        Construccion_Aproximada,
        NA_real_
      )
    ),
    
    Terreno_M2 = coalesce(
      if_else(
        !is.na(Metros_Conocidos) &
          is.finite(Metros_Conocidos) &
          Metros_Conocidos >= 0,
        Metros_Conocidos,
        NA_real_
      ),
      if_else(
        !is.na(Metros_Aproximados) &
          is.finite(Metros_Aproximados) &
          Metros_Aproximados >= 0,
        Metros_Aproximados,
        NA_real_
      )
    ),
    
    M2_construidos_x_persona = if_else(
      !is.na(Personas) &
        Personas > 0 &
        !is.na(Construccion_M2),
      Construccion_M2 / Personas,
      NA_real_
    ),
    
    M2_terreno_x_persona = if_else(
      !is.na(Personas) &
        Personas > 0 &
        !is.na(Terreno_M2),
      Terreno_M2 / Personas,
      NA_real_
    ),
    
    proporcion_construida = if_else(
      !is.na(Terreno_M2) &
        !is.na(Construccion_M2) &
        Terreno_M2 > 0,
      Construccion_M2 / Terreno_M2,
      NA_real_
    )
  )


#=================================================================
# 14. CLASIFICAR MATERIALES: De acuerdo a documento "Actualización 
# del Rezago habitacional Censo de Población y Vivienda 2020", 
# Criterios metodológicos del rezago habitacional
# 
#=================================================================

codigos_pared_deteriorada <- c(1, 2, 4, 5)
codigos_techo_deteriorado <- c(1, 2, 6)

codigos_pared_regular <- c(3, 6)
codigos_techo_regular <- c(3, 4, 7, 9)
codigos_piso_regular <- c(1)

codigos_pared_adecuada <- c(7, 8)
codigos_techo_adecuado <- c(5, 8, 10)
codigos_piso_adecuado <- c(2, 3)

mat_vivienda <- mat_vivienda %>%
  mutate(
    pared_det = case_when(
      is.na(paredes) ~ NA_integer_,
      paredes %in% codigos_pared_deteriorada ~ 1L,
      paredes %in% c(codigos_pared_regular,
                     codigos_pared_adecuada) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    techo_det = case_when(
      is.na(techo) ~ NA_integer_,
      techo %in% codigos_techo_deteriorado ~ 1L,
      techo %in% c(codigos_techo_regular,
                   codigos_techo_adecuado) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    mat_det = case_when(
      pared_det == 1L | techo_det == 1L ~ 1L,
      pared_det == 0L & techo_det == 0L ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    pared_reg = case_when(
      is.na(paredes) ~ NA_integer_,
      paredes %in% codigos_pared_regular ~ 1L,
      paredes %in% c(codigos_pared_deteriorada,
                     codigos_pared_adecuada) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    techo_reg = case_when(
      is.na(techo) ~ NA_integer_,
      techo %in% codigos_techo_regular ~ 1L,
      techo %in% c(codigos_techo_deteriorado,
                   codigos_techo_adecuado) ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    piso_reg = case_when(
      is.na(piso) ~ NA_integer_,
      piso %in% codigos_piso_regular ~ 1L,
      piso %in% codigos_piso_adecuado ~ 0L,
      TRUE ~ NA_integer_
    ),
    
    mat_reg = case_when(
      mat_det == 1L ~ 0L,
      
      mat_det == 0L &
        (pared_reg == 1L |
           techo_reg == 1L |
           piso_reg == 1L) ~ 1L,
      
      mat_det == 0L &
        pared_reg == 0L &
        techo_reg == 0L &
        piso_reg == 0L ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    clasif_materiales = case_when(
      mat_det == 1L ~ "Materiales deteriorados",
      
      mat_det == 0L & mat_reg == 1L ~
        "Materiales regulares",
      
      mat_det == 0L &
        mat_reg == 0L &
        paredes %in% codigos_pared_adecuada &
        techo %in% codigos_techo_adecuado &
        piso %in% codigos_piso_adecuado ~
        "Materiales adecuados",
      
      TRUE ~ "No clasificable"
    )
  )

#=================================================================
# 15. CARENCIA SANITARIA Y PRECARIEDAD DE ESPACIOS: De acuerdo a documento 
# "Actualización del Rezago habitacional Censo de Población y Vivienda 2020", 
# Criterios metodológicos del rezago habitacional y al documento: Rezago habitacional 
# 2024: Análisis de resultados
#=================================================================

mat_vivienda <- mat_vivienda %>%
  mutate(
    excu = case_when(
      sanitario == 1 ~ 0L,
      sanitario == 2 ~ 1L,  
      TRUE ~ NA_integer_
    ),
    
    prec_esp = case_when(
      hacin == 1L | excu == 1L ~ 1L,
      hacin == 0L & excu == 0L ~ 0L,
      TRUE ~ NA_integer_
    )
  )

#================================================================
# POBLACIÓN DE BAJOS INGRESOS: DE ACUERDO CO LA DEFINICIÓN DEL 
# DOCUMENTO: Metodología para el cálculo de la población potencial,
# objeto y prioritaria, Programa de Vivienda Social S-177, 2026.
#=================================================================


#=================================================================
# 16. INDICADOR EXPLORATORIO DE REZAGO HABITACIONAL
#=================================================================

mat_vivienda <- mat_vivienda %>%
  mutate(
    rezago = case_when(
      mat_det == 1L ~ 1L,
      mat_reg == 1L ~ 1L,
      prec_esp == 1L ~ 1L,
      
      mat_det == 0L &
        mat_reg == 0L &
        prec_esp == 0L ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    estado_rezago = case_when(
      rezago == 1L ~ "Con rezago habitacional",
      rezago == 0L ~ "Sin rezago habitacional",
      TRUE ~ "Sin clasificación de rezago"
    )
  )

cat("\nClasificación de materiales:\n")
print(table(mat_vivienda$clasif_materiales, useNA = "ifany"))

cat("\nHacinamiento:\n")
print(table(mat_vivienda$Nivel_hacinamiento, useNA = "ifany"))

cat("\nRezago habitacional:\n")
print(table(mat_vivienda$estado_rezago, useNA = "ifany"))

cat("\nClasificación de espacios precarios:\n")
print(table(mat_vivienda$prec_esp, useNA = "ifany"))


#=================================================================
# 17. RESUMEN GENERAL DE VIVIENDA: Solo sirve para comprobar que la
# información reflejada es la correcta, se buscan coincidencias de 
# registros así como las medidas obtenidas en los numeros anteriores
#=================================================================

resumen_vivienda <- mat_vivienda %>%
  summarise(
    total_registros = n(),
    
    viviendas_personas_validas =
      sum(!is.na(Personas) & Personas > 0),
    
    viviendas_cuartos_validos =
      sum(!is.na(Cuartos) & Cuartos > 0),
    
    viviendas_hacinamiento_valido =
      sum(!is.na(hacin)),
    
    viviendas_superficie_valida =
      sum(!is.na(M2_construidos_x_persona)),
    
    mediana_personas = mediana_segura(Personas),
    
    mediana_cuartos = mediana_segura(
      Cuartos[Cuartos > 0]
    ),
    
    mediana_hacinamiento = mediana_segura(Ind_hacinamiento),
    
    mediana_m2_construidos_persona =
      mediana_segura(M2_construidos_x_persona),
    
    viviendas_con_rezago =
      sum(rezago == 1L, na.rm = TRUE),
    
    viviendas_sin_rezago =
      sum(rezago == 0L, na.rm = TRUE),
    
    viviendas_no_clasificables =
      sum(is.na(rezago))
  )

print(resumen_vivienda)


#=================================================================
# 18. RESUMEN DE HACINAMIENTO: Al igual que el resumen de vivienda
# funciona como comprobación
#=================================================================

resumen_hacinamiento <- mat_vivienda %>%
  filter(!is.na(hacin)) %>%
  count(Nivel_hacinamiento, name = "Num_Viviendas") %>%
  mutate(
    porcentaje = 100 * Num_Viviendas / sum(Num_Viviendas)
  )

print(resumen_hacinamiento)


#=================================================================
# 19. ESTIMACIÓN PONDERADA DE HACINAMIENTO Y REZAGO: Pondera de acuerdo
# al factor de expansión de la tabla la cantidad de viviendas con 
# hacinamiento
#=================================================================

resumen_ponderado <- tibble(
  registros_ponderables = sum(
    !is.na(mat_vivienda$hacin) &
      !is.na(mat_vivienda$Factor) &
      is.finite(mat_vivienda$Factor) &
      mat_vivienda$Factor > 0
  ),
  
  viviendas_expand = sum(
    mat_vivienda$Factor[
      !is.na(mat_vivienda$hacin) &
        !is.na(mat_vivienda$Factor) &
        is.finite(mat_vivienda$Factor) &
        mat_vivienda$Factor > 0
    ],
    na.rm = TRUE
  ),
  
  porcentaje_hacinamiento = proporcion_ponderada(
    mat_vivienda$hacin == 1L,
    mat_vivienda$Factor
  ),
  
  porcentaje_rezago = proporcion_ponderada(
    mat_vivienda$rezago == 1L,
    mat_vivienda$Factor
  ),
  
rezago_expand = sum(
  mat_vivienda$Factor[
    mat_vivienda$rezago == 1L &
      !is.na(mat_vivienda$Factor) &
      is.finite(mat_vivienda$Factor) &
      mat_vivienda$Factor > 0
  ],
  nar.rm = TRUE
  )
)

print(resumen_ponderado)


#=================================================================
# 20. RESUMEN PONDERADO DE MATERIALES
#=================================================================

materiales_ponderables <- mat_vivienda %>%
  filter(
    !is.na(rezago),
    clasif_materiales != "No clasificable",
    !is.na(Factor),
    is.finite(Factor),
    Factor > 0
  )

resumen_materiales <- materiales_ponderables %>%
  group_by(clasif_materiales) %>%
  summarise(
    viviendas_observadas = n(),
    viviendas_expand = sum(Factor),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = 100 * viviendas_expand /
      sum(viviendas_expand)
  ) %>%
  arrange(desc(porcentaje))

print(resumen_materiales)


#=================================================================
# 21. RESUMEN PONDERADO DEL REZAGO
#=================================================================

datos_rezago_ponderados <- mat_vivienda %>%
  filter(
    !is.na(rezago),
    !is.na(Factor),
    is.finite(Factor),
    Factor > 0
  )

resultado_rezago <- datos_rezago_ponderados %>%
  summarise(
    viviendas_observadas = n(),
    
    viviendas_expand = sum(Factor),
    
    viviendas_en_rezago = sum(Factor * rezago),
    
    porcentaje_rezago =
      100 * sum(Factor * rezago) / sum(Factor),
    
    viviendas_sin_rezago =
      sum(Factor * (1L - rezago)),
    
    porcentaje_sin_rezago =
      100 * sum(Factor * (1L - rezago)) / sum(Factor)
  )

print(resultado_rezago)


#=================================================================
# 22. CONTROL DE CALIDAD: Se busca encontrar datos que pueden presentarse
# como faltantes 
#=================================================================

control_calidad <- tibble(
  indicador = c(
    "Total de viviendas",
    "Personas faltantes",
    "Cuartos faltantes",
    "Hacinamiento no clasificable",
    "Servicio sanitario faltante",
    "Precariedad no clasificable",
    "Materiales no clasificables",
    "Viviendas con rezago",
    "Viviendas sin rezago",
    "Rezago no clasificable",
    "Factor faltante",
    "Factor no positivo o no finito",
    "Entidad faltante",
    "Tamaño de localidad faltante",
    "Estrato de diseño faltante",
    "UPM faltante"
  ),
  
  registros = c(
    nrow(mat_vivienda),
    sum(is.na(mat_vivienda$Personas)),
    sum(is.na(mat_vivienda$Cuartos)),
    sum(is.na(mat_vivienda$hacin)),
    sum(is.na(mat_vivienda$sanitario)),
    sum(is.na(mat_vivienda$prec_esp)),
    sum(mat_vivienda$clasif_materiales == "No clasificable"),
    sum(mat_vivienda$rezago == 1L, na.rm = TRUE),
    sum(mat_vivienda$rezago == 0L, na.rm = TRUE),
    sum(is.na(mat_vivienda$rezago)),
    sum(is.na(mat_vivienda$Factor)),
    sum(
      !is.na(mat_vivienda$Factor) &
        (!is.finite(mat_vivienda$Factor) |
           mat_vivienda$Factor <= 0)
    ),
    sum(is.na(mat_vivienda$Entidad)),
    sum(is.na(mat_vivienda$Tam_Loc)),
    sum(is.na(mat_vivienda$Estrato_diseno)),
    sum(is.na(mat_vivienda$UPM))
  )
)

print(control_calidad)

#=================================================================
# 23. PREPARAR LA BASE TERRITORIAL
#=================================================================

analisis_territorial <- mat_vivienda %>%
  mutate(
    ENT = as.character(Entidad),
    Entidad_nombre = as.character(Entidad_nombre),
    TLOC_codigo = as.character(Tam_Loc),
    UPM_codigo = as.character(UPM),
    Estrato_codigo = as.character(Estrato_diseno)
  )

cat("\nTamaño de localidad:\n")
print(analisis_territorial %>% count(TLOC_codigo, sort = TRUE))

cat("\nEntidad:\n")
print(analisis_territorial %>% count(ENT, Entidad_nombre))

cat("\nEstrato de diseño:\n")
print(analisis_territorial %>% count(Estrato_codigo, sort = TRUE))


#=================================================================
# 24. CATÁLOGO OFICIAL DE TAMAÑO DE LOCALIDAD: Para poder hacer distinción
# urbano (alto, complemento) y rural
#=================================================================

catalogo_tloc <- tibble(
   TLOC_codigo = c("1", "2","3","4"),
   etiqueta_tloc = c(
     "100 000 habitantes o más",
     "15 000 a 99 999 habitantes",
     "2 500 a 14 999 habitantes",
     "Menos de 2 500 habitantes"
   ),
   ambito = c(
     "Urbano alto",
     "Complemento urbano",
     "Complemento urbano",
     "Rural"
   )
 )

if (nrow(catalogo_tloc) > 0) {
  analisis_territorial <- analisis_territorial %>%
    left_join(catalogo_tloc, by = "TLOC_codigo")
} else {
  analisis_territorial <- analisis_territorial %>%
    mutate(
      etiqueta_tloc = TLOC_codigo,
      ambito = NA_character_
    )
}

#=================================================================
# 25. FUNCIÓN DE RESUMEN TERRITORIAL
#=================================================================

resumir_territorio <- function(datos, grupos) {
  
  datos %>%
    group_by(across(all_of(grupos))) %>%
    summarise(
      viviendas_muestra = n(),
      
      viviendas_clasificables =
        sum(!is.na(rezago)),
      
      viviendas_expandibles =
        sum(
          !is.na(rezago) &
            !is.na(Factor) &
            is.finite(Factor) &
            Factor > 0
        ),
      
      viviendas_expand = sum(
        Factor[
          !is.na(rezago) &
            !is.na(Factor) &
            is.finite(Factor) &
            Factor > 0
        ],
        na.rm = TRUE
      ),
      
      viviendas_rezago_expand = sum(
        Factor[
          rezago == 1L &
            !is.na(rezago) &
            !is.na(Factor) &
            is.finite(Factor) &
            Factor > 0
        ],
        na.rm = TRUE
      ),
      
      viviendas_sin_rezago_expand = sum(
        Factor[
          rezago == 0L &
            !is.na(rezago) &
            !is.na(Factor) &
            is.finite(Factor) &
            Factor > 0
        ],
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    mutate(
      denominador_rezago =
        viviendas_rezago_expand +
        viviendas_sin_rezago_expand,
      
      porcentaje_rezago = if_else(
        denominador_rezago > 0,
        100 * viviendas_rezago_expand /
          denominador_rezago,
        NA_real_
      ),
      
      porcentaje_sin_rezago = if_else(
        denominador_rezago > 0,
        100 * viviendas_sin_rezago_expand /
          denominador_rezago,
        NA_real_
      )
    )
}


#=================================================================
# 26. REZAGO POR ENTIDAD FEDERATIVA
#=================================================================

rezago_entidad <- resumir_territorio(
  analisis_territorial,
  c("ENT", "Entidad_nombre")
) %>%
  arrange(desc(viviendas_rezago_expand))

print(rezago_entidad)


#=================================================================
# 27. REZAGO POR TAMAÑO DE LOCALIDAD
#=================================================================

rezago_tloc <- resumir_territorio(
  analisis_territorial,
  c("TLOC_codigo", "etiqueta_tloc")
) %>%
  arrange(TLOC_codigo)

print(rezago_tloc)


#=================================================================
# 28. REZAGO POR ÁMBITO URBANO-RURAL
#=================================================================

if (any(!is.na(analisis_territorial$ambito))) {
  
  rezago_ambito <- resumir_territorio(
    analisis_territorial %>%
      filter(!is.na(ambito)),
    c("ambito")
  )
  
  print(rezago_ambito)
  
} else {
  
  rezago_ambito <- tibble()
  
  message(
    "Falta completar catalogo_tloc para obtener resultados ",
    "urbanos y rurales válidamente etiquetados."
  )
}


#=================================================================
# 29. CRUCE DE ENTIDAD POR TAMAÑO DE LOCALIDAD
#=================================================================

rezago_entidad_tloc <- resumir_territorio(
  analisis_territorial,
  c(
    "ENT",
    "Entidad_nombre",
    "TLOC_codigo",
    "etiqueta_tloc"
  )
) %>%
  arrange(ENT, TLOC_codigo)

print(rezago_entidad_tloc)


#=================================================================
# 30. HACINAMIENTO POR ENTIDAD Y TAMAÑO DE LOCALIDAD
#=================================================================

resumen_hacinamiento_territorial <- analisis_territorial %>%
  group_by(
    ENT,
    Entidad_nombre,
    TLOC_codigo,
    etiqueta_tloc
  ) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_clasificables = sum(!is.na(hacin)),
    
    viviendas_expand = sum(
      Factor[
        !is.na(hacin) &
          !is.na(Factor) &
          is.finite(Factor) &
          Factor > 0
      ],
      na.rm = TRUE
    ),
    
    porcentaje_hacinamiento = {
      ok <- !is.na(hacin) &
        !is.na(Factor) &
        is.finite(Factor) &
        Factor > 0
      
      if (any(ok)) {
        100 * weighted.mean(
          hacin[ok],
          Factor[ok]
        )
      } else {
        NA_real_
      }
    },
    
    .groups = "drop"
  )

print(resumen_hacinamiento_territorial)


#=================================================================
# 31. RESUMEN DE UPM POR ENTIDAD:para describir la distribución muestral.
#=================================================================

resumen_upm <- analisis_territorial %>%
  filter(
    !is.na(ENT),
    !is.na(UPM_codigo)
  ) %>%
  group_by(
    ENT,
    Entidad_nombre,
    Estrato_codigo,
    UPM_codigo
  ) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_clasificables =
      sum(!is.na(rezago)),
    
    porcentaje_rezago_muestral = if (
      sum(!is.na(rezago)) > 0
    ) {
      100 * mean(rezago, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

print(head(resumen_upm))


#=================================================================
# 32. RESUMEN DE ESTRATOS DE DISEÑO
#=================================================================

resumen_estratos <- analisis_territorial %>%
  group_by(
    ENT,
    Entidad_nombre,
    Estrato_codigo
  ) %>%
  summarise(
    viviendas_muestra = n(),
    
    UPM_observadas = n_distinct(
      UPM_codigo,
      na.rm = TRUE
    ),
    
    porcentaje_rezago_muestral = if (
      sum(!is.na(rezago)) > 0
    ) {
      100 * mean(rezago, na.rm = TRUE)
    } else {
      NA_real_
    },
    
    .groups = "drop"
  )

print(resumen_estratos)


#=================================================================
# 33. ESTIMACIONES CON EL DISEÑO MUESTRAL (OPCIONAL)
#=================================================================

if (requireNamespace("survey", quietly = TRUE)) {
  
  library(dplyr)
  
  #---------------------------------------------------------------
  # 33.1 TRATAMIENTO DE ESTRATOS CON UNA SOLA UPM
  #---------------------------------------------------------------
  
  options(
    survey.lonely.psu = "adjust",
    survey.adjust.domain.lonely = TRUE
  )
  
  #---------------------------------------------------------------
  # 33.2 PREPARAR LA BASE DEL DISEÑO MUESTRAL
  #---------------------------------------------------------------
  
  datos_survey <- analisis_territorial %>%
    filter(
      !is.na(Factor),
      is.finite(Factor),
      Factor > 0,
      !is.na(UPM_codigo),
      !is.na(Estrato_codigo),
      !is.na(ENT)
    ) %>%
    mutate(
      UPM_id = interaction(
        ENT,
        UPM_codigo,
        drop = TRUE
      ),
      
      Estrato_id = interaction(
        ENT,
        Estrato_codigo,
        drop = TRUE
      )
    )
  
  if (nrow(datos_survey) > 0) {
    
    #-------------------------------------------------------------
    # 33.3 REVISAR CUÁNTAS UPM HAY POR ESTRATO
    #-------------------------------------------------------------
    
    control_upm_estrato <- datos_survey %>%
      distinct(Estrato_id, UPM_id) %>%
      count(
        Estrato_id,
        name = "num_UPM"
      )
    
    estratos_una_upm <- control_upm_estrato %>%
      filter(num_UPM == 1)
    
    cat("\nEstratos con una sola UPM:\n")
    print(estratos_una_upm)
    
    cat(
      "\nNúmero de estratos con una sola UPM:",
      nrow(estratos_una_upm),
      "\n"
    )
    
    #-------------------------------------------------------------
    # 33.4 CONSTRUIR EL DISEÑO MUESTRAL UNA SOLA VEZ
    #-------------------------------------------------------------
    
    diseno_envi <- survey::svydesign(
      ids = ~UPM_id,
      strata = ~Estrato_id,
      weights = ~Factor,
      data = datos_survey,
      nest = TRUE
    )
    
    #-------------------------------------------------------------
    # 33.5 ESTIMACIÓN NACIONAL DEL REZAGO
    #-------------------------------------------------------------
    
    # Importante: primero se construye el diseño completo y
    # después se restringe a los registros con rezago conocido.
    
    diseno_rezago <- subset(
      diseno_envi,
      !is.na(rezago)
    )
    
    estimacion_nacional <- survey::svymean(
      ~rezago,
      design = diseno_rezago,
      na.rm = TRUE
    )
    
    cat("\nEstimación nacional del rezago:\n")
    print(estimacion_nacional)
    
    cat("\nIntervalo de confianza nacional:\n")
    print(
      confint(estimacion_nacional)
    )
    
    #-------------------------------------------------------------
    # 33.6 ESTIMACIÓN POR ENTIDAD FEDERATIVA
    #-------------------------------------------------------------
    
    estimacion_por_entidad <- survey::svyby(
      ~rezago,
      ~Entidad_nombre,
      design = diseno_rezago,
      FUN = survey::svymean,
      na.rm = TRUE,
      vartype = c("se", "ci"),
      drop.empty.groups = TRUE
    )
    
    cat("\nEstimación de rezago por entidad:\n")
    print(estimacion_por_entidad)
    
    #-------------------------------------------------------------
    # 33.7 CONVERTIR LA ESTIMACIÓN NACIONAL A PORCENTAJE
    #-------------------------------------------------------------
    
    resultado_survey_nacional <- data.frame(
      indicador = "Proporción de viviendas con rezago",
      estimacion = as.numeric(
        coef(estimacion_nacional)[1]
      ),
      porcentaje = 100 * as.numeric(
        coef(estimacion_nacional)[1]
      ),
      error_estandar = 100 * as.numeric(
        SE(estimacion_nacional)[1]
      )
    )
    
    intervalo_nacional <- confint(
      estimacion_nacional
    )
    
    resultado_survey_nacional$IC95_inferior <-
      100 * intervalo_nacional[1, 1]
    
    resultado_survey_nacional$IC95_superior <-
      100 * intervalo_nacional[1, 2]
    
    cat("\nResumen nacional en porcentaje:\n")
    print(resultado_survey_nacional)
    
  } else {
    
    warning(
      "No hay registros válidos para construir el diseño muestral."
    )
  }
  
} else {
  
  message(
    "Instala el paquete survey con install.packages('survey')."
  )
}
#=================================================================
# 34. GRÁFICA GENERAL DEL HACINAMIENTO
#=================================================================

grafica_general <- ggplot(
  resumen_hacinamiento,
  aes(
    x = Nivel_hacinamiento,
    y = porcentaje,
    fill = Nivel_hacinamiento
  )
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Sin hacinamiento" = "#2A9D8F",
      "Hacinamiento" = "#C8553D"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Distribución del hacinamiento",
    subtitle = "Viviendas clasificables",
    x = "Condición",
    y = "Porcentaje",
    caption = "Fuente: ENVI 2020. Estimación sin ponderar."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafica_general)


#=================================================================
# 35. GRÁFICA DE REZAGO POR ENTIDAD
#=================================================================

top_entidades_rezago <- rezago_entidad %>%
  filter(
    !is.na(porcentaje_rezago),
    viviendas_clasificables > 0
  ) %>%
  slice_max(
    order_by = porcentaje_rezago,
    n = 10,
    with_ties = FALSE
  ) %>%
  mutate(
    Entidad_nombre = reorder(
      Entidad_nombre,
      porcentaje_rezago
    )
  )

grafica_rezago_entidad <- ggplot(
  top_entidades_rezago,
  aes(
    x = Entidad_nombre,
    y = porcentaje_rezago
  )
) +
  geom_col(fill = "#B54A4A", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje_rezago)),
    hjust = -0.1,
    size = 3.2
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "Entidades con mayor proporción estimada de rezago",
    subtitle = "Porcentaje ponderado entre viviendas clasificables",
    x = "Entidad federativa",
    y = "Rezago (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafica_rezago_entidad)


#=================================================================
# 36. MATRIZ DE REZAGO POR ENTIDAD Y TAMAÑO DE LOCALIDAD
#=================================================================

grafica_matriz_territorial <- ggplot(
  rezago_entidad_tloc %>%
    filter(!is.na(porcentaje_rezago)),
  aes(
    x = etiqueta_tloc,
    y = reorder(
      Entidad_nombre,
      porcentaje_rezago,
      FUN = median,
      na.rm = TRUE
    ),
    fill = porcentaje_rezago
  )
) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_gradient(
    low = "#F4E9D8",
    high = "#B54A4A",
    name = "Rezago (%)"
  ) +
  labs(
    title = "Rezago por entidad y tamaño de localidad",
    x = "Tamaño de localidad",
    y = "Entidad federativa",
    caption = "Usar etiquetas oficiales del catálogo ENVI."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 35, hjust = 1)
  )

print(grafica_matriz_territorial)


#=================================================================
# 37. GRÁFICA DE MATERIALES
#=================================================================

grafica_materiales <- ggplot(
  resumen_materiales,
  aes(
    x = clasif_materiales,
    y = porcentaje,
    fill = clasif_materiales
  )
) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.4,
    size = 3.8
  ) +
  scale_fill_manual(
    values = c(
      "Materiales deteriorados" = "#C8553D",
      "Materiales regulares" = "#E9C46A",
      "Materiales adecuados" = "#2A9D8F"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Clasificación exploratoria de materiales",
    subtitle = "Porcentaje ponderado de viviendas clasificables",
    x = "Clasificación",
    y = "Porcentaje"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafica_materiales)


#=================================================================
# 38. GRÁFICA DEL REZAGO NACIONAL
#=================================================================

resumen_grafica_rezago <- datos_rezago_ponderados %>%
  group_by(estado_rezago) %>%
  summarise(
    viviendas_expand = sum(Factor),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = 100 * viviendas_expand /
      sum(viviendas_expand)
  )

grafica_rezago <- ggplot(
  resumen_grafica_rezago,
  aes(
    x = estado_rezago,
    y = porcentaje,
    fill = estado_rezago
  )
) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(
    aes(label = sprintf("%.1f%%", porcentaje)),
    vjust = -0.4,
    size = 4
  ) +
  scale_fill_manual(
    values = c(
      "Con rezago" = "#C8553D",
      "Sin rezago" = "#2A9D8F"
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Rezago habitacional estimado",
    subtitle = "Porcentaje ponderado entre viviendas clasificables",
    x = "Condición de rezago",
    y = "Porcentaje",
    caption = "Indicador exploratorio; no equivale a una clasificación oficial."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafica_rezago)


#=================================================================
# 39. PREPARAR LA CARPETA DE RESULTADOS
#=================================================================

directorio_resultados <- "resultados_envi"

dir.create(
  directorio_resultados,
  showWarnings = FALSE,
  recursive = TRUE
)

exportar_csv <- function(datos, nombre) {
  readr::write_csv(
    datos,
    file.path(directorio_resultados, nombre),
    na = ""
  )
}


#=================================================================
# 40. EXPORTAR RESÚMENES DE VIVIENDA
#=================================================================

exportar_csv(
  resumen_vivienda,
  "resumen_general_vivienda.csv"
)

exportar_csv(
  resumen_hacinamiento,
  "distribucion_hacinamiento.csv"
)

exportar_csv(
  resumen_ponderado,
  "resumen_ponderado.csv"
)

exportar_csv(
  resumen_materiales,
  "resumen_materiales_ponderado.csv"
)

exportar_csv(
  resultado_rezago,
  "resultado_rezago_ponderado.csv"
)

exportar_csv(
  control_calidad,
  "control_calidad.csv"
)


#=================================================================
# 41. EXPORTAR RESULTADOS TERRITORIALES
#=================================================================

exportar_csv(
  rezago_entidad,
  "rezago_por_entidad.csv"
)

exportar_csv(
  rezago_tloc,
  "rezago_por_tamano_localidad.csv"
)

exportar_csv(
  rezago_entidad_tloc,
  "rezago_entidad_tamano_localidad.csv"
)

exportar_csv(
  resumen_hacinamiento_territorial,
  "hacinamiento_territorial.csv"
)

exportar_csv(
  resumen_upm,
  "resumen_upm.csv"
)

exportar_csv(
  resumen_estratos,
  "resumen_estratos.csv"
)

if (nrow(rezago_ambito) > 0) {
  exportar_csv(
    rezago_ambito,
    "rezago_urbano_rural.csv"
  )
}

if (exists("estimacion_por_entidad")) {
  exportar_csv(
    as.data.frame(estimacion_por_entidad),
    "estimacion_survey_por_entidad.csv"
  )
}


#=================================================================
# 42. GUARDAR LAS GRÁFICAS
#=================================================================

ggsave(
  file.path(directorio_resultados, "hacinamiento_general.png"),
  plot = grafica_general,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(directorio_resultados, "rezago_por_entidad.png"),
  plot = grafica_rezago_entidad,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  file.path(directorio_resultados, "matriz_rezago_territorial.png"),
  plot = grafica_matriz_territorial,
  width = 11,
  height = 10,
  dpi = 300
)

ggsave(
  file.path(directorio_resultados, "clasificacion_materiales.png"),
  plot = grafica_materiales,
  width = 9,
  height = 6,
  dpi = 300
)

ggsave(
  file.path(directorio_resultados, "rezago_habitacional.png"),
  plot = grafica_rezago,
  width = 9,
  height = 6,
  dpi = 300
)


#=================================================================
# 43. GUARDAR LOS OBJETOS ANALÍTICOS EN RDS
#=================================================================

objetos_resultados <- list(
  datos_vivienda = datos_vivienda,
  mat_vivienda = mat_vivienda,
  analisis_territorial = analisis_territorial,
  
  resumen_vivienda = resumen_vivienda,
  resumen_hacinamiento = resumen_hacinamiento,
  resumen_ponderado = resumen_ponderado,
  resumen_materiales = resumen_materiales,
  resultado_rezago = resultado_rezago,
  control_calidad = control_calidad,
  
  rezago_entidad = rezago_entidad,
  rezago_tloc = rezago_tloc,
  rezago_ambito = rezago_ambito,
  rezago_entidad_tloc = rezago_entidad_tloc,
  resumen_hacinamiento_territorial =
    resumen_hacinamiento_territorial,
  resumen_upm = resumen_upm,
  resumen_estratos = resumen_estratos,
  
  grafica_general = grafica_general,
  grafica_rezago_entidad = grafica_rezago_entidad,
  grafica_matriz_territorial = grafica_matriz_territorial,
  grafica_materiales = grafica_materiales,
  grafica_rezago = grafica_rezago
)

if (exists("estimacion_nacional")) {
  objetos_resultados$estimacion_nacional <-
    estimacion_nacional
}

if (exists("estimacion_por_entidad")) {
  objetos_resultados$estimacion_por_entidad <-
    estimacion_por_entidad
}

saveRDS(
  objetos_resultados,
  file = file.path(
    directorio_resultados,
    "resultados_analisis_envi.rds"
  )
)


#=================================================================
# 44. FINALIZAR
#=================================================================

message(
  "Análisis terminado. Resultados guardados en: ",
  normalizePath(directorio_resultados)
)

#=================================================================
# 45. PREPARAR LA SEGUNDA ETAPA DEL ANÁLISIS: esto es expandible a
# población objetivo y población prioritaria teniendo en cuenta 
# caracteristicas estructurales
#=================================================================

dir.create(
  "resultados_poblacion_potencial",
  showWarnings = FALSE,
  recursive = TRUE
)

# Comprobar variables disponibles
variables_requeridas <- c(
  "Folio", "vivienda", "Entidad", "Tam_Loc",
  "Personas", "Cuartos", "paredes", "techo", "piso",
  "sanitario", "Factor", "hacin", "mat_det",
  "mat_reg", "prec_esp", "rezago"
)

faltantes <- setdiff(
  variables_requeridas,
  names(mat_vivienda)
)

if (length(faltantes) > 0) {
  stop(
    "Faltan variables en mat_vivienda: ",
    paste(faltantes, collapse = ", ")
  )
}

# Crear una base independiente para la segunda etapa
poblacion_vivienda <- mat_vivienda


#=================================================================
# 46. IDENTIFICAR FALLAS FÍSICAS REPORTADAS: no son tomadas como rezago
# pero sí son entendidas como elementos de calidad de la vivienda o como
# motivos de intervencion 
#=================================================================

variables_fallas <- c(
  "grietas",
  "pandeos",
  "levantamientos",
  "humedad",
  "fracturas",
  "electricos",
  "tuberias"
)

for (v in variables_fallas) {
  
  if (v %in% names(poblacion_vivienda)) {
    
    nueva <- paste0("falla_", v)
    
    poblacion_vivienda[[nueva]] <- case_when(
      is.na(poblacion_vivienda[[v]]) ~ NA_integer_,
      as.character(poblacion_vivienda[[v]]) == "1" ~ 1L,
      as.character(poblacion_vivienda[[v]]) == "2" ~ 0L,
      TRUE ~ NA_integer_
    )
  }
}

indicadores_fallas <- intersect(
  paste0("falla_", variables_fallas),
  names(poblacion_vivienda)
)

# Indicador de al menos una falla reportada
if (length(indicadores_fallas) > 0) {
  
  matriz_fallas <- as.data.frame(
    poblacion_vivienda[indicadores_fallas]
  )
  
  poblacion_vivienda$falla_reportada <- case_when(
    rowSums(matriz_fallas == 1L, na.rm = TRUE) > 0 ~ 1L,
    rowSums(!is.na(matriz_fallas)) == 0 ~ NA_integer_,
    rowSums(matriz_fallas == 0L, na.rm = TRUE) ==
      rowSums(!is.na(matriz_fallas)) ~ 0L,
    TRUE ~ NA_integer_
  )
  
} else {
  
  poblacion_vivienda$falla_reportada <- NA_integer_
  
  message(
    "No se encontraron variables de fallas físicas. ",
    "Agrega P4_25_1 a P4_25_7 a sql_vivienda."
  )
}


#=================================================================
# 47. CLASIFICAR la Forma de intervención
#=================================================================


poblacion_vivienda <- poblacion_vivienda %>%
  mutate(
    
    # PRIORIDAD 1: Sustitución
    # Paredes deterioradas o regulares
    sustitucion = case_when(
      is.na(paredes) ~ NA_integer_,
      paredes %in% c(
        codigos_pared_deteriorada,
        codigos_pared_regular
      ) ~ 1L,
      TRUE ~ 0L
    ),
    
    # PRIORIDAD 2: Mejoramiento
    # Techos deteriorados o regulares, o pisos regulares
    mejoramiento = case_when(
      sustitucion == 1L ~ 0L,
      
      sustitucion == 0L &
        (
          techo_det == 1L |
            techo_reg == 1L |
            piso_reg == 1L
        ) ~ 1L,
      
      sustitucion == 0L &
        techo_det == 0L &
        techo_reg == 0L &
        piso_reg == 0L ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    # PRIORIDAD 3: Ampliación o reemplazo
    # Hacinamiento O carencia de sanitario con excusado
    ampliacion = case_when(
      sustitucion == 1L ~ 0L,
      mejoramiento == 1L ~ 0L,
      
      sustitucion == 0L &
        mejoramiento == 0L &
        (hacin == 1L | excu == 1L) ~ 1L,
      
      sustitucion == 0L &
        mejoramiento == 0L &
        hacin == 0L &
        excu == 0L ~ 0L,
      
      TRUE ~ NA_integer_
    ),
    
    # Verificar que los componentes necesarios estén observados
    componentes_observados =
      !is.na(paredes) &
      !is.na(techo_det) &
      !is.na(techo_reg) &
      !is.na(piso_reg) &
      !is.na(hacin) &
      !is.na(excu),
    
    # Clasificación final
    nivel_intervencion = case_when(
      sustitucion == 1L ~ "Sustitución",
      mejoramiento == 1L ~ "Mejoramiento",
      ampliacion == 1L ~ "Ampliación o reemplazo",
      
      componentes_observados &
        mat_det == 0L &
        mat_reg == 0L &
        hacin == 0L &
        excu == 0L &
        (is.na(falla_reportada) |
           falla_reportada == 0L) ~
        "Sin nivel de intervención clasificable",
      
      sustitucion == 0L &
        mejoramiento == 0L &
        ampliacion == 0L ~
        "Sin rezago habitacional",
      
      TRUE ~ NA_character_
    )
  )
# Comprobar la distribución
print(
  table(
    poblacion_vivienda$nivel_intervencion,
    useNA = "ifany"
  )
)

# Exportar base de vivienda clasificada
write_csv(
  poblacion_vivienda,
  "resultados_poblacion_potencial/viviendas_clasificadas.csv",
  na = ""
)

#=================================================================
# 47.1 INCORPORAR EL CATÁLOGO DE TAMAÑO DE LOCALIDAD
#=================================================================

catalogo_tloc <- tibble(
  Tam_Loc = c("1", "2", "3", "4"),
  etiqueta_tloc = c(
    "100 000 habitantes o más",
    "15 000 a 99 999 habitantes",
    "2 500 a 14 999 habitantes",
    "Menos de 2 500 habitantes"
  ),
  ambito = c(
    "Urbano alto",
    "Complemento urbano",
    "Complemento urbano",
    "Rural"
  ),
  ambito_general = c(
    "Urbano",
    "Urbano",
    "Urbano",
    "Rural"
  )
)

poblacion_vivienda <- poblacion_vivienda %>%
  select(
    -any_of(c(
      "etiqueta_tloc",
      "ambito",
      "ambito_general"
    ))
  ) %>%
  mutate(
    Tam_Loc = trimws(as.character(Tam_Loc))
  ) %>%
  left_join(
    catalogo_tloc,
    by = "Tam_Loc"
  )

# Comprobar que las columnas existen
print(
  poblacion_vivienda %>%
    count(
      Tam_Loc,
      etiqueta_tloc,
      ambito_general,
      .drop = FALSE
    )
)

# Revisar códigos sin correspondencia
print(
  poblacion_vivienda %>%
    filter(
      !is.na(Tam_Loc),
      is.na(ambito_general)
    ) %>%
    count(Tam_Loc)
)

#=================================================================
# 48. PREPARAR VARIABLES PARA LA ESTIMACIÓN PONDERADA
#=================================================================

# Verificar si el número de hogares está disponible
if (!"Hogares" %in% names(poblacion_vivienda)) {
  poblacion_vivienda$Hogares <- NA_real_
}

# Asegurar tipos numéricos
poblacion_vivienda <- poblacion_vivienda %>%
  mutate(
    Factor = suppressWarnings(
      as.numeric(as.character(Factor))
    ),
    Personas = suppressWarnings(
      as.numeric(as.character(Personas))
    ),
    Hogares = suppressWarnings(
      as.numeric(as.character(Hogares))
    )
  )

# Base válida para estimaciones ponderadas
base_ponderada <- poblacion_vivienda %>%
  filter(
    !is.na(Factor),
    is.finite(Factor),
    Factor > 0,
    !is.na(nivel_intervencion)
  )


#=================================================================
# 49. ESTIMAR nivel de intervención A NIVEL NACIONAL
#=================================================================

estimacion_nacional_intervencion <- base_ponderada %>%
  group_by(nivel_intervencion) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_estimadas = sum(
      Factor,
      na.rm = TRUE
    ),
    
    personas_en_viviendas_estimadas = sum(
      Personas * Factor,
      na.rm = TRUE
    ),
    
    hogares_estimados = if (
      all(is.na(Hogares))
    ) {
      NA_real_
    } else {
      sum(Hogares * Factor, na.rm = TRUE)
    },
    
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje_viviendas =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  arrange(desc(viviendas_estimadas))

print(estimacion_nacional_intervencion)

write_csv(
  estimacion_nacional_intervencion,
  "resultados_poblacion_potencial/estimacion_nacional_intervencion.csv",
  na = ""
)


#=================================================================
# 50. ESTIMACIÓN POR ENTIDAD FEDERATIVA
#=================================================================

estimacion_entidad_intervencion <- base_ponderada %>%
  filter(!is.na(Entidad)) %>%
  group_by(Entidad, nivel_intervencion) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_estimadas = sum(
      Factor,
      na.rm = TRUE
    ),
    
    personas_estimadas = sum(
      Personas * Factor,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(Entidad) %>%
  mutate(
    porcentaje_viviendas =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  ungroup()

print(estimacion_entidad_intervencion)

write_csv(
  estimacion_entidad_intervencion,
  "resultados_poblacion_potencial/estimacion_por_entidad.csv",
  na = ""
)

#=============================================================
# ESTIMACION POR NIVEL DE REZAGO
#=============================================================

estimacion_entidad_rezago <- base_ponderada %>%
  filter(!is.na(Entidad)) %>%
  group_by(Entidad, estado_rezago) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_estimadas = sum(
      Factor,
      na.rm = TRUE
    ),
    
    personas_estimadas = sum(
      Personas * Factor,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(Entidad) %>%
  mutate(
    porcentaje_viviendas =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  ungroup()

print(estimacion_entidad_rezago)

write_csv(
  estimacion_entidad_rezago,
  "resultados_poblacion_potencial/rezago_por_entidad.csv",
  na = ""
)

#=================================================================
# 50.1 CATÁLOGO DE TAMAÑO DE LOCALIDAD Y ÁMBITO URBANO-RURAL
#=================================================================

codigos_sin_catalogo <- poblacion_vivienda %>%
  filter(
    !is.na(Tam_Loc),
    is.na(etiqueta_tloc)
  ) %>%
  count(
    Tam_Loc,
    name = "viviendas_muestra"
  )

print(codigos_sin_catalogo)

if (nrow(codigos_sin_catalogo) > 0) {
  warning(
    "Hay códigos Tam_Loc que no están en el catálogo. ",
    "Revisa sus valores antes de interpretar los resultados."
  )
}

#=================================================================
# 51. ESTIMACIÓN POR ENTIDAD, TAMAÑO Y ÁMBITO DE LOCALIDAD
#=================================================================

estimacion_entidad_tloc <- base_ponderada %>%
  filter(
    !is.na(Entidad),
    !is.na(Tam_Loc),
    !is.na(ambito_general),
    !is.na(nivel_intervencion),
    !is.na(Factor),
    is.finite(Factor),
    Factor > 0
  ) %>%
  group_by(
    Entidad,
    Tam_Loc,
    etiqueta_tloc,
    ambito,
    ambito_general,
    nivel_intervencion
  ) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_estimadas = sum(
      Factor,
      na.rm = TRUE
    ),
    
    personas_estimadas = sum(
      Personas * Factor,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(
    Entidad,
    Tam_Loc
  ) %>%
  mutate(
    porcentaje_viviendas =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  ungroup() %>%
  arrange(
    Entidad,
    Tam_Loc,
    nivel_intervencion
  )

print(
  head(
    estimacion_entidad_tloc,
    20
  )
)

#=================================================================
# 51.1 BALANCE URBANO-RURAL POR ENTIDAD
#=================================================================

balance_urbano_rural <- estimacion_entidad_tloc %>%
  group_by(
    Entidad,
    ambito_general,
    nivel_intervencion
  ) %>%
  summarise(
    viviendas_muestra = sum(viviendas_muestra),
    
    viviendas_estimadas = sum(
      viviendas_estimadas,
      na.rm = TRUE
    ),
    
    personas_estimadas = sum(
      personas_estimadas,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(
    Entidad,
    nivel_intervencion
  ) %>%
  mutate(
    porcentaje_del_nivel =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  ungroup() %>%
  arrange(
    Entidad,
    nivel_intervencion,
    ambito_general
  )

print(
  head(
    balance_urbano_rural,
    20
  )
)

readr::write_csv(
  balance_urbano_rural,
  "resultados_poblacion_potencial/03b_balance_urbano_rural.csv",
  na = ""
)

#=================================================================
# 52. IDENTIFICAR TERRITORIOS CON MAYOR nivel de intervencion
#=================================================================

territorios_sustitucion <- estimacion_entidad_tloc %>%
  filter(
    nivel_intervencion == "Sustitución"
  ) %>%
  arrange(desc(viviendas_estimadas))

print(
  head(
    territorios_sustitucion,
    20
  )
)

write_csv(
  territorios_sustitucion,
  "resultados_poblacion_potencial/territorios_sustitucion.csv",
  na = ""
)


#=================================================================
# 53. GRÁFICA DE NECESIDAD HABITACIONAL POR ENTIDAD
#=================================================================

grafica_intervencion_entidad <- estimacion_entidad_intervencion %>%
  filter(
    nivel_intervencion == "Sustitución"
  ) %>%
  ggplot(
    aes(
      x = reorder(
        as.character(Entidad),
        viviendas_estimadas
      ),
      y = viviendas_estimadas
    )
  ) +
  geom_col(
    fill = "#B54A4A",
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ",",
      decimal.mark = "."
    )
  ) +
  labs(
    title = "Viviendas con nivel de intervención: Sustitución",
    subtitle = "Estimación ponderada por entidad",
    x = "Entidad",
    y = "Viviendas estimadas",
    caption = paste(
      "Fuente: ENVI 2020.",
      "Clasificación SIESCO/CONAVI 2024."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafica_intervencion_entidad)

ggsave(
  "resultados_poblacion_potencial/sustitucion_por_entidad.png",
  plot = grafica_intervencion_entidad,
  width = 10,
  height = 8,
  dpi = 300
)

#=================================================================
# 54. EXTRAER LA INFORMACIÓN DE DEMANDA DESDE DUCKDB
#=================================================================

tabla_demanda <- "conjunto_de_datos_tdemanda_envi_2020"

tablas_disponibles <- DBI::dbListTables(con)

if (!tabla_demanda %in% tablas_disponibles) {
  stop(
    "No existe la tabla ",
    tabla_demanda,
    " en DuckDB."
  )
}

columnas_demanda <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT * FROM ",
    '"', tabla_demanda, '"',
    " LIMIT 0"
  )
)

variables_demanda_requeridas <- c(
  "FOLIO",
  "VIV_SEL",
  "HOGAR",
  "C_DENEVI"
)

faltantes_demanda <- setdiff(
  variables_demanda_requeridas,
  names(columnas_demanda)
)

if (length(faltantes_demanda) > 0) {
  stop(
    "Faltan variables en la tabla de demanda: ",
    paste(faltantes_demanda, collapse = ", ")
  )
}

demanda_original <- DBI::dbGetQuery(
  con,
  paste0(
    'SELECT "FOLIO", "VIV_SEL", "HOGAR", "C_DENEVI" ',
    "FROM ", '"', tabla_demanda, '"'
  )
)

# Comprobar que se recuperaron registros
message(
  "Registros de demanda recuperados: ",
  nrow(demanda_original)
)


#=================================================================
# 55. NORMALIZAR LAS CLAVES DE VIVIENDA
# Se normalizan como texto para evitar problemas cuando unas
# tablas guardan los identificadores como números y otras como texto.
#=================================================================


normalizar_folio <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  stringr::str_pad(
    x,
    width = 7,
    side = "left",
    pad = "0"
  )
}

normalizar_vivienda <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  
  stringr::str_pad(
    x,
    width = 2,
    side = "left",
    pad = "0"
  )
}

poblacion_vivienda <- poblacion_vivienda %>%
  mutate(
    Folio = normalizar_folio(Folio),
    vivienda = normalizar_vivienda(vivienda)
  )

demanda_original <- demanda_original %>%
  mutate(
    FOLIO = normalizar_folio(FOLIO),
    VIV_SEL = normalizar_vivienda(VIV_SEL),
    HOGAR = trimws(as.character(HOGAR))
  )


#=================================================================
# 56. AGREGAR LA DEMANDA AL NIVEL DE VIVIENDA
#=================================================================

demanda_vivienda <- demanda_original %>%
  filter(
    !is.na(FOLIO),
    !is.na(VIV_SEL)
  ) %>%
  group_by(
    FOLIO,
    VIV_SEL
  ) %>%
  summarise(
    registros_demanda = n(),
    
    hogares_con_registro_demanda = n_distinct(
      HOGAR[!is.na(HOGAR) & HOGAR != ""]
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    tiene_registro_demanda = 1L
  )

# Revisar que haya una sola fila por vivienda
duplicados_demanda_agregada <- demanda_vivienda %>%
  count(FOLIO, VIV_SEL) %>%
  filter(n > 1)

stopifnot(
  nrow(duplicados_demanda_agregada) == 0
)


#=================================================================
# 57. COMPROBAR LA COBERTURA DEL CRUCE
#=================================================================

viviendas_antes_cruce <- nrow(poblacion_vivienda)

viviendas_demanda <- poblacion_vivienda %>%
  left_join(
    demanda_vivienda,
    by = c(
      "Folio" = "FOLIO",
      "vivienda" = "VIV_SEL"
    )
  ) %>%
  mutate(
    tiene_registro_demanda = if_else(
      is.na(tiene_registro_demanda),
      0L,
      tiene_registro_demanda
    ),
    
    registros_demanda = coalesce(
      registros_demanda,
      0L
    ),
    
    hogares_con_registro_demanda = coalesce(
      hogares_con_registro_demanda,
      0L
    )
  )

# Control: el cruce no debe multiplicar las viviendas
stopifnot(
  nrow(viviendas_demanda) == viviendas_antes_cruce
)

cat(
  "\nViviendas antes del cruce:",
  viviendas_antes_cruce,
  "\n"
)

cat(
  "Viviendas después del cruce:",
  nrow(viviendas_demanda),
  "\n"
)

cat(
  "Viviendas con algún registro en demanda:",
  sum(viviendas_demanda$tiene_registro_demanda == 1L),
  "\n"
)

# Exportar base integrada
readr::write_csv(
  viviendas_demanda,
  "resultados_poblacion_potencial/viviendas_con_demanda.csv",
  na = ""
)


#=================================================================
# 58. CRUZAR NECESIDAD HABITACIONAL CON REGISTROS DE DEMANDA
#=================================================================

cruce_necesidad_demanda <- viviendas_demanda %>%
  filter(
    !is.na(nivel_intervencion),
    !is.na(Factor),
    is.finite(Factor),
    Factor > 0
  ) %>%
  group_by(
    nivel_intervencion,
    tiene_registro_demanda
  ) %>%
  summarise(
    viviendas_muestra = n(),
    
    viviendas_estimadas = sum(
      Factor,
      na.rm = TRUE
    ),
    
    personas_estimadas = sum(
      Personas * Factor,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  group_by(nivel_intervencion) %>%
  mutate(
    porcentaje_dentro_nivel =
      100 * viviendas_estimadas /
      sum(viviendas_estimadas)
  ) %>%
  ungroup() %>%
  mutate(
    registro_demanda = case_when(
      tiene_registro_demanda == 1L ~
        "Con registro en TDEMANDA",
      TRUE ~
        "Sin registro en TDEMANDA"
    )
  )

print(cruce_necesidad_demanda)

readr::write_csv(
  cruce_necesidad_demanda,
  "resultados_poblacion_potencial/cruce_necesidad_demanda.csv",
  na = ""
)

#=================================================================
# 59. RECUPERAR INFORMACIÓN DE HOGARES DESDE LA TABLA DE VIVIENDAS
#=================================================================

tabla_vivienda <- "conjunto_de_datos_tvivienda_envi_2020"

columnas_tvivienda <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT * FROM ",
    '"', tabla_vivienda, '"',
    " LIMIT 0"
  )
)

variables_hogar_disponibles <- c(
  "P1_2",
  "P1_3"
)

faltantes_hogar <- setdiff(
  variables_hogar_disponibles,
  names(columnas_tvivienda)
)

if (length(faltantes_hogar) == 0) {
  
  hogares_sql <- paste0(
    'SELECT "FOLIO", "VIV_SEL", ',
    '"P1_2" AS gasto_comun, ',
    '"P1_3" AS hogares ',
    "FROM ", '"', tabla_vivienda, '"'
  )
  
  datos_hogar_vivienda <- DBI::dbGetQuery(
    con,
    hogares_sql
  ) %>%
    mutate(
      FOLIO = normalizar_folio(FOLIO),
      VIV_SEL = normalizar_vivienda(VIV_SEL),
      hogares = suppressWarnings(
        as.numeric(as.character(hogares))
      )
    )
  
  # Verificar que exista una sola fila por vivienda
  duplicados_hogar <- datos_hogar_vivienda %>%
    count(FOLIO, VIV_SEL) %>%
    filter(n > 1)
  
  if (nrow(duplicados_hogar) > 0) {
    stop(
      "La tabla de viviendas tiene claves repetidas. ",
      "Revisa los registros antes del cruce."
    )
  }
  
  viviendas_demanda <- viviendas_demanda %>%
    select(
      -any_of(c("gasto_comun", "hogares"))
    ) %>%
    left_join(
      datos_hogar_vivienda,
      by = c(
        "Folio" = "FOLIO",
        "vivienda" = "VIV_SEL"
      )
    )
  
  # Eliminar el valor anterior que se usó como marcador
  # si el número de hogares no estaba disponible.
  viviendas_demanda$Hogares <- NULL
  
  viviendas_demanda <- viviendas_demanda %>%
    mutate(
      Hogares = hogares
    )
  
  message(
    "Se incorporaron las variables de gasto común ",
    "y número de hogares."
  )
  
} else {
  
  message(
    "No se incorporaron variables de hogar. Faltan: ",
    paste(faltantes_hogar, collapse = ", ")
  )
}


#=================================================================
# 60. RECUPERAR VARIABLES DE LA TABLA DE DEMANDA: (REVISAR ESTAS)
#=================================================================

demanda_detallada <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT * FROM ",
    '"', tabla_demanda, '"'
  )
)

variables_demanda_interes <- c(
  "P3A2_2",
  "P3A2_3",
  "P3A3",
  "P3A4",
  "P3A5",
  "P3A6",
  "P3A7",
  "P3A8_1",
  "P3A8_2",
  "P3A8_3",
  "P3A9",
  "P3A10",
  "P3A11"
)

variables_demanda_interes <- intersect(
  variables_demanda_interes,
  names(demanda_detallada)
)

distribuciones_demanda <- list()

for (v in variables_demanda_interes) {
  
  temporal <- demanda_detallada %>%
    mutate(
      categoria = as.character(.data[[v]])
    ) %>%
    count(
      categoria,
      name = "registros"
    ) %>%
    mutate(
      variable = v,
      porcentaje_registros =
        100 * registros / sum(registros)
    ) %>%
    select(
      variable,
      categoria,
      registros,
      porcentaje_registros
    )
  
  distribuciones_demanda[[v]] <- temporal
}

if (length(distribuciones_demanda) > 0) {
  
  resumen_variables_demanda <- bind_rows(
    distribuciones_demanda
  )
  
  readr::write_csv(
    resumen_variables_demanda,
    "resultados_poblacion_potencial/distribucion_variables_demanda.csv",
    na = ""
  )
  
  print(
    head(
      resumen_variables_demanda,
      30
    )
  )
}


#=================================================================
# 61. EXAMINAR LOS CÓDIGOS DE TAMAÑO DE LOCALIDAD
#=================================================================

catalogo_localidades_observado <- poblacion_vivienda %>%
  count(
    Tam_Loc,
    name = "viviendas_muestra"
  ) %>%
  arrange(Tam_Loc)

print(catalogo_localidades_observado)

readr::write_csv(
  catalogo_localidades_observado,
  "resultados_poblacion_potencial/catalogo_tamano_localidad_observado.csv",
  na = ""
)

#=================================================================
# 62. PREPARAR UNA NOTA METODOLÓGICA
#=================================================================

nota_metodologica <- c(
  paste("ANÁLISIS EXPLORATORIO DE NECESIDADES HABITACIONALES Y",
  "POBLACIÓN OBJETIVO PARA UN PROGRAMA PÚBLICO:", 
  "COMO EL PROGRAMA 'VIVIENDA PARA EL BIENESTAR'."
  ),
  "",
  "Fuente: Encuesta Nacional de Vivienda (ENVI) 2020, INEGI.",
  "",
  "Objetivo:",
  paste(
    "Identificar y dimensionar viviendas con condiciones",
    "habitacionales potencialmente susceptibles de atención."
  ),
  "",
  "Unidad principal de análisis:",
  "Vivienda particular habitada.",
  "",
  "Clasificación:",
  paste(
    "Se utilizan indicadores exploratorios de materiales,",
    "hacinamiento, carencia sanitaria, fallas reportadas así",
    "como demanda de vivienda"
  ),
  "",
  "Estimación:",
  paste(
    "Se utiliza el factor de expansión de la ENVI.",
    "La validez de las estimaciones depende de la correcta",
    "especificación del diseño muestral."
  ),
  "",
  "Integración de demanda:",
  paste(
    "Los registros de demanda se agregan al nivel de vivienda",
    "antes del cruce para evitar multiplicar los registros."
  ),
  "",
  "Limitaciones:",
  paste(
    "La clasificación no equivale al rezago habitacional oficial",
    "Se trata solo de un ejercicio práctico de curces de datos."
  ),
  paste(
    "Los resultados de demanda requieren interpretación con",
    "el cuestionario y los catálogos oficiales."
  ),
  paste(
    "El número estimado de personas corresponde a quienes",
    "residen en viviendas clasificadas, no a beneficiarios de ningun programa."
  )
)

writeLines(
  nota_metodologica,
  "resultados_poblacion_potencial/nota_metodologica.txt"
)


#=================================================================
# 63. EXPORTAR TABLAS FINALES
#=================================================================

readr::write_csv(
  estimacion_nacional_intervencion,
  "resultados_poblacion_potencial/01_estimacion_nacional.csv",
  na = ""
)

readr::write_csv(
  estimacion_entidad_intervencion,
  "resultados_poblacion_potencial/02_estimacion_entidad.csv",
  na = ""
)

readr::write_csv(
  estimacion_entidad_tloc,
  "resultados_poblacion_potencial/03_estimacion_entidad_tloc.csv",
  na = ""
)

readr::write_csv(
  balance_urbano_rural,
  "resultados_poblacion_potencial/03b_balance_urbano_rural.csv",
  na = ""
)

readr::write_csv(
  cruce_necesidad_demanda,
  "resultados_poblacion_potencial/04_cruce_necesidad_demanda.csv",
  na = ""
)

readr::write_csv(
  viviendas_demanda,
  "resultados_poblacion_potencial/05_base_integrada.csv",
  na = ""
)

readr::write_csv(
  estimacion_entidad_rezago,
  "resultados_poblacion_potencial/06_estimacion_rezagos.csv",
  na = ""
)

#=================================================================
# 64. GUARDAR LOS RESULTADOS EN RDS
#=================================================================

saveRDS(
  list(
    poblacion_vivienda = poblacion_vivienda,
    viviendas_demanda = viviendas_demanda,
    estimacion_nacional_intervencion =
      estimacion_nacional_intervencion,
    estimacion_entidad_intervencion =
      estimacion_entidad_intervencion,
    estimacion_entidad_tloc =
      estimacion_entidad_tloc,
    territorios_sustitucion =
      territorios_sustitucion,
    cruce_necesidad_demanda =
      cruce_necesidad_demanda,
    catalogo_localidades_observado =
      catalogo_localidades_observado
  ),
  file = "resultados_poblacion_potencial/analisis_poblacion_potencial.rds"
)


#=================================================================
# 65. FINALIZAR
#=================================================================

message(
  "Segunda etapa finalizada. Resultados en: ",
  normalizePath(
    "resultados_poblacion_potencial"
  )
)

dir.create("datos", showWarnings = FALSE)

saveRDS(
  list(
    nacional = estimacion_nacional_intervencion,
    entidad = estimacion_entidad_intervencion,
    entidad_tloc = estimacion_entidad_tloc,
    hacinamiento = resumen_hacinamiento_territorial,
    materiales = resumen_materiales,
    rezago = rezago_entidad,
    rezago_entidad = estimacion_entidad_rezago,
    demanda = cruce_necesidad_demanda
  ),
  file = "datos/dashboard_envi.rds"
)
