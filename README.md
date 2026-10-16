Procesamiento de datos ENVI 2020

Descripción
Este repositorio contiene scripts en R para procesar los datos de ENVI 2020 utilizando DuckDB y paquetes del ecosistema R.


Requisitos
R instalado en la computadora.
Los paquetes DBI, duckdb, dplyr, ggplot2, readr, tibble y survey.
El archivo ZIP de datos requerido por el script.
Instalación de paquetes

paquetes <- c(
  "DBI", "duckdb", "dplyr", "ggplot2",
  "readr", "tibble", "survey"
)

instalar <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(instalar) > 0) {
  install.packages(instalar)
}

Uso
Descarga o clona este repositorio.
Coloca el ZIP de datos en tú carpeta del proyecto.
Abre el proyecto en RStudio.
Ejecuta el script de tratamiento.
La base DuckDB se generará localmente durante el procesamiento, según la configuración del script.


Datos
Puedes acceder a ellos desde https://www.inegi.org.mx/programas/envi/2020/#datos_abiertos 

Reproducibilidad
Los resultados pueden variar según las versiones de R, los paquetes utilizados y los archivos de entrada.

Puedes consultar la aplicación interactiva del proyecto ENVI 2020:

https://edwin-pineda-prueba-dashboards-mx-01-r.shinyapps.io/app_v1/
