# Precariedad salarial entre trabajadores dependientes en el Perú

## Trabajo Final – Curso ASET 2026

Este repositorio contiene el trabajo final desarrollado a partir de la Encuesta Nacional de Hogares (ENAHO) 2025 del Instituto Nacional de Estadística e Informática (INEI).

El objetivo del trabajo es analizar la incidencia de ingresos laborales inferiores a la Remuneración Mínima Vital (SMV) entre trabajadores dependientes en el Perú, considerando diferencias según sexo, edad, categoría ocupacional y situación contractual.

## Fuente de información

Se utiliza el módulo de **Empleo e Ingresos** de la ENAHO 2025.

La población de análisis está conformada por trabajadores dependientes pertenecientes a las siguientes categorías ocupacionales:

- Empleados.
- Obreros.
- Trabajadores del hogar.

La muestra analítica está compuesta por **25 208 observaciones**.

## Indicador

Se construye la variable dummy `bajo_smv`, que identifica a los trabajadores según su ingreso laboral mensual:

- `1`: ingreso mensual inferior a S/ 1 130.
- `0`: ingreso mensual igual o superior a S/ 1 130.

Los casos sin información válida de ingreso se mantienen como valores perdidos.

Para obtener estimaciones poblacionales se utiliza el factor de expansión `FAC500A`.

El indicador construido representa específicamente una **dimensión salarial de la precariedad laboral** y no pretende abarcar todas las dimensiones que pueden formar parte de este concepto.

## Estructura del repositorio

- `procesamiento.R`: carga y procesamiento de la ENAHO, construcción de variables y generación de resultados.
- `Proyecto final ASET.Rmd`: código fuente del informe reproducible.
- `Proyecto final ASET.html`: informe final generado en formato HTML.
- `dashboard/app.R`: aplicación interactiva desarrollada con Shiny.
- `resultados/`: tablas, gráficos, base utilizada por el dashboard y registro de ejecución.
- `bases/`: directorio destinado a la base original de ENAHO. La base no se incluye en el repositorio debido al tamaño del archivo.

## Reproducción del análisis

La base original de la ENAHO no se incluye en este repositorio debido al tamaño del archivo. Los microdatos pueden descargarse gratuitamente desde el Sistema de Microdatos del Instituto Nacional de Estadística e Informática (INEI):

[INEI – Sistema de Microdatos](https://proyectos.inei.gob.pe/microdatos/index.htm)

Para obtener la misma base utilizada en este proyecto, en la sección **Consulta por Encuesta** deben seleccionarse las siguientes opciones:

- **Encuesta general:** ENAHO Metodología ACTUALIZADA
- **Encuesta específica:** Condiciones de Vida y Pobreza - ENAHO
- **Año:** 2025
- **Período:** Anual - (Ene-Dic)
- **Módulo:** Empleo e Ingresos (Código Módulo 5)
- **Formato de descarga:** CSV

El archivo utilizado para el análisis corresponde al módulo de Empleo e Ingresos y, una vez descargado y descomprimido, se identifica como:

    Enaho01a-2025-500.csv

Para reproducir el análisis, este archivo debe colocarse dentro de una carpeta denominada `bases` en la raíz del proyecto.

La estructura esperada es:

    proyecto-aset-2026/
    ├── bases/
    │   └── Enaho01a-2025-500.csv
    ├── resultados/
    ├── dashboard/
    │   └── app.R
    ├── procesamiento.R
    ├── Proyecto final ASET.Rmd
    └── Proyecto final ASET.html

Una vez incorporada la base, se debe ejecutar `procesamiento.R`. El script identifica el archivo correspondiente al módulo 500 dentro de la carpeta `bases`, procesa la información y genera las tablas, gráficos, archivos para el dashboard y el registro de ejecución en la carpeta `resultados`.

## Dashboard

El dashboard interactivo fue desarrollado utilizando Shiny.

Para ejecutarlo localmente se debe abrir:

    dashboard/app.R

y seleccionar **Run App** en RStudio.

El dashboard permite visualizar la incidencia estimada de ingresos inferiores al SMV según:

- Sexo.
- Grupo de edad.
- Categoría ocupacional.
- Tipo de contrato.

## Software

El procesamiento, análisis y visualización de los datos fueron desarrollados en **R** y **RStudio**, utilizando principalmente los siguientes paquetes:

- `tidyverse`
- `shiny`

## Autor

**Rodrigo Palao Paucar**