# ============================================================
# TRABAJO FINAL ASET
# Procesamiento de ENAHO 2025 - Perú
# ============================================================



# Paso 1: PAQUETES ---------------------------------------------

library(tidyverse)
library(haven)



# Paso 2: IDENTIFICACIÓN DE ARCHIVOS ---------------------------

# Identificación de archivos en la carpeta "bases"
archivos <- list.files(
  path = "bases",
  full.names = TRUE)

archivos



# Paso 3: CARGA DE LA BASE ENAHO -------------------------------

# Identificar los archivos disponibles en la carpeta bases
archivos <- list.files(
  path = "bases",
  pattern = "\\.csv$",
  full.names = TRUE)

archivos


# Identificar el módulo 500 de la ENAHO
archivo_500 <- archivos[
  grepl("500", basename(archivos))]

archivo_500


# Verificar que se haya encontrado un único archivo
if (length(archivo_500) != 1) {
  stop("No se encontró un único archivo correspondiente al módulo 500 en la carpeta bases.")}


# Cargar el módulo 500
ENAHO_informacion <- read_delim(
  archivo_500,
  delim = ";",
  locale = locale(
    encoding = "Latin1",
    decimal_mark = ","
    ),
  show_col_types = FALSE)



# Paso 4: RECODIFICACIÓN DE VARIABLES ---------------------------

# Recodificación de sexo y departamento

ENAHO_informacion <- ENAHO_informacion %>%
  mutate(
    
    # Sexo
    sexo = case_when(
      P207 == 1 ~ "Varón",
      P207 == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),
    
    # Departamento a partir de UBIGEO
    departamento = case_when(
      substr(UBIGEO, 1, 2) == "01" ~ "Amazonas",
      substr(UBIGEO, 1, 2) == "02" ~ "Áncash",
      substr(UBIGEO, 1, 2) == "03" ~ "Apurímac",
      substr(UBIGEO, 1, 2) == "04" ~ "Arequipa",
      substr(UBIGEO, 1, 2) == "05" ~ "Ayacucho",
      substr(UBIGEO, 1, 2) == "06" ~ "Cajamarca",
      substr(UBIGEO, 1, 2) == "07" ~ "Callao",
      substr(UBIGEO, 1, 2) == "08" ~ "Cusco",
      substr(UBIGEO, 1, 2) == "09" ~ "Huancavelica",
      substr(UBIGEO, 1, 2) == "10" ~ "Huánuco",
      substr(UBIGEO, 1, 2) == "11" ~ "Ica",
      substr(UBIGEO, 1, 2) == "12" ~ "Junín",
      substr(UBIGEO, 1, 2) == "13" ~ "La Libertad",
      substr(UBIGEO, 1, 2) == "14" ~ "Lambayeque",
      substr(UBIGEO, 1, 2) == "15" ~ "Lima",
      substr(UBIGEO, 1, 2) == "16" ~ "Loreto",
      substr(UBIGEO, 1, 2) == "17" ~ "Madre de Dios",
      substr(UBIGEO, 1, 2) == "18" ~ "Moquegua",
      substr(UBIGEO, 1, 2) == "19" ~ "Pasco",
      substr(UBIGEO, 1, 2) == "20" ~ "Piura",
      substr(UBIGEO, 1, 2) == "21" ~ "Puno",
      substr(UBIGEO, 1, 2) == "22" ~ "San Martín",
      substr(UBIGEO, 1, 2) == "23" ~ "Tacna",
      substr(UBIGEO, 1, 2) == "24" ~ "Tumbes",
      substr(UBIGEO, 1, 2) == "25" ~ "Ucayali",
      TRUE ~ NA_character_
    )
  )



# Paso 5: DEFINICIÓN DE LA POBLACIÓN DE ANÁLISIS ----------------

# Se consideran trabajadores dependientes:
# 3 = Empleado
# 4 = Obrero
# 6 = Trabajador del hogar

ENAHO_ASALA <- ENAHO_informacion %>%
  filter(P507 %in% c(3, 4, 6)) %>%
  mutate(
    categoria_ocupacional = case_when(
      P507 == 3 ~ "Empleado",
      P507 == 4 ~ "Obrero",
      P507 == 6 ~ "Trabajador del hogar",
      TRUE ~ NA_character_
    )
  )

# Comprobación de la población ----------------------------

ENAHO_ASALA %>%
  count(P507, categoria_ocupacional)



# Paso 6: CONSTRUCCIÓN DEL INGRESO MENSUAL ---------------------

# Convertir la variable de ingreso a formato numérico

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    P524A1 = as.numeric(P524A1)
  )


# Reemplazar el código 999999 por NA

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    ingreso_pago = if_else(
      P524A1 == 999999,
      NA_real_,
      P524A1
    )
  )


# Convertir los ingresos a periodicidad mensual

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    ingreso_mensual = case_when(
      P523 == 1 ~ ingreso_pago * 30,
      P523 == 2 ~ ingreso_pago * 4.33,
      P523 == 3 ~ ingreso_pago * 2,
      P523 == 4 ~ ingreso_pago,
      TRUE ~ NA_real_
    )
  )



# Paso 7: CONSTRUCCIÓN DE LA VARIABLE DUMMY BAJO SMV ------------

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    bajo_smv = if_else(
      ingreso_mensual < 1130,
      1,
      0,
      missing = NA_real_
    )
  )


summary(ENAHO_ASALA$ingreso_mensual)

quantile(
  ENAHO_ASALA$ingreso_mensual,
  probs = c(0, .10, .25, .50, .75, .90, .95, .99, 1),
  na.rm = TRUE
)

table(ENAHO_ASALA$bajo_smv, useNA = "ifany")



# Paso 8: GRUPOS DE EDAD -------------------------------------

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    grupo_edad = case_when(
      P208A >= 14 & P208A <= 24 ~ "14 a 24 años",
      P208A >= 25 & P208A <= 44 ~ "25 a 44 años",
      P208A >= 45 & P208A <= 64 ~ "45 a 64 años",
      P208A >= 65 ~ "65 años o más",
      TRUE ~ NA_character_
    )
  )

ENAHO_ASALA %>%
  count(grupo_edad)


# BAJO SMV SEGÚN SEXO

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  count(sexo, bajo_smv) %>%
  group_by(sexo) %>%
  mutate(porcentaje = n / sum(n) * 100)


# BAJO SMV SEGÚN GRUPO DE EDAD

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  count(grupo_edad, bajo_smv) %>%
  group_by(grupo_edad) %>%
  mutate(porcentaje = n / sum(n) * 100)



# Paso 9: BAJO SMV SEGÚN CATEGORÍA OCUPACIONAL ---------------

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  count(categoria_ocupacional, bajo_smv) %>%
  group_by(categoria_ocupacional) %>%
  mutate(
    porcentaje = n / sum(n) * 100
  )



# Paso 10: SITUACIÓN CONTRACTUAL -----------------------------

ENAHO_ASALA <- ENAHO_ASALA %>%
  mutate(
    tipo_contrato = case_when(
      P511A == "1" ~ "Indefinido / permanente",
      P511A == "2" ~ "Plazo fijo",
      P511A == "3" ~ "Período de prueba",
      P511A == "4" ~ "Formación laboral / prácticas",
      P511A == "5" ~ "Locación de servicios",
      P511A == "6" ~ "CAS",
      P511A == "7" ~ "Sin contrato",
      P511A == "8" ~ "Otro",
      TRUE ~ NA_character_
    )
  )


ENAHO_ASALA %>%
  count(tipo_contrato, sort = TRUE)



# Paso 11: ANÁLISIS CON FACTOR DE EXPANSIÓN --------------------

# DISTRIBUCIÓN PONDERADA DEL INDICADOR BAJO SMV

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  )


# BAJO SMV SEGÚN SEXO - PONDERO

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(sexo, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# BAJO SMV SEGÚN EDAD - PONDERADO

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(grupo_edad, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# BAJO SMV SEGÚN CATEGORÍA OCUPACIONAL - PONDERADO

ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(categoria_ocupacional, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# BAJO SMV SEGÚN TIPO DE CONTRATO - PONDERADO

ENAHO_ASALA %>%
  filter(
    !is.na(bajo_smv),
    !is.na(tipo_contrato)
  ) %>%
  group_by(tipo_contrato, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()



# Paso 12: TABLAS DE RESULTADOS ------------------------------

# CARACTERIZACIÓN DE LA MUESTRA SEGÚN SEXO
tabla_muestra_sexo <- ENAHO_ASALA %>%
  count(sexo, name = "casos") %>%
  mutate(
    porcentaje = casos / sum(casos) * 100
  )


# CARACTERIZACIÓN DE LA MUESTRA SEGÚN GRUPO DE EDAD
tabla_muestra_edad <- ENAHO_ASALA %>%
  count(grupo_edad, name = "casos") %>%
  mutate(
    porcentaje = casos / sum(casos) * 100
  )


# RESULTADO GENERAL
tabla_general <- ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  )


# SEGÚN SEXO
tabla_sexo <- ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(sexo, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# SEGÚN GRUPO DE EDAD
tabla_edad <- ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(grupo_edad, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# SEGÚN CATEGORÍA OCUPACIONAL
tabla_categoria <- ENAHO_ASALA %>%
  filter(!is.na(bajo_smv)) %>%
  group_by(categoria_ocupacional, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()


# SEGÚN TIPO DE CONTRATO
tabla_contrato <- ENAHO_ASALA %>%
  filter(
    !is.na(bajo_smv),
    !is.na(tipo_contrato)
  ) %>%
  group_by(tipo_contrato, bajo_smv) %>%
  summarise(
    poblacion_estimada = sum(FAC500A, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  mutate(
    porcentaje = poblacion_estimada / sum(poblacion_estimada) * 100
  ) %>%
  ungroup()



# Paso 13: EXPORTACIÓN DE TABLAS -----------------------------------

write_csv(tabla_general, "resultados/tabla_general.csv")
write_csv(tabla_sexo, "resultados/tabla_sexo.csv")
write_csv(tabla_edad, "resultados/tabla_edad.csv")
write_csv(tabla_categoria, "resultados/tabla_categoria.csv")
write_csv(tabla_contrato, "resultados/tabla_contrato.csv")
write_csv(tabla_muestra_sexo, "resultados/tabla_muestra_sexo.csv")
write_csv(tabla_muestra_edad, "resultados/tabla_muestra_edad.csv")


# Paso 14: GRÁFICOS ------------------------------------------------

# GRÁFICO 1: DISTRIBUCIÓN DEL INGRESO MENSUAL

grafico_ingreso <- ENAHO_ASALA %>%
  filter(!is.na(ingreso_mensual)) %>%
  ggplot(aes(x = ingreso_mensual)) +
  geom_histogram(
    bins = 40
  ) +
  geom_vline(
    xintercept = 1130,
    linetype = "dashed",
    linewidth = 1
  ) +
  coord_cartesian(xlim = c(0, 6000)) +
  labs(
    title = "Distribución del ingreso mensual",
    subtitle = "Trabajadores dependientes, ENAHO 2025",
    x = "Ingreso mensual (S/)",
    y = "Número de casos"
  ) +
  theme_minimal()


# GRÁFICO 2: INCIDENCIA GENERAL DE LA DUMMY

grafico_smv <- tabla_general %>%
  mutate(
    situacion = if_else(
      bajo_smv == 1,
      "Por debajo del SMV",
      "Igual o superior al SMV"
    )
  ) %>%
  ggplot(aes(x = situacion, y = porcentaje)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.4
  ) +
  labs(
    title = "Trabajadores según relación entre ingreso mensual y SMV",
    subtitle = "Resultados ponderados, ENAHO 2025",
    x = NULL,
    y = "Porcentaje"
  ) +
  theme_minimal()


# GRÁFICO 3: BAJO SMV SEGÚN SEXO

grafico_sexo <- tabla_sexo %>%
  filter(bajo_smv == 1) %>%
  ggplot(aes(x = sexo, y = porcentaje)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.4
  ) +
  labs(
    title = "Incidencia de ingresos inferiores al SMV según sexo",
    subtitle = "Trabajadores dependientes, ENAHO 2025",
    x = NULL,
    y = "Porcentaje"
  ) +
  theme_minimal()


# GRÁFICO 4: BAJO SMV SEGÚN GRUPO DE EDAD

grafico_edad <- tabla_edad %>%
  filter(bajo_smv == 1) %>%
  ggplot(aes(x = grupo_edad, y = porcentaje)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.4
  ) +
  labs(
    title = "Incidencia de ingresos inferiores al SMV según edad",
    subtitle = "Trabajadores dependientes, ENAHO 2025",
    x = NULL,
    y = "Porcentaje"
  ) +
  theme_minimal()



# Paso 15: EXPORTACIÓN DE GRÁFICOS ---------------------------------

ggsave(
  "resultados/grafico_ingreso.png",
  grafico_ingreso,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "resultados/grafico_smv.png",
  grafico_smv,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "resultados/grafico_sexo.png",
  grafico_sexo,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "resultados/grafico_edad.png",
  grafico_edad,
  width = 8,
  height = 5,
  dpi = 300
)



# Paso 16: BASE PARA DASHBOARD SHINY -------------------------------

base_dashboard <- ENAHO_ASALA %>%
  select(
    sexo,
    grupo_edad,
    categoria_ocupacional,
    tipo_contrato,
    bajo_smv,
    FAC500A
  )

write_csv(
  base_dashboard,
  "resultados/base_dashboard.csv"
)



# Paso 17: REGISTRO DE EJECUCIÓN -----------------------------------

registro <- tibble(
  fecha = Sys.Date(),
  hora = format(Sys.time(), "%H:%M:%S"),
  archivo = "procesamiento.R",
  estado = "Ejecución completada"
)

archivo_log <- "resultados/log_ejecucion.csv"

if (file.exists(archivo_log)) {
  write_csv(
    registro,
    archivo_log,
    append = TRUE
  )
} else {
  write_csv(
    registro,
    archivo_log
  )
}


glimpse(
  ENAHO_informacion %>%
    select(P207, P208A, UBIGEO, P507, P523, P524A1, P511A, FAC500A)
)