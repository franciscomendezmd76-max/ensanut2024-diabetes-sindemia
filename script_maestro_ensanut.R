# ==============================================================================
# SCRIPT MAESTRO DE REPRODUCCIÓN EPIDEMIOLÓGICA: SINDEMIA Y DIABETES EN MÉXICO
# Análisis de Microdatos de la ENSANUT 2024 bajo Criterios ADA 2026
# ------------------------------------------------------------------------------
# Autor: M.C.M Francisco Méndez Ramírez
# Contacto: Franciscomendezmd@hotmail.com
# Institución: UMAPS La Oriental, Secretaría de Salud de Guanajuato
# Fecha de última actualización: Mayo, 2026
# Licencia: MIT (Apta para compartir en GitHub)
# ==============================================================================

# ------------------------------------------------------------------------------
# FASE 1: CONFIGURACIÓN DEL ENTORNO Y CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
cat("\n[1/7] Instalando y cargando librerías del ecosistema Tidyverse y Survey...\n")

# Listado de paquetes requeridos para el diseño complejo y exportación profesional
paquetes <- c("tidyverse", "srvyr", "survey", "openxlsx", "haven")
nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(nuevos_paquetes)) install.packages(nuevos_paquetes, dependencies = TRUE)

library(tidyverse)  # Manipulación de datos (dplyr, ggplot2, stringr)
library(srvyr)      # Sintaxis tidy para encuestas de diseño complejo
library(survey)     # Motor estadístico para muestras complejas
library(openxlsx)   # Generación de reportes estilizados en Excel
library(haven)      # Lectura de archivos .sav (SPSS) de la ENSANUT

# Configuración de rutas (Ruta absoluta de la Mac del Dr. Méndez)
ruta_base <- "/Users/franciscomendez/Desktop/Características clínicas/ENSANUT 2024"
setwd(ruta_base)

# Ajuste metodológico para manejar Unidades Primarias de Muestreo (UPM) únicas/solitarias
options(survey.lonely.psu = "adjust")

# ------------------------------------------------------------------------------
# FASE 2: DEPURACIÓN, FILTRADO Y RECODIFICACIÓN CLÍNICA (DATA WRANGLING)
# ------------------------------------------------------------------------------
cat("[2/7] Procesando variables, escalas psicométricas y criterios ADA 2026...\n")

# Función auxiliar para limpiar códigos de no respuesta de la ENSANUT (8, 9, 98, 99)
limpiar_valores <- function(x) {
  ifelse(x %in% c(8, 9, 98, 99), NA_real_, x)
}

# Flujo principal de depuración desde la base integrada original
base_analitica <- base_maestra %>%
  # Criterios de Inclusión Basales
  filter(EDAD >= 20 & A0301 == 1) %>%                  # Adultos con diagnóstico previo de diabetes
  filter(!AN06 %in% c(1, 3) | is.na(AN06)) %>%          # Exclusión estricta de mujeres embarazadas
  
  # Conversión y limpieza de biomarcadores laboratoriales
  mutate(
    HB1AC   = as.numeric(HB1AC),
    COL_LDL = as.numeric(COL_LDL),
    
    # CORRECCIÓN DE PRESIÓN ARTERIAL (Variables reales del módulo de antropometría AN27)
    # Se extraen las lecturas de la 2da y 3ra toma para el promedio epidemiológico
    pas_toma2 = as.numeric(AN27_02S),
    pad_toma2 = as.numeric(AN27_02D),
    pas_toma3 = as.numeric(AN27_03S),
    pad_toma3 = as.numeric(AN27_03D),
    
    # Cálculo del promedio descartando la 1ra toma por sesgo de estrés
    PAS = rowMeans(cbind(pas_toma2, pas_toma3), na.rm = TRUE),
    PAD = rowMeans(cbind(pad_toma2, pad_toma3), na.rm = TRUE),
    PAS = ifelse(is.nan(PAS), NA_real_, PAS),
    PAD = ifelse(is.nan(PAD), NA_real_, PAD)
  ) %>%
  
  # EVALUACIÓN DE METAS CLÍNICAS INDIVIDUALES Y COMPUESTAS (ADA 2026)
  mutate(
    meta_A_glucosa = ifelse(HB1AC < 7.0, 1, 0),
    meta_B_presion = ifelse(PAS < 130 & PAD < 80, 1, 0),
    meta_C_ldl_70  = ifelse(COL_LDL < 70, 1, 0),
    meta_C_ldl_100 = ifelse(COL_LDL < 100, 1, 0),
    meta_C_ldl_55  = ifelse(COL_LDL < 55, 1, 0)
  ) %>%
  mutate(
    meta_ABC_estandar = ifelse(meta_A_glucosa == 1 & meta_B_presion == 1 & meta_C_ldl_100 == 1, 1, 0),
    meta_ABC_clinica  = ifelse(meta_A_glucosa == 1 & meta_B_presion == 1 & meta_C_ldl_70 == 1, 1, 0),
    meta_ABC_estricta = ifelse(meta_A_glucosa == 1 & meta_B_presion == 1 & meta_C_ldl_55 == 1, 1, 0)
  ) %>%
  
  # PROCESAMIENTO DE SALUD MENTAL: Escala de Depresión del Centro de Estudios Epidemiológicos (CES-D7)
  mutate(
    across(c(A0211, A0212, A0213, A0214, A0215, A0217), limpiar_valores),
    across(c(A0211, A0212, A0213, A0214, A0215, A0217), ~ . - 1), # Recodificar de 1-4 a escala 0-3
    A0216_rev = case_when(
      limpiar_valores(A0216) == 1 ~ 3, limpiar_valores(A0216) == 2 ~ 2,
      limpiar_valores(A0216) == 3 ~ 1, limpiar_valores(A0216) == 4 ~ 0, TRUE ~ NA_real_
    ),
    score_depresion = A0211 + A0212 + A0213 + A0214 + A0215 + A0217 + A0216_rev,
    sospecha_depresion = ifelse(score_depresion >= 9, "Con sospecha", "Sin sospecha"),
    depresion_factor   = fct_relevel(as.factor(sospecha_depresion), "Sin sospecha")
  ) %>%
  
  # PROCESAMIENTO DE SEGURIDAD NUTRICIONAL: Escala de Inseguridad Alimentaria (ELCSA)
  mutate(
    across(c(H0701, H0702, H0703, H0704, H0705), ~ case_when(. == 1 ~ 1, . == 2 ~ 0, TRUE ~ NA_real_)),
    score_elcsa = H0701 + H0702 + H0703 + H0704 + H0705,
    inseguridad_alimentaria = case_when(
      score_elcsa == 0 ~ "Seguridad", score_elcsa %in% 1:2 ~ "IA Leve",
      score_elcsa %in% 3:4 ~ "IA Moderada", score_elcsa == 5 ~ "IA Severa", TRUE ~ NA_character_
    ),
    ia_factor = fct_relevel(as.factor(inseguridad_alimentaria), "Seguridad")
  ) %>%
  
  # AGRUPACIÓN EPIDEMIOLÓGICA DE LOS SITIOS DE ATENCIÓN MÉDICA
  mutate(
    sitio_atencion_txt = haven::as_factor(A0305),
    sitio_agrupado = case_when(
      str_detect(sitio_atencion_txt, "IMSS|ISSSTE|PEMEX") & !str_detect(sitio_atencion_txt, "BIENESTAR") ~ "1. Seguridad Social",
      str_detect(sitio_atencion_txt, "SSA|BIENESTAR|Centro de Salud") ~ "2. Público sin SS",
      str_detect(sitio_atencion_txt, "farmacia") ~ "3. CAF (Farmacias)",
      str_detect(sitio_atencion_txt, "privad|domicilio|empresa") ~ "4. Privado",
      TRUE ~ "5. Otro"
    ),
    sitio_agrupado = fct_relevel(as.factor(sitio_agrupado), "1. Seguridad Social")
  ) %>%
  
  # CONSTRUCCIÓN DEL ÍNDICE DE CALIDAD TÉCNICA DE LA ATENCIÓN (Cuidado Mínimo ADA)
  mutate(
    revision_pies = case_when(A0306D == 1 ~ 1, A0306D == 2 ~ 0, TRUE ~ NA_real_),
    revision_ojos = case_when(A0306H == 1 ~ 1, A0306H == 2 ~ 0, TRUE ~ NA_real_),
    labs_sangre   = case_when(A0306J == 1 ~ 1, A0306J == 2 ~ 0, TRUE ~ NA_real_),
    labs_lipidos  = case_when(A0306K == 1 ~ 1, A0306K == 2 ~ 0, TRUE ~ NA_real_),
    score_cuidado = revision_pies + revision_ojos + labs_sangre + labs_lipidos,
    calidad_atencion = case_when(
      score_cuidado >= 3 ~ "Adecuada (3-4 acciones)",
      score_cuidado < 3  ~ "Deficiente (0-2 acciones)", TRUE ~ NA_character_
    ),
    calidad_atencion = fct_relevel(as.factor(calidad_atencion), "Deficiente (0-2 acciones)")
  ) %>%
  
  # VARIABLES DEMOGRÁFICAS Y SOCIOECONÓMICAS ADICIONALES
  mutate(
    sexo_factor      = haven::as_factor(SEXO),
    nsef_terciles    = haven::as_factor(NSEF),
    rec_ejercicio    = case_when(A0306F == 1 ~ 1, A0306F == 2 ~ 0, TRUE ~ NA_real_),
    actividad_fisica = ifelse(rec_ejercicio == 1, "Con recomendación", "Sin recomendación"),
    actividad_fisica = fct_relevel(as.factor(actividad_fisica), "Sin recomendación")
  )

# Restricción estricta de la submuestra analítica con biomarcadores laboratorios completos
base_analitica_laboratorio <- base_analitica %>%
  filter(!is.na(PONDE_VENOSA_ST) & PONDE_VENOSA_ST > 0)

# ------------------------------------------------------------------------------
# FASE 3: DECLARACIÓN DEL DISEÑO MUESTRAL COMPLEJO
# ------------------------------------------------------------------------------
cat("[3/7] Estableciendo la estructura del diseño complejo de la ENSANUT...\n")

diseno_final <- base_analitica_laboratorio %>%
  as_survey_design(
    ids = UPM,              
    strata = EST_SEL,       
    weights = PONDE_VENOSA_ST, 
    nest = TRUE             
  )

# ------------------------------------------------------------------------------
# FASE 4: ANÁLISIS DESCRIPTIVO DE LA POBLACIÓN (TABLAS 1 Y 2)
# ------------------------------------------------------------------------------
cat("[4/7] Ejecutando estimaciones univariadas y prevalencias ponderadas...\n")

generar_tabla1_descriptiva <- function(diseno, variable_str) {
  diseno %>%
    filter(!is.na(!!sym(variable_str))) %>%
    group_by(Categoria = as.character(!!sym(variable_str))) %>% 
    summarise(
      n_muestral  = unweighted(n()),
      N_expandida = survey_total(vartype = NULL),
      Porcentaje  = survey_mean(vartype = "ci") * 100
    ) %>%
    mutate(
      Variable = variable_str, N_expandida = round(N_expandida, 0),
      Porcentaje = round(Porcentaje, 1), Porcentaje_low = round(Porcentaje_low, 1), Porcentaje_upp = round(Porcentaje_upp, 1)
    ) %>%
    select(Variable, Categoria, n_muestral, N_expandida, Porcentaje, Porcentaje_low, Porcentaje_upp)
}

Tabla_1_Consolidada <- bind_rows(
  generar_tabla1_descriptiva(diseno_final, "sexo_factor"),
  generar_tabla1_descriptiva(diseno_final, "depresion_factor"),
  generar_tabla1_descriptiva(diseno_final, "ia_factor"),
  generar_tabla1_descriptiva(diseno_final, "sitio_agrupado"),
  generar_tabla1_descriptiva(diseno_final, "calidad_atencion"),
  generar_tabla1_descriptiva(diseno_final, "actividad_fisica"),
  generar_tabla1_descriptiva(diseno_final, "nsef_terciles")
)

# Prevalencias Nacionales de las Metas ADA 2026
estimar_prevalencia_meta <- function(diseno, meta_var) {
  diseno %>% filter(!is.na(!!sym(meta_var))) %>% summarise(prop = survey_mean(!!sym(meta_var), vartype = "ci"))
}

prev_A   <- estimar_prevalencia_meta(diseno_final, "meta_A_glucosa")
prev_B   <- estimar_prevalencia_meta(diseno_final, "meta_B_presion")
prev_C   <- estimar_prevalencia_meta(diseno_final, "meta_C_ldl_70")
prev_abc_est <- estimar_prevalencia_meta(diseno_final, "meta_ABC_estandar")
prev_abc_cli <- estimar_prevalencia_meta(diseno_final, "meta_ABC_clinica")
prev_abc_estric <- estimar_prevalencia_meta(diseno_final, "meta_ABC_estricta")

Tabla_2_Prevalencias <- data.frame(
  Criterio_ADA_2026 = c("Meta A: Glucosa", "Meta B: Presión", "Meta C: Lípidos", "ABC Estándar", "ABC Clínica", "ABC Estricta"),
  Prevalencia_Porcentaje = round(c(prev_A$prop, prev_B$prop, prev_C$prop, prev_abc_est$prop, prev_abc_cli$prop, prev_abc_estric$prop) * 100, 2),
  IC_95_Inferior = round(c(prev_A$prop_low, prev_B$prop_low, prev_C$prop_low, prev_abc_est$prop_low, prev_abc_cli$prop_low, prev_abc_estric$prop_low) * 100, 2),
  IC_95_Superior = round(c(prev_A$prop_upp, prev_B$prop_upp, prev_C$prop_upp, prev_abc_est$prop_upp, prev_abc_cli$prop_upp, prev_abc_estric$prop_upp) * 100, 2)
)
Tabla_2_Prevalencias$IC_95_Inferior <- ifelse(Tabla_2_Prevalencias$IC_95_Inferior < 0, 0, Tabla_2_Prevalencias$IC_95_Inferior)

# ------------------------------------------------------------------------------
# FASE 5: ANÁLISIS BIVARIADO DE CALIDAD SEGÚN SITIO DE ATENCIÓN (TABLA 5)
# ------------------------------------------------------------------------------
cat("[5/7] Calculando cruces bivariados de procesos de calidad mediante pruebas de Wald...\n")

test_pies    <- svychisq(~revision_pies + sitio_agrupado, design = diseno_final, statistic = "Wald")
test_ojos    <- svychisq(~revision_ojos + sitio_agrupado, design = diseno_final, statistic = "Wald")
test_sangre  <- svychisq(~labs_sangre + sitio_agrupado, design = diseno_final, statistic = "Wald")
test_lipidos <- svychisq(~labs_lipidos + sitio_agrupado, design = diseno_final, statistic = "Wald")
test_indice  <- svychisq(~calidad_atencion + sitio_agrupado, design = diseno_final, statistic = "Wald")

# Imprimir resultados del análisis bivariado en la consola
print(svyby(~revision_pies, ~sitio_agrupado, design = diseno_final, FUN = svymean, na.rm = TRUE))
print(svyby(~revision_ojos, ~sitio_agrupado, design = diseno_final, FUN = svymean, na.rm = TRUE))
print(svyby(~labs_sangre, ~sitio_agrupado, design = diseno_final, FUN = svymean, na.rm = TRUE))
print(svyby(~labs_lipidos, ~sitio_agrupado, design = diseno_final, FUN = svymean, na.rm = TRUE))
print(svyby(~calidad_atencion, ~sitio_agrupado, design = diseno_final, FUN = svymean, na.rm = TRUE))

# ------------------------------------------------------------------------------
# FASE 6: MODELADO MULTIVARIADO - REGRÉSIÓN DE POISSON ROBUSTA (TABLA 3)
# ------------------------------------------------------------------------------
cat("[6/7] Ajustando modelos robustos multivariados log-lineales (svyglm)...\n")

ajustar_poisson_robusto <- function(meta_desenlace, diseno) {
  formula_mod <- as.formula(paste(meta_desenlace, "~ depresion_factor + ia_factor + sitio_agrupado + calidad_atencion + actividad_fisica + nsef_terciles + EDAD + sexo_factor"))
  svyglm(formula_mod, design = diseno, family = quasipoisson(link = "log"))
}

modelo_A_glucosa <- ajustar_poisson_robusto("meta_A_glucosa", diseno_final)
modelo_B_presion <- ajustar_poisson_robusto("meta_B_presion", diseno_final)
modelo_C_lipidos <- ajustar_poisson_robusto("meta_C_ldl_70", diseno_final)

extraer_estimadores_rp <- function(modelo) {
  sum_mod  <- summary(modelo)
  coef_mat <- sum_mod$coefficients
  ci_mat   <- confint(modelo)
  
  data.frame(
    Determinante = rownames(coef_mat),
    Razon_Prevalencia_RP = round(exp(coef_mat[, "Estimate"]), 2),
    IC_95_Inferior = round(exp(ci_mat[, 1]), 2),
    IC_95_Superior = round(exp(ci_mat[, 2]), 2),
    Error_Estandar = round(coef_mat[, "Std. Error"], 3),
    Valor_p = round(coef_mat[, "Pr(>|t|)"], 4)
  )
}

Tabla_3_Glucosa <- extraer_estimadores_rp(modelo_A_glucosa)
Tabla_3_Presion <- extraer_estimadores_rp(modelo_B_presion)
Tabla_3_Lipidos <- extraer_estimadores_rp(modelo_C_lipidos)

# ------------------------------------------------------------------------------
# FASE 7: EXPORTACIÓN AUTOMATIZADA A DISCO (CSV Y EXCEL PROFESIONAL)
# ------------------------------------------------------------------------------
cat("[7/7] Exportando resultados y aplicando formato ejecutivo a libro Excel...\n")

write.csv(Tabla_1_Consolidada, file.path(ruta_base, "Tabla1_Descriptiva_Nacional.csv"), row.names = FALSE)

wb <- createWorkbook()
estilo_titulo <- createStyle(fontName = "Arial", fontSize = 14, textDecoration = "bold", fontColour = "#1F4E78")
estilo_header <- createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold", fontColour = "#FFFFFF", fgFill = "#1F4E78", halign = "center", border = "bottom")

addWorksheet(wb, "Prevalencias_Nacionales")
writeData(wb, "Prevalencias_Nacionales", "PREVALENCIAS NACIONALES REALES - METAS ADA 2026", startCol = 1, startRow = 1)
addStyle(wb, "Prevalencias_Nacionales", estilo_titulo, rows = 1, cols = 1)
writeData(wb, "Prevalencias_Nacionales", Tabla_2_Prevalencias, startCol = 1, startRow = 3, headerStyle = estilo_header)
setColWidths(wb, "Prevalencias_Nacionales", cols = 1:4, widths = "auto")

addWorksheet(wb, "Modelo_Glucosa_MetaA")
writeData(wb, "Modelo_Glucosa_MetaA", "REGRESIÓN MULTIVARIADA DE POISSON: METAA GLUCOSA", startCol = 1, startRow = 1)
addStyle(wb, "Modelo_Glucosa_MetaA", estilo_titulo, rows = 1, cols = 1)
writeData(wb, "Modelo_Glucosa_MetaA", Tabla_3_Glucosa, startCol = 1, startRow = 3, headerStyle = estilo_header)
setColWidths(wb, "Modelo_Glucosa_MetaA", cols = 1:6, widths = "auto")

addWorksheet(wb, "Modelo_Presion_MetaB")
writeData(wb, "Modelo_Presion_MetaB", "REGRESIÓN MULTIVARIADA DE POISSON: METAB PRESIÓN", startCol = 1, startRow = 1)
addStyle(wb, "Modelo_Presion_MetaB", estilo_titulo, rows = 1, cols = 1)
writeData(wb, "Modelo_Presion_MetaB", Tabla_3_Presion, startCol = 1, startRow = 3, headerStyle = estilo_header)
setColWidths(wb, "Modelo_Presion_MetaB", cols = 1:6, widths = "auto")

addWorksheet(wb, "Modelo_Lipidos_MetaC")
writeData(wb, "Modelo_Lipidos_MetaC", "REGRESIÓN MULTIVARIADA DE POISSON: METAC COLERSTEROL LDL", startCol = 1, startRow = 1)
addStyle(wb, "Modelo_Lipidos_MetaC", estilo_titulo, rows = 1, cols = 1)
writeData(wb, "Modelo_Lipidos_MetaC", Tabla_3_Lipidos, startCol = 1, startRow = 3, headerStyle = estilo_header)
setColWidths(wb, "Modelo_Lipidos_MetaC", cols = 1:6, widths = "auto")

saveWorkbook(wb, file.path(ruta_base, "Resultados_Modelos_Diabetes_ENSANUT2024.xlsx"), overwrite = TRUE)

cat("\n==================================================================\n")
cat("   ¡REPLICABILIDAD COMPLETA LOGRADA CON ÉXITO, ATTE. DR. MÉNDEZ!        \n")
cat("==================================================================\n")