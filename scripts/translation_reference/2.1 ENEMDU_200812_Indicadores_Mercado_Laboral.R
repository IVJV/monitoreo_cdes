# Load libraries
rm(list = ls())      #Clear workspace
library(haven)
library(survey)      #Analyzes complex survey data using weights, strata, and clusters.
library(dplyr) 
library(srvyr)       #dplyr-style interface for analyzing complex survey data
library(stringr) 
library(gt)          #Creates professional, publication-ready tables from data frames.


###==========================================================================###
###                                                                          ###
###                                                                          ###
###                             1. Load database                             ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###                    1.1. Database import and validation                   ###
###--------------------------------------------------------------------------###
# a. Define source file and recover period
input_file <- "ENEMDU_200812_DECILE.rds"
if (!file.exists(input_file)) stop(paste("Input file not found:", input_file))

# Period is recovered once and kept separate from internal object names
input_name <- basename(input_file)
period_match <- regexec("^ENEMDU_([0-9]{4})([0-9]{2})_DECILE\\.rds$", input_name)
period_parts <- regmatches(input_name, period_match)[[1]]

if (length(period_parts) != 3L) stop("The input file name does not contain a valid ENEMDU YYYYMM period.")

period_year <- as.integer(period_parts[2])
period_month <- as.integer(period_parts[3])
period_code <- paste0(period_parts[2], period_parts[3])

if (is.na(period_year) || is.na(period_month) || period_month < 1L || period_month > 12L)
  stop(paste("Invalid period recovered from input file:", period_code))


# b. Import decile analytical database
# ENEMDU_decile is preserved as the input analytical base; labor indicators are created later in a separate object
ENEMDU_decile <- readRDS(input_file)

if (nrow(ENEMDU_decile) == 0 || ncol(ENEMDU_decile) == 0) stop("ENEMDU_decile is empty.")


# c. Essential input validation
required_vars <- c("CONDACTN", "fexp")
missing_required_vars <- setdiff(required_vars, names(ENEMDU_decile))

if (length(missing_required_vars) > 0)
  stop(paste("Required variables are missing:", paste(missing_required_vars, collapse = ", ")))

non_numeric_required_vars <- required_vars[!vapply(ENEMDU_decile[required_vars], is.numeric, logical(1))]

if (length(non_numeric_required_vars) > 0)
  stop(paste("Required variables are not numeric:", paste(non_numeric_required_vars, collapse = ", ")))

# fexp is a positive, finite survey expansion factor
if (anyNA(ENEMDU_decile$fexp)) stop("The expansion factor fexp contains missing values.")
if (any(!is.finite(ENEMDU_decile$fexp))) stop("The expansion factor fexp contains non-finite values.")
if (any(ENEMDU_decile$fexp <= 0)) stop("The expansion factor fexp contains non-positive values.")

input_sample_n <- nrow(ENEMDU_decile)
input_expanded_n <- sum(ENEMDU_decile$fexp)

# References required for exploration and subsequent CONDACTN validation
valid_n <- vapply(ENEMDU_decile[required_vars], function(x) sum(!is.na(x)), integer(1))
missing_n <- vapply(ENEMDU_decile[required_vars], function(x) sum(is.na(x)), integer(1))
valid_expanded <- vapply(required_vars, function(variable) sum(ENEMDU_decile$fexp[!is.na(ENEMDU_decile[[variable]])]), numeric(1))
missing_expanded <- vapply(required_vars, function(variable) sum(ENEMDU_decile$fexp[is.na(ENEMDU_decile[[variable]])]), numeric(1))


###--------------------------------------------------------------------------###
###                      1.2. Initial exploration                            ###
###--------------------------------------------------------------------------###
# a. General database overview
database_overview <- data.frame(
  Indicador = c("Archivo", "Periodo", "Observaciones", "Variables", "Tamaño (MB)"),
  Valor = c(input_name, period_code, input_sample_n, ncol(ENEMDU_decile), round(as.numeric(object.size(ENEMDU_decile)) / 1024^2, 2)),
  stringsAsFactors = FALSE
)

# b. Required-variable overview
required_variables_overview <- data.frame(
  Variable = required_vars,
  Class = vapply(ENEMDU_decile[required_vars], function(x) class(x)[1], character(1)),
  Valid_N = valid_n,
  Missing_N = missing_n,
  Valid_Expanded = round(valid_expanded, 0),
  Missing_Expanded = round(missing_expanded, 0),
  Label = vapply(ENEMDU_decile[required_vars], function(x) {
    variable_label <- attr(x, "label")
    if (is.null(variable_label)) "" else as.character(variable_label)
  }, character(1)),
  stringsAsFactors = FALSE
)


# c. Recover and validate original CONDACTN metadata
condactn_variable_label <- attr(ENEMDU_decile$CONDACTN, "label")
condactn_value_labels <- attr(ENEMDU_decile$CONDACTN, "labels")

if (is.null(condactn_variable_label) || is.null(condactn_value_labels) || length(condactn_value_labels) == 0)
  stop("CONDACTN variable or value labels were not found.")

condactn_dictionary <- data.frame(
  CONDACTN = as.numeric(unname(condactn_value_labels)),
  Category = names(condactn_value_labels),
  stringsAsFactors = FALSE
) %>% arrange(CONDACTN)

observed_condactn <- sort(unique(as.numeric(ENEMDU_decile$CONDACTN[!is.na(ENEMDU_decile$CONDACTN)])))

if (any(!(observed_condactn %in% 0:9)))
  stop(paste("Unexpected CONDACTN code(s) were found:", paste(observed_condactn[!(observed_condactn %in% 0:9)], collapse = ", ")))


# d. Preserve exact original CONDACTN distribution for subsequent validation
condactn_frequency_reference <- ENEMDU_decile %>%
  filter(!is.na(CONDACTN)) %>%
  transmute(CONDACTN = as.numeric(CONDACTN), fexp) %>%
  group_by(CONDACTN) %>%
  summarise(Frequency = sum(fexp), .groups = "drop") %>%
  left_join(condactn_dictionary, by = "CONDACTN") %>%
  arrange(CONDACTN)

if (anyNA(condactn_frequency_reference$Category))
  stop("At least one observed CONDACTN code has no value label.")

if (!isTRUE(all.equal(sum(condactn_frequency_reference$Frequency), unname(valid_expanded["CONDACTN"]), tolerance = 1e-8)))
  stop("The weighted CONDACTN frequency total does not match its valid expanded population.")


# e. Prepare CONDACTN display table
condactn_valid_expanded <- unname(valid_expanded["CONDACTN"])

condactn_frequency_table <- condactn_frequency_reference %>%
  mutate(
    Valid_Percentage = Frequency / condactn_valid_expanded * 100,
    Cumulative_Percentage = cumsum(Valid_Percentage)
  ) %>%
  select(CONDACTN, Category, Frequency, Valid_Percentage, Cumulative_Percentage) %>%
  bind_rows(data.frame(
    CONDACTN = NA_real_, Category = "Total", Frequency = condactn_valid_expanded,
    Valid_Percentage = 100, Cumulative_Percentage = NA_real_
  )) %>%
  mutate(
    CONDACTN = ifelse(is.na(CONDACTN), "", as.character(CONDACTN)),
    Frequency = round(Frequency, 0),
    across(c(Valid_Percentage, Cumulative_Percentage), ~ round(.x, 1))
  )


# f. Technical output
technical_output_point1 <- paste0(
  "\n============================================================\n",
  "RESUMEN DE LA BASE IMPORTADA\n\n",
  paste(capture.output(print(database_overview, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "VARIABLES REQUERIDAS PARA EL PROCESAMIENTO — EXPLORACIÓN INICIAL DE ENTRADA\n\n",
  paste(capture.output(print(required_variables_overview, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "DISTRIBUCIÓN DE CONDACTN — ", condactn_variable_label, "\n\n",
  paste(capture.output(print(as.data.frame(condactn_frequency_table), row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "VALIDACIÓN PUNTO 1: APROBADO\n"
)

cat(technical_output_point1)


# g. Remove temporary validation objects
rm(
  required_vars, missing_required_vars, non_numeric_required_vars, period_match, period_parts,
  valid_n, missing_n, valid_expanded, missing_expanded, condactn_value_labels,
  condactn_dictionary, observed_condactn, condactn_valid_expanded, technical_output_point1
)



###==========================================================================###
###                                                                          ###
###                                                                          ###
###                    2. Population classification                          ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###        2.1 Auxiliary population-classification variables                 ###
###--------------------------------------------------------------------------###
# Preserve the decile database and create a separate labor-market analytical base
ENEMDU_laboral <- ENEMDU_decile

# Aggregates constructed from the original CONDACTN classification:
# PET = 1:9 | PEA = 1:8 | PO = 1:6 | Subempleo = 2,3 | Desempleo = 7,8
# Menores de 15 años = 0 | PEI = 9 | Población total = valid CONDACTN
condactn_num <- as.numeric(ENEMDU_laboral$CONDACTN)

make_indicator <- function(condition) {
  dplyr::if_else(is.na(condactn_num), NA_integer_, as.integer(condition))
}

# a. Create general population-classification variables
ENEMDU_laboral <- ENEMDU_laboral %>%
  mutate(
    pet = make_indicator(dplyr::between(condactn_num, 1, 9)),
    pea = make_indicator(dplyr::between(condactn_num, 1, 8)),
    poblacion_ocupada = make_indicator(dplyr::between(condactn_num, 1, 6)),
    subempleo = make_indicator(condactn_num %in% c(2, 3)),
    desempleo = make_indicator(condactn_num %in% c(7, 8)),
    poblacion_total = if_else(is.na(condactn_num), NA_integer_, 1L),
    menor_15 = make_indicator(condactn_num == 0),
    pei = make_indicator(condactn_num == 9)
  )


###--------------------------------------------------------------------------###
###         2.2 Auxiliary variables for CONDACTN subcategories               ###
###--------------------------------------------------------------------------###
# b. Create CONDACTN subcategory variables
ENEMDU_laboral <- ENEMDU_laboral %>%
  mutate(
    empleo_adecuado = make_indicator(condactn_num == 1),
    subempleo_tiempo = make_indicator(condactn_num == 2),
    subempleo_ingresos = make_indicator(condactn_num == 3),
    otro_empleo_inadecuado = make_indicator(condactn_num == 4),
    empleo_no_remunerado = make_indicator(condactn_num == 5),
    empleo_no_clasificado = make_indicator(condactn_num == 6),
    desempleo_abierto = make_indicator(condactn_num == 7),
    desempleo_oculto = make_indicator(condactn_num == 8)
  )


###--------------------------------------------------------------------------###
###                   2.3 Official variable metadata                         ###
###--------------------------------------------------------------------------###
# c. Assign official variable and value labels
auxiliary_variable_labels <- c(
  pet = "Población en Edad de Trabajar (PET)",
  pea = "Población Económicamente Activa (PEA)",
  poblacion_ocupada = "Población Ocupada (PO)",
  subempleo = "Subempleo",
  desempleo = "Desempleo",
  poblacion_total = "Población total",
  menor_15 = "Población menor de 15 años",
  pei = "Población Económicamente Inactiva (PEI)",
  empleo_adecuado = "Empleo adecuado",
  subempleo_tiempo = "Subempleo por insuficiencia de tiempo de trabajo",
  subempleo_ingresos = "Subempleo por insuficiencia de ingresos",
  otro_empleo_inadecuado = "Otro empleo inadecuado",
  empleo_no_remunerado = "Empleo no remunerado",
  empleo_no_clasificado = "Empleo no clasificado",
  desempleo_abierto = "Desempleo abierto",
  desempleo_oculto = "Desempleo oculto"
)

indicator_value_labels <- c("No pertenece" = 0L, "Pertenece" = 1L)

for (variable in names(auxiliary_variable_labels)) {
  ENEMDU_laboral[[variable]] <- haven::labelled(
    as.integer(ENEMDU_laboral[[variable]]),
    labels = indicator_value_labels,
    label = unname(auxiliary_variable_labels[[variable]])
  )
}


###--------------------------------------------------------------------------###
###                2.4 Hierarchical population table                         ###
###--------------------------------------------------------------------------###
# d. Define hierarchical order and parent category
population_hierarchy <- data.frame(
  Order = 1:16,
  Level = c(0, 1, 1, 2, 3, 4, 4, 5, 5, 4, 4, 4, 3, 4, 4, 2),
  Parent = c(
    NA, "poblacion_total", "poblacion_total", "pet", "pea",
    "poblacion_ocupada", "poblacion_ocupada", "subempleo", "subempleo",
    "poblacion_ocupada", "poblacion_ocupada", "poblacion_ocupada",
    "pea", "desempleo", "desempleo", "pet"
  ),
  Variable = c(
    "poblacion_total", "menor_15", "pet", "pea", "poblacion_ocupada",
    "empleo_adecuado", "subempleo", "subempleo_tiempo",
    "subempleo_ingresos", "otro_empleo_inadecuado",
    "empleo_no_remunerado", "empleo_no_clasificado", "desempleo",
    "desempleo_abierto", "desempleo_oculto", "pei"
  ),
  stringsAsFactors = FALSE
)

population_hierarchy$Category <- unname(auxiliary_variable_labels[population_hierarchy$Variable])

# Calculate exact expanded frequencies
population_hierarchy$Expanded_Frequency <- vapply(
  population_hierarchy$Variable,
  function(variable) sum(ENEMDU_laboral$fexp * as.numeric(ENEMDU_laboral[[variable]]), na.rm = TRUE),
  numeric(1)
)

total_population <- population_hierarchy$Expanded_Frequency[
  population_hierarchy$Variable == "poblacion_total"
]

parent_frequency <- population_hierarchy$Expanded_Frequency[
  match(population_hierarchy$Parent, population_hierarchy$Variable)
]

population_hierarchy$Population_Proportion <- population_hierarchy$Expanded_Frequency / total_population

population_hierarchy$Parent_Proportion <- ifelse(
  is.na(population_hierarchy$Parent),
  1,
  population_hierarchy$Expanded_Frequency / parent_frequency
)

population_classification_table_num <- population_hierarchy %>% arrange(Order)


###--------------------------------------------------------------------------###
###           2.5 Validation against original CONDACTN                       ###
###--------------------------------------------------------------------------###
# e. Compare constructed indicators with the original CONDACTN distribution
population_totals <- setNames(
  population_classification_table_num$Expanded_Frequency,
  population_classification_table_num$Variable
)

# Complete original CONDACTN reference, allowing zero-frequency categories
condactn_totals <- setNames(rep(0, 10), as.character(0:9))
condactn_totals[as.character(condactn_frequency_reference$CONDACTN)] <-
  condactn_frequency_reference$Frequency

expected_from_condactn <- c(
  poblacion_total = sum(condactn_totals),
  menor_15 = unname(condactn_totals["0"]),
  pet = sum(condactn_totals[as.character(1:9)]),
  pea = sum(condactn_totals[as.character(1:8)]),
  poblacion_ocupada = sum(condactn_totals[as.character(1:6)]),
  empleo_adecuado = unname(condactn_totals["1"]),
  subempleo = sum(condactn_totals[c("2", "3")]),
  subempleo_tiempo = unname(condactn_totals["2"]),
  subempleo_ingresos = unname(condactn_totals["3"]),
  otro_empleo_inadecuado = unname(condactn_totals["4"]),
  empleo_no_remunerado = unname(condactn_totals["5"]),
  empleo_no_clasificado = unname(condactn_totals["6"]),
  desempleo = sum(condactn_totals[c("7", "8")]),
  desempleo_abierto = unname(condactn_totals["7"]),
  desempleo_oculto = unname(condactn_totals["8"]),
  pei = unname(condactn_totals["9"])
)

condactn_validation_difference <-
  population_totals[names(expected_from_condactn)] - expected_from_condactn

if (any(abs(condactn_validation_difference) > 1e-8)) {
  stop(paste(
    "Constructed population indicators do not reproduce the original CONDACTN distribution for:",
    paste(names(condactn_validation_difference)[abs(condactn_validation_difference) > 1e-8], collapse = ", ")
  ))
}


###--------------------------------------------------------------------------###
###                  2.6 Hierarchical visual table                           ###
###--------------------------------------------------------------------------###
# f. Create technical visual table
population_classification_gt <- population_classification_table_num %>%
  mutate(
    Visual_Group = case_when(
      Variable == "poblacion_total" ~ 0L,
      Variable %in% c("menor_15", "pet") ~ 1L,
      Variable %in% c("pea", "pei") ~ 2L,
      Variable %in% c("poblacion_ocupada", "desempleo") ~ 3L,
      Variable %in% c(
        "empleo_adecuado", "subempleo", "otro_empleo_inadecuado",
        "empleo_no_remunerado", "empleo_no_clasificado"
      ) ~ 4L,
      Variable %in% c(
        "subempleo_tiempo", "subempleo_ingresos",
        "desempleo_abierto", "desempleo_oculto"
      ) ~ 5L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(Visual_Group, Category, Expanded_Frequency, Population_Proportion, Parent_Proportion) %>%
  gt::gt() %>%
  gt::tab_header(title = "Clasificación de la población") %>%
  gt::cols_label(
    Category = "Indicador",
    Expanded_Frequency = "Frecuencia",
    Population_Proportion = "Proporción de la población",
    Parent_Proportion = "Proporción del nivel padre"
  ) %>%
  gt::fmt_number(columns = Expanded_Frequency, decimals = 0, sep_mark = ".", dec_mark = ",") %>%
  gt::fmt_number(columns = c(Population_Proportion, Parent_Proportion), decimals = 2, dec_mark = ",") %>%
  gt::tab_style(
    style = list(gt::cell_fill(color = "#4F6FA3"), gt::cell_text(color = "white", weight = "bold")),
    locations = gt::cells_body(rows = Visual_Group == 0)
  ) %>%
  gt::tab_style(style = gt::cell_fill(color = "#6F8FB5"), locations = gt::cells_body(rows = Visual_Group == 1)) %>%
  gt::tab_style(style = gt::cell_fill(color = "#8FA8C5"), locations = gt::cells_body(rows = Visual_Group == 2)) %>%
  gt::tab_style(style = gt::cell_fill(color = "#AABACD"), locations = gt::cells_body(rows = Visual_Group == 3)) %>%
  gt::tab_style(style = gt::cell_fill(color = "#C0CBD8"), locations = gt::cells_body(rows = Visual_Group == 4)) %>%
  gt::tab_style(style = gt::cell_fill(color = "#D6DCE4"), locations = gt::cells_body(rows = Visual_Group == 5)) %>%
  gt::cols_hide(columns = Visual_Group)

print(population_classification_gt)


###--------------------------------------------------------------------------###
###                     2.7 Technical output                                 ###
###--------------------------------------------------------------------------###
internal_controls_summary <- data.frame(
  Control = "Indicadores construidos vs CONDACTN original",
  Resultado = "OK",
  stringsAsFactors = FALSE
)

technical_output_point2 <- paste0(
  "\n============================================================\n",
  "RESUMEN DE CONTROLES INTERNOS\n\n",
  paste(capture.output(print(internal_controls_summary, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "VALIDACIÓN PUNTO 2: APROBADO\n"
)

cat(technical_output_point2)


###--------------------------------------------------------------------------###
###                  2.8 Remove temporary objects                             ###
###--------------------------------------------------------------------------###
rm(
  condactn_num, make_indicator, auxiliary_variable_labels, indicator_value_labels,
  population_hierarchy, total_population, parent_frequency, population_totals,
  condactn_totals, expected_from_condactn, condactn_validation_difference,
  internal_controls_summary, technical_output_point2, variable
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###                 3. Save updated analytical database                      ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###              3.1 Save and verify updated analytical database             ###
###--------------------------------------------------------------------------###

# a. Define output file automatically from period metadata
updated_rds_file <- paste0("ENEMDU_", period_code, "_INDICADORES_MERCADO_LABORAL.rds")

# b. Save labor-market analytical database
saveRDS(ENEMDU_laboral, file = updated_rds_file, compress = "xz")

# c. Verify persisted database
saved_database <- readRDS(updated_rds_file)

if (!identical(saved_database, ENEMDU_laboral))
  stop("The saved analytical database differs from the in-memory labor-market database.")


###--------------------------------------------------------------------------###
###                       3.2 Technical output                                ###
###--------------------------------------------------------------------------###
technical_output_point3 <- paste0(
  "\n============================================================\n",
  "VALIDACIÓN PUNTO 3: APROBADO\n"
)

cat(technical_output_point3)


###--------------------------------------------------------------------------###
###                  3.3 Remove temporary objects                             ###
###--------------------------------------------------------------------------###
rm(updated_rds_file, saved_database, technical_output_point3)
