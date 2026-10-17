# Load libraries
library(dplyr) 

###==========================================================================###
###                                                                          ###
###                                                                          ###
###                  1B. CONDACTN validation and exploration                 ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###           1B.1. CONDACTN exploration and reference distribution          ###
###--------------------------------------------------------------------------###
# a. QA [condactn_reference] — Validate original CONDACTN metadata and observed codes
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

# b. Preserve exact original CONDACTN distribution for subsequent validation
condactn_frequency_reference <- ENEMDU_decile %>%
  filter(!is.na(CONDACTN)) %>%
  transmute(CONDACTN = as.numeric(CONDACTN), fexp) %>%
  group_by(CONDACTN) %>%
  summarise(Frequency = sum(fexp), .groups = "drop") %>%
  left_join(condactn_dictionary, by = "CONDACTN") %>%
  arrange(CONDACTN)

# QA [condactn_reference] — Validate value labels for all observed CONDACTN codes
if (anyNA(condactn_frequency_reference$Category))
  stop("At least one observed CONDACTN code has no value label.")


# c. Prepare CONDACTN display table
condactn_valid_expanded <- sum(condactn_frequency_reference$Frequency)

condactn_frequency_table <- condactn_frequency_reference %>%
  mutate(Valid_Percentage = Frequency / condactn_valid_expanded * 100, Cumulative_Percentage = cumsum(Valid_Percentage)) %>%
  select(CONDACTN, Category, Frequency, Valid_Percentage, Cumulative_Percentage) %>%
  bind_rows(data.frame(
    CONDACTN = NA_real_, Category = "Total", Frequency = condactn_valid_expanded, Valid_Percentage = 100, Cumulative_Percentage = NA_real_)) %>%
  mutate(
    CONDACTN = ifelse(is.na(CONDACTN), "", as.character(CONDACTN)), Frequency = round(Frequency, 0), across(c(Valid_Percentage, Cumulative_Percentage), ~ round(.x, 1)))


# d. Technical output
technical_output_point1 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "DISTRIBUCIÓN DE LA POBLACIÓN SEGÚN CONDICIÓN DE ACTIVIDAD\n\n",
  paste(capture.output(print(as.data.frame(condactn_frequency_table), row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "PUNTO 1: APROBADO\n"
)
cat(technical_output_point1)


# e. Remove temporary validation objects
rm(
  condactn_variable_label, condactn_value_labels, condactn_dictionary, observed_condactn,
  condactn_valid_expanded, condactn_frequency_table, technical_output_point1
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###                   2B. Population classification                          ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###       2B.1 Auxiliary population-classification variables                 ###
###--------------------------------------------------------------------------###
# Preserve the decile database and create a separate labor-market analytical base
ENEMDU_laboral <- ENEMDU_decile

# Aggregates constructed from the original CONDACTN classification:
# [PET = 1:9 | PEA = 1:8 | PO = 1:6 | Subempleo = 2,3 | Desempleo = 7,8
# Menores de 15 años = 0 | PEI = 9 | Población total = valid CONDACTN]
condactn_num <- as.numeric(ENEMDU_laboral$CONDACTN)
make_indicator <- function(condition) {dplyr::if_else(is.na(condactn_num), NA_integer_, as.integer(condition))}

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
###        2B.2 Auxiliary variables for CONDACTN subcategories               ###
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
###                  2B.3 Official variable metadata                         ###
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
###               2B.4 Hierarchical population table                         ###
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
  population_hierarchy$Variable, function(variable) sum(ENEMDU_laboral$fexp * as.numeric(ENEMDU_laboral[[variable]]), na.rm = TRUE),numeric(1))

total_population <- population_hierarchy$Expanded_Frequency[population_hierarchy$Variable == "poblacion_total"]
parent_frequency <- population_hierarchy$Expanded_Frequency[match(population_hierarchy$Parent, population_hierarchy$Variable)]
population_hierarchy$Population_Proportion <- population_hierarchy$Expanded_Frequency / total_population
population_hierarchy$Parent_Proportion <- ifelse(is.na(population_hierarchy$Parent), 1, population_hierarchy$Expanded_Frequency / parent_frequency)
population_classification_table_num <- population_hierarchy %>% arrange(Order)


###--------------------------------------------------------------------------###
###          2B.5 Validation against original CONDACTN                       ###
###--------------------------------------------------------------------------###
# e. Build comparative evidence against original CONDACTN distribution
population_totals <- setNames(population_classification_table_num$Expanded_Frequency, population_classification_table_num$Variable)

# Complete original CONDACTN reference, allowing zero-frequency categories
condactn_totals <- setNames(rep(0, 10), as.character(0:9))
condactn_totals[as.character(condactn_frequency_reference$CONDACTN)] <-condactn_frequency_reference$Frequency

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

condactn_validation_difference <- population_totals[names(expected_from_condactn)] - expected_from_condactn

population_validation <- data.frame(
  Indicator = names(expected_from_condactn),
  Reference = as.numeric(expected_from_condactn),
  Observed = as.numeric(population_totals[names(expected_from_condactn)]),
  Difference = as.numeric(condactn_validation_difference),
  row.names = NULL
)


# f. QA [population_classification_consistency] — Validate constructed indicators against original CONDACTN distribution
if (any(abs(condactn_validation_difference) > 1e-8)) {
  stop(paste("Constructed population indicators do not reproduce the original CONDACTN distribution for:",
    paste(names(condactn_validation_difference)[abs(condactn_validation_difference) > 1e-8], collapse = ", ")))
}


###--------------------------------------------------------------------------###
###               2B.6 Visual grouping and hierarchical table                ###
###--------------------------------------------------------------------------###
# g. Create technical visual table; Visual_Group is a display grouping
# [Distinct from the conceptual Level/Parent hierarchy defined in Point 2.4]
population_classification_gt <- population_classification_table_num %>%
  mutate(
    Visual_Group = case_when(
      Variable == "poblacion_total" ~ 0L,
      Variable %in% c("menor_15", "pet") ~ 1L,
      Variable %in% c("pea", "pei") ~ 2L,
      Variable %in% c("poblacion_ocupada", "desempleo") ~ 3L,
      Variable %in% c("empleo_adecuado", "subempleo", "otro_empleo_inadecuado", "empleo_no_remunerado", "empleo_no_clasificado") ~ 4L,
      Variable %in% c("subempleo_tiempo", "subempleo_ingresos", "desempleo_abierto", "desempleo_oculto") ~ 5L,
      TRUE ~ NA_integer_
    )
  ) %>%
  select(Visual_Group, Category, Expanded_Frequency, Population_Proportion, Parent_Proportion) %>%
  gt::gt() %>%
  gt::tab_header(title = "Clasificación de la población") %>%
  gt::cols_label(
    Category = "Indicador",
    Expanded_Frequency = "Población",
    Population_Proportion = "Proporción de la población",
    Parent_Proportion = "Proporción respecto a la categoría superior"
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
###                    2B.7 Technical output                                 ###
###--------------------------------------------------------------------------###
technical_output_point2 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "Salida mostrada en Viewer:\n",
  "  - Clasificación de la población",
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "EVIDENCIA OBSERVADA\n\n",
  paste(capture.output(print(population_validation, row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "PUNTO 2: APROBADO\n"
)
cat(technical_output_point2)


# Remove temporary objects
rm(
  condactn_num, make_indicator, auxiliary_variable_labels, indicator_value_labels,
  population_hierarchy, total_population, parent_frequency, population_totals,
  condactn_totals, expected_from_condactn, condactn_validation_difference, population_validation,
  condactn_frequency_reference, technical_output_point2, variable
)
