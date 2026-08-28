# Load libraries
rm(list = ls())      #Clear workspace
library(haven)
library(dplyr) 
library(tidyr)
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

# a. Define input file and recover period metadata
input_file <- "ENEMDU_200812_INDICADORES_MERCADO_LABORAL.rds"

if (!file.exists(input_file)) stop(paste("Input file not found:", input_file))

input_name <- basename(input_file)
period_match <- regexec("^ENEMDU_([0-9]{4})([0-9]{2})_INDICADORES_MERCADO_LABORAL\\.rds$", input_name)
period_parts <- regmatches(input_name, period_match)[[1]]

if (length(period_parts) != 3L) stop("The input file name does not contain a valid ENEMDU YYYYMM period.")

period_year <- as.integer(period_parts[2])
period_month <- as.integer(period_parts[3])
period_code <- paste0(period_parts[2], period_parts[3])

if (is.na(period_year) || is.na(period_month) || period_month < 1L || period_month > 12L)
  stop(paste("Invalid period recovered from input file:", period_code))

# Generate standardized period labels for subsequent outputs
month_abbr_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
month_names_es <- c("ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO", "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE")

period_excel <- paste0(month_abbr_es[period_month], "-", substr(period_code, 3, 4))
period_title <- paste(month_names_es[period_month], period_year)
period_file <- paste0(month_abbr_es[period_month], "_", substr(period_code, 3, 4))


# b. Import database
ENEMDU_indicators <- readRDS(input_file)


# c. Essential input validation
if (nrow(ENEMDU_indicators) == 0 || ncol(ENEMDU_indicators) == 0)
  stop("ENEMDU_indicators is empty.")

required_vars <- c(
  "ningpc", "ingpc", "fexp", "CONDACTN",
  "poblacion_total", "menor_15", "pet", "pea", "poblacion_ocupada",
  "empleo_adecuado", "subempleo", "subempleo_tiempo",
  "subempleo_ingresos", "otro_empleo_inadecuado",
  "empleo_no_remunerado", "empleo_no_clasificado",
  "desempleo", "desempleo_abierto", "desempleo_oculto", "pei"
)

missing_required_vars <- setdiff(required_vars, names(ENEMDU_indicators))

if (length(missing_required_vars) > 0)
  stop(paste("Required variables are missing:", paste(missing_required_vars, collapse = ", ")))

non_numeric_required_vars <- required_vars[
  !vapply(ENEMDU_indicators[required_vars], is.numeric, logical(1))
]

if (length(non_numeric_required_vars) > 0)
  stop(paste("Required variables are not numeric:", paste(non_numeric_required_vars, collapse = ", ")))

# Validate expansion factor
if (anyNA(ENEMDU_indicators$fexp)) stop("The expansion factor fexp contains missing values.")
if (any(!is.finite(ENEMDU_indicators$fexp))) stop("The expansion factor fexp contains non-finite values.")
if (any(ENEMDU_indicators$fexp <= 0)) stop("The expansion factor fexp contains non-positive values.")

# Validate decile classification
if (anyNA(ENEMDU_indicators$ningpc)) stop("The decile variable ningpc contains missing values.")

observed_deciles <- sort(unique(as.integer(ENEMDU_indicators$ningpc)))
missing_deciles <- setdiff(1:10, observed_deciles)
invalid_deciles <- setdiff(observed_deciles, 1:10)

if (length(missing_deciles) > 0)
  stop(paste("Expected deciles are missing:", paste(missing_deciles, collapse = ", ")))

if (length(invalid_deciles) > 0)
  stop(paste("Invalid decile values were found:", paste(invalid_deciles, collapse = ", ")))



###--------------------------------------------------------------------------###
###                       1.2. Initial overview                              ###
###--------------------------------------------------------------------------###
# d. Prepare technical output
database_overview <- data.frame(
  Indicador = c("Archivo", "Periodo", "Etiqueta de periodo", "Observaciones", "Variables", "Tamaño (MB)"),
  Valor = c(
    input_name, period_code, period_excel,
    nrow(ENEMDU_indicators), ncol(ENEMDU_indicators),
    round(as.numeric(object.size(ENEMDU_indicators)) / 1024^2, 2)
  ),
  stringsAsFactors = FALSE
)

technical_output_point1 <- paste0(
  "\n============================================================\n",
  "RESUMEN DE LA BASE IMPORTADA\n\n",
  paste(capture.output(print(database_overview, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "VALIDACIÓN PUNTO 1: APROBADO\n"
)

# e. Technical output
cat(technical_output_point1)


# f. Remove temporary validation objects
rm(
  required_vars, missing_required_vars, non_numeric_required_vars,
  period_match, period_parts, month_abbr_es, month_names_es,
  observed_deciles, missing_deciles, invalid_deciles,
  technical_output_point1
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###      2. Table 1. Population Distribution by Income Per Capita Decile     ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###                  2.1 Population-distribution calculation                 ###
###--------------------------------------------------------------------------###
# a. Define population-classification variables in hierarchical order
population_vars <- c(
  "poblacion_total", "menor_15", "pet", "pea", "poblacion_ocupada",
  "empleo_adecuado", "subempleo", "subempleo_tiempo",
  "subempleo_ingresos", "otro_empleo_inadecuado",
  "empleo_no_remunerado", "empleo_no_clasificado",
  "desempleo", "desempleo_abierto", "desempleo_oculto", "pei"
)

# b. Recover official variable labels
population_labels <- vapply(
  population_vars,
  function(variable) {
    variable_label <- attr(ENEMDU_indicators[[variable]], "label")
    if (is.null(variable_label)) variable else as.character(variable_label)
  },
  character(1)
)

# c. Calculate population and percentage by classification and decile
population_distribution_decile <- ENEMDU_indicators %>%
  transmute(
    ningpc = as.integer(ningpc),
    fexp = as.numeric(fexp),
    across(all_of(population_vars), as.numeric)
  ) %>%
  pivot_longer(cols = all_of(population_vars), names_to = "Variable", values_to = "Indicator") %>%
  group_by(ningpc, Variable) %>%
  summarise(Population = sum(fexp * Indicator), .groups = "drop") %>%
  group_by(ningpc) %>%
  mutate(Percentage = Population / Population[Variable == "poblacion_total"] * 100) %>%
  ungroup()

# d. Calculate total population and percentage
population_distribution_total <- population_distribution_decile %>%
  group_by(Variable) %>%
  summarise(Population = sum(Population), .groups = "drop") %>%
  mutate(
    ningpc = 0L,
    Percentage = Population / Population[Variable == "poblacion_total"] * 100
  )

# e. Create numeric distribution table retained for Point 4 validation
population_distribution_table_num <- bind_rows(
  population_distribution_total,
  population_distribution_decile
) %>%
  mutate(
    Periodo = period_excel,
    Variable = factor(Variable, levels = population_vars),
    Decile = if_else(ningpc == 0, "Total", paste("Decil", ningpc)),
    Decile = factor(Decile, levels = c("Total", paste("Decil", 1:10))),
    Indicator = unname(population_labels[as.character(Variable)])
  ) %>%
  arrange(Variable, Decile) %>%
  select(Periodo, Variable, Indicator, Decile, Population, Percentage)


###--------------------------------------------------------------------------###
###                    2.2 Population-distribution table                     ###
###--------------------------------------------------------------------------###
# f. Create display table
population_distribution_table <- population_distribution_table_num %>%
  pivot_longer(cols = c(Population, Percentage), names_to = "Result", values_to = "Value") %>%
  mutate(
    Result = factor(Result, levels = c("Population", "Percentage")),
    Display = if_else(
      Result == "Population",
      formatC(Value, format = "f", digits = 0, big.mark = ".", decimal.mark = ","),
      paste0(formatC(Value, format = "f", digits = 1, decimal.mark = ","), "%")
    ),
    Indicator = if_else(Result == "Population", Indicator, "")
  ) %>%
  select(Variable, Result, Indicator, Decile, Display) %>%
  pivot_wider(names_from = Decile, values_from = Display) %>%
  arrange(Variable, Result) %>%
  select(
    Indicator, Total,
    `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
    `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
  )

# g. Create formatted table retained for release after Point 4 validation
population_distribution_gt <- population_distribution_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md(
      paste0(
        "**DISTRIBUCIÓN DE LA POBLACIÓN POR DECILES DE INGRESO PER CÁPITA — ",
        period_title,
        "**"
      )
    ),
    subtitle = gt::md("*Población y porcentaje de la población*")
  ) %>%
  gt::cols_label(Indicator = "Indicador") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(columns = everything())
  ) %>%
  gt::cols_align(align = "left", columns = Indicator) %>%
  gt::cols_align(
    align = "right",
    columns = c(
      Total,
      `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
      `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
    )
  )


###--------------------------------------------------------------------------###
###                      2.3 HTML output preparation                          ###
###--------------------------------------------------------------------------###
# h. Define HTML output file; generation is deferred until Point 4 approval
population_distribution_html <- paste0(
  "Tabla_1_Distribucion_ingpc_",
  period_file,
  ".html"
)


###--------------------------------------------------------------------------###
###                     2.4 Excel output preparation                         ###
###--------------------------------------------------------------------------###
# i. Create numeric Excel table with the same structure
population_distribution_excel_num <- population_distribution_table_num %>%
  pivot_longer(cols = c(Population, Percentage), names_to = "Result", values_to = "Value") %>%
  mutate(
    Result = factor(Result, levels = c("Population", "Percentage")),
    Value = if_else(Result == "Percentage", Value / 100, Value),
    Indicator = if_else(Result == "Population", Indicator, "")
  ) %>%
  select(Periodo, Variable, Result, Indicator, Decile, Value) %>%
  pivot_wider(names_from = Decile, values_from = Value) %>%
  arrange(Periodo, Variable, Result) %>%
  group_by(Periodo) %>%
  mutate(Periodo_display = if_else(row_number() == 1L, first(Periodo), "")) %>%
  ungroup() %>%
  select(
    Periodo = Periodo_display, Result, Indicator, Total,
    `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
    `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
  )

# j. Define Excel rows by result type
population_excel_rows <- which(population_distribution_excel_num$Result == "Population") + 4L
percentage_excel_rows <- which(population_distribution_excel_num$Result == "Percentage") + 4L

population_distribution_excel <- population_distribution_excel_num %>% select(-Result)

# k. Define Excel output file; saving is deferred until Point 4 approval
population_distribution_xlsx <- paste0(
  "Tabla_1_Distribucion_ingpc_",
  period_file,
  ".xlsx"
)

# l. Create Excel workbook
population_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(population_wb, "Tabla 1")

# m. Write title and subtitle
openxlsx::writeData(
  population_wb, "Tabla 1",
  "DISTRIBUCIÓN DE LA POBLACIÓN POR DECILES DE INGRESO PER CÁPITA",
  startRow = 1, startCol = 1
)

openxlsx::writeData(
  population_wb, "Tabla 1",
  "Población y porcentaje de la población",
  startRow = 2, startCol = 1
)

openxlsx::mergeCells(population_wb, "Tabla 1", cols = 1:13, rows = 1)
openxlsx::mergeCells(population_wb, "Tabla 1", cols = 1:13, rows = 2)

# n. Write table
openxlsx::writeData(
  population_wb, "Tabla 1", population_distribution_excel,
  startRow = 4, startCol = 1, colNames = TRUE
)

# o. Merge period cell across complete period block
excel_first_data_row <- 5L
excel_last_data_row <- 4L + nrow(population_distribution_excel)

openxlsx::mergeCells(
  population_wb, "Tabla 1",
  cols = 1,
  rows = excel_first_data_row:excel_last_data_row
)

# p. Merge each indicator across population and percentage rows
for (row_i in population_excel_rows) {
  openxlsx::mergeCells(population_wb, "Tabla 1", cols = 2, rows = row_i:(row_i + 1L))
}

# q. Define Excel styles
title_style <- openxlsx::createStyle(
  fontSize = 14, textDecoration = "bold",
  halign = "center", valign = "center"
)

subtitle_style <- openxlsx::createStyle(
  fontSize = 11, textDecoration = "italic",
  halign = "center", valign = "center"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center", valign = "center",
  border = "Bottom"
)

period_style <- openxlsx::createStyle(halign = "center", valign = "center")
indicator_style <- openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE)
population_style <- openxlsx::createStyle(numFmt = "#,##0")
percentage_style <- openxlsx::createStyle(numFmt = "0.0%")

# r. Apply Excel styles
openxlsx::addStyle(
  population_wb, "Tabla 1", title_style,
  rows = 1, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", subtitle_style,
  rows = 2, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", header_style,
  rows = 4, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", period_style,
  rows = excel_first_data_row:excel_last_data_row,
  cols = 1, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", indicator_style,
  rows = excel_first_data_row:excel_last_data_row,
  cols = 2, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", population_style,
  rows = population_excel_rows,
  cols = 3:13, gridExpand = TRUE
)

openxlsx::addStyle(
  population_wb, "Tabla 1", percentage_style,
  rows = percentage_excel_rows,
  cols = 3:13, gridExpand = TRUE
)

# s. Adjust column widths
openxlsx::setColWidths(population_wb, "Tabla 1", cols = 1, widths = 12)
openxlsx::setColWidths(population_wb, "Tabla 1", cols = 2, widths = 42)
openxlsx::setColWidths(population_wb, "Tabla 1", cols = 3:13, widths = 12)

# HTML and Excel outputs are released only after Point 4 validation.





###==========================================================================###
###                                                                          ###
###                                                                          ###
###    3. Table 2. Income Per Capita Concentration by Income Decile          ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###                   3.1 Income-concentration calculation                   ###
###--------------------------------------------------------------------------###
# a. Calculate aggregated income and participation by classification and decile
income_concentration_decile <- ENEMDU_indicators %>%
  transmute(
    ningpc = as.integer(ningpc),
    ingpc = as.numeric(ingpc),
    fexp = as.numeric(fexp),
    across(all_of(population_vars), as.numeric)
  ) %>%
  pivot_longer(cols = all_of(population_vars), names_to = "Variable", values_to = "Indicator") %>%
  group_by(ningpc, Variable) %>%
  summarise(Income_Millions = sum(fexp * ingpc * Indicator) / 1000000, .groups = "drop") %>%
  group_by(ningpc) %>%
  mutate(Participation = Income_Millions / Income_Millions[Variable == "poblacion_total"] * 100) %>%
  ungroup()

# b. Calculate total aggregated income and participation
income_concentration_total <- income_concentration_decile %>%
  group_by(Variable) %>%
  summarise(Income_Millions = sum(Income_Millions), .groups = "drop") %>%
  mutate(
    ningpc = 0L,
    Participation = Income_Millions / Income_Millions[Variable == "poblacion_total"] * 100
  )

# c. Create numeric concentration table retained for Point 4 validation
income_concentration_table_num <- bind_rows(
  income_concentration_total,
  income_concentration_decile
) %>%
  mutate(
    Periodo = period_excel,
    Variable = factor(Variable, levels = population_vars),
    Decile = if_else(ningpc == 0, "Total", paste("Decil", ningpc)),
    Decile = factor(Decile, levels = c("Total", paste("Decil", 1:10))),
    Indicator = unname(population_labels[as.character(Variable)])
  ) %>%
  arrange(Variable, Decile) %>%
  select(Periodo, Variable, Indicator, Decile, Income_Millions, Participation)


###--------------------------------------------------------------------------###
###                     3.2 Income-concentration table                       ###
###--------------------------------------------------------------------------###
# d. Create display table
income_concentration_table <- income_concentration_table_num %>%
  pivot_longer(cols = c(Income_Millions, Participation), names_to = "Result", values_to = "Value") %>%
  mutate(
    Result = factor(Result, levels = c("Income_Millions", "Participation")),
    Display = if_else(
      Result == "Income_Millions",
      formatC(Value, format = "f", digits = 2, big.mark = ".", decimal.mark = ","),
      paste0(formatC(Value, format = "f", digits = 1, decimal.mark = ","), "%")
    ),
    Indicator = if_else(Result == "Income_Millions", Indicator, "")
  ) %>%
  select(Variable, Result, Indicator, Decile, Display) %>%
  pivot_wider(names_from = Decile, values_from = Display) %>%
  arrange(Variable, Result) %>%
  select(
    Indicator, Total,
    `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
    `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
  )

# e. Create formatted table retained for release after Point 4 validation
income_concentration_gt <- income_concentration_table %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md(
      paste0(
        "**CONCENTRACIÓN DEL INGRESO PER CÁPITA POR DECILES — ",
        period_title,
        "**"
      )
    ),
    subtitle = gt::md("*Ingreso per cápita (millones de USD) y participación (%)*")
  ) %>%
  gt::cols_label(Indicator = "Indicador") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(columns = everything())
  ) %>%
  gt::cols_align(align = "left", columns = Indicator) %>%
  gt::cols_align(
    align = "right",
    columns = c(
      Total,
      `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
      `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
    )
  )


###--------------------------------------------------------------------------###
###                      3.3 HTML output preparation                          ###
###--------------------------------------------------------------------------###
# f. Define HTML output file; generation is deferred until Point 4 approval
income_concentration_html <- paste0(
  "Tabla_2_Concentracion_ingpc_",
  period_file,
  ".html"
)


###--------------------------------------------------------------------------###
###                      3.4 Excel output preparation                         ###
###--------------------------------------------------------------------------###
# g. Create numeric Excel table with the same structure
income_concentration_excel_num <- income_concentration_table_num %>%
  pivot_longer(cols = c(Income_Millions, Participation), names_to = "Result", values_to = "Value") %>%
  mutate(
    Result = factor(Result, levels = c("Income_Millions", "Participation")),
    Value = if_else(Result == "Participation", Value / 100, Value),
    Indicator = if_else(Result == "Income_Millions", Indicator, "")
  ) %>%
  select(Periodo, Variable, Result, Indicator, Decile, Value) %>%
  pivot_wider(names_from = Decile, values_from = Value) %>%
  arrange(Periodo, Variable, Result) %>%
  group_by(Periodo) %>%
  mutate(Periodo_display = if_else(row_number() == 1L, first(Periodo), "")) %>%
  ungroup() %>%
  select(
    Periodo = Periodo_display, Result, Indicator, Total,
    `Decil 1`, `Decil 2`, `Decil 3`, `Decil 4`, `Decil 5`,
    `Decil 6`, `Decil 7`, `Decil 8`, `Decil 9`, `Decil 10`
  )

# h. Define Excel rows by result type
income_excel_rows <- which(income_concentration_excel_num$Result == "Income_Millions") + 4L
participation_excel_rows <- which(income_concentration_excel_num$Result == "Participation") + 4L

income_concentration_excel <- income_concentration_excel_num %>% select(-Result)

# i. Define Excel output file; saving is deferred until Point 4 approval
income_concentration_xlsx <- paste0(
  "Tabla_2_Concentracion_ingpc_",
  period_file,
  ".xlsx"
)

# j. Create Excel workbook
income_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(income_wb, "Tabla 2")

# k. Write title and subtitle
openxlsx::writeData(
  income_wb, "Tabla 2",
  "CONCENTRACIÓN DEL INGRESO PER CÁPITA POR DECILES",
  startRow = 1, startCol = 1
)

openxlsx::writeData(
  income_wb, "Tabla 2",
  "Ingreso per cápita (millones de USD) y participación (%)",
  startRow = 2, startCol = 1
)

openxlsx::mergeCells(income_wb, "Tabla 2", cols = 1:13, rows = 1)
openxlsx::mergeCells(income_wb, "Tabla 2", cols = 1:13, rows = 2)

# l. Write table
openxlsx::writeData(
  income_wb, "Tabla 2", income_concentration_excel,
  startRow = 4, startCol = 1, colNames = TRUE
)

# m. Merge period cell across complete period block
income_excel_first_data_row <- 5L
income_excel_last_data_row <- 4L + nrow(income_concentration_excel)

openxlsx::mergeCells(
  income_wb, "Tabla 2",
  cols = 1,
  rows = income_excel_first_data_row:income_excel_last_data_row
)

# n. Merge each indicator across income and participation rows
for (row_i in income_excel_rows) {
  openxlsx::mergeCells(income_wb, "Tabla 2", cols = 2, rows = row_i:(row_i + 1L))
}

# o. Define Excel styles
income_title_style <- openxlsx::createStyle(
  fontSize = 14, textDecoration = "bold",
  halign = "center", valign = "center"
)

income_subtitle_style <- openxlsx::createStyle(
  fontSize = 11, textDecoration = "italic",
  halign = "center", valign = "center"
)

income_header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center", valign = "center",
  border = "Bottom"
)

income_period_style <- openxlsx::createStyle(halign = "center", valign = "center")
income_indicator_style <- openxlsx::createStyle(halign = "left", valign = "center", wrapText = TRUE)
income_value_style <- openxlsx::createStyle(numFmt = "#,##0.00")
participation_style <- openxlsx::createStyle(numFmt = "0.0%")

# p. Apply Excel styles
openxlsx::addStyle(
  income_wb, "Tabla 2", income_title_style,
  rows = 1, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", income_subtitle_style,
  rows = 2, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", income_header_style,
  rows = 4, cols = 1:13, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", income_period_style,
  rows = income_excel_first_data_row:income_excel_last_data_row,
  cols = 1, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", income_indicator_style,
  rows = income_excel_first_data_row:income_excel_last_data_row,
  cols = 2, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", income_value_style,
  rows = income_excel_rows,
  cols = 3:13, gridExpand = TRUE
)

openxlsx::addStyle(
  income_wb, "Tabla 2", participation_style,
  rows = participation_excel_rows,
  cols = 3:13, gridExpand = TRUE
)

# q. Adjust column widths
openxlsx::setColWidths(income_wb, "Tabla 2", cols = 1, widths = 12)
openxlsx::setColWidths(income_wb, "Tabla 2", cols = 2, widths = 42)
openxlsx::setColWidths(income_wb, "Tabla 2", cols = 3:13, widths = 12)

# HTML and Excel outputs are released only after Point 4 validation.




###==========================================================================###
###                                                                          ###
###                                                                          ###
###                4. Verification and internal controls                     ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###             4.1 Reference totals from original CONDACTN                  ###
###--------------------------------------------------------------------------###

# a. Prepare original variables used for independent verification
verification_source <- ENEMDU_indicators %>%
  transmute(
    CONDACTN_code = as.integer(CONDACTN),
    Activity_condition = as.character(haven::as_factor(CONDACTN, levels = "labels")),
    ningpc = as.integer(ningpc),
    ingpc = as.numeric(ingpc),
    fexp = as.numeric(fexp)
  )

# b. Calculate population and income directly by original CONDACTN category
verification_table_num <- verification_source %>%
  group_by(CONDACTN_code, Activity_condition) %>%
  summarise(Population = sum(fexp), Income_Per_Capita_Total_USD = sum(ingpc * fexp), .groups = "drop") %>%
  arrange(CONDACTN_code)

# c. Add original-source total row
verification_table_num <- bind_rows(
  verification_table_num,
  verification_table_num %>%
    summarise(
      CONDACTN_code = NA_integer_,
      Activity_condition = "Total",
      Population = sum(Population),
      Income_Per_Capita_Total_USD = sum(Income_Per_Capita_Total_USD)
    )
)

# d. Original CONDACTN codes corresponding to each analytical indicator
verification_codes <- list(
  poblacion_total = 0:9,
  menor_15 = 0,
  pet = 1:9,
  pea = 1:8,
  poblacion_ocupada = 1:6,
  empleo_adecuado = 1,
  subempleo = 2:3,
  subempleo_tiempo = 2,
  subempleo_ingresos = 3,
  otro_empleo_inadecuado = 4,
  empleo_no_remunerado = 5,
  empleo_no_clasificado = 6,
  desempleo = 7:8,
  desempleo_abierto = 7,
  desempleo_oculto = 8,
  pei = 9
)

# e. Obtain reference totals directly from original CONDACTN
source_total <- function(variable, codes) {
  verification_source %>%
    filter(CONDACTN_code %in% codes) %>%
    summarise(
      Variable = variable,
      Population_CONDACTN = sum(fexp),
      Income_Millions_CONDACTN = sum(ingpc * fexp) / 1000000
    )
}

verification_reference_totals <- bind_rows(
  lapply(names(verification_codes), function(variable) {
    source_total(variable, verification_codes[[variable]])
  })
)


###--------------------------------------------------------------------------###
###                 4.2 Verification of total columns                        ###
###--------------------------------------------------------------------------###

# f. Extract Table 1 and Table 2 totals
table1_totals <- population_distribution_table_num %>%
  filter(as.character(Decile) == "Total") %>%
  transmute(Variable = as.character(Variable), Population_Table1 = Population)

table2_totals <- income_concentration_table_num %>%
  filter(as.character(Decile) == "Total") %>%
  transmute(Variable = as.character(Variable), Income_Millions_Table2 = Income_Millions)

# g. Compare total columns against original CONDACTN
verification_tolerance <- 1e-6

total_verification <- verification_reference_totals %>%
  left_join(table1_totals, by = "Variable") %>%
  left_join(table2_totals, by = "Variable") %>%
  mutate(
    Population_Difference = Population_Table1 - Population_CONDACTN,
    Income_Difference = Income_Millions_Table2 - Income_Millions_CONDACTN,
    Table1_Total_OK = !is.na(Population_Table1) & abs(Population_Difference) <= verification_tolerance,
    Table2_Total_OK = !is.na(Income_Millions_Table2) & abs(Income_Difference) <= verification_tolerance
  )


###--------------------------------------------------------------------------###
###        4.3 Independent D1-D10 verification from original CONDACTN        ###
###--------------------------------------------------------------------------###

# h. Build an independent decile reference directly from CONDACTN, ningpc, fexp and ingpc
source_decile <- function(variable, codes) {
  verification_source %>%
    filter(CONDACTN_code %in% codes) %>%
    group_by(ningpc) %>%
    summarise(
      Population_CONDACTN = sum(fexp),
      Income_Millions_CONDACTN = sum(ingpc * fexp) / 1000000,
      .groups = "drop"
    ) %>%
    tidyr::complete(
      ningpc = 1:10,
      fill = list(Population_CONDACTN = 0, Income_Millions_CONDACTN = 0)
    ) %>%
    mutate(Variable = variable, .before = 1)
}

verification_reference_deciles <- bind_rows(
  lapply(names(verification_codes), function(variable) {
    source_decile(variable, verification_codes[[variable]])
  })
) %>%
  group_by(ningpc) %>%
  mutate(
    Percentage_CONDACTN =
      Population_CONDACTN / Population_CONDACTN[Variable == "poblacion_total"] * 100,
    Participation_CONDACTN =
      Income_Millions_CONDACTN / Income_Millions_CONDACTN[Variable == "poblacion_total"] * 100
  ) %>%
  ungroup()

# i. Extract D1-D10 results from Tables 1 and 2
table1_deciles <- population_distribution_table_num %>%
  filter(as.character(Decile) != "Total") %>%
  transmute(
    Variable = as.character(Variable),
    ningpc = match(as.character(Decile), paste("Decil", 1:10)),
    Population_Table1 = Population,
    Percentage_Table1 = Percentage
  )

table2_deciles <- income_concentration_table_num %>%
  filter(as.character(Decile) != "Total") %>%
  transmute(
    Variable = as.character(Variable),
    ningpc = match(as.character(Decile), paste("Decil", 1:10)),
    Income_Millions_Table2 = Income_Millions,
    Participation_Table2 = Participation
  )

# j. Compare all decile results against the independently reconstructed source
decile_verification <- verification_reference_deciles %>%
  left_join(table1_deciles, by = c("Variable", "ningpc")) %>%
  left_join(table2_deciles, by = c("Variable", "ningpc")) %>%
  mutate(
    Population_Difference = Population_Table1 - Population_CONDACTN,
    Percentage_Difference = Percentage_Table1 - Percentage_CONDACTN,
    Income_Difference = Income_Millions_Table2 - Income_Millions_CONDACTN,
    Participation_Difference = Participation_Table2 - Participation_CONDACTN,
    
    Table1_Decile_OK =
      !is.na(Population_Table1) &
      !is.na(Percentage_Table1) &
      abs(Population_Difference) <= verification_tolerance &
      abs(Percentage_Difference) <= verification_tolerance,
    
    Table2_Decile_OK =
      !is.na(Income_Millions_Table2) &
      !is.na(Participation_Table2) &
      abs(Income_Difference) <= verification_tolerance &
      abs(Participation_Difference) <= verification_tolerance
  )


###--------------------------------------------------------------------------###
###                         4.4 Validation gate                               ###
###--------------------------------------------------------------------------###

# k. Consolidate independent control results
table1_total_ok <- all(total_verification$Table1_Total_OK %in% TRUE)
table2_total_ok <- all(total_verification$Table2_Total_OK %in% TRUE)
table1_deciles_ok <- all(decile_verification$Table1_Decile_OK %in% TRUE)
table2_deciles_ok <- all(decile_verification$Table2_Decile_OK %in% TRUE)

table1_total_discrepancies <- sum(!total_verification$Table1_Total_OK)
table2_total_discrepancies <- sum(!total_verification$Table2_Total_OK)
table1_decile_discrepancies <- sum(!decile_verification$Table1_Decile_OK)
table2_decile_discrepancies <- sum(!decile_verification$Table2_Decile_OK)

total_discrepancies <-
  table1_total_discrepancies +
  table2_total_discrepancies +
  table1_decile_discrepancies +
  table2_decile_discrepancies

verification_approved <-
  table1_total_ok &&
  table2_total_ok &&
  table1_deciles_ok &&
  table2_deciles_ok

# l. Report discrepancies and stop before releasing outputs when validation fails
if (!verification_approved) {
  
  verification_discrepancies <- bind_rows(
    
    total_verification %>%
      filter(!Table1_Total_OK) %>%
      transmute(
        Control = "Tabla 1 — Total vs CONDACTN",
        Variable,
        Decile = "Total",
        Observed = Population_Table1,
        Expected = Population_CONDACTN,
        Difference = Population_Difference
      ),
    
    total_verification %>%
      filter(!Table2_Total_OK) %>%
      transmute(
        Control = "Tabla 2 — Total vs CONDACTN",
        Variable,
        Decile = "Total",
        Observed = Income_Millions_Table2,
        Expected = Income_Millions_CONDACTN,
        Difference = Income_Difference
      ),
    
    decile_verification %>%
      filter(is.na(Population_Table1) | abs(Population_Difference) > verification_tolerance) %>%
      transmute(
        Control = "Tabla 1 — Población por decil vs CONDACTN",
        Variable,
        Decile = paste("Decil", ningpc),
        Observed = Population_Table1,
        Expected = Population_CONDACTN,
        Difference = Population_Difference
      ),
    
    decile_verification %>%
      filter(is.na(Percentage_Table1) | abs(Percentage_Difference) > verification_tolerance) %>%
      transmute(
        Control = "Tabla 1 — Porcentaje por decil vs CONDACTN",
        Variable,
        Decile = paste("Decil", ningpc),
        Observed = Percentage_Table1,
        Expected = Percentage_CONDACTN,
        Difference = Percentage_Difference
      ),
    
    decile_verification %>%
      filter(is.na(Income_Millions_Table2) | abs(Income_Difference) > verification_tolerance) %>%
      transmute(
        Control = "Tabla 2 — Ingreso por decil vs CONDACTN",
        Variable,
        Decile = paste("Decil", ningpc),
        Observed = Income_Millions_Table2,
        Expected = Income_Millions_CONDACTN,
        Difference = Income_Difference
      ),
    
    decile_verification %>%
      filter(is.na(Participation_Table2) | abs(Participation_Difference) > verification_tolerance) %>%
      transmute(
        Control = "Tabla 2 — Participación por decil vs CONDACTN",
        Variable,
        Decile = paste("Decil", ningpc),
        Observed = Participation_Table2,
        Expected = Participation_CONDACTN,
        Difference = Participation_Difference
      )
  )
  
  technical_output_point4 <- paste0(
    "\n============================================================\n",
    "VERIFICACIÓN GENERAL — ", period_title, "\n\n",
    "Totales Tabla 1 vs CONDACTN: ", if (table1_total_ok) "OK" else "ERROR", "\n",
    "Totales Tabla 2 vs CONDACTN: ", if (table2_total_ok) "OK" else "ERROR", "\n",
    "D1-D10 Tabla 1 vs CONDACTN: ", if (table1_deciles_ok) "OK" else "ERROR", "\n",
    "D1-D10 Tabla 2 vs CONDACTN: ", if (table2_deciles_ok) "OK" else "ERROR", "\n",
    "Discrepancias: ", total_discrepancies,
    "\n\nDETALLE DE DISCREPANCIAS\n\n",
    paste(capture.output(print(as.data.frame(verification_discrepancies), row.names = FALSE)), collapse = "\n"),
    "\n\n============================================================\n",
    "VALIDACIÓN PUNTO 4: NO APROBADO\n"
  )
  
  cat(technical_output_point4)
  stop("Validation controls for Tables 1 and 2 failed.", call. = FALSE)
}


###--------------------------------------------------------------------------###
###                    4.5 Verification support table                         ###
###--------------------------------------------------------------------------###

# m. Create support table directly from original CONDACTN
verification_gt <- verification_table_num %>%
  select(Activity_condition, Population, Income_Per_Capita_Total_USD) %>%
  gt::gt() %>%
  gt::tab_header(
    title = gt::md(
      paste0(
        "**POBLACIÓN E INGRESO PER CÁPITA POR CONDICIÓN DE ACTIVIDAD — ",
        period_title,
        "**"
      )
    )
  ) %>%
  gt::cols_label(
    Activity_condition = "Condición de actividad",
    Population = "Población",
    Income_Per_Capita_Total_USD = "Ingreso per cápita total (USD)"
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels(columns = everything())
  ) %>%
  gt::fmt_number(columns = Population, decimals = 0, sep_mark = ".", dec_mark = ",") %>%
  gt::fmt_number(columns = Income_Per_Capita_Total_USD, decimals = 0, sep_mark = ".", dec_mark = ",")


###--------------------------------------------------------------------------###
###                       4.6 Release validated outputs                       ###
###--------------------------------------------------------------------------###

# n. Generate final HTML output with vertically merged indicator cells
save_merged_gt_html <- function(gt_object, filename) {
  
  gt::gtsave(gt_object, filename = filename)
  html_doc <- xml2::read_html(filename)
  html_rows <- xml2::xml_find_all(html_doc, "//tbody[contains(@class, 'gt_table_body')]/tr")
  
  for (row_i in seq(1L, length(html_rows), by = 2L)) {
    
    indicator_cell <- xml2::xml_find_first(html_rows[[row_i]], "./td[1]")
    empty_indicator_cell <- xml2::xml_find_first(html_rows[[row_i + 1L]], "./td[1]")
    
    xml2::xml_set_attr(indicator_cell, "rowspan", "2")
    
    indicator_style_html <- xml2::xml_attr(indicator_cell, "style")
    if (is.na(indicator_style_html)) indicator_style_html <- ""
    
    xml2::xml_set_attr(
      indicator_cell,
      "style",
      paste0(indicator_style_html, "; vertical-align: middle;")
    )
    
    xml2::xml_remove(empty_indicator_cell)
  }
  
  xml2::write_html(html_doc, filename)
  invisible(filename)
}

# o. Release Table 1
save_merged_gt_html(population_distribution_gt, population_distribution_html)
openxlsx::saveWorkbook(population_wb, population_distribution_xlsx, overwrite = TRUE)

# p. Release Table 2
save_merged_gt_html(income_concentration_gt, income_concentration_html)
openxlsx::saveWorkbook(income_wb, income_concentration_xlsx, overwrite = TRUE)

# q. Display validated outputs
utils::browseURL(normalizePath(population_distribution_html, mustWork = TRUE))
utils::browseURL(normalizePath(income_concentration_html, mustWork = TRUE))
print(verification_gt)


###--------------------------------------------------------------------------###
###                         4.7 Technical output                              ###
###--------------------------------------------------------------------------###

# r. Display final validation result
technical_output_point4 <- paste0(
  "\n============================================================\n",
  "VERIFICACIÓN GENERAL — ", period_title, "\n\n",
  "Totales Tabla 1 vs CONDACTN: OK\n",
  "Totales Tabla 2 vs CONDACTN: OK\n",
  "D1-D10 Tabla 1 vs CONDACTN: OK\n",
  "D1-D10 Tabla 2 vs CONDACTN: OK\n",
  "Discrepancias: ", total_discrepancies,
  "\n\n============================================================\n",
  "VALIDACIÓN PUNTO 4: APROBADO\n"
)

cat(technical_output_point4)
