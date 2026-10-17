# Load libraries
library(haven)
library(survey)      #Analyzes complex survey data using weights, strata, and clusters.
library(dplyr) 
library(srvyr)       #dplyr-style interface for analyzing complex survey data


###==========================================================================###
###                                                                          ###
###                                                                          ###
###                            1A. Load database                             ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###                   1A.1. Database import and validation                   ###
###--------------------------------------------------------------------------###
#...............................................................................
# a. Define source file and recover period
#...............................................................................
input_file <- "ENEMDU_2008_12.sav"
# QA [input_reception] — Source file exists
if (!file.exists(input_file)) stop(paste("Input file not found:", input_file))

# Period is recovered once and kept separate from internal object names
input_name <- basename(input_file)
period_match <- regexec("^ENEMDU_([0-9]{4})_([0-9]{2})\\.sav$", input_name)
period_parts <- regmatches(input_name, period_match)[[1]]

# QA [input_reception] — Valid ENEMDU period format
if (length(period_parts) != 3L) stop("The input file name does not contain a valid ENEMDU YYYY_MM period.")

period_year <- as.integer(period_parts[2])
period_month <- as.integer(period_parts[3])
period_code <- paste0(period_parts[2], "_", period_parts[3])

# QA [input_reception] — Valid recovered period
if (is.na(period_year) || is.na(period_month) || period_month < 1L || period_month > 12L)
  stop(paste("Invalid period recovered from input file:", period_code))


#...............................................................................
# b. Import complete working database
#...............................................................................
# [ENEMDU_base is preserved as the complete base; analytical subsets are created separately]
ENEMDU_base <- haven::read_sav(input_file, user_na = FALSE) %>% as.data.frame()

# QA [input_reception] — Imported database is non-empty
if (nrow(ENEMDU_base) == 0 || ncol(ENEMDU_base) == 0) stop("ENEMDU_base is empty.")


#...............................................................................
# c. Essential input validation
#...............................................................................
required_vars <- c("prov", "ciudad", "zona", "sector", "rn", "area", "fexp", "ingpc", "pobreza", "epobreza", "CONDACTN")
missing_required_vars <- setdiff(required_vars, names(ENEMDU_base))

# QA [input_structure] — Required variables present
if (length(missing_required_vars) > 0)
  stop(paste("Required variables are missing:", paste(missing_required_vars, collapse = ", ")))

required_numeric_vars <- c("prov", "ciudad", "rn", "area", "ingpc", "fexp", "CONDACTN")     
non_numeric_required_vars <- required_numeric_vars[!vapply(ENEMDU_base[required_numeric_vars], is.numeric, logical(1))]

# QA [input_structure] — Required numeric types
if (length(non_numeric_required_vars) > 0)                                                                  
  stop(paste("Required variables are not numeric:", paste(non_numeric_required_vars, collapse = ", ")))

# QA [fexp_validity] — Missingness, finiteness, and positivity
if (anyNA(ENEMDU_base$fexp)) stop("The expansion factor fexp contains missing values.")
if (any(!is.finite(ENEMDU_base$fexp))) stop("The expansion factor fexp contains non-finite values.")
if (any(ENEMDU_base$fexp <= 0)) stop("The expansion factor fexp contains non-positive values.")


#...............................................................................
# d. Preserve input references required downstream
#...............................................................................
input_sample_n <- nrow(ENEMDU_base)
input_expanded_n <- sum(ENEMDU_base$fexp)

valid_n <- vapply(ENEMDU_base[required_vars], function(x) sum(!is.na(x)), integer(1))
missing_n <- vapply(ENEMDU_base[required_vars], function(x) sum(is.na(x)), integer(1))

valid_expanded <- vapply(required_vars, function(variable) sum(ENEMDU_base$fexp[!is.na(ENEMDU_base[[variable]])]), numeric(1))
missing_expanded <- vapply(required_vars, function(variable) sum(ENEMDU_base$fexp[is.na(ENEMDU_base[[variable]])]), numeric(1))

# References used later to verify the ingpc analytical universe
input_ingpc_valid_n <- unname(valid_n["ingpc"])
input_ingpc_missing_n <- unname(missing_n["ingpc"])
input_ingpc_valid_expanded <- unname(valid_expanded["ingpc"])
input_ingpc_missing_expanded <- unname(missing_expanded["ingpc"])



###--------------------------------------------------------------------------###
###                     1A.2. Initial exploration                            ###
###--------------------------------------------------------------------------###
# a. General database overview
database_overview <- data.frame(
  Indicador = c("Archivo", "Período", "Observaciones", "Variables", "Tamaño (MB)", "Variables con etiqueta", "Variables con etiquetas de valor"),
  Valor = c(
    input_name, period_code, input_sample_n, ncol(ENEMDU_base), round(as.numeric(object.size(ENEMDU_base)) / 1024^2, 2),
    sum(vapply(ENEMDU_base, function(x) !is.null(attr(x, "label")), logical(1))),
    sum(vapply(ENEMDU_base, function(x) { labels <- attr(x, "labels"); !is.null(labels) && length(labels) > 0 }, logical(1)))
  ),
  stringsAsFactors = FALSE
)

# b. Required-variable overview
required_variables_overview <- data.frame(
  Variable = required_vars,
  Class = vapply(ENEMDU_base[required_vars], function(x) class(x)[1], character(1)),
  Valid_N = valid_n,
  Missing_N = missing_n,
  Valid_Expanded = round(valid_expanded, 0),
  Missing_Expanded = round(missing_expanded, 0),
  Label = vapply(ENEMDU_base[required_vars], function(x) { variable_label <- attr(x, "label"); if (is.null(variable_label)) "" else as.character(variable_label) }, character(1)),
  stringsAsFactors = FALSE
)

# c. Reference outputs for Technical Report (RT) and Audit Report (RA)
technical_output_point1 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "RESUMEN DE LA BASE IMPORTADA\n\n",
  paste(capture.output(print(database_overview, row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "REVISIÓN DE VARIABLES REQUERIDAS PARA EL PROCESAMIENTO — EXPLORACIÓN INICIAL DE ENTRADA\n\n",
  paste(capture.output(print(required_variables_overview, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "PUNTO 1: APROBADO\n"
)
cat(technical_output_point1)


# d. Remove temporary validation objects
rm(
  required_vars, required_numeric_vars, missing_required_vars, non_numeric_required_vars,
  period_match, period_parts, valid_n, missing_n, valid_expanded, missing_expanded,
  technical_output_point1
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###                         2A. Create identifiers:                          ###
###               Province, natural region, PSU, and study domains           ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
###--------------------------------------------------------------------------###
###                    2A.1. Province reconstruction                         ###
###--------------------------------------------------------------------------###
# a. Reconstruct province from ciudad following the original SPSS methodology
ENEMDU_base$provin <- as.integer(substr(sprintf("%06d", as.integer(ENEMDU_base$ciudad)), 1, 2))

# b. Preserve metadata from the original source variable prov
attr(ENEMDU_base$provin, "label") <- attr(ENEMDU_base$prov, "label")
attr(ENEMDU_base$provin, "labels") <- attr(ENEMDU_base$prov, "labels")

# c. QA [province_reconstruction] — Validate reconstruction against original province: missingness, valid codes, and equivalence
prov_original <- as.integer(ENEMDU_base$prov)
provin_reconstructed <- as.integer(ENEMDU_base$provin)
province_difference <- (is.na(prov_original) != is.na(provin_reconstructed)) | (!is.na(prov_original) & !is.na(provin_reconstructed) & prov_original != provin_reconstructed)
invalid_province_code <- !is.na(provin_reconstructed) & !(provin_reconstructed %in% c(1:22, 90L))

if (anyNA(provin_reconstructed)) stop("Province reconstruction from ciudad produced missing values.")
if (any(invalid_province_code)) stop(paste(sum(invalid_province_code), "records have invalid reconstructed province codes."))
if (any(province_difference)) stop(paste(sum(province_difference), "records differ between original prov and reconstructed provin."))


###--------------------------------------------------------------------------###
###                 2A.2. Natural region reconstruction                      ###
###--------------------------------------------------------------------------###
# a. Reconstruct natural region using the province grouping defined in SPSS
provin_num <- as.integer(ENEMDU_base$provin)

ENEMDU_base$rnatura <- dplyr::case_when(
  provin_num %in% c(1:6, 10, 11, 17, 18) ~ 1L,
  provin_num %in% c(7:9, 12, 13, 90) ~ 2L,
  provin_num %in% c(14:16, 19, 21, 22) ~ 3L,
  provin_num == 20 ~ 4L,
  TRUE ~ NA_integer_
)

# b. Add variable and value labels
attr(ENEMDU_base$rnatura, "label") <- "Región natural"
attr(ENEMDU_base$rnatura, "labels") <- c("Sierra" = 1L, "Costa" = 2L, "Amazonía" = 3L, "Insular" = 4L)

# c. QA [natural_region_reconstruction] — Validate reconstruction against original region: missingness, valid codes, and equivalence
rn_original <- as.integer(ENEMDU_base$rn)
rnatura_reconstructed <- as.integer(ENEMDU_base$rnatura)
region_difference <- (is.na(rn_original) != is.na(rnatura_reconstructed)) | (!is.na(rn_original) & !is.na(rnatura_reconstructed) & rn_original != rnatura_reconstructed)
invalid_region_code <- !is.na(rnatura_reconstructed) & !(rnatura_reconstructed %in% 1:4)

if (anyNA(rnatura_reconstructed)) stop("Natural-region reconstruction from provin produced missing values.")
if (any(invalid_region_code)) stop(paste(sum(invalid_region_code), "records have invalid reconstructed natural-region codes."))
if (any(region_difference)) stop(paste(sum(region_difference), "records differ between original rn and reconstructed rnatura."))


###--------------------------------------------------------------------------###
###                           2A.3. PSU identifier                           ###
###--------------------------------------------------------------------------###
# a. Reproduce the PSU/UPM identifier defined in the original methodology
ENEMDU_base$identif <- paste0(
  sprintf("%02d", as.integer(ENEMDU_base$provin)),
  substr(sprintf("%06d", as.integer(ENEMDU_base$ciudad)), 3, 6),
  trimws(as.character(ENEMDU_base$zona)),
  trimws(as.character(ENEMDU_base$sector))
)
attr(ENEMDU_base$identif, "label") <- "Identificación del sector"

# b. QA [psu_identifier] — Validate expected 12-digit PSU structure
invalid_identif <- is.na(ENEMDU_base$identif) | !grepl("^[0-9]{12}$", ENEMDU_base$identif)

if (any(invalid_identif)) stop(paste(sum(invalid_identif), "records have an invalid PSU identifier."))


###--------------------------------------------------------------------------###
###                       2A.4. Study domain identifier                      ###
###--------------------------------------------------------------------------###
# a. Reproduce study-domain classification defined in the original SPSS
rnatura_num <- as.integer(ENEMDU_base$rnatura)
area_num <- as.integer(ENEMDU_base$area)
ciudad_num <- as.integer(ENEMDU_base$ciudad)

ENEMDU_base$dominio <- dplyr::case_when(
  # Self-represented cities
  ciudad_num == 170150 & area_num == 1 ~ "01",
  ciudad_num ==  90150 & area_num == 1 ~ "02",
  ciudad_num ==  10150 & area_num == 1 ~ "03",
  ciudad_num ==  70150 & area_num == 1 ~ "04",
  ciudad_num == 180150 & area_num == 1 ~ "05",
  # Urban domains by natural region
  rnatura_num == 1 & area_num == 1 & !(ciudad_num %in% c(10150, 170150, 180150)) ~ "06",
  rnatura_num == 2 & area_num == 1 & !(ciudad_num %in% c(70150, 90150)) ~ "07",
  rnatura_num == 3 & area_num == 1 ~ "08",
  # Rural domains by natural region
  rnatura_num == 1 & area_num == 2 ~ "09",
  rnatura_num == 2 & area_num == 2 ~ "10",
  rnatura_num == 3 & area_num == 2 ~ "11",
  TRUE ~ NA_character_
)

# b. QA [study_domain] — Validate complete assignment, valid codes, and consistency with area
valid_domain_codes <- sprintf("%02d", 1:11)
unassigned_domain <- is.na(ENEMDU_base$dominio)
invalid_domain_code <- !unassigned_domain & !(ENEMDU_base$dominio %in% valid_domain_codes)

domain_area_mismatch <- !unassigned_domain & (
  (ENEMDU_base$dominio %in% sprintf("%02d", 1:8) & area_num != 1) |
    (ENEMDU_base$dominio %in% sprintf("%02d", 9:11) & area_num != 2)
)

if (any(unassigned_domain)) stop(paste(sum(unassigned_domain), "records were not assigned to a study domain."))
if (any(invalid_domain_code)) stop(paste(sum(invalid_domain_code), "records have invalid study-domain codes."))
if (any(domain_area_mismatch)) stop(paste(sum(domain_area_mismatch), "records have study-domain and area inconsistencies."))


###--------------------------------------------------------------------------###
###                     2A.5. Technical summaries                            ###
###--------------------------------------------------------------------------###
# Technical summary of constructed identifiers
identifier_variables <- c("provin", "rnatura", "identif", "dominio")
identifier_descriptions <- c("Provincia reconstruida", "Región natural reconstruida", "Identificador UPM", "Dominio de estudio")

identifier_summary <- data.frame(
  Variable = identifier_variables,
  Description = identifier_descriptions,
  Sample_Valid = vapply(identifier_variables, function(x) sum(!is.na(ENEMDU_base[[x]])), integer(1)),
  Population_Valid = vapply(identifier_variables, function(x) sum(ENEMDU_base$fexp[!is.na(ENEMDU_base[[x]])]), numeric(1)),
  Distinct_Values = vapply(identifier_variables, function(x) dplyr::n_distinct(ENEMDU_base[[x]], na.rm = TRUE), integer(1)),
  stringsAsFactors = FALSE
)

# Diagnostic distribution by study domain
domain_distribution <- ENEMDU_base %>%
  group_by(dominio, area) %>%
  summarise(Sample_N = n(), Expanded_N = round(sum(fexp), 0), Unique_PSUs = n_distinct(identif), .groups = "drop") %>%
  arrange(dominio, area)

# Technical output
technical_output_point2 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "RESUMEN DE IDENTIFICADORES CONSTRUIDOS\n\n",
  paste(capture.output(print(identifier_summary, row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "DISTRIBUCIÓN POR DOMINIO DE ESTUDIO\n\n",
  paste(capture.output(print(as.data.frame(domain_distribution), row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "PUNTO 2: APROBADO\n"
)
cat(technical_output_point2)


# Remove temporary validation objects
rm(
  prov_original, provin_reconstructed, province_difference, invalid_province_code, provin_num, 
  rn_original, rnatura_reconstructed, region_difference, invalid_region_code, invalid_identif, 
  rnatura_num, area_num, ciudad_num, valid_domain_codes, unassigned_domain, invalid_domain_code, 
  domain_area_mismatch, technical_output_point2, identifier_variables, identifier_descriptions
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###                        3A. ingpc treatment                               ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
# a. Identify records with non-missing per capita income
# [Same analytical universe as the original SPSS: SELECT IF (NOT SYSMIS(ingpc))]
idx_ingpc_valid <- !is.na(ENEMDU_base$ingpc)

# b. Create a separate analytical database; ENEMDU_base remains unchanged
ENEMDU_decile <- ENEMDU_base[idx_ingpc_valid, , drop = FALSE]

# c. Calculate observed ingpc universe once
observed_ingpc_missing_n <- sum(!idx_ingpc_valid)
observed_ingpc_valid_n <- nrow(ENEMDU_decile)
observed_ingpc_missing_expanded <- sum(ENEMDU_base$fexp[!idx_ingpc_valid])
observed_ingpc_valid_expanded <- sum(ENEMDU_decile$fexp)

# d. Build comparative evidence against Point 1 references
ingpc_universe_comparison <- data.frame(
  Universe = c("Missing ingpc records", "Valid ingpc analytical universe"),
  Reference_Sample_N = c(input_ingpc_missing_n, input_ingpc_valid_n),
  Reference_Population = round(c(input_ingpc_missing_expanded, input_ingpc_valid_expanded), 0),
  Observed_Sample_N = c(observed_ingpc_missing_n, observed_ingpc_valid_n),
  Observed_Population = round(c(observed_ingpc_missing_expanded, observed_ingpc_valid_expanded), 0)
)

# e. QA [ingpc_universe] — Analytical selection rule
if (observed_ingpc_valid_n != sum(idx_ingpc_valid) || anyNA(ENEMDU_decile$ingpc))
  stop("The analytical INGPC universe does not reproduce the rule: ingpc must be non-missing.")

# f. QA [ingpc_universe] — Consistency with Point 1 sample and population references
if (observed_ingpc_missing_n != input_ingpc_missing_n ||
    observed_ingpc_valid_n != input_ingpc_valid_n)
  stop("INGPC treatment does not match the sample counts registered in Point 1.")

if (!isTRUE(all.equal(observed_ingpc_missing_expanded, input_ingpc_missing_expanded, tolerance = 1e-8)) ||
    !isTRUE(all.equal(observed_ingpc_valid_expanded, input_ingpc_valid_expanded, tolerance = 1e-8)))
  stop("INGPC treatment does not match the expanded counts registered in Point 1.")

# g. Technical output
ingpc_treatment_summary <- data.frame(
  Step = c("Base ENEMDU original", "Registros con ingpc perdido", "Universo analítico (ingpc válido)"),
  Sample_N = c(input_sample_n, observed_ingpc_missing_n, observed_ingpc_valid_n),
  Expanded_N = round(c(input_expanded_n, observed_ingpc_missing_expanded, observed_ingpc_valid_expanded), 0),
  stringsAsFactors = FALSE
)
technical_output_point3 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "UNIVERSO DE INGRESO PER CÁPITA\n\n",
  paste(capture.output(print(ingpc_treatment_summary, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "EVIDENCIA OBSERVADA\n\n",
  paste(capture.output(print(ingpc_universe_comparison, row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "PUNTO 3: APROBADO\n"
)
cat(technical_output_point3)

# h. Remove temporary validation objects
rm(
  idx_ingpc_valid, observed_ingpc_missing_n, observed_ingpc_valid_n, observed_ingpc_missing_expanded, 
  observed_ingpc_valid_expanded, ingpc_universe_comparison, technical_output_point3
)




###==========================================================================###
###                                                                          ###
###                                                                          ###
###            4A. Per capita income decile calculation                      ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
# a. SPSS-equivalent weighted decile function
# [Reproduces RANK ... /NTILES(10) /TIES=MEAN using fexp; methodology unchanged]
weighted_ntile_spss <- function(x, w, n = 10) {
  rank_base <- aggregate(
    w ~ x,
    data = data.frame(x = as.numeric(x), w = as.numeric(w)),
    FUN = sum
  )
  rank_base <- rank_base[order(rank_base$x), , drop = FALSE]
  total_w <- sum(rank_base$w)
  cum_w_before <- c(0, head(cumsum(rank_base$w), -1))
  rank_base$mean_rank <- cum_w_before + (rank_base$w + 1) / 2
  rank_base$ntile <- floor(1 + (rank_base$mean_rank * n) / (total_w + 1))
  as.integer(rank_base$ntile[match(as.numeric(x), rank_base$x)])
}

# b. Calculate per capita income deciles on the analytical database
ENEMDU_decile$ningpc <- weighted_ntile_spss(ENEMDU_decile$ingpc, ENEMDU_decile$fexp, n = 10)

# c. Add variable label
attr(ENEMDU_decile$ningpc, "label") <- "Per capita income decile"

# d. QA [decile_validity] — Complete assignment, 1–10 coverage, and valid codes
observed_deciles <- sort(unique(ENEMDU_decile$ningpc))
missing_deciles <- setdiff(1:10, observed_deciles)
invalid_deciles <- setdiff(observed_deciles, 1:10)

if (anyNA(ENEMDU_decile$ningpc)) stop("The per capita income decile variable contains missing values.")
if (length(missing_deciles) > 0) stop(paste("Expected deciles are missing:", paste(missing_deciles, collapse = ", ")))
if (length(invalid_deciles) > 0) stop(paste("Invalid decile values were generated:", paste(invalid_deciles, collapse = ", ")))


###--------------------------------------------------------------------------###
###                    4A.1 Decile frequency table                           ###
###--------------------------------------------------------------------------###
# a. Total expanded population in the valid ingpc analytical universe
total_expanded_valid <- sum(ENEMDU_decile$fexp)
decile_frequency_table <- aggregate(fexp ~ ningpc, data = ENEMDU_decile, FUN = sum)
names(decile_frequency_table) <- c("Decile", "Frequency")
decile_frequency_table$Percentage <- (decile_frequency_table$Frequency / total_expanded_valid) * 100

# b. SPSS-style display
decile_frequency_table$Frequency <- round(decile_frequency_table$Frequency, 0)
decile_frequency_table$Percentage <- round(decile_frequency_table$Percentage, 1)
decile_frequency_table <- rbind(decile_frequency_table,data.frame(Decile = "Total", Frequency = round(total_expanded_valid, 0), Percentage = 100.0))

# c. QA [decile_population] — Validate against the valid INGPC expanded population preserved in Point 1
if (!isTRUE(all.equal(total_expanded_valid, input_ingpc_valid_expanded, tolerance = 1e-8)))
  stop("The expanded population in the decile frequency table differs from the valid INGPC population registered in Point 1.")


###--------------------------------------------------------------------------###
###                      4A.2 Decile report table                            ###
###--------------------------------------------------------------------------###
# a. Weighted SPSS-style report by decile
decile_report_table <- do.call(
  rbind,
  lapply(split(ENEMDU_decile, ENEMDU_decile$ningpc), function(df) {
    expanded_n <- sum(df$fexp)
    weighted_sum <- sum(df$ingpc * df$fexp)
    data.frame(
      Decile = unique(df$ningpc),
      N = expanded_n,
      Sum = weighted_sum,
      Mean = weighted_sum / expanded_n,
      Min = min(df$ingpc),
      Max = max(df$ingpc)
    )
  })
)

# b. Preserve exact unrounded means for subsequent survey-estimate validation
point4_ingpc_mean_reference <- decile_report_table[, c("Decile", "Mean")]
rownames(point4_ingpc_mean_reference) <- NULL

# Add total row
total_report_row <- data.frame(
  Decile = "Total",
  N = sum(ENEMDU_decile$fexp),
  Sum = sum(ENEMDU_decile$ingpc * ENEMDU_decile$fexp),
  Mean = sum(ENEMDU_decile$ingpc * ENEMDU_decile$fexp) / sum(ENEMDU_decile$fexp),
  Min = min(ENEMDU_decile$ingpc),
  Max = max(ENEMDU_decile$ingpc)
)
decile_report_table <- rbind(decile_report_table, total_report_row)

# c. SPSS-style display; exact means above remain unrounded for Point 6 validation
decile_report_table$N <- round(decile_report_table$N, 0)
decile_report_table$Sum <- round(decile_report_table$Sum, 0)
decile_report_table$Mean <- round(decile_report_table$Mean, 2)
decile_report_table$Min <- round(decile_report_table$Min, 0)
decile_report_table$Max <- round(decile_report_table$Max, 0)


# d. Technical output
technical_output_point4 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "DISTRIBUCIÓN DE LA POBLACIÓN POR DECIL DE INGRESO PER CÁPITA\n\n",
  paste(capture.output(print(decile_frequency_table, row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "ESTADÍSTICOS DEL INGRESO PER CÁPITA POR DECIL\n\n",
  paste(capture.output(print(decile_report_table, row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "PUNTO 4: APROBADO\n"
)
cat(technical_output_point4)

# e. Remove temporary validation objects
rm(
  weighted_ntile_spss, observed_deciles, missing_deciles, invalid_deciles,
  total_expanded_valid, total_report_row, technical_output_point4
)






###==========================================================================###
###                                                                          ###
###                                                                          ###
###                       5A. Sample Design for ENEMDU Survey                ###           
###                                                                          ###
###                                                                          ###
###==========================================================================###
options(survey.lonely.psu = "certainty")  # INEC methodology: Treatment for strata with a single PSU.

d1 <- ENEMDU_decile %>%
  as_survey_design(
    ids = identif,                        # PSU/UPM
    strata = dominio,                     # Survey strata / study domains
    weights = fexp,                       # Expansion factor
    nest = TRUE
  )





###==========================================================================###
###                                                                          ###
###                                                                          ###
###                6A. Survey-adjusted mean of ingpc by decile               ###
###                                                                          ###
###                                                                          ###
###==========================================================================###
# a. Estimate mean per capita income by decile using the complex survey design
svy_ingpc_by_decile <- d1 %>%
  group_by(ningpc) %>%
  summarise(Mean = survey_mean(ingpc, vartype = c("se", "ci"), level = 0.95, na.rm = TRUE))

# b. Keep numeric estimates for validation and subsequent calculations
svy_ingpc_by_decile_table_num <- svy_ingpc_by_decile %>%
  transmute(Decile = ningpc, Mean = Mean, Linearized_Std_Err = Mean_se, CI_95_Lower = Mean_low, CI_95_Upper = Mean_upp)

# c. Build comparative evidence against Point 4 reference
mean_validation <- svy_ingpc_by_decile_table_num %>%
  select(Decile, Mean) %>%
  left_join(point4_ingpc_mean_reference %>% rename(Mean_reference = Mean), by = "Decile") %>%
  mutate(Difference = abs(Mean - Mean_reference))

# d. QA [survey_mean_consistency] — Validate survey means against exact unrounded weighted means preserved in Point 4
mean_tolerance <- 1e-8

if (anyNA(mean_validation$Mean_reference))
  stop("Survey mean validation failed: one or more deciles have no Point 4 reference mean.")
if (anyNA(mean_validation$Mean) || any(!is.finite(mean_validation$Mean)))
  stop("Survey mean validation failed: one or more deciles have missing or non-finite survey means.")
if (any(mean_validation$Difference > mean_tolerance))
  stop(paste(
    "Survey mean validation failed for decile(s):",
    paste(mean_validation$Decile[mean_validation$Difference > mean_tolerance], collapse = ", ")
  ))


# e. Prepare display table with fixed decimal precision for Stata comparison
svy_ingpc_by_decile_table <- svy_ingpc_by_decile_table_num %>%
  mutate(
    Mean = sprintf("%.6f", Mean),
    Linearized_Std_Err = sprintf("%.6f", Linearized_Std_Err),
    CI_95_Lower = sprintf("%.6f", CI_95_Lower),
    CI_95_Upper = sprintf("%.6f", CI_95_Upper)
  )


# f. Technical output
technical_output_point6 <- paste0(
  "\n============================================================\n",
  "REPORTE TÉCNICO (RT)\n",
  "============================================================\n",
  "ESTIMACIÓN DEL INGRESO PER CÁPITA POR DECIL — DISEÑO MUESTRAL COMPLEJO\n\n",
  paste(capture.output(print(as.data.frame(svy_ingpc_by_decile_table), row.names = FALSE)), collapse = "\n"),
  "\n\n============================================================\n",
  "REPORTE DE AUDITORÍA (RA)\n",
  "============================================================\n",
  "EVIDENCIA OBSERVADA\n\n",
  paste(capture.output(print(as.data.frame(mean_validation), row.names = FALSE)), collapse = "\n"),
  "\n\n------------------------------------------------------------\n",
  "PUNTO 6: APROBADO\n"
)
cat(technical_output_point6)


# g. Remove temporary validation objects
rm(svy_ingpc_by_decile, mean_tolerance, mean_validation, technical_output_point6)
