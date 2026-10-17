# ==============================================================================
# Stage 04A contract tests
# Run from the repository root: Rscript tests/test_p4a_contract.R
# Synthetic data only. No ENEMDU microdata are read or modified.
# ==============================================================================

source("R/validation_errors.R")
source("R/04A_deciles_ingpc.R")
source("R/control_catalog.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_stage_error <- function(expr, expected_check, expected_message) {
  result <- tryCatch(
    { force(expr); NULL },
    error = function(e) e
  )
  assert(inherits(result, "rap_validation_error"),
         paste("Expected structured error:", expected_check))
  assert(identical(result$stage_id, "04a_deciles_ingpc"),
         paste("Unexpected stage_id:", expected_check))
  assert(identical(result$check_id, expected_check),
         paste("Unexpected check_id:", expected_check))
  assert(identical(conditionMessage(result), expected_message),
         paste("Unexpected error message:", expected_check))
}

# ==============================================================================
# 1. Canonical controls and audit diagnostics
# ==============================================================================

stage_id <- "04a_deciles_ingpc"
expected_checks <- c(
  "decile_no_missing",
  "decile_complete_coverage",
  "decile_codes_valid",
  "decile_expanded_population_consistent"
)
expected_groups <- c(
  "decile_classification_validity",
  "decile_population_consistency"
)

stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]
stage_diagnostics <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]
assert(nrow(stage_controls) == 4L, "Expected four canonical 04A checks")
assert(nrow(stage_diagnostics) == 2L, "Expected two 04A audit diagnostics")
assert(identical(as.character(stage_controls$check_id), expected_checks),
       "Unexpected 04A check IDs or order")
assert(identical(as.character(stage_diagnostics$audit_group), expected_groups),
       "Unexpected 04A audit groups")
assert(!anyDuplicated(stage_controls$check_id), "Duplicate 04A check IDs")
cat("PASS: four canonical checks and two audit diagnostics\n")

# ==============================================================================
# 2. Independent weighted-rank check including tied income values
# ==============================================================================

# Group weights: ingpc=1 -> 8; ingpc=2 -> 2; ingpc=3 -> 4.
# Weighted mean ranks are 4.5, 9.5 and 12.5; total weight is 14.
ranked <- weighted_ntile_spss(c(1, 1, 2, 3), c(3, 5, 2, 4))
assert(identical(ranked, c(4L, 4L, 7L, 9L)),
       "Weighted SPSS ranking or tie handling changed")
cat("PASS: SPSS-style weighted ranks and mean ties\n")

# ==============================================================================
# 3. Reusable stage outputs, population reference and unchanged input
# ==============================================================================

synthetic_base <- data.frame(
  ingpc = c(-20, 0, seq(1, 38)),
  fexp = rep(1:4, 10)
)
original_base <- synthetic_base
input_references <- list(input_ingpc_valid_expanded = sum(synthetic_base$fexp))

result <- build_ingpc_decile_stage(synthetic_base, input_references)
assert(identical(synthetic_base, original_base), "Stage 04A modified its input")
assert(nrow(result$data) == nrow(synthetic_base), "04A dropped observations")
assert(identical(sort(unique(result$data$ningpc)), 1:10),
       "04A did not produce all ten deciles")
assert(identical(attr(result$data$ningpc, "label"), "Per capita income decile"),
       "The original decile variable label changed")

# Preserve default pre-existing function contracts.
assert(identical(create_ingpc_deciles(synthetic_base), result$data),
       "The default create_ingpc_deciles() result changed")
assert(identical(build_decile_frequency_table(result$data, input_references),
                 result$decile_frequency_table),
       "The default frequency-table result changed")
assert(identical(build_decile_report_table(result$data)$decile_report_table,
                 result$decile_report_table),
       "The default decile report table changed")

frequency_table <- result$decile_frequency_table
assert(identical(as.character(tail(frequency_table$Decile, 1)), "Total"),
       "The total row is missing")
assert(isTRUE(all.equal(tail(frequency_table$Frequency, 1),
                        input_references$input_ingpc_valid_expanded)),
       "The frequency-table total differs from Stage 01A")
assert(isTRUE(all.equal(sum(head(frequency_table$Percentage, -1)), 100)),
       "The decile frequencies do not total 100 percent")
assert(isTRUE(all.equal(sum(head(frequency_table$Frequency, -1)),
                        tail(frequency_table$Frequency, 1))),
       "The population per decile does not add to the full population")

report_table <- result$decile_report_table
reference <- result$point4_ingpc_mean_reference
assert(nrow(report_table) == 11L && nrow(reference) == 10L,
       "Unexpected report or reference row count")
assert(identical(names(reference), c("Decile", "Mean")),
       "06A reference contract changed")
assert(isTRUE(all.equal(reference$Mean, head(report_table$Mean, 10))),
       "The unrounded 06A mean references differ from the report")
assert(isTRUE(all.equal(tail(report_table$N, 1), sum(synthetic_base$fexp))),
       "Incorrect total expanded N")
assert(isTRUE(all.equal(tail(report_table$Sum, 1),
                        sum(synthetic_base$ingpc * synthetic_base$fexp))),
       "Incorrect total weighted income")

assert(nrow(result$validation_evidence) == 4L, "Missing 04A control evidence")
assert(identical(as.character(result$validation_evidence$check_id), expected_checks),
       "04A evidence and catalogue have different check IDs")
assert(all(result$validation_evidence$stage_id == stage_id) &&
         all(result$validation_evidence$passed),
       "04A evidence must indicate successful execution")

audit <- build_stage_audit_diagnostics(
  result$validation_evidence,
  control_catalog,
  audit_diagnostic_catalog,
  stage_id
)
assert(isTRUE(audit$stage_passed), "04A was not approved")
assert(nrow(audit$diagnostics) == 2L &&
         all(audit$diagnostics$Resultado == "OK"),
       "Unexpected 04A audit result")
cat("PASS: decile tables, unrounded 06A references and audit evidence\n")

# ==============================================================================
# 4. Four reachable structured failure cases
# ==============================================================================

# Normal weighted ranking with too few incomes cannot cover all ten deciles.
expect_stage_error(
  create_ingpc_deciles(synthetic_base[1:4, , drop = FALSE]),
  "decile_complete_coverage",
  paste(
    "Faltan deciles esperados:",
    paste(setdiff(1:10, unique(weighted_ntile_spss(
      synthetic_base$ingpc[1:4], synthetic_base$fexp[1:4]
    ))), collapse = ", ")
  )
)

# Substitute only the ranking function during this synthetic test to exercise
# otherwise unreachable negative branches. Restore the exact original afterward.
original_rank <- weighted_ntile_spss
weighted_ntile_spss <- function(x, w, n = 10) rep(NA_integer_, length(x))
expect_stage_error(
  create_ingpc_deciles(synthetic_base),
  "decile_no_missing",
  "La variable de decil de ingreso per cápita contiene valores perdidos."
)

weighted_ntile_spss <- function(x, w, n = 10) {
  actual <- original_rank(x, w, n)
  actual[1] <- 11L  # Keep the 1–10 coverage; introduce just an invalid code.
  actual
}
expect_stage_error(
  create_ingpc_deciles(synthetic_base),
  "decile_codes_valid",
  "Se generaron valores de decil no válidos: 11"
)
weighted_ntile_spss <- original_rank

bad_references <- input_references
bad_references$input_ingpc_valid_expanded <-
  bad_references$input_ingpc_valid_expanded + 1
expect_stage_error(
  build_decile_frequency_table(result$data, bad_references),
  "decile_expanded_population_consistent",
  paste(
    "La población expandida de la tabla de frecuencias por decil",
    "difiere de la población con INGPC válido registrada en la Etapa 01A."
  )
)
cat("PASS: four structured validation failures\n")
cat("P4A CONTRACT TESTS: ALL PASSED\n")
