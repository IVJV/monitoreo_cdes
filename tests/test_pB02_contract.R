# ==============================================================================
# Stage B02 — Population classification contract tests (synthetic input)
# Run from project root: Rscript tests/test_pB02_contract.R
# ==============================================================================

source("R/validation_errors.R")
source("R/B01_validacion_exploracion_condactn.R")
source("R/B02_clasificacion_poblacion.R")
source("R/control_catalog.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_error <- function(expr, label) {
  outcome <- tryCatch({ force(expr); NULL }, error = function(e) e)
  assert(inherits(outcome, "error"), paste("Expected error:", label))
}

expect_stage_error <- function(expr, check_id) {
  outcome <- tryCatch({ force(expr); NULL }, error = function(e) e)
  assert(inherits(outcome, "rap_validation_error"),
         paste("Expected structured validation error:", check_id))
  assert(identical(outcome$stage_id, "B02_population_classification"),
         paste("Incorrect error stage:", check_id))
  assert(identical(outcome$check_id, check_id),
         paste("Incorrect check ID:", check_id))
}

stage_id <- "B02_population_classification"
check_id <- "population_classification_consistency"
stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]
stage_diagnostics <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]
assert(nrow(stage_controls) == 1L, "B02 must define exactly one atomic check")
assert(nrow(stage_diagnostics) == 1L, "B02 must define one audit diagnosis")
assert(identical(stage_controls$check_id, check_id),
       "B02 control catalogue has a different check ID")

# Include every official code and an NA record with a large expansion factor.
# Use fractional weights to detect inadvertent rounding or missing-value leakage.
codes <- c(0:9, NA_real_)
weights <- c(1.25, 2.50, 3.25, 4.50, 5.125,
             6.75, 7.375, 8.25, 9.625, 10.875, 500)
source_data <- data.frame(
  CONDACTN = haven::labelled(
    codes,
    labels = stats::setNames(as.numeric(0:9), paste0("Categoria ", 0:9)),
    label = "Condicion de actividad"
  ),
  fexp = weights
)
reference <- build_condactn_stage(source_data)$condactn_frequency_reference
result <- build_population_classification_stage(source_data, reference)
hierarchy <- result$population_classification_table_num
comparison <- result$population_validation
evidence <- result$validation_evidence

# Explicit expected membership by source code; independent of B02 implementation.
expected_codes <- list(
  poblacion_total = 0:9,
  menor_15 = 0L,
  pet = 1:9,
  pea = 1:8,
  poblacion_ocupada = 1:6,
  empleo_adecuado = 1L,
  subempleo = 2:3,
  subempleo_tiempo = 2L,
  subempleo_ingresos = 3L,
  otro_empleo_inadecuado = 4L,
  empleo_no_remunerado = 5L,
  empleo_no_clasificado = 6L,
  desempleo = 7:8,
  desempleo_abierto = 7L,
  desempleo_oculto = 8L,
  pei = 9L
)
expected_variables <- names(expected_codes)
expected_levels <- c(0, 1, 1, 2, 3, 4, 4, 5, 5, 4, 4, 4, 3, 4, 4, 2)
expected_parents <- c(
  NA_character_, "poblacion_total", "poblacion_total", "pet", "pea",
  "poblacion_ocupada", "poblacion_ocupada", "subempleo", "subempleo",
  "poblacion_ocupada", "poblacion_ocupada", "poblacion_ocupada",
  "pea", "desempleo", "desempleo", "pet"
)

assert(identical(as.character(hierarchy$Variable), expected_variables),
       "B02 hierarchy order or indicator names have changed")
assert(identical(as.integer(hierarchy$Order), 1:16),
       "B02 hierarchy order numbers differ")
assert(identical(as.numeric(hierarchy$Level), as.numeric(expected_levels)),
       "B02 hierarchy levels differ")
assert(identical(as.character(hierarchy$Parent), expected_parents),
       "B02 parent-child relationships differ")
assert(nrow(comparison) == 16L, "B02 comparison must have 16 rows")
assert(identical(as.character(comparison$Indicator), expected_variables),
       "B02 comparison indicator order differs")

expected_frequencies <- vapply(expected_codes, function(group_codes) {
  sum(weights[!is.na(codes) & codes %in% group_codes])
}, numeric(1))
assert(isTRUE(all.equal(as.numeric(hierarchy$Expanded_Frequency),
                        unname(expected_frequencies), tolerance = 1e-12)),
       "B02 expanded frequencies differ from independent synthetic totals")
assert(isTRUE(all.equal(as.numeric(hierarchy$Population_Proportion),
                        unname(expected_frequencies / expected_frequencies[["poblacion_total"]]),
                        tolerance = 1e-12)),
       "B02 total-population proportions differ")

expected_parent_proportions <- vapply(seq_along(expected_variables), function(i) {
  parent <- expected_parents[[i]]
  if (is.na(parent)) return(1)
  expected_frequencies[[i]] / expected_frequencies[[parent]]
}, numeric(1))
assert(isTRUE(all.equal(as.numeric(hierarchy$Parent_Proportion),
                        expected_parent_proportions, tolerance = 1e-12)),
       "B02 parent-category proportions differ")

# Each labelled indicator must preserve 0/1 membership and missingness.
for (variable in expected_variables) {
  indicator <- result$data[[variable]]
  observed_membership <- as.integer(indicator)
  expected_membership <- as.integer((0:9) %in% expected_codes[[variable]])
  assert(identical(observed_membership[1:10], expected_membership),
         paste("Binary classification differs:", variable))
  assert(is.na(observed_membership[11L]),
         paste("Missing CONDACTN was not preserved:", variable))
  assert(is.character(attr(indicator, "label", exact = TRUE)) &&
           nzchar(attr(indicator, "label", exact = TRUE)),
         paste("Missing indicator variable label:", variable))
  assert(identical(unname(as.integer(attr(indicator, "labels", exact = TRUE))),
                   c(0L, 1L)),
         paste("Binary value labels differ:", variable))
}
cat("PASS: 16 labelled binary indicators, hierarchy and proportions\n")

assert(isTRUE(all.equal(as.numeric(comparison$Reference),
                        unname(expected_frequencies), tolerance = 1e-12)),
       "Reference comparison values differ")
assert(isTRUE(all.equal(as.numeric(comparison$Observed),
                        unname(expected_frequencies), tolerance = 1e-12)),
       "Observed comparison values differ")
assert(all(is.finite(comparison$Difference)) &&
         all(abs(comparison$Difference) <= 1e-8),
       "B02 expected zero-tolerance reconciliation failed")
assert(nrow(evidence) == 1L &&
         identical(evidence$stage_id, stage_id) &&
         identical(evidence$check_id, check_id) &&
         isTRUE(evidence$passed),
       "B02 check evidence has a different contract")

accepted <- build_stage_audit_diagnostics(
  evidence, control_catalog, audit_diagnostic_catalog, stage_id
)
assert(isTRUE(accepted$stage_passed), "B02 valid evidence was not approved")
assert(nrow(accepted$diagnostics) == 1L &&
         identical(accepted$diagnostics$Resultado, "OK"),
       "B02 valid audit diagnosis differs")
cat("PASS: 16 independent population reconciliations and audit OK\n")

# A changed reference must be rejected by the production control, not reported OK.
invalid_reference <- reference
invalid_reference$Frequency[1L] <- invalid_reference$Frequency[1L] + 1
expect_stage_error(
  build_population_classification_stage(source_data, invalid_reference),
  check_id
)

failed_evidence <- evidence
failed_evidence$passed <- FALSE
rejected <- build_stage_audit_diagnostics(
  failed_evidence, control_catalog, audit_diagnostic_catalog, stage_id
)
assert(!isTRUE(rejected$stage_passed), "Failed B02 evidence was approved")
assert(any(rejected$diagnostics$Resultado == "NO SUPERADO"),
       "B02 failed audit diagnosis was not shown")
expect_error(
  build_stage_audit_diagnostics(
    evidence[0, , drop = FALSE], control_catalog,
    audit_diagnostic_catalog, stage_id
  ),
  "missing B02 evidence"
)
expect_error(
  build_stage_audit_diagnostics(
    rbind(evidence, evidence), control_catalog,
    audit_diagnostic_catalog, stage_id
  ),
  "duplicate B02 evidence"
)
cat("PASS: altered reference and invalid/missing/duplicated evidence rejected\n")
cat("PB02 CONTRACT TESTS: ALL PASSED\n")
