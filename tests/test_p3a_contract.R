# ==============================================================================
# Stage A03 contract tests
# Run from the repository root:
#   Rscript tests/test_p3a_contract.R
# Synthetic data only. No ENEMDU microdata are read or modified.
# ==============================================================================

source("R/validation_errors.R")
source("R/A03_tratamiento_ingpc.R")
source("R/control_catalog.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_stage_error <- function(expr, expected_check, expected_message) {
  result <- tryCatch(
    { force(expr); NULL },
    error = function(e) e
  )

  assert(
    inherits(result, "rap_validation_error"),
    paste("Expected structured validation error:", expected_check)
  )
  assert(
    identical(result$stage_id, "A03_ingpc_treatment"),
    paste("Unexpected stage_id:", expected_check)
  )
  assert(
    identical(result$check_id, expected_check),
    paste("Unexpected check_id:", expected_check)
  )
  assert(
    identical(conditionMessage(result), expected_message),
    paste("Unexpected error message:", expected_check)
  )
}

# ==============================================================================
# 1. Verify the canonical catalogue and its independent identifiers
# ==============================================================================

stage_id <- "A03_ingpc_treatment"
expected_checks <- c(
  "ingpc_selection_rule_valid",
  "ingpc_sample_counts_consistent",
  "ingpc_expanded_population_consistent"
)
expected_audit_group <- "ingpc_analytical_universe_consistency"

stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]
stage_diagnostics <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]

assert(nrow(stage_controls) == 3L, "A03 must catalogue three atomic checks")
assert(nrow(stage_diagnostics) == 1L, "A03 must catalogue one audit diagnostic")
assert(
  identical(as.character(stage_controls$check_id), expected_checks),
  "A03 control IDs or their order differ from the canonical contract"
)
assert(!anyDuplicated(stage_controls$check_id), "Duplicate A03 check IDs")
assert(
  identical(as.character(stage_diagnostics$audit_group), expected_audit_group),
  "Unexpected A03 audit-group identifier"
)

cat("PASS: three canonical checks and one audit diagnostic\n")

# ==============================================================================
# 2. Verify the SPSS selection rule and cross-stage comparison
# ==============================================================================

# The selection is NOT SYSMIS(ingpc), not ingpc > 0: zero and negative
# observations must remain in the analytical universe.
synthetic_base <- data.frame(
  ingpc = c(NA_real_, 0, 100, NA_real_, -20, 200),
  fexp = c(2, 3, 5, 7, 11, 13)
)
original_base <- synthetic_base

# These references represent the values already preserved by Stage A01.
input_references <- list(
  input_ingpc_missing_n = 2L,
  input_ingpc_valid_n = 4L,
  input_ingpc_missing_expanded = 9,
  input_ingpc_valid_expanded = 32
)

result <- create_ingpc_analytical_base(
  synthetic_base,
  input_references,
  return_evidence = TRUE
)

assert(identical(synthetic_base, original_base), "Stage A03 modified ENEMDU_base")
assert(
  identical(result$data$ingpc, c(0, 100, -20, 200)),
  "A03 analytical universe does not reproduce the non-missing rule"
)
assert(
  identical(result$data$fexp, c(3, 5, 11, 13)),
  "A03 analytical universe changed expansion factors"
)
assert(nrow(result$data) == 4L, "Wrong number of analytical observations")
assert(!anyNA(result$data$ingpc), "Analytical INGPC universe contains NA")

# Default return contract remains a data frame, without embedded audit data.
default_result <- create_ingpc_analytical_base(
  synthetic_base, input_references
)
assert(is.data.frame(default_result), "Default A03 result must be a data frame")
assert(
  identical(default_result, result$data),
  "Default and evidence-returning A03 results differ"
)

comparison <- result$ingpc_universe_comparison
assert(nrow(comparison) == 2L, "Expected two cross-stage comparison rows")
assert(
  identical(comparison$universe_id, c("ingpc_missing", "ingpc_valid")),
  "A03 comparison must use the canonical universe IDs"
)
assert(
  identical(as.integer(comparison$Reference_Sample_N), c(2L, 4L)) &&
    identical(as.integer(comparison$Observed_Sample_N), c(2L, 4L)),
  "A03 reference and observed sample counts differ"
)
assert(
  isTRUE(all.equal(comparison$Reference_Expanded_N, c(9, 32))) &&
    isTRUE(all.equal(comparison$Observed_Expanded_N, c(9, 32))),
  "A03 reference and observed expanded populations differ"
)

cat("PASS: methodological selection, intact base and A01 comparison\n")

# ==============================================================================
# 3. Verify evidence for every executed atomic control and audit aggregation
# ==============================================================================

evidence <- result$validation_evidence
assert(nrow(evidence) == 3L, "Expected three executed A03 checks")
assert(
  identical(as.character(evidence$check_id), expected_checks),
  "Executed A03 controls differ from the canonical catalogue"
)
assert(
  all(evidence$stage_id == stage_id),
  "A03 evidence contains an incorrect stage_id"
)
assert(is.logical(evidence$passed) && all(evidence$passed),
       "Valid synthetic A03 data failed an atomic check")

audit <- build_stage_audit_diagnostics(
  evidence,
  control_catalog,
  audit_diagnostic_catalog,
  stage_id
)
assert(isTRUE(audit$stage_passed), "Valid A03 stage was not approved")
assert(nrow(audit$diagnostics) == 1L, "Expected one A03 audit diagnostic")
assert(all(audit$diagnostics$Resultado == "OK"), "Unexpected A03 audit result")

cat("PASS: three atomic controls produce one passing audit diagnostic\n")

# ==============================================================================
# 4. Verify the two reachable fail-fast conditions and original messages
# ==============================================================================

# This condition is reachable when A01 sample-count references are inconsistent.
bad_counts <- input_references
bad_counts$input_ingpc_valid_n <- 5L
expect_stage_error(
  create_ingpc_analytical_base(synthetic_base, bad_counts),
  "ingpc_sample_counts_consistent",
  "El tratamiento de INGPC no coincide con los conteos muestrales registrados en la Etapa A01."
)

# This condition is reachable when A01 expanded-population references differ.
bad_population <- input_references
bad_population$input_ingpc_valid_expanded <- 33
expect_stage_error(
  create_ingpc_analytical_base(
    synthetic_base, bad_population, return_evidence = TRUE
  ),
  "ingpc_expanded_population_consistent",
  "El tratamiento de INGPC no coincide con las poblaciones expandidas registradas en la Etapa A01."
)

# ingpc_selection_rule_valid is covered above as a passing control. Its
# negative case cannot be reached through ordinary inputs: the function
# itself selects exactly the records where ingpc is not missing.
cat("PASS: two reachable structured validation failures\n")
cat("P3A CONTRACT TESTS: ALL PASSED\n")
