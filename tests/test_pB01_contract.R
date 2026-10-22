# ==============================================================================
# Stage B01 — CONDACTN contract tests (synthetic data; no ENEMDU file required)
# Run from the project root: Rscript tests/test_pB01_contract.R
# ==============================================================================

source("R/validation_errors.R")
source("R/B01_validacion_exploracion_condactn.R")
source("R/control_catalog.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_error <- function(expr, label) {
  result <- tryCatch(
    { force(expr); NULL },
    error = function(e) e
  )
  assert(inherits(result, "error"), paste("Expected error:", label))
}

expect_stage_error <- function(expr, check_id, expected_message) {
  result <- tryCatch(
    { force(expr); NULL },
    error = function(e) e
  )
  assert(inherits(result, "rap_validation_error"),
         paste("Expected structured validation error:", check_id))
  assert(identical(result$stage_id, "B01_condactn_reference"),
         paste("Unexpected stage_id:", check_id))
  assert(identical(result$check_id, check_id),
         paste("Unexpected check_id:", check_id))
  assert(identical(conditionMessage(result), expected_message),
         paste("Unexpected error message:", check_id))
}

stage_id <- "B01_condactn_reference"
expected_checks <- c(
  "condactn_metadata_present",
  "condactn_codes_valid",
  "condactn_observed_codes_labelled"
)

stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]
stage_groups <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]

assert(nrow(stage_controls) == 3L, "PB01 must catalogue three atomic checks")
assert(nrow(stage_groups) == 1L, "PB01 must catalogue one audit diagnosis")
assert(setequal(stage_controls$check_id, expected_checks), "PB01 check IDs differ")
assert(!anyDuplicated(stage_controls$check_id), "Duplicate PB01 check IDs")

# Realistic labelled input with fractional expansion factors.
# The missing CONDACTN record has a large weight and MUST be excluded.
value_labels <- c("Fuera de PET" = 0, "Ocupados" = 1, "Desocupados" = 2)
synthetic <- data.frame(
  CONDACTN = haven::labelled(
    c(0, 1, 2, 2, NA_real_),
    labels = value_labels,
    label = "Condicion de actividad"
  ),
  fexp = c(1.25, 2.65, 3.125, 4.125, 100)
)

result <- build_condactn_stage(synthetic)
reference <- result$condactn_frequency_reference
display <- result$condactn_frequency_table
evidence <- result$validation_evidence

assert(nrow(reference) == 3L, "Reference has an unexpected category count")
assert(identical(as.numeric(reference$CONDACTN), c(0, 1, 2)),
       "Reference category codes or order differ")
assert(identical(as.character(reference$Category), names(value_labels)),
       "Original SAV-style value labels were not preserved")
assert(isTRUE(all.equal(as.numeric(reference$Frequency),
                        c(1.25, 2.65, 7.25), tolerance = 1e-12)),
       "Weighted reference frequencies differ")
assert(isTRUE(all.equal(sum(reference$Frequency), 11.15, tolerance = 1e-12)),
       "Missing CONDACTN was not excluded from the weighted reference")
assert(!("Total" %in% reference$Category),
       "Numeric reference must not contain a display-only total row")
assert(any(reference$Frequency != round(reference$Frequency)),
       "Numeric reference was unexpectedly rounded")

assert(nrow(display) == 4L, "Display table must include three categories and Total")
assert(identical(as.character(display$CONDACTN), c("0", "1", "2", "")),
       "Display category codes differ")
assert(identical(as.character(display$Category),
                 c(names(value_labels), "Total")),
       "Display category labels or Total differ")
assert(isTRUE(all.equal(as.numeric(display$Frequency),
                        round(c(1.25, 2.65, 7.25, 11.15), 0))),
       "Display frequencies are not rounded as defined")
expected_percent <- round(c(1.25, 2.65, 7.25) / 11.15 * 100, 1)
expected_cumulative <- round(cumsum(c(1.25, 2.65, 7.25)) / 11.15 * 100, 1)
assert(isTRUE(all.equal(as.numeric(display$Valid_Percentage),
                        c(expected_percent, 100))),
       "Display percentages differ")
assert(isTRUE(all.equal(as.numeric(display$Cumulative_Percentage[1:3]),
                        expected_cumulative)),
       "Cumulative percentages differ")
assert(is.na(display$Cumulative_Percentage[4]),
       "Total cumulative percentage must be absent")

assert(identical(evidence$check_id, expected_checks),
       "Production evidence does not match catalogue checks")
assert(all(evidence$stage_id == stage_id) && all(evidence$passed),
       "A valid synthetic source failed the PB01 controls")

successful <- build_stage_audit_diagnostics(
  validation_evidence = evidence,
  control_catalog = control_catalog,
  audit_diagnostic_catalog = audit_diagnostic_catalog,
  stage_id = stage_id
)
assert(isTRUE(successful$stage_passed), "Valid PB01 stage was not approved")
assert(nrow(successful$diagnostics) == 1L,
       "Expected a single PB01 audit diagnosis")
assert(all(successful$diagnostics$Resultado == "OK"),
       "Passing PB01 diagnosis was not marked OK")
cat("PASS: labelled input -> exact weighted reference -> display -> audit OK\n")

# Every critical production check must reject the corresponding invalid input.
no_variable_label <- synthetic
attr(no_variable_label$CONDACTN, "label") <- NULL
expect_stage_error(
  build_condactn_stage(no_variable_label),
  "condactn_metadata_present",
  "No se encontraron las etiquetas de variable o de valores de CONDACTN."
)

invalid_code <- synthetic
invalid_code$CONDACTN <- haven::labelled(
  c(10, 1, 2, 2, NA_real_), labels = value_labels,
  label = "Condicion de actividad"
)
expect_stage_error(
  build_condactn_stage(invalid_code),
  "condactn_codes_valid",
  "Se encontraron códigos CONDACTN inesperados: 10"
)

unlabelled_code <- synthetic
attr(unlabelled_code$CONDACTN, "labels") <- value_labels[1:2]
expect_stage_error(
  build_condactn_stage(unlabelled_code),
  "condactn_observed_codes_labelled",
  "Existe al menos un código CONDACTN observado sin etiqueta de valor."
)
cat("PASS: three invalid PB01 inputs stop at their respective atomic checks\n")

# The audit engine must not approve failed, absent, or duplicated evidence.
failed_evidence <- evidence
failed_evidence$passed[2] <- FALSE
failed <- build_stage_audit_diagnostics(
  failed_evidence, control_catalog, audit_diagnostic_catalog, stage_id
)
assert(!isTRUE(failed$stage_passed), "Failed PB01 check approved the stage")
assert(any(failed$diagnostics$Resultado == "NO SUPERADO"),
       "Failed PB01 check was hidden by the diagnosis")

expect_error(
  build_stage_audit_diagnostics(
    evidence[-1, , drop = FALSE],
    control_catalog, audit_diagnostic_catalog, stage_id
  ),
  "missing PB01 evidence"
)
expect_error(
  build_stage_audit_diagnostics(
    rbind(evidence, evidence[1, , drop = FALSE]),
    control_catalog, audit_diagnostic_catalog, stage_id
  ),
  "duplicate PB01 evidence"
)
cat("PASS: failed, missing and duplicate PB01 evidence rejected\n")
cat("PB01 CONTRACT TESTS: ALL PASSED\n")
