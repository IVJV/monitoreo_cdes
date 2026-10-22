# P1A contract smoke tests. Run from the repository root:
# Rscript test_p1a_contract.R
# All input data are synthetic; no ENEMDU microdata are read or modified.

source("R/validation_errors.R")
source("R/A01_ingesta_enemdu.R")
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

  assert(
    inherits(result, "rap_validation_error"),
    paste("Expected structured validation error:", check_id)
  )

  assert(
    identical(result$stage_id, "A01_ingestion"),
    paste("Unexpected stage_id:", check_id)
  )

  assert(
    identical(result$check_id, check_id),
    paste("Unexpected check_id:", check_id)
  )

  assert(
    identical(conditionMessage(result), expected_message),
    paste("Unexpected error message:", check_id)
  )
}

stage_id <- "A01_ingestion"
expected_checks <- c(
  "required_variables_present",
  "required_numeric_types_valid",
  "fexp_no_missing",
  "fexp_finite",
  "fexp_positive"
)

stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]
stage_groups <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]

assert(nrow(stage_controls) == 5L, "P1A must catalogue five atomic checks")
assert(nrow(stage_groups) == 2L, "P1A must catalogue two audit diagnostics")
assert(setequal(stage_controls$check_id, expected_checks), "P1A check IDs differ")
assert(!anyDuplicated(stage_controls$check_id), "Duplicate P1A check IDs")

# Exercise the actual production import and validation on a synthetic SAV file.
synthetic <- data.frame(
  prov = c(1, 1, 1),
  ciudad = c(10150, 10150, 10150),
  zona = c("001", "002", "003"),
  sector = c("0001", "0002", "0003"),
  rn = c(1, 1, 1),
  area = c(1, 1, 1),
  fexp = c(1.1, 2.2, 3.3),
  ingpc = c(100, NA_real_, 200),
  pobreza = c(0, 1, 0),
  epobreza = c(0, 0, 0),
  CONDACTN = c(1, 2, 3),
  stringsAsFactors = FALSE
)

scratch <- tempfile(pattern = "p1a_smoke_")
dir.create(scratch)
input_file <- file.path(scratch, "ENEMDU_2008_12.sav")
write_input <- function(data) haven::write_sav(data, input_file)

write_input(synthetic)
metadata <- get_period_metadata(input_file)
result <- read_validate_enemdu(input_file, return_evidence = TRUE)
profile <- build_input_profile(result$data, metadata)

assert(metadata$period_code == "2008_12", "Period code was not recovered")
assert(identical(names(result$data), names(synthetic)), "Imported schema changed")
assert(nrow(result$data) == 3L, "Unexpected sample size")
assert(setequal(result$validation_evidence$check_id, expected_checks), "Evidence does not match catalogue")
assert(all(result$validation_evidence$passed), "Valid input failed a P1A control")
assert(profile$references$input_sample_n == 3L, "Sample reference mismatch")
assert(isTRUE(all.equal(profile$references$input_expanded_n, 6.6)), "Expanded reference mismatch")
assert(profile$references$input_ingpc_valid_n == 2L, "INGPC valid count mismatch")
assert(profile$references$input_ingpc_missing_n == 1L, "INGPC missing count mismatch")
assert(isTRUE(all.equal(profile$references$input_ingpc_valid_expanded, 4.4)), "INGPC valid expanded reference mismatch")
assert(isTRUE(all.equal(profile$references$input_ingpc_missing_expanded, 2.2)), "INGPC missing expanded reference mismatch")

successful <- build_stage_audit_diagnostics(
  result$validation_evidence, control_catalog, audit_diagnostic_catalog, stage_id
)
assert(successful$stage_passed, "Valid stage was not approved")
assert(nrow(successful$diagnostics) == 2L, "Expected two visible diagnostics")
assert(all(successful$diagnostics$Resultado == "OK"), "Valid diagnostics not marked OK")
cat("PASS: synthetic valid source -> five checks -> two diagnostics -> references\n")

# Critical validation errors must stop processing rather than approve the stage.
missing_var <- synthetic
missing_var$CONDACTN <- NULL
write_input(missing_var)
expect_stage_error(
  read_validate_enemdu(input_file, return_evidence = TRUE),
  check_id = "required_variables_present",
  expected_message = "Faltan variables requeridas: CONDACTN"
)

wrong_type <- synthetic
wrong_type$CONDACTN <- c("A", "B", "C")
write_input(wrong_type)
expect_stage_error(
  read_validate_enemdu(input_file, return_evidence = TRUE),
  check_id = "required_numeric_types_valid",
  expected_message = "Las siguientes variables requeridas no son numéricas: CONDACTN"
)

missing_fexp <- synthetic
missing_fexp$fexp[1] <- NA_real_
write_input(missing_fexp)
expect_stage_error(
  read_validate_enemdu(input_file, return_evidence = TRUE),
  check_id = "fexp_no_missing",
  expected_message = "El factor de expansión fexp contiene valores perdidos."
)

# Verify how the SAV round trip handles an infinite expansion factor.
nonfinite_fexp <- synthetic
nonfinite_fexp$fexp[1] <- Inf
write_input(nonfinite_fexp)

expect_stage_error(
  read_validate_enemdu(input_file, return_evidence = TRUE),
  check_id = "fexp_no_missing",
  expected_message = "El factor de expansión fexp contiene valores perdidos."
)

# Test the finiteness control directly, without SAV serialization.
assert(
  isTRUE(validate_stage_A01_fexp_finite(c(1.1, 2.2))),
  "Finite expansion factors should pass validation"
)

expect_stage_error(
  validate_stage_A01_fexp_finite(c(1.1, Inf)),
  check_id = "fexp_finite",
  expected_message = "El factor de expansión fexp contiene valores no finitos."
)

cat("PASS: fexp_finite validated directly in memory\n")

nonpositive_fexp <- synthetic
nonpositive_fexp$fexp[1] <- 0
write_input(nonpositive_fexp)
expect_stage_error(
  read_validate_enemdu(input_file, return_evidence = TRUE),
  check_id = "fexp_positive",
  expected_message = "El factor de expansión fexp contiene valores no positivos."
)
cat("PASS: five invalid sources abort during the correct stage\n")

# Verify that catalogue interpretation never turns a failed atomic check into OK.
failed_evidence <- result$validation_evidence
failed_evidence$passed[1] <- FALSE
failed <- build_stage_audit_diagnostics(
  failed_evidence, control_catalog, audit_diagnostic_catalog, stage_id
)
assert(!failed$stage_passed, "Failed atomic check produced a passing stage")
assert(any(failed$diagnostics$Resultado == "NO SUPERADO"), "Failed atomic check was hidden in a passing diagnostic")

missing_check <- result$validation_evidence[-1, , drop = FALSE]
expect_error(
  build_stage_audit_diagnostics(missing_check, control_catalog, audit_diagnostic_catalog, stage_id),
  "missing evidence for a catalogued check"
)

duplicate_check <- rbind(result$validation_evidence, result$validation_evidence[1, , drop = FALSE])
expect_error(
  build_stage_audit_diagnostics(duplicate_check, control_catalog, audit_diagnostic_catalog, stage_id),
  "duplicate evidence"
)
cat("PASS: failed check and missing/duplicate evidence handled correctly\n")

unlink(scratch, recursive = TRUE)
cat("P1A CONTRACT TESTS: ALL PASSED\n")
