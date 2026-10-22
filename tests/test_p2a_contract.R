
# ==============================================================================
# Stage A02 contract tests
# Run from the repository root:
#   Rscript tests/test_p2a_contract.R
# Synthetic data only. No ENEMDU microdata are read or modified.
# ==============================================================================

source("R/validation_errors.R")
source("R/A02_identificadores_enemdu.R")
source("R/control_catalog.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
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
    identical(result$stage_id, "A02_identifiers"),
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

# ==============================================================================
# 1. Verify the canonical catalogue
# ==============================================================================

stage_id <- "A02_identifiers"

expected_checks <- c(
  "province_no_missing",
  "province_codes_valid",
  "province_matches_original",
  "natural_region_no_missing",
  "natural_region_codes_valid",
  "natural_region_matches_original",
  "psu_structure_valid",
  "study_domain_assigned",
  "study_domain_codes_valid",
  "study_domain_area_consistent"
)

stage_controls <- control_catalog[
  control_catalog$stage_id == stage_id, , drop = FALSE
]

stage_groups <- audit_diagnostic_catalog[
  audit_diagnostic_catalog$stage_id == stage_id, , drop = FALSE
]

assert(nrow(stage_controls) == 10L, "A02 must catalogue ten checks")
assert(nrow(stage_groups) == 4L, "A02 must catalogue four diagnostics")
assert(setequal(stage_controls$check_id, expected_checks), "A02 check IDs differ")
assert(!anyDuplicated(stage_controls$check_id), "Duplicate A02 check IDs")

cat("PASS: ten canonical checks and four audit diagnostics\n")

# ==============================================================================
# 2. Exercise the complete production contract with synthetic data
# ==============================================================================

synthetic <- data.frame(
  prov = c(17L, 9L, 1L, 7L, 18L, 11L, 13L, 14L, 11L, 13L, 14L),
  ciudad = c(
    170150L, 90150L, 10150L, 70150L, 180150L,
    110150L, 130150L, 140150L,
    110150L, 130150L, 140150L
  ),
  zona = rep("01", 11),
  sector = sprintf("%04d", 1:11),
  rn = c(1L, 2L, 1L, 2L, 1L, 1L, 2L, 3L, 1L, 2L, 3L),
  area = c(rep(1L, 8), rep(2L, 3)),
  fexp = seq_len(11) + 0.5,
  stringsAsFactors = FALSE
)

result <- create_identifiers(
  synthetic,
  return_evidence = TRUE
)

assert(
  identical(as.integer(result$data$provin), synthetic$prov),
  "Province reconstruction differs"
)

assert(
  identical(as.integer(result$data$rnatura), synthetic$rn),
  "Natural-region reconstruction differs"
)

assert(
  all(grepl("^[0-9]{12}$", result$data$identif)),
  "Invalid PSU structure"
)

assert(
  identical(result$data$dominio, sprintf("%02d", 1:11)),
  "Study-domain assignment differs"
)

assert(
  nrow(result$validation_evidence) == 10L,
  "Expected ten executed checks"
)

assert(
  setequal(result$validation_evidence$check_id, expected_checks),
  "Execution evidence differs from catalogue"
)

assert(
  all(result$validation_evidence$stage_id == stage_id),
  "Unexpected evidence stage_id"
)

assert(
  all(result$validation_evidence$passed),
  "Valid synthetic input failed a control"
)

# Verify that the default function contract still returns a data frame.
default_result <- create_identifiers(synthetic)

assert(
  is.data.frame(default_result),
  "Default contract must return a data frame"
)

assert(
  identical(default_result$dominio, result$data$dominio),
  "Default and evidence-returning contracts differ"
)

profile <- build_identifiers_profile(result$data)

assert(
  nrow(profile$identifier_summary) == 4L,
  "Identifier profile must contain four variables"
)

assert(
  nrow(profile$domain_distribution) == 11L,
  "Expected eleven domain-area combinations"
)

assert(
  isTRUE(all.equal(
    sum(profile$domain_distribution$Expanded_N),
    sum(synthetic$fexp)
  )),
  "Expanded population differs"
)

cat("PASS: four transformations, ten checks and diagnostic profile\n")

# ==============================================================================
# 3. Verify audit interpretation
# ==============================================================================

audit <- build_stage_audit_diagnostics(
  result$validation_evidence,
  control_catalog,
  audit_diagnostic_catalog,
  stage_id
)

assert(audit$stage_passed, "Valid A02 stage was not approved")
assert(nrow(audit$diagnostics) == 4L, "Expected four diagnostics")
assert(all(audit$diagnostics$Resultado == "OK"), "Unexpected audit status")

cat("PASS: ten checks produce four passing audit diagnostics\n")

# ==============================================================================
# 4. Verify reachable structured validation errors
# ==============================================================================

expect_stage_error(
  create_province(data.frame(ciudad = NA_integer_, prov = 1L)),
  "province_no_missing",
  "La reconstrucción de provincia a partir de ciudad produjo valores perdidos."
)

expect_stage_error(
  create_province(data.frame(ciudad = 230150L, prov = 23L)),
  "province_codes_valid",
  "1 registros presentan códigos de provincia reconstruidos no válidos."
)

expect_stage_error(
  create_province(data.frame(ciudad = 10150L, prov = 2L)),
  "province_matches_original",
  "1 registros difieren entre la variable original prov y la variable reconstruida provin."
)

expect_stage_error(
  create_natural_region(data.frame(provin = NA_integer_, rn = 1L)),
  "natural_region_no_missing",
  "La reconstrucción de región natural a partir de provin produjo valores perdidos."
)

expect_stage_error(
  create_natural_region(data.frame(provin = 1L, rn = 2L)),
  "natural_region_matches_original",
  "1 registros difieren entre la variable original rn y la variable reconstruida rnatura."
)

expect_stage_error(
  create_psu(data.frame(
    provin = 1L,
    ciudad = 10150L,
    zona = "01",
    sector = "00001"
  )),
  "psu_structure_valid",
  "1 registros presentan un identificador PSU/UPM no válido."
)

expect_stage_error(
  create_study_domain(data.frame(
    ciudad = 200150L,
    rnatura = 4L,
    area = 1L
  )),
  "study_domain_assigned",
  "1 registros no fueron asignados a un dominio de estudio."
)

cat("PASS: seven reachable structured validation failures\n")

# ==============================================================================
# 5. Verify fail-fast behavior for the complete stage
# ==============================================================================

invalid_input <- synthetic
invalid_input$prov[1] <- 2L

expect_stage_error(
  create_identifiers(invalid_input, return_evidence = TRUE),
  "province_matches_original",
  "1 registros difieren entre la variable original prov y la variable reconstruida provin."
)

cat("PASS: invalid input interrupts the complete A02 stage\n")
cat("P2A CONTRACT TESTS: ALL PASSED\n")
