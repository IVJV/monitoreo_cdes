# ==============================================================================
# Stage C04 — Independent verification contract tests (synthetic input)
# Run from the project root: Rscript tests/test_pC04_contract.R
# No ENEMDU microdata, report generation or production publication is required.
# ==============================================================================

source("R/validation_errors.R")
source("R/C04_verificacion_tablas.R")
source("R/C04_publicacion_resultados.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_stage_failure <- function(data, table1, table2, check_id,
                                 require_details = TRUE) {
  outcome <- tryCatch(
    build_table_output_verification_stage(data, table1, table2),
    error = identity
  )
  assert(inherits(outcome, "rap_validation_error"),
         paste("C04 did not reject the invalid result:", check_id))
  assert(identical(outcome$stage_id, "C04_table_output_verification"),
         paste("C04 reported the wrong stage:", check_id))
  assert(identical(outcome$check_id, check_id),
         paste("C04 reported the wrong failed control:", check_id))
  if (require_details) {
    assert(is.list(outcome$details),
           paste("C04 discarded audit evidence on failure:", check_id))
    assert(isFALSE(outcome$details$verification_approved),
           paste("C04 incorrectly approved a failed result:", check_id))
    assert(outcome$details$total_discrepancies > 0L,
           paste("C04 did not preserve discrepancies:", check_id))
    assert(nrow(outcome$details$validation_evidence) == 4L,
           paste("C04 lost control results:", check_id))
  }
}

expect_publication_blocked <- function(context, output_root) {
  outcome <- tryCatch(
    publish_stage_c_results(context, output_root = output_root),
    error = identity
  )
  assert(inherits(outcome, "error"),
         "The publisher accepted C04 without valid approval")
  assert(!dir.exists(output_root),
         "The publisher created an output directory before approval")
}

# Deliberately small, independently calculable reference:
# one record for every activity code (0:9) in every decile (1:10).
# Expansion weights differ by activity code; income differs by decile and code.
# An NA activity record with a large weight must be excluded from group totals.
source_grid <- expand.grid(ningpc = 1:10, activity = 0:9)
source_data <- data.frame(
  CONDACTN = haven::labelled(
    c(as.integer(source_grid$activity), NA_integer_),
    labels = stats::setNames(as.integer(0:9), paste0("Categoria ", 0:9)),
    label = "Condicion de actividad"
  ),
  ningpc = c(as.integer(source_grid$ningpc), 1L),
  ingpc = c(10 * source_grid$ningpc + source_grid$activity, 999),
  fexp = c(source_grid$activity + 1, 10000)
)

# Methodological category definitions; expected values below use closed-form
# arithmetic on the synthetic source, not C02/C03 or C04 calculations.
activity_groups <- list(
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

all_weight <- sum(1:10)                              # 55
all_weighted_code <- sum((0:9) * (1:10))            # 330
reference_rows <- lapply(names(activity_groups), function(variable) {
  codes <- activity_groups[[variable]]
  weight <- sum(codes + 1)
  weighted_code <- sum(codes * (codes + 1))
  lapply(c(NA_integer_, 1:10), function(decile) {
    is_total <- is.na(decile)
    population <- if (is_total) 10 * weight else weight
    income_numerator <- if (is_total)
      550 * weight + 10 * weighted_code else
      10 * decile * weight + weighted_code
    income_denominator <- if (is_total)
      550 * all_weight + 10 * all_weighted_code else
      10 * decile * all_weight + all_weighted_code
    data.frame(
      Variable = variable,
      Decile = if (is_total) "Total" else paste("Decil", decile),
      Population = population,
      Percentage = 100 * weight / all_weight,
      Income_Millions = income_numerator / 1000000,
      Participation = 100 * income_numerator / income_denominator,
      stringsAsFactors = FALSE
    )
  })
})
reference <- do.call(rbind, unlist(reference_rows, recursive = FALSE))
row.names(reference) <- NULL

table1 <- reference[c("Variable", "Decile", "Population", "Percentage")]
table2 <- reference[c("Variable", "Decile", "Income_Millions", "Participation")]
source_copy <- source_data
table1_copy <- table1
table2_copy <- table2

# Valid results: all four checks, 352 individual comparisons and evidence.
result <- build_table_output_verification_stage(source_data, table1, table2)
expected_checks <- c(
  "table1_total_consistency", "table2_total_consistency",
  "table1_decile_consistency", "table2_decile_consistency"
)
assert(isTRUE(result$verification_approved), "C04 rejected valid tables")
assert(identical(result$total_discrepancies, 0L), "C04 reported false discrepancies")
assert(identical(as.integer(result$verification_summary$Verificaciones),
                 c(16L, 16L, 160L, 160L)),
       "C04 did not perform the expected 352 comparisons")
assert(all(result$verification_summary$Resultado == "OK"),
       "C04 did not approve all four checks")
assert(identical(as.character(result$validation_evidence$check_id), expected_checks) &&
         all(result$validation_evidence$passed),
       "C04 did not report four passing controls")
assert(identical(source_data, source_copy) &&
         identical(table1, table1_copy) && identical(table2, table2_copy),
       "C04 modified the source or table inputs")
cat("PASS: valid synthetic data and four C04 checks (352 comparisons)\n")

# Deliberately corrupt one result for each mandatory control.
idx_total <- which(table1$Variable == "empleo_adecuado" & table1$Decile == "Total")
idx_decile <- which(table1$Variable == "empleo_adecuado" & table1$Decile == "Decil 1")
assert(length(idx_total) == 1L && length(idx_decile) == 1L,
       "Synthetic test cell selection failed")

bad_table1 <- table1
bad_table1$Population[idx_total] <- bad_table1$Population[idx_total] + 1
expect_stage_failure(source_data, bad_table1, table2, "table1_total_consistency")

bad_table2 <- table2
bad_table2$Income_Millions[idx_total] <- bad_table2$Income_Millions[idx_total] + 0.01
expect_stage_failure(source_data, table1, bad_table2, "table2_total_consistency")

bad_table1 <- table1
bad_table1$Percentage[idx_decile] <- bad_table1$Percentage[idx_decile] + 1
expect_stage_failure(source_data, bad_table1, table2, "table1_decile_consistency")

bad_table2 <- table2
bad_table2$Participation[idx_decile] <- bad_table2$Participation[idx_decile] + 1
expect_stage_failure(source_data, table1, bad_table2, "table2_decile_consistency")

# Reject damaged numeric decile values as well as damaged percentages.
bad_table1 <- table1
bad_table1$Population[idx_decile] <- bad_table1$Population[idx_decile] + 1
expect_stage_failure(source_data, bad_table1, table2, "table1_decile_consistency")
bad_table2 <- table2
bad_table2$Income_Millions[idx_decile] <- bad_table2$Income_Millions[idx_decile] + 0.01
expect_stage_failure(source_data, table1, bad_table2, "table2_decile_consistency")

# Missing or duplicated indicator/decile entries cannot pass the contract.
expect_stage_failure(source_data, table1[-idx_decile, , drop = FALSE],
                     table2, "table_output_consistency", require_details = FALSE)
expect_stage_failure(source_data, rbind(table1, table1[idx_decile, , drop = FALSE]),
                     table2, "table_output_consistency", require_details = FALSE)
# The control catalogue must be able to render failed evidence in the RA.
source("R/control_catalog.R")
bad_table1 <- table1
bad_table1$Population[idx_total] <- bad_table1$Population[idx_total] + 1
failure <- tryCatch(
  build_table_output_verification_stage(source_data, bad_table1, table2),
  rap_validation_error = identity
)
audit <- build_stage_audit_diagnostics(
  validation_evidence = failure$details$validation_evidence,
  control_catalog = control_catalog,
  audit_diagnostic_catalog = audit_diagnostic_catalog,
  stage_id = "C04_table_output_verification"
)
assert(isFALSE(audit$stage_passed), "Failed C04 audit was incorrectly approved")
assert(sum(audit$diagnostics$Resultado == "NO SUPERADO") == 1L,
       "C04 audit diagnostics did not preserve the failed control")
cat("PASS: failed verification evidence is available to the audit report\n")

cat("PASS: altered values and incomplete/duplicated tables are rejected\n")

# The separate publication gate must not write files without C04 approval.
approved <- result
approved$stage_passed <- TRUE
context <- list(
  stage_C01 = list(period_file = "2008_12", period_title = "DICIEMBRE 2008"),
  stage_C02 = list(population_distribution_table_num = table1),
  stage_C03 = list(income_concentration_table_num = table2),
  stage_C04 = approved
)

bad_context <- context
bad_context$stage_C04$verification_approved <- FALSE
expect_publication_blocked(bad_context, tempfile("c04_rejected_"))

bad_context <- context
bad_context$stage_C04$validation_evidence$passed[1] <- FALSE
expect_publication_blocked(bad_context, tempfile("c04_rejected_"))
cat("PASS: publication is blocked without valid C04 approval\n")
# Verify XLSX package integrity on a workbook produced by openxlsx.
# The package check must repair phantom drawing relations, if emitted,
# without changing the numeric contents or the visible worksheet.
source("R/reporting_publication_format.R")
test_wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(test_wb, "Prueba")
openxlsx::writeData(test_wb, "Prueba", data.frame(
  Codigo = c("A", "B"), Valor = c(1.25, 2.50)
))
test_xlsx <- tempfile("c04_package_test_", fileext = ".xlsx")
openxlsx::saveWorkbook(test_wb, test_xlsx, overwrite = TRUE)
stage_c_xlsx_package_check(test_xlsx, repair = TRUE)
stage_c_xlsx_package_check(test_xlsx, repair = FALSE)
roundtrip <- openxlsx::read.xlsx(test_xlsx)
assert(identical(as.character(roundtrip$Codigo), c("A", "B")) &&
         isTRUE(all.equal(as.numeric(roundtrip$Valor), c(1.25, 2.50))),
       "The XLSX package repair changed the worksheet values")
unlink(test_xlsx)
cat("PASS: XLSX package has no dangling drawings and retains its values\n")

cat("PC04 CONTRACT TESTS: ALL PASSED\n")
