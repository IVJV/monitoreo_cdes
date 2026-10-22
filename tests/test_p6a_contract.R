# ==============================================================================
# Stage A06 contract tests — survey-adjusted INGPC means by decile
# Run from the repository root: Rscript tests/test_p6a_contract.R
# Synthetic data only. No ENEMDU microdata are read or modified.
# ==============================================================================

source("R/A05_diseno_muestral_enemdu.R")
source("R/A06_media_ingpc_por_decil_ajustada_por_diseno_muestral.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

expect_error <- function(expr, message) {
  result <- tryCatch({ force(expr); NULL }, error = function(e) e)
  assert(inherits(result, "error"), message)
}

# Five nested PSUs across three strata; the second stratum has one PSU.
synthetic_base <- data.frame(
  identif = rep(c("100", "200", "100", "100", "200"), each = 20),
  dominio = rep(c("01", "01", "02", "03", "03"), each = 20),
  fexp = rep(1:4, 25),
  ingpc = rep(seq(10, 200, by = 10), 5) + rep(0:4, each = 20),
  ningpc = rep(rep(1:10, each = 2), 5),
  stringsAsFactors = FALSE
)
input_copy <- synthetic_base

d1 <- create_enemdu_survey_design(synthetic_base)

# Independent exact, unrounded point-A04 reference on the synthetic sample.
reference <- do.call(rbind, lapply(
  split(synthetic_base, synthetic_base$ningpc),
  function(df) data.frame(
    Decile = unique(df$ningpc),
    Mean = stats::weighted.mean(df$ingpc, df$fexp)
  )
))
rownames(reference) <- NULL

result <- build_ingpc_survey_mean_stage(d1, reference)
num <- result$svy_ingpc_by_decile_table_num
comparison <- result$mean_validation

assert(identical(synthetic_base, input_copy), "A06 modified the input data")
assert(nrow(num) == 10L, "A06 did not estimate ten decile means")
assert(identical(names(num), c(
  "Decile", "Mean", "Linearized_Std_Err", "CI_95_Lower", "CI_95_Upper"
)), "A06 numeric output columns differ from the original methodology")
assert(isTRUE(all.equal(num$Mean, reference$Mean, tolerance = 1e-8)),
       "A06 survey means differ from independently weighted means")
assert(all(is.finite(num$Mean)), "A06 produced non-finite point estimates")
assert(all(comparison$Difference <= 1e-8), "A06 reference comparison failed")
assert(identical(names(comparison), c("Decile", "Mean", "Mean_reference", "Difference")),
       "A06 comparison columns differ from the original methodology")
assert(identical(as.character(result$validation_evidence$check_id),
                 "survey_mean_consistency"), "A06 did not retain the original control")
assert(isTRUE(result$validation_evidence$passed), "A06 did not validate the original control")
assert(identical(result$svy_ingpc_by_decile_table$Mean,
                 sprintf("%.6f", num$Mean)),
       "A06 did not retain the original six-decimal presentation")

# Reachable failures in the original methodological consistency check.
bad_reference <- reference
bad_reference$Mean[1] <- bad_reference$Mean[1] + 0.001
expect_error(validate_ingpc_decile_means(num, bad_reference),
             "A06 accepted an inconsistent Stage A04 reference")

missing_reference <- reference
missing_reference$Mean[1] <- NA_real_
expect_error(validate_ingpc_decile_means(num, missing_reference),
             "A06 accepted a missing Stage A04 reference")

missing_estimate <- num
missing_estimate$Mean[1] <- NA_real_
expect_error(validate_ingpc_decile_means(missing_estimate, reference),
             "A06 accepted a missing survey mean")

nonfinite_estimate <- num
nonfinite_estimate$Mean[1] <- Inf
expect_error(validate_ingpc_decile_means(nonfinite_estimate, reference),
             "A06 accepted a non-finite survey mean")

cat("P6A CONTRACT TESTS: ALL PASSED\n")
