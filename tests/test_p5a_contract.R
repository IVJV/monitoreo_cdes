# ==============================================================================
# Stage 05A contract test — ENEMDU complex survey design
# Run from the repository root: Rscript tests/test_p5a_contract.R
# Synthetic data only; no ENEMDU microdata are used.
# ==============================================================================

source("R/05A_diseno_muestral_enemdu.R")

assert <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

# Source methodology: 1.2 ENEMDU_2008_12_Deciles.R, section 5A.
# PSU codes repeat across strata to test nest = TRUE; domain 02 has a lonely PSU.
synthetic_base <- data.frame(
  identif = rep(c("100", "200", "100", "100", "200"), each = 4),
  dominio = rep(c("01", "01", "02", "03", "03"), each = 4),
  fexp = rep(c(1, 2, 3, 4), 5),
  ingpc = seq(10, 200, by = 10),
  ningpc = rep(1:10, each = 2),
  stringsAsFactors = FALSE
)
input_copy <- synthetic_base

d1 <- create_enemdu_survey_design(synthetic_base)

assert(inherits(d1, "tbl_svy") && inherits(d1, "survey.design"),
       "05A did not return a srvyr survey design")
assert(identical(synthetic_base, input_copy),
       "05A modified the input data")
assert(nrow(d1$variables) == nrow(synthetic_base),
       "05A changed the number of observations")
assert(length(unique(d1$strata[[1]])) == 3L,
       "05A did not preserve the study-domain strata")
assert(length(unique(d1$cluster[[1]])) == 5L,
       "05A did not nest repeated PSU codes within strata")
assert(identical(getOption("survey.lonely.psu"), "certainty"),
       "05A changed the lonely-PSU treatment")
assert(isTRUE(all.equal(as.numeric(stats::weights(d1)), synthetic_base$fexp)),
       "05A did not preserve fexp as sampling weights")

observed_mean <- as.numeric(survey::svymean(~ingpc, d1))
expected_mean <- stats::weighted.mean(synthetic_base$ingpc, synthetic_base$fexp)
assert(isTRUE(all.equal(observed_mean, expected_mean, tolerance = 1e-8)),
       "05A weighted mean differs from the input-weighted mean")

cat("P5A CONTRACT TESTS: ALL PASSED\n")
