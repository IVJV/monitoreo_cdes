# ==============================================================================
# ENEMDU survey design
# ==============================================================================


# ==============================================================================
# 1. Create ENEMDU survey design
# ==============================================================================

create_enemdu_survey_design <- function(ENEMDU_decile) {

  # INEC methodology: Treatment for strata with a single PSU.
  options(survey.lonely.psu = "certainty")

  # Create the complex survey design defined in the validated methodology
  d1 <- ENEMDU_decile |>
    srvyr::as_survey_design(
      ids = identif,       # PSU/UPM
      strata = dominio,    # Survey strata / study domains
      weights = fexp,      # Expansion factor
      nest = TRUE
    )

  d1
}