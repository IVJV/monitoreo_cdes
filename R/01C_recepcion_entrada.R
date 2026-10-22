# ==============================================================================
# Stage 01C — Reception of labor-market indicators and period labels
# Methodological source: 3.1 ENEMDU_2008_12_Distribución_Concentración_ingpc.R,
# historical Point 1C (lines 9–25).
# ==============================================================================

build_input_reception_stage <- function(ENEMDU_laboral, period_metadata) {

  # The working database is the unchanged output of Stage 02B.
  if (!is.data.frame(ENEMDU_laboral))
    stop("La Etapa 01C requiere la base laboral construida en 02B.")

  # Period metadata are already recovered and validated in Stage 01A.
  if (!is.list(period_metadata) ||
      !all(c("period_year", "period_month", "period_code") %in% names(period_metadata)))
    stop("La Etapa 01C requiere los metadatos del período de 01A.")

  month_abbr_es <- c(
    "Ene", "Feb", "Mar", "Abr", "May", "Jun",
    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"
  )
  month_names_es <- c(
    "ENERO", "FEBRERO", "MARZO", "ABRIL", "MAYO", "JUNIO",
    "JULIO", "AGOSTO", "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
  )

  period_month <- period_metadata$period_month
  period_year <- period_metadata$period_year

  list(
    stage_id = "01c_input_reception",
    data = ENEMDU_laboral,
    period_excel = paste0(
      month_abbr_es[period_month], "-", substr(as.character(period_year), 3L, 4L)
    ),
    period_title = paste(month_names_es[period_month], period_year),
    period_file = period_metadata$period_code
  )
}
