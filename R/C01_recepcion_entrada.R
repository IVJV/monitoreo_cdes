# ==============================================================================
# Stage C01 — Reception of labor-market indicators and period labels
# Methodological source: 3.1 ENEMDU_2008_12_Distribución_Concentración_ingpc.R,
# historical Point C01 (lines 9–25).
# ==============================================================================

build_input_reception_stage <- function(ENEMDU_laboral, period_metadata) {

  # The working database is the unchanged output of Stage B02.
  if (!is.data.frame(ENEMDU_laboral))
    stop("La Etapa C01 requiere la base laboral construida en B02.")

  # Period metadata are already recovered and validated in Stage A01.
  if (!is.list(period_metadata) ||
      !all(c("period_year", "period_month", "period_code") %in% names(period_metadata)))
    stop("La Etapa C01 requiere los metadatos del período de A01.")

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
    stage_id = "C01_input_reception",
    data = ENEMDU_laboral,
    period_excel = paste0(
      month_abbr_es[period_month], "-", substr(as.character(period_year), 3L, 4L)
    ),
    period_title = paste(month_names_es[period_month], period_year),
    period_file = period_metadata$period_code
  )
}
