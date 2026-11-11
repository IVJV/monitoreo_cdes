# ==============================================================================
# Reusable stage-assurance indicator for technical reports
# ==============================================================================
# The processing layer and the control catalogue remain the sources of truth.
# This component only presents stage-level validation already available in the
# reporting context; it never runs controls or recalculates stage_passed.

# Encode a URL in a quoted HTML href attribute (shared by both directions).
escape_stage_href <- function(href) {
  safe <- gsub("&", "&amp;", href, fixed = TRUE)
  safe <- gsub('"', "&quot;", safe, fixed = TRUE)
  safe <- gsub("<", "&lt;", safe, fixed = TRUE)
  gsub(">", "&gt;", safe, fixed = TRUE)
}

render_stage_assurance <- function(stage, audit_href = NULL) {

  if (!is.list(stage))
    stop("Stage assurance requires a stage context list.")

  # A functional stage without its own controls has no assurance indicator.
  evidence <- stage$validation_evidence

  if (is.null(evidence)) {
    if (!is.null(stage$stage_passed))
      stop("Stage assurance found a status without validation evidence.")
    return("")
  }

  if (!is.data.frame(evidence))
    stop("Stage assurance requires tabular validation evidence.")

  if (nrow(evidence) == 0L) {
    if (!is.null(stage$stage_passed))
      stop("Stage assurance found a status without executed controls.")
    return("")
  }

  required_columns <- c("stage_id", "check_id", "passed")

  if (!all(required_columns %in% names(evidence)))
    stop("Stage assurance received incomplete validation evidence.")

  if (!is.character(stage$stage_id) ||
      length(stage$stage_id) != 1L ||
      is.na(stage$stage_id) ||
      !nzchar(stage$stage_id))
    stop("Stage assurance requires a valid stage_id.")

  if (anyNA(evidence$stage_id) ||
      !all(evidence$stage_id == stage$stage_id))
    stop("Stage assurance evidence does not match its stage_id.")

  if (!is.logical(stage$stage_passed) ||
      length(stage$stage_passed) != 1L ||
      is.na(stage$stage_passed))
    stop("Stage assurance requires a non-missing logical stage_passed.")

  if (!is.null(audit_href) &&
      (!is.character(audit_href) || length(audit_href) != 1L ||
       is.na(audit_href) || !nzchar(audit_href)))
    stop("Stage assurance requires a valid audit link or NULL.")

  passed <- isTRUE(stage$stage_passed)
  state_class <- if (passed) "passed" else "failed"
  state_label <- if (passed) "Controles superados" else "Controles no superados"
  link_label <- if (passed) "Ver controles superados" else "Ver controles no superados"

  # The icon displays the stage status; the optional link opens its audit evidence.
  # Accessible labels retain both meanings without repeating text in the heading.
  state_symbol <- if (passed) {
    '<path d="M8 12.2l2.7 2.7L16.4 9"/>'
  } else {
    '<path d="M8.5 8.5l7 7M15.5 8.5l-7 7"/>'
  }

  icon_svg <- paste0(
    '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false" ',
    'fill="none" stroke="currentColor" stroke-width="1.8" ',
    'stroke-linecap="round" stroke-linejoin="round">',
    '<circle cx="12" cy="12" r="9"/>', state_symbol, '</svg>'
  )

  if (!is.null(audit_href)) {
    safe_href <- escape_stage_href(audit_href)

    # Match the RA return icon: the nonbreaking space keeps the link attached
    # to the last title word, even when the heading wraps over multiple lines.
    return(paste0(
      '&nbsp;<a class="rap-stage-link" href="', safe_href,
      '" aria-label="', link_label, ' de la etapa"',
      ' data-tooltip="', link_label, '">',
      '<span class="rap-stage-assurance ', state_class, '" aria-hidden="true">',
      icon_svg, '</span></a>'
    ))
  }

  paste0(
    '&nbsp;<span class="rap-stage-assurance ', state_class,
    '" role="img" aria-label="', state_label,
    '" data-tooltip="', state_label, '">', icon_svg, '</span>'
  )
}


# Navigation from an audit stage to its technical results. No control state is
# inferred from this link; it remains strictly a presentation component.
render_stage_return_link <- function(technical_href) {

  if (!is.character(technical_href) ||
      length(technical_href) != 1L ||
      is.na(technical_href) ||
      !nzchar(trimws(technical_href)))
    stop("Stage return link requires a non-empty technical report URL.")

  safe_href <- escape_stage_href(technical_href)

  # A nonbreaking space keeps the icon attached to the final title word.
  # The heading's inline R expression has no preceding ordinary space.
  paste0(
    '&nbsp;<a class="rap-stage-return-link" href="', safe_href,
    '" aria-label="Ver resultados técnicos" data-tooltip="Ver resultados técnicos">',
    '<svg viewBox="0 0 24 24" aria-hidden="true" focusable="false" ',
    'fill="none" stroke="currentColor" stroke-width="1.8" ',
    'stroke-linecap="round" stroke-linejoin="round">',
    '<path d="M15 3h6v6"/>',
    '<path d="M10 14 21 3"/>',
    '<path d="M21 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h6"/>',
    '</svg></a>'
  )
}
