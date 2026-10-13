# ==============================================================================
# Reusable structured validation errors
# ==============================================================================

abort_stage_validation <- function(stage_id, check_id, message) {

  condition <- errorCondition(
    message = message,
    call = NULL,
    class = "rap_validation_error",
    stage_id = stage_id,
    check_id = check_id
  )

  stop(condition)
}