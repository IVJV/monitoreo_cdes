# Prueba controlada de rechazo de C04 y generación de RA.
# Ejecutar desde la raíz del RAP:
#   Rscript scripts/qa/probe_c04_failure.R
# No modifica R/, reporting/, RT, output/publications/ ni output/qa/ del proyecto original.
# El RA de la prueba se examina en un directorio temporal, eliminado al finalizar.

run_probe <- function() {
  required <- c(
    "R/report_context.R",
    "R/C04_verificacion_tablas.R",
    "scripts/reporting/render_reports.R",
    "reporting/reporte_controles_internos.qmd",
    "_quarto.yml", "_quarto-audit.yml", "_quarto-technical.yml",
    "data/raw/ENEMDU_2008_12.sav"
  )
  missing <- required[!file.exists(required)]
  if (length(missing))
    stop("Faltan archivos en la raíz del RAP: ", paste(missing, collapse = ", "))

  rscript <- file.path(R.home("bin"),
    if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!file.exists(rscript)) stop("No se encontró Rscript en la instalación de R.")
  if (!nzchar(Sys.which("quarto"))) stop("No se encontró Quarto en PATH.")

  original <- getwd()
  sandbox <- tempfile("rap_c04_fallo_", tmpdir = tempdir())
  dir.create(sandbox, recursive = TRUE)
  on.exit({
    # Volver a la raíz antes de borrar la copia temporal (también en Windows).
    setwd(original)
    unlink(sandbox, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  # Clonar únicamente código, fuentes Quarto, configuraciones y datos de entrada.
  # No copiar ni modificar los resultados ya aprobados del RAP.
  for (directory in c("R", "reporting")) {
    if (!isTRUE(file.copy(directory, sandbox, recursive = TRUE)))
      stop("No se pudo copiar el directorio: ", directory)
  }
  dir.create(file.path(sandbox, "scripts"))
  if (!isTRUE(file.copy("scripts/reporting", file.path(sandbox, "scripts"),
                        recursive = TRUE)))
    stop("No se pudo copiar scripts/reporting")
  for (config in c("_quarto.yml", "_quarto-audit.yml", "_quarto-technical.yml")) {
    if (!isTRUE(file.copy(config, sandbox))) stop("No se pudo copiar ", config)
  }
  dir.create(file.path(sandbox, "data", "raw"), recursive = TRUE)
  if (!isTRUE(file.copy("data/raw/ENEMDU_2008_12.sav",
                        file.path(sandbox, "data", "raw"))))
    stop("No se pudo copiar la base ENEMDU al entorno temporal")

  # En la COPIA se altera exactamente una cifra de la Tabla 1 antes de C04.
  # No se alteran fórmulas ni el código original del verificador.
  context_path <- file.path(sandbox, "R", "report_context.R")
  code <- readLines(context_path, warn = FALSE, encoding = "UTF-8")
  marker <- "  table_verification_result <- tryCatch("
  location <- which(code == marker)
  if (length(location) != 1L)
    stop("No se encontró de forma única el punto de verificación C04; no se inyectó fallo.")
  injection <- c(
    "  # Fallo sintético exclusivamente en la copia temporal (no producción)",
    "  population_distribution_result$population_distribution_table_num$Population[1L] <-",
    "    population_distribution_result$population_distribution_table_num$Population[1L] + 1"
  )
  code <- append(code, injection, after = location - 1L)
  writeLines(code, context_path, useBytes = TRUE)

  # El proceso hijo necesita los mismos paquetes ya instalados en la sesión R.
  old_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  old_user <- Sys.getenv("R_LIBS_USER", unset = NA_character_)
  on.exit({
    if (is.na(old_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = old_libs)
    if (is.na(old_user)) Sys.unsetenv("R_LIBS_USER") else Sys.setenv(R_LIBS_USER = old_user)
  }, add = TRUE)
  Sys.setenv(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep),
             R_LIBS_USER = .libPaths()[1L])
  setwd(sandbox)

  shell <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  process_output <- suppressWarnings(system2(
    command = rscript,
    args = c(shQuote("scripts/reporting/render_reports.R", type = shell),
             shQuote("data/raw/ENEMDU_2008_12.sav", type = shell),
             "both"),
    stdout = TRUE, stderr = TRUE
  ))
  status <- attr(process_output, "status")
  if (is.null(status)) status <- 0L

  ra_path <- file.path(sandbox, "output", "reports", "audit",
                       "reporting", "reporte_controles_internos.html")
  rt_path <- file.path(sandbox, "output", "reports", "technical",
                       "reporting", "reporte_tecnico_resultados.html")
  pubs_path <- file.path(sandbox, "output", "publications")

  has_failure_text <- FALSE
  if (file.exists(ra_path)) {
    ra_html <- paste(readLines(ra_path, warn = FALSE, encoding = "UTF-8"),
                     collapse = "\n")
    has_failure_text <- grepl("NO APROBADO|NO SUPERADO", ra_html)
  }
  no_pubs <- !dir.exists(pubs_path) ||
    length(list.files(pubs_path, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE)) == 0L
  checks <- c(
    "C04 devuelve error" = status != 0L,
    "RA generado" = file.exists(ra_path),
    "RA indica rechazo" = has_failure_text,
    "RT no actualizado" = !file.exists(rt_path),
    "Publicaciones bloqueadas" = no_pubs
  )

  cat("\n=== PRUEBA DE FALLO CONTROLADO C04 ===\n")
  for (name in names(checks))
    cat(if (checks[[name]]) "PASS" else "FAIL", ": ", name, "\n", sep = "")

  if (!all(checks)) {
    cat("\nSalida del proceso de prueba (últimas líneas):\n")
    cat(tail(process_output, 25L), sep = "\n")
    stop("La prueba de fallo controlado NO fue satisfactoria.")
  }

  # El RA fue comprobado en la copia temporal; no se guarda en output/qa/.
  # La copia completa se elimina al salir de esta función.
  cat("\nPASS: todos los controles de fallo superados.\n")
  cat("El RA de prueba se comprobó y sus archivos temporales serán eliminados.\n")
  invisible(TRUE)
}

tryCatch(run_probe(), error = function(e) {
  message("\nFAIL: ", conditionMessage(e))
  quit(save = "no", status = 1L, runLast = FALSE)
})
