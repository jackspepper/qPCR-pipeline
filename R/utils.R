#' Default Limits of Detection (LOD)
#'
#' A list containing the default upper (Hi) and lower (Lo) limits of detection
#' for various qPCR targets.
#'
#' @export
DEFAULT_target_lod <- list(
  LOD_Hi = list(
    hi_fucp   = 2000,
    fucp   = 2000,
    `hi-fucp` = 2000,
    `hi fucp` = 2000,
    hi_hpd3   = 2000,
    `hi-hpd3` = 2000,
    hpd3      = 2000,
    lyta      = 2000,
    copb      = 2000,
    speb      = 2000,
    nuc       = 2000,
    gyrb      = 2000,
    uni       = 200,
    univ      = 200,
    universal = 200,
    hh_hpd3   = 2000,
    hh_hypd   = 2000
  ),
  LOD_Lo = list(
    hi_fucp   = 0.012,
    fucp   = 0.012,
    `hi-fucp` = 0.012,
    `hi fucp` = 0.012,
    hi_hpd3   = 0.012,
    `hi-hpd3` = 0.012,
    hpd3      = 0.012,
    lyta      = 0.012,
    copb      = 0.012,
    speb      = 0.012,
    nuc       = 0.012,
    gyrb      = 0.012,
    uni       = 0.0012,
    univ      = 0.0012,
    universal = 0.0012,
    hh_hpd3   = 0.012,
    hh_hypd   = 0.012
  )
)

#' Default Always-Positive Targets
#'
#' Character vector of targets that are expected to always produce a detectable result
#' (used for internal quality controls).
#'
#' @export
DEFAULT_always_positive_targets <- c("uni", "univ", "universal")

#' Create a new qPCR project template
#'
#' Copies the example runner script and folder structure to your local directory.
#'
#' @param path Target directory path where the template should be created.
#' @return Invisible TRUE on success.
#' @export
use_qpcr_template <- function(path = ".") {
  # Locate template directory in the package
  template_dir <- system.file("example_project", package = "qpcrpipeline")
  if (template_dir == "") {
    # If not built yet or running locally in development
    template_dir <- "inst/example_project"
    if (!dir.exists(template_dir)) {
      template_dir <- "templates/example_project"
    }
  }

  if (!dir.exists(template_dir)) {
    stop("Template project structure not found in package.")
  }

  # Ensure the destination path exists
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Copy files to the target path
  files_to_copy <- list.files(template_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  for (f in files_to_copy) {
    if (file.info(f)$isdir) {
      fs::dir_copy(f, file.path(path, basename(f)), overwrite = TRUE)
    } else {
      file.copy(f, file.path(path, basename(f)), overwrite = TRUE)
    }
  }

  cli::cli_alert_success("qPCR template successfully created at: {path}")
  invisible(TRUE)
}
