#' Run a Shiny Application
#'
#' \code{Shiny} runs one of the Shiny Applications that are included in the package
#'
#' @param app The name of the Shiny application to run. Currently the available Shiny apps are "LBSPR" and "Sim"
#' @references Modified from Deal Attali's code: \url{http://deanattali.com/2015/04/21/r-package-shiny-app/}
#' @importFrom utils install.packages installed.packages
#' @export
Shiny <- function(app) {
  temp <- try(class(app), silent=TRUE)
  pkgs <- c("Hmisc", "colourpicker",  "shiny", "shinyBS", "Hmisc", "xtable")
  instpkgs <- installed.packages()
  ind <- which(!pkgs %in% instpkgs[,1])
  if (length(ind) > 0) {
    install.packages(pkgs[ind])
  }
  if (class(temp) == "try-error") app <- deparse(substitute(app))
  Apps <- list.files(system.file("shiny_apps", package = "LBSPR"))
  validAppMsg <- paste0("Valid examples are:\n '", paste(Apps, collapse = "', '"), "'")
  if (missing(app) || !nzchar(app) || !app %in% Apps) {
    stop(
      'Please run `Shiny()` with a valid Shiny app',
      validAppMsg,
      call. = FALSE)
  }
  appDir <- system.file("shiny_apps", app, package = "LBSPR")
  shiny::runApp(appDir, display.mode = "normal")
}
