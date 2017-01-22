#' Calculate F/M given SPR and other parameters
#'
#' A internal function that optimizes for F/M when SPR is provided in the simulation parameters.
#'
#' @param FM a F/M value
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param Control a list of control options for the LBSPR model.
#'
#' @details The Control options are:
#' \describe{
#'  \item{\code{modtype}}{Model Type: either Growth-Type-Group Model (default: "GTG") or Age-Structured ("absel")}
#'  \item{\code{maxsd}}{Maximum number of standard deviations for length-at-age distribution (default is 2)}
#'  \item{\code{ngtg}}{Number of groups for the GTG model. Default is 13}
#'  \item{\code{P}}{Proportion of survival of initial cohort for maximum age for Age-Structured model. Default is 0.01}
#'  \item{\code{Nage}}{Number of pseudo-age classes in the Age Structured model. Default is 101}
#'  \item{\code{maxFM}}{Maximum value for F/M. Estimated values higher than this are trunctated to \code{maxFM}. Default is 4}
#' }
#' @return sum of squares value
#' @author A. Hordyk
#'
#' @export
getFMfun <- function(FM, LB_pars, Control=list()) {
  LB_pars@FM <- FM
  (LB_pars@SPR - LBSPRsim_(LB_pars, Control=Control, verbose=FALSE)@SPR)^2
}
