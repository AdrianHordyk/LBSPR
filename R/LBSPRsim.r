#' LBSPR Simulation Model
#'
#' Function that generates the expected equilbrium size composition given biological parameters, and fishing mortality and selectivity pattern.
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param Control a list of control options for the LBSPR model.
#' @param verbose display messages?
#' @details The Control options are:
#' \describe{
#'  \item{\code{modtype}}{Model Type: either Growth-Type-Group Model (default: "GTG") or Age-Structured ("absel")}
#'  \item{\code{maxsd}}{Maximum number of standard deviations for length-at-age distribution (default is 2)}
#'  \item{\code{ngtg}}{Number of groups for the GTG model. Default is 13}
#'  \item{\code{P}}{Proportion of survival of initial cohort for maximum age for Age-Structured model. Default is 0.01}
#'  \item{\code{Nage}}{Number of pseudo-age classes in the Age Structured model. Default is 101}
#'  \item{\code{maxFM}}{Maximum value for F/M. Estimated values higher than this are trunctated to \code{maxFM}. Default is 4}
#' }
#' @return a object of class \code{'LB_obj'}
#' @author A. Hordyk
#' @useDynLib LBSPR
#' @importFrom Rcpp evalCpp sourceCpp
#'
#' @export
LBSPRsim <- function(LB_pars=NULL, Control=list(), verbose=TRUE) {
  # if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")
  if (length(LB_pars@SPR)>0) {
    if (LB_pars@SPR > 1 | LB_pars@SPR < 0) stop("SPR must be between 0 and 1", call. = FALSE)
    if (length(LB_pars@FM) >0) message("Both SPR and F/M have been specified. Using SPR and ignoring F/M")
	opt <- optimise(getFMfun, interval=c(0.001, 7), LB_pars, Control=Control)
	LB_pars@FM <- opt$minimum
	temp <- LBSPRsim_(LB_pars, Control=Control, verbose=verbose)
	temp@SPR <- round(temp@SPR,2)
	temp@FM <- round(temp@FM, 2)
	if (temp@SPR != round(LB_pars@SPR,2)) {
	  warning("Not possible to reach specified SPR. SPR may be too low for current selectivity pattern", call. = FALSE)
	  message("SPR is ", temp@SPR, " instead of ", LB_pars@SPR)
	}
	return(temp)
  } else {
    return(LBSPRsim_(LB_pars, Control=Control, verbose=verbose))
  }
}
