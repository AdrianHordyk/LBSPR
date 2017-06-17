#' Optimisation Routine for fitting LBSPR
#'
#' A function that calculate the negative log-likelihood of the LBSPR model
#'
#' @param trypars a vector of exploitation parameters in log space
#' @param yr index of the year column to fit the model to
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the length data
#' @param Control a list of control options for the LBSPR model.
#' @param pen apply a penalty if estimate of selectivity is very high?
#' @details The Control options are:
#' \describe{
#'  \item{\code{modtype}}{Model Type: either Growth-Type-Group Model (default: "GTG") or Age-Structured ("absel")}
#'  \item{\code{maxsd}}{Maximum number of standard deviations for length-at-age distribution (default is 2)}
#'  \item{\code{ngtg}}{Number of groups for the GTG model. Default is 13}
#'  \item{\code{P}}{Proportion of survival of initial cohort for maximum age for Age-Structured model. Default is 0.01}
#'  \item{\code{Nage}}{Number of pseudo-age classes in the Age Structured model. Default is 101}
#'  \item{\code{maxFM}}{Maximum value for F/M. Estimated values higher than this are trunctated to \code{maxFM}. Default is 4}
#' }
#' @return a NLL value
#' @author A. Hordyk
#'
#' @export
LBSPRopt <- function(trypars, yr=1, LB_pars=NULL, LB_lengths=NULL,  Control=list(), pen=TRUE) {
  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')", call. = FALSE)
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')", call. = FALSE)

  LB_pars@SL50 <- exp(trypars)[1] * LB_pars@Linf
  LB_pars@SL95 <- LB_pars@SL50 + (exp(trypars)[2]* LB_pars@Linf)
  LB_pars@FM <- exp(trypars[3])

  runMod <- LBSPRsim_(LB_pars, Control=Control, verbose=FALSE, doCheck=FALSE)

  ldat <- LB_lengths@LData[,yr] + 1E-15 # add tiny constant for zero catches
  LenProb <- ldat/sum(ldat)
  # print(runMod@pLCatch)

  predProb <- runMod@pLCatch + 1E-15 # add tiny constant for zero catches
  NLL <- -sum(ldat * log(predProb/LenProb))

  # add penalty for SL50
  trySL50 <- exp(trypars[1])
  PenVal <- NLL
  Pen <- dbeta(trySL50, shape1=5, shape2=0.01) * PenVal
  if(!is.finite(NLL)) return(1E9 + runif(1, 1E4, 1E5))
  if (Pen == 0) Pen <- PenVal * trySL50
  if (!pen) Pen <- 0

  NLL <- NLL+Pen

  NLL
}
