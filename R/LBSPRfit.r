#' Fit LBSPR model to length data
#'
#' A function that fits the LBSPR model to length data
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the length data
#' @param yrs index of years to include. If NA the model is run on all years
#' @param Control a list of control options for the LBSPR model.
#' @param pen apply a penalty if estimate of selectivity is very high?
#' @param verbose display messages?
#' @param useCPP use cpp optimization code?
#' @param ... additional parameters to pass to \code{FilterSmooth}
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
#' @examples
#' \dontrun{
#' MyFit <- LBSPRfit(LBparameters, LBlengths)
#' MyFit@Ests
#' }
#'
#' @importFrom utils flush.console
#' @importFrom methods new slot slot<- slotNames validObject
#' @export
LBSPRfit <- function(LB_pars=NULL, LB_lengths=NULL, yrs=NA, Control=list(), pen=TRUE, verbose=TRUE, useCPP=TRUE, ...) {

  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')", call. = FALSE)
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')", call. = FALSE)

  # Error Checks
  # if (length(LB_pars@Species) < 1) {
    # if (verbose) message("No species name provided - using a default")
	# LB_pars@Species <- "My_Species"
  # }
  if (length(LB_pars@SL50) == 0) LB_pars@SL50 <- 1
  if (length(LB_pars@SL95) == 0) LB_pars@SL95 <- 2
  if (length(LB_pars@FM) == 0) LB_pars@FM <- 1

  check_LB_pars(LB_pars)
  validObject(LB_pars)

  if (class(LB_lengths@Years) != "numeric" & class(LB_lengths@Years) != "integer") {
    warning("Years must be numeric values", call. = FALSE)
	message("Attempting to convert to numeric values")
	options(warn=-1)
	LB_lengths@Years <-  gsub("X", "", LB_lengths@Years)
	LB_lengths@Years <- as.numeric(LB_lengths@Years)
	options(warn=1)
    if (all(is.na(LB_lengths@Years))) LB_lengths@Years <- 1:length(LB_lengths@Years)
  }


  if (all(is.na(yrs))) { # run model on all years
    nyrs <- ncol(LB_lengths@LData)
    yearNames <- LB_lengths@Years
    if (is.null(yearNames)) yearNames <- 1:nyrs
	cols <- 1:nyrs
  } else { # run model on some years
    if (class(yrs) != "numeric" & class(yrs) != "integer")
	  stop("yrs must numeric value indicating column(s), or NA for all", call. = FALSE)
    nyrs <- length(yrs)
    yearNames <- LB_lengths@Years[yrs]
    if (is.null(yearNames)) yearNames <- yrs
	cols <- yrs
  }
  if (verbose) message("Fitting model")
  if (verbose) message("Year:")
  flush.console()
  runMods <- sapply(cols, function (X)
	LBSPRfit_(yr=X, LB_pars=LB_pars, LB_lengths=LB_lengths, Control=Control,
	  pen=pen, useCPP=useCPP, verbose=verbose))

  LBobj <- new("LB_obj")
  Slots <- slotNames(LB_pars)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
  Slots <- slotNames(LB_lengths)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_lengths, Slots[X])

  LBobj@NYears <- nyrs
  LBobj@Years <- yearNames
  LBobj@LData <- LB_lengths@LData[,cols, drop=FALSE]
  LBobj@NLL <- unlist(lapply(runMods, slot, "NLL"))
  LBobj@SL50 <- round(unlist(lapply(runMods, slot, "SL50")),2)
  LBobj@SL95 <- round(unlist(lapply(runMods, slot, "SL95")),2)
  LBobj@FM <- round(unlist(lapply(runMods, slot, "FM")),2)
  LBobj@SPR <- unlist(lapply(runMods, slot, "SPR"))
  LBobj@Yield <- round(unlist(lapply(runMods, slot, "Yield")),2)
  LBobj@YPR <- round(unlist(lapply(runMods, slot, "YPR")),2)
  LBobj@fitLog <- unlist(lapply(runMods, slot, "fitLog"))
  LBobj@Vars <-  matrix(unlist(lapply(runMods, slot, "Vars")), ncol=4, byrow=TRUE)
  colnames(LBobj@Vars) <- c("SL50", "SL95", "FM", "SPR")
  LBobj@pLCatch <- do.call(cbind, lapply(runMods, slot, "pLCatch"))
  LBobj@maxFM <- unlist(lapply(runMods, slot, "maxFM"))[1]

  DF <- data.frame(SL50=LBobj@SL50, SL95=LBobj@SL95, FM=LBobj@FM, SPR=LBobj@SPR)
  if (nrow(DF) == 1) LBobj@Ests <- as.matrix(DF)
  if (nrow(DF) > 1) LBobj@Ests <- apply(DF, 2, FilterSmooth, ...)
  LBobj@Ests <- round(LBobj@Ests, 2)

  LBobj
}
