#' Internal function to fit LBSPR model to length data
#'
#' An internal function that fits the LBSPR model to a single year of length data
#'
#' @param yr index of the year column to fit model to
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the length data
#' @param Control a list of control options for the LBSPR model.
#' @param pen apply a penalty if estimate of selectivity is very high?
#' @param useCPP use cpp optimization code?
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
#'
#' @importFrom stats dbeta dnorm median nlminb optimise pnorm optim runif
#' @export
LBSPRfit_ <- function(yr=1, LB_pars=NULL, LB_lengths=NULL, Control=list(),
  pen=TRUE, useCPP=TRUE, verbose=TRUE) {
  if (verbose) message(yr)
  flush.console()

  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')", call. = FALSE)
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')", call. = FALSE)
  if (yr > LB_lengths@NYears) stop("yr greater than LBSPR_obj@NYears", call. = FALSE)

  SingYear <- LB_lengths
  SingYear@LData <- as.matrix(SingYear@LData[,yr])
  SingYear@Years <- LB_lengths@Years[yr]
  SingYear@NYears <- 1
  ldat <- SingYear@LData
  LMids <- SingYear@LMids

  LB_pars@BinWidth <- LMids[2] - LMids[1]
  LB_pars@BinMin <- LMids[1] - 0.5 * LB_pars@BinWidth
  LB_pars@BinMax <- LMids[length(LMids)] + 0.5 * LB_pars@BinWidth

  # Control Parameters
  con <- list(maxsd=2, modtype=c("GTG","absel"), ngtg=13, P=0.01, Nage=101,
    maxFM=4, method="BFGS")
  nmsC <- names(con)
  con[(namc <- names(Control))] <- Control
  if (length(noNms <- namc[!namc %in% nmsC])) {
    warning("unknown names in Control: ", paste(noNms, collapse = ", "), call. = FALSE)
	cat("Options are: ", paste(names(con), collapse = ", "), "\n")
  }
  maxsd <- con$maxsd # maximum number of standard deviations from the mean for length-at-age distributions
  if (maxsd < 1) warning("maximum standard deviation is too small. See the help documentation", call. = FALSE)
  modType <- match.arg(arg=con$modtype, choices=c("GTG", "absel"))
  ngtg <- con$ngtg
  # Starts
  sSL50 <- LMids[which.max(ldat)]/LB_pars@Linf # Starting guesses
  sDel <- 0.2 * LMids[which.max(ldat)]/LB_pars@Linf
  sFM <- 0.5
  Start <- log(c(sSL50, sDel, sFM))

  if (useCPP & modType=="GTG") { # use cpp code
    By <- SingYear@LMids[2] - SingYear@LMids[1]
	LMids <- SingYear@LMids
    LBins <- seq(from=LMids[1]-0.5*By, by=By, length.out=length(SingYear@LMids)+1)
	LDat <- SingYear@LData
	if (LBins[1] !=0 & (LBins[1] -By) > 0) {
	  fstBins <- seq(from=0, by=By, to=LBins[1]-By)
	  fstMids <- seq(from=0.5*By, by=By, to=LMids[1]-By)
	  ZeroDat <- rep(0, length(fstMids))
	  LMids <- c(fstMids, LMids)
	  LBins <- c(fstBins, LBins)
	  LDat <- c(ZeroDat, LDat)
	}
  SDLinf <- LB_pars@CVLinf * LB_pars@Linf
	gtgLinfs <- seq(from= LB_pars@Linf-maxsd*SDLinf, to= LB_pars@Linf+maxsd*SDLinf, length=ngtg)
	MKMat <- matrix(LB_pars@MK, nrow=length(LBins), ncol=ngtg)
	recP <- dnorm(gtgLinfs, LB_pars@Linf, sd=SDLinf) / sum(dnorm(gtgLinfs, LB_pars@Linf, sd=SDLinf))
    usePen <- 1
	if (!pen) usePen <- 0
	opt <- try(optim(Start, LBSPR_NLLgtg, LMids=LMids, LBins=LBins, LDat=LDat,
	  gtgLinfs=gtgLinfs, MKMat=MKMat,  MK=LB_pars@MK, Linf=LB_pars@Linf,
	  ngtg=ngtg, recP=recP,usePen=usePen, hessian=TRUE, method=Control$method),
	  silent=TRUE)
	varcov <- try(solve(opt$hessian), silent=TRUE)
	if (class(varcov) == "try-error") class(opt) <- "try-error"
	if (class(varcov) != "try-error" && any(diag(varcov) < 0)) class(opt) <- "try-error"
	count <- 0
	countmax <- 10
	quants <- seq(from=0, to=0.95, length.out=countmax)
	while (class(opt) == "try-error" & count < countmax) { # optim crashed - try different starts
      count <- count + 1
	  sSL50 <- quantile(c(LMids[min(which(ldat>0))]/LB_pars@Linf,
	    LMids[which.max(ldat)]/LB_pars@Linf), probs=quants)[count]
	  sSL50 <- as.numeric(sSL50)
	  Start <- log(c(sSL50, sDel, sFM))
      opt <- try(optim(Start, LBSPR_NLLgtg, LMids=LMids, LBins=LBins, LDat=LDat,
	    gtgLinfs=gtgLinfs, MKMat=MKMat,  MK=LB_pars@MK, Linf=LB_pars@Linf,
	    ngtg=ngtg, recP=recP,usePen=usePen, hessian=TRUE, method=Control$method),
		silent=TRUE)
	  varcov <- try(solve(opt$hessian), silent=TRUE)
	  if (class(varcov) == "try-error") class(opt) <- "try-error"
	  if (class(varcov) != "try-error" && any(diag(varcov) < 0))
	    class(opt) <- "try-error"
	}

	if (class(opt) == "try-error") { # optim crashed - try without hessian
      opt <- try(optim(Start, LBSPR_NLLgtg, LMids=LMids, LBins=LBins, LDat=LDat,
	    gtgLinfs=gtgLinfs, MKMat=MKMat,  MK=LB_pars@MK, Linf=LB_pars@Linf,
	    ngtg=ngtg, recP=recP,usePen=usePen, hessian=FALSE, method=Control$method))
	  varcov <- matrix(NA, 3,3)
	}
	NLL <- opt$value
  } else {
    # opt <- nlminb(Start, LBSPRopt, LB_pars=LB_pars, LB_lengths=SingYear,
	# Control=Control, pen=pen, control=list(iter.max=300, eval.max=400,
	# abs.tol=1E-20))
	# NLL <- opt$objective
	opt <- optim(Start, LBSPRopt, LB_pars=LB_pars, LB_lengths=SingYear,
	Control=Control, pen=pen, hessian=TRUE, method=Control$method)
	varcov <- solve(opt$hessian)
	NLL <- opt$value
  }
  LB_pars@SL50 <- exp(opt$par)[1] * LB_pars@Linf
  LB_pars@SL95 <- LB_pars@SL50 + (exp(opt$par)[2] * LB_pars@Linf)
  LB_pars@FM <- exp(opt$par)[3]

  # Estimate variance of derived parameters using delta method
  MLEs <- opt$par
  vSL50 <- (exp(opt$par[1]) * LB_pars@Linf)^2 * varcov[1,1]
  vSL95 <- (LB_pars@Linf * exp(MLEs[2]))^2 * varcov[2,2] +
             (LB_pars@Linf * exp(MLEs[1]))^2 * varcov[1,1] +
              LB_pars@Linf * exp(MLEs[2]) * LB_pars@Linf * exp(MLEs[1]) *
			  varcov[1,2]
  vFM <- exp(opt$par[3])^2 * varcov[3,3]
  vSPR <- varSPR(opt$par, varcov, LB_pars)
  elog <- 0
  # Error Logs
  if (all(is.na(varcov)) | any(diag(varcov) < 0)) {
    warning("The final Hessian is not positive definite. Estimates may be unreliable", call. = FALSE)
	flush.console()
	elog <- 1 #
  }
  if (LB_pars@SL50/LB_pars@Linf > 0.85) elog <- 2
  if (LB_pars@FM > 5) elog <- 3
  if (LB_pars@SL50/LB_pars@Linf > 0.85 & LB_pars@FM > 5) elog <- 4

  runMod <- LBSPRsim_(LB_pars, Control=Control, verbose=FALSE, doCheck=FALSE)

  LBobj <- new("LB_obj")
  Slots <- slotNames(LB_pars)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
  Slots <- slotNames(SingYear)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(SingYear, Slots[X])


  LBobj@Vars <- matrix(c(vSL50, vSL95, vFM, vSPR), ncol=4)
  LBobj@pLCatch <- runMod@pLCatch
  LBobj@NLL <- NLL
  LBobj@SPR <- runMod@SPR
  LBobj@Yield <- runMod@Yield
  LBobj@YPR <- runMod@YPR
  LBobj@maxFM <- runMod@maxFM
  LBobj@fitLog <- elog
  LBobj

}
