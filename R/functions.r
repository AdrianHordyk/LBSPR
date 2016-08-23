
#' LBSPR Simulation Model
#'
#' Function that generates the expected equilbrium size composition given biological parameters, and fishing mortality and selectivity pattern.
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param Control a list of control options for the LBSPR model.
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
LBSPRsim <- function(LB_pars=NULL, Control=list()) {
  # if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")
  if (length(LB_pars@SPR)>0) {
    if (LB_pars@SPR > 1 | LB_pars@SPR < 0) stop("SPR must be between 0 and 1")
    if (length(LB_pars@FM) >0) message("Both SPR and F/M have been specified. Using SPR and ignoring F/M")
	opt <- optimise(getFMfun, interval=c(0.001, 20), LB_pars, Control=Control)
	LB_pars@FM <- opt$minimum
	temp <- LBSPRsim_(LB_pars, Control=Control)
	if (round(temp@SPR,1) != round(LB_pars@SPR,1)) {
	  warning("Not possible to reach specified SPR. SPR may be too low for current selectivity pattern")
	  message("SPR is ", temp@SPR, " instead of ", LB_pars@SPR)
	}
	return(temp)
  } else {
    return(LBSPRsim_(LB_pars, Control=Control))
  }
}

#' Interal LBSPR Simulation Model
#'
#' A internal function that generates the expected equilbrium size composition given biological parameters, and fishing mortality and selectivity pattern.  Typically only used by other functions in the package.
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param Control a list of control options for the LBSPR model.
#' @param msg display messages?
#' @param doCheck check if the LB_pars object is valid? Switch off when calling function from a optimization routine.
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
#' @export
LBSPRsim_ <- function(LB_pars=NULL, Control=list(), msg=TRUE, doCheck=TRUE) {
  # if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")

  # Error Checks
  if (length(LB_pars@Species) < 1) {
    if (msg) message("No species name provided - using a default")
	LB_pars@Species <- "My_Species"
  }

  if (doCheck) check_LB_pars(LB_pars)
  if (doCheck) validObject(LB_pars)

  # Biological Parameters
  Species <- LB_pars@Species
  Linf <- LB_pars@Linf
  CVLinf <- LB_pars@CVLinf
  SDLinf <- CVLinf * Linf # Standard Deviation of Length-at-Age # Assumed constant CV here
  MK <- LB_pars@MK
  L50 <- LB_pars@L50
  L95 <- LB_pars@L95
  Walpha <- LB_pars@Walpha
  if (is.null(Walpha) | length(Walpha) < 1) {
    if (msg) message("Walpha not set. Model not sensitive to this parameter - using default")
	LB_pars@Walpha <- Walpha <- 0.001
  }
  Wbeta <- LB_pars@Wbeta
  if (is.null(Wbeta) | length(Wbeta) < 1) {
    if (msg) message("Wbeta not set. Model not sensitive to this parameter - using default")
	LB_pars@Wbeta <- Wbeta <- 3
  }
  FecB <- LB_pars@FecB
  if (is.null(FecB) | length(FecB) < 1) {
    if (msg) message("FecB (Fecundity-at-length allometric parameter) not set. Using default - check value")
	LB_pars@FecB <- FecB <- 3
  }
  Steepness <- LB_pars@Steepness
  if (is.null(Steepness) | length(Steepness) < 1) {
      if (msg) message("Steepness not set. Only used for yield analysis. Not sensitive if per-recruit. Using default of 1 (per-recruit model)")
	LB_pars@Steepness <- Steepness <- 0.99
  }
  if (Steepness <= 0.2 | Steepness >= 1) stop("Steepness must be greater than 0.2 and less than 1.0")

  Mpow <- LB_pars@Mpow
  if (is.null(Mpow) | length(Mpow) < 1) Mpow <- 0
  R0 <- LB_pars@R0
  if (is.null(R0) | length(R0) < 1) R0 <- 1

  # Exploitation Parameters
  SL50 <- LB_pars@SL50
  SL95 <- LB_pars@SL95
  FM <- LB_pars@FM

  # Length Classes
  MaxL <- LB_pars@BinMax
  if (is.null(MaxL) | length(MaxL) < 1) {
    if (msg) message("BinMax not set. Using default of 1.3 Linf")
	MaxL <- LB_pars@BinMax <- 1.3 * Linf
  }
  MinL <- LB_pars@BinMin
  if (is.null(MinL) | length(MinL) < 1) {
    if (msg) message("BinMin not set. Using default value of 0")
	MinL <- LB_pars@BinMin <- 0
  }
  BinWidth <- LB_pars@BinWidth
  if (is.null(BinWidth) | length(BinWidth) < 1) {
    if (msg) message("BinWidth not set. Using default value of 1/20 Linf")
	BinWidth <- LB_pars@BinWidth <- 1/20 * Linf
  }

  LBins <- seq(from=MinL, by=BinWidth, to=MaxL)
  LMids <- seq(from=LBins[1] + 0.5*BinWidth, by=BinWidth, length.out=length(LBins)-1)

  By <- BinWidth
  if (MinL > 0 & (MinL-By) > 0) { # the simulation model must start from 0 size class
	  fstBins <- rev(seq(from=MinL-By, to=0, by=-By))
	  LBins <- c(fstBins, LBins)
	  LMids <- seq(from=LBins[1] + 0.5*BinWidth, by=BinWidth, length.out=length(LBins)-1)
  }
  if (MaxL < Linf) stop(paste0("Maximum length bin (", MaxL, ") can't be smaller than asymptotic size (", Linf ,"). Increase size of maximum length class ['maxL']"))
  # Control Parameters
  con <- list(maxsd=2, modtype=c("GTG","absel"), ngtg=13, P=0.01, Nage=101, 
    maxFM=4, method="BFGS")
  nmsC <- names(con)
  con[(namc <- names(Control))] <- Control
  if (length(noNms <- namc[!namc %in% nmsC]))
        warning("unknown names in Control: ", paste(noNms, collapse = ", "))
  maxsd <- con$maxsd # maximum number of standard deviations from the mean for length-at-age distributions
  if (maxsd < 1) warning("maximum standard deviation is too small. See the help documentation")
  modType <- match.arg(arg=con$modtype, choices=c("GTG", "absel"))

  # Model control parameters
  P <- con$P
  if (P > 0.1 | P < 0.0001) warning("P parameter may be set to high or too low. See the help documentation")
  Nage <- con$Nage
  if (Nage < 90) warning("Nage should be higher. See the help documentation")
  maxFM <- con$maxFM
  ngtg <- con$ngtg
  Yield <- vector()
  
  newngtg <- max(ngtg, ceiling((2*maxsd*SDLinf + 1)/BinWidth))
  if (newngtg != ngtg) {
    if(msg) message("ngtg increased to ", newngtg, " because of small bin size")
	ngtg <- newngtg  
  }
  
  if (modType == "GTG") {
    # Linfs of the GTGs
    gtgLinfs <- seq(from=Linf-maxsd*SDLinf, to=Linf+maxsd*SDLinf, length=ngtg)
    dLinf <- gtgLinfs[2] - gtgLinfs[1]
    
    # Distribute Recruits across GTGS
    recP <- dnorm(gtgLinfs, Linf, sd=SDLinf) / sum(dnorm(gtgLinfs, Linf, sd=SDLinf))

    Weight <- Walpha * LMids^Wbeta
    # Maturity and Fecundity for each GTG
    L50GTG <- L50/Linf * gtgLinfs # Maturity at same relative size
    L95GTG <- L95/Linf * gtgLinfs # Assumes maturity age-dependant
    DeltaGTG <- L95GTG - L50GTG
    MatLengtg <- sapply(seq_along(gtgLinfs), function (X)
	  1.0/(1+exp(-log(19)*(LMids-L50GTG[X])/DeltaGTG[X])))
    FecLengtg <- MatLengtg * LMids^FecB # Fecundity across GTGs

    # Selectivity - asymptotic only at this stage - by careful with knife-edge
    SelLen <- 1.0/(1+exp(-log(19)*(LBins-(SL50+0.5*By))/ ((SL95+0.5*By)-(SL50+0.5*By))))

    # Life-History Ratios
    MKL <- MK * (Linf/(LBins+0.5*By))^Mpow # M/K ratio for each length class
    # Matrix of MK for each GTG
    MKMat <- matrix(rep(MKL, ngtg), nrow=length(MKL), byrow=FALSE)
    FK <- FM * MK # F/K ratio
    FKL <- FK * SelLen # F/K ratio for each length class
    ZKLMat <- MKMat + FKL # Z/K ratio (total mortality) for each GTG

    # Set Up Empty Matrices
    # number-per-recruit at length
    NPRFished <- NPRUnfished <- matrix(0, nrow=length(LBins), ncol=ngtg)
    NatLUF <- matrix(0, nrow=length(LMids), ncol=ngtg) # N at L unfished
    NatLF <- matrix(0, nrow=length(LMids), ncol=ngtg) # N at L fished
    FecGTG <- matrix(0, nrow=length(LMids), ncol=ngtg) # fecundity of GTG

    # Distribute Recruits into first length class
    NPRFished[1, ] <- NPRUnfished[1, ] <- recP * R0
    for (L in 2:length(LBins)) { # Calc number at each size class
      NPRUnfished[L, ] <- NPRUnfished[L-1, ] * ((gtgLinfs-LBins[L])/(gtgLinfs-LBins[L-1]))^MKMat[L-1, ]
      NPRFished[L, ] <- NPRFished[L-1, ] * ((gtgLinfs-LBins[L])/(gtgLinfs-LBins[L-1]))^ZKLMat[L-1, ]
	  ind <- gtgLinfs  < LBins[L]
	  NPRFished[L, ind] <- 0
	  NPRUnfished[L, ind] <- 0
    }
    NPRUnfished[is.nan(NPRUnfished)] <- 0
    NPRFished[is.nan(NPRFished)] <- 0
    NPRUnfished[NPRUnfished < 0] <- 0
    NPRFished[NPRFished < 0] <- 0

    for (L in 1:length(LMids)) { # integrate over time in each size class
      NatLUF[L, ] <- (NPRUnfished[L,] - NPRUnfished[L+1,])/MKMat[L, ]
      NatLF[L, ] <- (NPRFished[L,] - NPRFished[L+1,])/ZKLMat[L, ]
	  FecGTG[L, ] <- NatLUF[L, ] * FecLengtg[L, ]
    }

    SelLen2 <- 1.0/(1+exp(-log(19)*(LMids-SL50)/(SL95-SL50))) # Selectivity-at-Length
    NatLV <- NatLUF * SelLen2 # Unfished Vul Pop
    NatLC <- NatLF * SelLen2 # Catch Vul Pop

    # Aggregate across GTGs
    Nc <- apply(NatLC, 1, sum)/sum(apply(NatLC, 1, sum))
    VulnUF <- apply(NatLV, 1, sum)/sum(apply(NatLV, 1, sum))
    PopUF <- apply(NatLUF, 1, sum)/sum(apply(NatLUF, 1, sum))
    PopF <- apply(NatLF, 1, sum)/sum(apply(NatLF, 1, sum))

    # Calc SPR
    EPR0 <- sum(NatLUF * FecLengtg) # Eggs-per-recruit Unfished
    EPRf <- sum(NatLF * FecLengtg) # Eggs-per-recruit Fished
    SPR <- EPRf/EPR0

    # Equilibrium Relative Recruitment
    recK <- (4*Steepness)/(1-Steepness) # Goodyear compensation ratio
    reca <- recK/EPR0
    recb <- (reca * EPR0 - 1)/(R0*EPR0)
    RelRec <- max(0, (reca * EPRf-1)/(recb*EPRf))
    if (!is.finite(RelRec)) RelRec <- 0
    # RelRec/R0 - relative recruitment
    YPR <- sum(NatLC  * Weight * SelLen2) * FM
    Yield <- YPR * RelRec

    # Calc Unfished Fitness - not used here
    Fit <- apply(FecGTG, 2, sum, na.rm=TRUE) # Total Fecundity per Group
    FitPR <- Fit/recP # Fitness per-recruit
    FitPR <- FitPR/median(FitPR, na.rm=TRUE)

    # Calculate spawning-per-recruit at each size class
    SPRatsize <- cumsum(rowSums(NatLUF * FecLengtg))
    SPRatsize <- SPRatsize/max(SPRatsize)

    # Simulated length data
    LenOut <- cbind(mids=LMids, n=Nc)
    LenOut <- LenOut[LenOut[,1] >= MinL,]
    LenOut[,2] <- LenOut[,2]/sum(LenOut[,2])
  }
  if (modType == "absel") {
    # LBSPR model with pseudo-age classes
    x <- seq(from=0, to=1, length.out=Nage) # relative age vector
    EL <- (1-P^(x/MK)) * Linf # length at relative age
    rLens <- EL/Linf # relative length
    SDL <- EL * CVLinf # standard deviation of length-at-age
    Nlen <- length(LMids)
    Prob <- matrix(NA, nrow=Nage, ncol=Nlen)
    Prob[,1] <- pnorm((LBins[2] - EL)/SDL, 0, 1) # probablility of length-at-age
    for (i in 2:(Nlen-1)) {
      Prob[,i] <- pnorm((LBins[i+1] - EL)/SDL, 0, 1) -
	  	pnorm((LBins[i] - EL)/SDL, 0, 1)
    }
    Prob[,Nlen] <- 1 - pnorm((LBins[Nlen] - EL)/SDL, 0, 1)

    # Truncate normal dist at MaxSD
    mat <- array(1, dim=dim(Prob))
    for (X in 1:Nage) {
	  ind <- NULL
      if (EL[X] > (0.25 * Linf)) ind <- which(abs((LMids - EL[X]) /SDL[X]) >= maxsd)
      mat[X,ind] <- 0
    }

	Prob <- Prob * mat
	
    SL <- 1/(1+exp(-log(19)*(LMids-SL50)/(SL95-SL50))) # Selectivity at length
    Sx <- apply(t(Prob) * SL, 2, sum) # Selectivity at relative age
    MSX <- cumsum(Sx) / seq_along(Sx) # Mean cumulative selectivity for each age
    Ns <- (1-rLens)^(MK+(MK*FM)*MSX) # number at relative age in population

    Cx <- t(t(Prob) * SL) # Conditional catch length-at-age probablilities
    Nc <- apply(Ns * Cx, 2, sum) #

	PopF <- apply(Ns * Prob, 2, sum)
	PopF <- PopF/sum(PopF)

    Ml <- 1/(1+exp(-log(19)*(LMids-L50)/(L95-L50))) # Maturity at length
    Ma <-  apply(t(Prob) * Ml, 2, sum) # Maturity at relative age

    N0 <- (1-rLens)^MK # Unfished numbers-at-age
    PopUF <- apply(N0 * Prob, 2, sum)
	PopUF <- PopUF/sum(PopUF)
    VulnUF	<- apply(N0 * Cx, 2, sum) #
	VulnUF <- VulnUF/sum(VulnUF)
    SPR <- sum(Ma * Ns * rLens^FecB)/sum(Ma * N0 * rLens^FecB)
    
	# Equilibrium Relative Recruitment
	EPR0 <- sum(Ma * N0 * rLens^FecB)
	EPRf <- sum(Ma * Ns * rLens^FecB)
    recK <- (4*Steepness)/(1-Steepness) # Goodyear compensation ratio
    reca <- recK/EPR0
    recb <- (reca * EPR0 - 1)/(R0*EPR0)
    RelRec <- max(0, (reca * EPRf-1)/(recb*EPRf))
    if (!is.finite(RelRec)) RelRec <- 0
	
    # RelRec/R0 - relative recruitment
    YPR <- sum(Nc  * LMids^FecB ) * FM
    Yield <- YPR * RelRec

	# Simulated length data
    LenOut <- cbind(mids=LMids, n=Nc)
    LenOut <- LenOut[LenOut[,1] >= MinL,]
    LenOut[,2] <- LenOut[,2]/sum(LenOut[,2])
  }

  if (FM > maxFM) {
    if (msg) message("F/M (", round(FM,2), ") greater than max F/M parameter (", maxFM, ")")
	if (msg) message("setting F/M to maxFM (see Control in documentation)")
    FM <- maxFM
  }
  LBobj <- new("LB_obj")
  Slots <- slotNames(LB_pars)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
  LBobj@SPR <- SPR
  LBobj@Yield <- Yield
  LBobj@LMids <- LenOut[,1]
  LBobj@pLCatch <- matrix(LenOut[,2])
  LBobj@RelRec <- RelRec
  LBobj@pLPop <- round(array(c(LMids, PopUF, PopF, VulnUF, Nc),
    dim=c(length(PopUF), 5), dimnames=list(NULL, c("LMids", "PopUF", "PopF", "VulnUF", "VulnF")))
	, 6)
  LBobj@maxFM <- maxFM
  LBobj
}

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
  (LB_pars@SPR - LBSPRsim_ (LB_pars, Control=Control, msg=FALSE)@SPR)^2
}


#' Fit LBSPR model to length data
#'
#' A function that fits the LBSPR model to length data
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the length data
#' @param yrs index of years to include. If NA the model is run on all years
#' @param Control a list of control options for the LBSPR model.
#' @param pen apply a penalty if estimate of selectivity is very high?
#' @param msg display messages?
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
#'
#' @importFrom utils flush.console
#' @importFrom methods new slot slot<- slotNames validObject
#' @export
LBSPRfit <- function(LB_pars=NULL, LB_lengths=NULL, yrs=NA, Control=list(), pen=TRUE, msg=TRUE, useCPP=TRUE, ...) {

  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')")

  # Error Checks
  if (length(LB_pars@Species) < 1) {
    if (msg) message("No species name provided - using a default")
	LB_pars@Species <- "My_Species"
  }
  if (length(LB_pars@SL50) == 0) LB_pars@SL50 <- 1
  if (length(LB_pars@SL95) == 0) LB_pars@SL95 <- 2
  if (length(LB_pars@FM) == 0) LB_pars@FM <- 1

  check_LB_pars(LB_pars)
  validObject(LB_pars)

  if (class(LB_lengths@Years) != "numeric" & class(LB_lengths@Years) != "integer") {
    warning("Years must be numeric values")
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
	  stop("yrs must numeric value indicating column(s), or NA for all")
    nyrs <- length(yrs)
    yearNames <- LB_lengths@Years[yrs]
    if (is.null(yearNames)) yearNames <- yrs
	cols <- yrs
  }
  message("Fitting model")
  message("Year:")
  flush.console()
  runMods <- sapply(cols, function (X)
	LBSPRfit_(yr=X, LB_pars=LB_pars, LB_lengths=LB_lengths, Control=Control,  pen=pen, useCPP=useCPP))

  LBobj <- new("LB_obj")
  Slots <- slotNames(LB_pars)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
  Slots <- slotNames(LB_lengths)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_lengths, Slots[X])

  LBobj@NYears <- nyrs
  LBobj@Years <- yearNames
  LBobj@LData <- LB_lengths@LData[,cols, drop=FALSE]
  LBobj@NLL <- unlist(lapply(runMods, slot, "NLL"))
  LBobj@SL50 <- unlist(lapply(runMods, slot, "SL50"))
  LBobj@SL95 <- unlist(lapply(runMods, slot, "SL95"))
  LBobj@FM <- unlist(lapply(runMods, slot, "FM"))
  LBobj@SPR <- unlist(lapply(runMods, slot, "SPR"))
  LBobj@Yield <- unlist(lapply(runMods, slot, "Yield"))
  LBobj@fitLog <- unlist(lapply(runMods, slot, "fitLog"))
  LBobj@Vars <-  matrix(unlist(lapply(runMods, slot, "Vars")), ncol=4, byrow=TRUE)
  colnames(LBobj@Vars) <- c("SL50", "SL95", "FM", "SPR")
  LBobj@pLCatch <- do.call(cbind, lapply(runMods, slot, "pLCatch"))
  LBobj@maxFM <- unlist(lapply(runMods, slot, "maxFM"))[1]
 
  DF <- data.frame(SL50=LBobj@SL50, SL95=LBobj@SL95, FM=LBobj@FM, SPR=LBobj@SPR)
  if (nrow(DF) == 1) LBobj@Ests <- as.matrix(DF)
  if (nrow(DF) > 1) LBobj@Ests <- apply(DF, 2, FilterSmooth, ...)

  LBobj
}


#' Kalman filter and Rauch-Tung-Striebel smoother
#'
#' A function that applies a filter and smoother to estimates
#'
#' @param RawEsts a vector of estimated values
#' @param R variance of sampling noise
#' @param Q variance of random walk increments
#' @param Int covariance of initial uncertainty
#' @return a vector of smoothed values
#' @author A. Hordyk
#'
#' @export
FilterSmooth <- function(RawEsts, R=1, Q=0.1, Int=100) {
  # Kalman smoother and Rauch-Tung-Striebel smoother on random walk estimation
  #http://read.pudn.com/downloads88/ebook/336360/Kalman%20Filtering%20Theory%20and%20Practice,%20Using%20MATLAB/CHAPTER4/RTSvsKF.m__.htm
  # R  # Variance of sampling noise
  # Q  # Variance of random walk increments
  # Int # Covariance of initial uncertainty
  Ppred <-  rep(Int, length(RawEsts))
  nNA <- sum(is.na(RawEsts))
  while(nNA > 0) { # NAs get replaced with last non-NA
    RawEsts[is.na(RawEsts)] <- RawEsts[which(is.na(RawEsts))-1]
    nNA <- sum(is.na(RawEsts))
  }

  Pcorr <- xcorr <- xpred <- rep(0, length(RawEsts))
  # Kalman Filter
  for (X in 1:length(Ppred)) {
    if (X !=1) {
	  Ppred[X] <- Pcorr[X-1] + Q
	  xpred[X] <- xcorr[X-1]
	}
	W <- Ppred[X]/(Ppred[X] + R)
	xcorr[X] <- xpred[X] + W * (RawEsts[X] - xpred[X]) # Kalman filter estimate
	Pcorr[X] <- Ppred[X] - W * Ppred[X]
  }
  # Smoother
  xsmooth <- xcorr
  for (X in (length(Pcorr)-1):1) {
    A <- Pcorr[X]/Ppred[X+1]
	xsmooth[X] <- xsmooth[X] + A*(xsmooth[X+1] - xpred[X+1])
  }
  return(xsmooth)

}

#' Interal function to fit LBSPR model to length data
#'
#' An internal function that fits the LBSPR model to a single year of length data
#'
#' @param yr index of the year column to fit model to
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the length data
#' @param Control a list of control options for the LBSPR model.
#' @param pen apply a penalty if estimate of selectivity is very high?
#' @param useCPP use cpp optimization code?
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
#' @importFrom stats dbeta dnorm median nlminb optimise pnorm optim
#' @export
LBSPRfit_ <- function(yr=1, LB_pars=NULL, LB_lengths=NULL, Control=list(), pen=TRUE, useCPP=TRUE) {
  message(yr)
  flush.console()

  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')")
  if (yr > LB_lengths@NYears) stop("yr greater than LBSPR_obj@NYears")

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
    warning("unknown names in Control: ", paste(noNms, collapse = ", "))
	cat("Options are: ", paste(names(con), collapse = ", "), "\n")
  }
  maxsd <- con$maxsd # maximum number of standard deviations from the mean for length-at-age distributions
  if (maxsd < 1) warning("maximum standard deviation is too small. See the help documentation")
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
	opt <- optim(Start, LBSPR_NLLgtg, LMids=LMids, LBins=LBins, LDat=LDat,
	  gtgLinfs=gtgLinfs, MKMat=MKMat,  MK=LB_pars@MK, Linf=LB_pars@Linf,
	  ngtg=ngtg, recP=recP,usePen=usePen, hessian=TRUE, method=Control$method)
	varcov <- solve(opt$hessian)

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
  if (any(diag(varcov) < 0)) {
    warning("The final Hessian is not positive definite. Estimates may be unreliable")
	flush.console()
	elog <- 1 # 
  }
  
  runMod <- LBSPRsim_(LB_pars, Control=Control, msg=FALSE, doCheck=FALSE)

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
  LBobj@maxFM <- runMod@maxFM
  LBobj@fitLog <- elog
  LBobj

}

varSPR <- function(MLEs, varcov, LB_pars) {
  var <- diag(varcov)
  vars <- c("lSL50", "ldL", "lFM")
  p1 <- 0
  for (i in seq_along(MLEs)) p1 <- p1 + derivative(dSPR, x=MLEs[i], var=vars[i], 
    LB_pars=LB_pars)^2 * var[i]

  p2 <- derivative(dSPR, x=MLEs[1], var=vars[1],  LB_pars=LB_pars) * 
    derivative(dSPR, x=MLEs[2], var=vars[2],  LB_pars=LB_pars) * varcov[1,2]
  
  p3 <- derivative(dSPR, x=MLEs[1], var=vars[1],  LB_pars=LB_pars) * 
    derivative(dSPR, x=MLEs[3], var=vars[3],  LB_pars=LB_pars) * varcov[1,3]
  
  p4 <- derivative(dSPR, x=MLEs[3], var=vars[3],  LB_pars=LB_pars) * 
    derivative(dSPR, x=MLEs[2], var=vars[2],  LB_pars=LB_pars) * varcov[3,2]
  p1 + p2 + p3 + p4   
}

dSPR <- function(x, LB_pars, var=c("lSL50", "ldL", "lFM"),Control=NULL) {
  lvar <- match.arg(var)
  ex <- exp(x)
  if (lvar == "lFM") myslot <- "FM" 
  if (lvar == "lSL50") {
    myslot <- "SL50"
	ex <- ex * LB_pars@Linf
  }
  if (lvar == "ldL") {
    myslot <- "SL95"
	ex <-  ex * LB_pars@Linf + LB_pars@L50
  }
  slot(LB_pars, myslot) <- ex 
  temp <- LBSPRsim_(LB_pars, Control=Control, msg=FALSE, doCheck=FALSE)
  temp@SPR
}

# From http://blog.quantitations.com/tutorial/2013/02/12/numerical-derivatives-in-r/
derivative <- function(f, x, ..., order = 1, delta = 0.01, sig = 6) {
    # Numerically computes the specified order derivative of f at x
    vals <- matrix(NA, nrow = order + 1, ncol = order + 1)
    grid <- seq(x - delta/2, x + delta/2, length.out = order + 1)
    vals[1, ] <- sapply(grid, f, ...) - f(x, ...)
    for (i in 2:(order + 1)) {
        for (j in 1:(order - i + 2)) {
            stepsize <- grid[i + j - 1] - grid[i + j - 2]
            vals[i, j] <- (vals[i - 1, j + 1] - vals[i - 1, j])/stepsize
        }
    }
    return(signif(vals[order + 1, 1], sig))
}

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
  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')")

  LB_pars@SL50 <- exp(trypars)[1] * LB_pars@Linf
  LB_pars@SL95 <- LB_pars@SL50 + (exp(trypars)[2]* LB_pars@Linf)
  LB_pars@FM <- exp(trypars[3])

  runMod <- LBSPRsim_(LB_pars, Control=Control, msg=FALSE, doCheck=FALSE)

  ldat <- LB_lengths@LData[,yr] + 1E-15 # add tiny constant for zero catches
  LenProb <- ldat/sum(ldat)
  predProb <- runMod@pLCatch
  predProb <- predProb + 1E-15 # add tiny constant for zero catches
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

#' Plot simulated size composition
#'
#' A function that plots the expected size composition in the fished and unfished state
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param type a character value indicating if the Catch or Population should be plotted
#' @param perRec a logical to indicate if plot should be per-recruit (ignore steepness) or not (zero recruitment if SPR below replacement level)
#' @param Cols optional character vector of colours for the plot
#' @param axTex size of the axis text
#' @param axTitle size of axis title
#' @return a ggplot object
#' @author A. Hordyk
#' @importFrom ggplot2 ggplot aes geom_line geom_bar scale_color_manual guides guide_legend xlab ylab theme theme_bw element_text scale_fill_manual scale_fill_discrete ggtitle
#' @export
plotSim <- function(LB_obj=NULL, type=c("Catch", "Pop"), perRec=FALSE, Cols=NULL, axTex=12, axTitle=14) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use: LBSPRsim")
  type <- match.arg(type)
  LMids <- LB_obj@LMids
  
  pLCatch <- LB_obj@pLCatch # predicted size comp of catch
  pLPop <- LB_obj@pLPop # predicted size comp of population
  

  if (length(pLPop) < 1) stop("No simulated population data")
  PopF <- pLPop[,"PopF"] 
  PopUF <- pLPop[,"PopUF"] 
  PopSizeDat <- data.frame(pLPop)
  
  if (!perRec) {
    relativePop <- PopF / (PopF[1]/PopUF[1]) * (LB_obj@RelRec/LB_obj@R0)
    PopSizeDat[,"PopF"] <- relativePop
  
    ind <- which(PopSizeDat[,"VulnUF"] > 0)[1]
    relativeCatch <- pLCatch / (pLCatch[ind]/PopSizeDat[,"VulnUF"][ind]) * (LB_obj@RelRec/LB_obj@R0)
    pLCatch <- relativeCatch
  }

  if (type == "Catch") {
    ind <- match(LMids, PopSizeDat[,1])
    Dat <- data.frame(LMids=LMids, VulnUF=PopSizeDat[ind, "VulnUF"], pLCatch=pLCatch)
	longDat <- gather(Dat, "PopType", "PLength", 2:ncol(Dat))
	Title <- "Catch"
	Leg <- c("Fished", "Unfished")
  }
  if (type == "Pop") {
    longDat <- gather(PopSizeDat, "PopType", "PLength", 2:ncol(PopSizeDat))
    longDat <- dplyr::filter(longDat, PopType == "PopUF" | PopType == "PopF")
	Title <- "Population"
	Leg <- c("Fished", "Unfished")
  }

  PopType <- PLength <- NULL # hack to get past CRAN
  Plot <- ggplot(longDat, aes(x=LMids, y=PLength, fill=PopType)) +
	geom_bar(stat="identity", position = "identity") +
	xlab("Length") +
    ylab("Relative Number") +
	theme_bw() +
	theme(axis.text=element_text(size=axTex),
        axis.title=element_text(size=axTitle,face="bold"))
 if (all(is.null(Cols))) Plot <- Plot + scale_fill_discrete(Title, labels = Leg)
 if (!all(is.null(Cols))) Plot <- Plot + scale_fill_manual(Title, labels = Leg, values=Cols)

 Plot
}

#' Plot the maturity-at-length and selectivity-at-length curves
#'
#' A function that plots the maturity-at-length and selectivity-at-length curves
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param axTex size of the axis text
#' @param axTitle size of axis title
#' @param useSmooth use the smoothed estimates?
#' @param Title optional character string for plot title
#' @return a ggplot object
#' @author A. Hordyk
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom tidyr gather
#' @importFrom RColorBrewer brewer.pal
#' @export
plotMat <- function(LB_obj=NULL, axTex=12, axTitle=14, useSmooth=TRUE, Title=NULL) {
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_pars") stop("LB_obj must be of class 'LB_obj' or class 'LB_pars'")

  if ("LMids" %in% slotNames(LB_obj)) Lens <- seq(from=LB_obj@LMids[1], to=LB_obj@LMids[length(LB_obj@LMids)], by=1)
  if (!("LMids" %in% slotNames(LB_obj))) Lens <- seq(from=0, to=LB_obj@Linf, by=1)
  # Length at Maturity
  LenMat <- 1.0/(1+exp(-log(19)*(Lens-LB_obj@L50)/(LB_obj@L95-LB_obj@L50)))
  DF <- data.frame(Lens=Lens, Dat=LenMat, Line="Maturity")
  Dat <- Proportion <- Line <- SelDat <- Year <- NULL # hack to get past CRAN check
  mplot <- ggplot(DF, aes(x=Lens, y=Dat)) +
    geom_line(aes(color="Maturity"), size=1.5) +
	scale_color_manual(values="black") +
    guides(color=guide_legend(title="")) +
	xlab("Length") +
    ylab("Proportion") +
	theme_bw() +
	theme(axis.text=element_text(size=axTex),
    axis.title=element_text(size=axTitle,face="bold"), legend.position="top",
	plot.title = element_text(lineheight=.8, face="bold"))

  if (class(LB_obj) == "LB_obj") {
    if (length(LB_obj@Ests)>0 & (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer")) {
      warning("Years must be numeric values")
	  message("Attempting to convert to numeric values")
	  options(warn=-1)
      LB_obj@Years <-  gsub("X", "", LB_obj@Years)
	  LB_obj@Years <- as.numeric(LB_obj@Years)
	  options(warn=1)
      if (all(is.na(LB_obj@Years))) LB_obj@Years <- 1:length(LB_obj@Years)
    }

    years <- LB_obj@Years
    if (length(years) < 1) years <- 1

    if (useSmooth & length(LB_obj@Ests) > 0) {
      SL50 <- LB_obj@Ests[,"SL50"]
      SL95 <- LB_obj@Ests[,"SL95"]
    }
    if (!useSmooth | length(LB_obj@Ests) == 0) {
      SL50 <- LB_obj@SL50
      SL95 <- LB_obj@SL95
    }
    if (length(SL50) > 0 & (length(years) == 1 | length(SL50) < 2)) {
      LenSel <- 1.0/(1+exp(-log(19)*(Lens-(SL50))/((SL95)-(SL50))))
      longSel <- data.frame(Lens=Lens, Selectivity=LenSel, Maturity=LenMat)
	  longSel <- gather(longSel, "Line", "Proportion", 2:3)
	  mplot <- ggplot(longSel, aes(x=Lens, y=Proportion)) +
        geom_line(aes(color=Line), size=1.5) +
	    xlab("Length") +
        ylab("Proportion") +
	    guides(color=guide_legend(title="")) +
	    theme_bw() +
	    theme(axis.text=element_text(size=axTex),
        axis.title=element_text(size=axTitle,face="bold"), legend.position="top")
    }
    if (length(SL50) > 0 & (length(years) > 1 | length(SL50) > 1)) { # Multiple years exist
      LenSel <- sapply(1:length(years), function(X)
	    1.0/(1+exp(-log(19)*(Lens-(SL50[X]))/((SL95[X])-(SL50[X])))))
      LenSel <- data.frame(LenSel, check.names=FALSE)
	  colnames(LenSel) <- years
      longSel <- gather(LenSel, "Year", "SelDat")
	  colourCount <- length(years)
	  getPalette <- colorRampPalette(brewer.pal(12, "Set3"))
	  cols <- rep(getPalette(min(12, colourCount)),5)[1:colourCount]
	  longSel$Lens <- DF$Lens
	  suppressMessages(
	    mplot <-  mplot +
	      guides(color=guide_legend(title="Est. Selectivity")) +
	      scale_color_manual(values = c(cols, "black")) +
	      geom_line(aes(x=Lens, y=SelDat, color=Year), longSel, size=1)
	  )
    }
  }
  if (!(is.null(Title)) & class(Title)=="character")  mplot <- mplot + ggtitle(Title)

  mplot
}

#' Plot the size data and model fits
#'
#' A function that plots size data and the fitted LBSPR model
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param axTex size of the axis text
#' @param axTitle size of axis title
#' @param Title optional character string for plot title
#' @return a ggplot object
#' @author A. Hordyk
#'
#' @importFrom ggplot2 facet_wrap
#' @export
plotSize <- function(LB_obj=NULL, axTex=12, axTitle=14, Title=NULL) {
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_lengths") stop("Require LB_lengths or LB_obj object")

  if (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer") {
    warning("Years must be numeric values")
	message("Attempting to convert to numeric values")
	options(warn=-1)
    LB_obj@Years <-  gsub("X", "", LB_obj@Years)
	LB_obj@Years <- as.numeric(LB_obj@Years)
	options(warn=1)
    if (all(is.na(LB_obj@Years))) LB_obj@Years <- 1:length(LB_obj@Years)
  }

  NYrs <- max(1, length(LB_obj@Years))
  Years <- LB_obj@Years
  Ldat <- LB_obj@LData
  if (length(Ldat) < 1) stop("No length data found")
  LMids <- LB_obj@LMids
  Ldat <- data.frame(Ldat, check.names=FALSE)
  colnames(Ldat) <- as.character(Years)
  longDat <- gather(Ldat, "Year", "LBSPR_len")
  longDat$LMids <- LMids
  longDat$Year <- factor(longDat$Year, levels=colnames(Ldat))
  NCol <- ceiling(sqrt(NYrs))
  NRow <- ceiling(NYrs/NCol)
  LBSPR_len <- NULL # hack to get past CRAN check
  bplot <- ggplot(longDat, aes(x=LMids, y=LBSPR_len)) +
   facet_wrap(~Year, ncol=NCol) +
   geom_bar(stat="identity") +
   xlab("Length") +
   ylab("Count") +
   theme_bw() +
   theme(axis.text=element_text(size=axTex),
   axis.title=element_text(size=axTitle,face="bold"),
   plot.title = element_text(lineheight=.8, face="bold"))

  if (!(is.null(Title)) & class(Title)=="character") bplot <- bplot + ggtitle(Title)

  chk <- ("pLCatch" %in% slotNames(LB_obj))
  chk2 <- FALSE
  if (chk) if (length(LB_obj@pLCatch) > 0) chk2 <- TRUE
  if (chk & chk2) { # model has been fitted
	NSamp <- apply(LB_obj@LData, 2, sum)
	predlen <- data.frame(sweep(LB_obj@pLCatch, MARGIN=2, NSamp, "*")) #
    longDat2 <- gather(predlen, "Year", "PredLen")
	longDat2$LMids <- LMids
    bplot <- bplot +
	  geom_line(aes(x=longDat2$LMids, y=longDat2$PredLen), colour="black", size=1.25)
	fitLog <- LB_obj@fitLog
	ind <- which(fitLog > 0)
	if (length(ind) > 0) {
	  yrs <- unique(longDat$Year)[which(fitLog > 0)]
	  text_dat <- data.frame(Year=factor(yrs), levels=levels(longDat$Year),
	    LMids=longDat$LMids[0.1*length(longDat$LMids)],
		LBSPR_len=0.15 * max(longDat$LBSPR_len), lab="Model didn't converge")
      bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=6)
	
	}
  }

  bplot
}

#' Circle of estimated SPR and target and limit points
#'
#' A function that creates a circle plot showing the estimated SPR relative to the
#' target and limit reference points
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param SPRTarg a numeric value specifying the SPR target
#' @param SPRLim a numeric value specifying the SPR limit
#' @param useSmooth use the smoothed estimates? Usually would want to do this
#' @param Title include the title?
#' @param Leg include the legend?

#' @author A. Hordyk
#' @importFrom plotrix draw.circle draw.ellipse draw.radial.line radialtext
#' @export
plotSPRCirc <- function(LB_obj=NULL, SPRTarg=0.4, SPRLim=0.2, useSmooth=TRUE, Title=FALSE, Leg=TRUE) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use LBSPRfit")

  par(mfrow=c(1,1), mar=c(1,2,3,2), oma=c(0,0,0,0))
  plot(1:10, asp = 1,main="", type="n", bty="n", axes=FALSE,
    xlim=c(0,10), ylim=c(0,10), xlab="", ylab="")
  a <- 4.5
  x <- 5
  if (useSmooth) spr <- LB_obj@Ests[,"SPR"]
  if (!useSmooth) spr <- LB_obj@SPR
  if (length(spr) > 1) {
    message("More than one SPR value. Using last value")
    flush.console()
	spr <- spr[length(spr)]
  }

  ang <- 90 - (spr*360)
  ang2 <- 90
  tg  <- 90 - (SPRTarg*360)
  lim <- 90 - (SPRLim*360)
  limcol <- "#ff1919"
  targcol <- "#ffb732"
  abtgcol <- "#32ff36"
  nv <- 200
  texcex <- 1.3 
  texcex2 <- 2
  # Circle
	  
  draw.circle(x=x, y=x, radius=a, border="#FAFAFA", col="#FAFAFA", nv=nv)
  # Limit Ellipse
  draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(max(lim, ang), ang2),
      col=limcol, arc.only=FALSE, border=FALSE, nv=nv)
  if (spr > SPRLim) {
  draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(max(tg, ang), lim),
    col=targcol, arc.only=FALSE, border=FALSE, nv=nv)
  }
  if (spr > SPRTarg) {
    draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(min(360, ang), tg),
      col=abtgcol, arc.only=FALSE, border=FALSE, nv=nv)
  }
  # radialtext(as.character(round(spr,2)), center=c(x,x), start=NA,
    # middle=a/2, end=NA, deg=ang, expand=FALSE, stretch=1, nice=TRUE,
   # cex=1, xpd=NA)
  draw.radial.line(0, a, center=c(x, x), deg=lim,
       expand=FALSE, col=limcol, lwd=1, lty=1)
  draw.radial.line(0, a, center=c(x, x), deg=tg,
       expand=FALSE, col=targcol, lwd=1, lty=1)
  draw.radial.line(0, x-0.5, center=c(x, x), deg=ang,
       expand=FALSE, col="black", lwd=3, lty=2)
	   
  rndspr <- round(spr,2)*100
 
  if (rndspr <= SPRLim*100) textcol <- limcol
  if (rndspr <= SPRTarg*100 & rndspr > SPRLim*100) textcol <- targcol
  if (rndspr > SPRTarg*100)  textcol <- abtgcol
  radialtext(paste0(round(spr,2)*100, "%"), 
    center=c(x,x), start=x, middle=1, end=NA, deg=ang,  expand=0, stretch=1, 
	nice=TRUE, cex=texcex2,xpd=NA, col=textcol)
  
  if (Title) mtext(side=3, paste0("Estimated SPR = ", round(spr,2)),
    cex=1.25, line=-4 ,outer=TRUE)
  if (Leg) legend("topleft", legend=c(as.expression(bquote(Below ~ Limit ~ .(SPRLim*100) * "%")),
    as.expression(bquote(Below ~ Target ~ .(SPRTarg*100) * "%")), "Above Target"), bty="n", pch=15, pt.cex=2,
	col=c(limcol, targcol, abtgcol),
	bg=c(limcol, targcol, abtgcol), title=expression(bold("SPR")), cex=texcex)
  # if (Leg) legend("topright", bty="n",
    # legend=as.expression(bquote(Estimate ~ .(round(spr,2)*100) * "%")),
	# lty=2,lwd=3, cex=texcex)
    
  text(x, x+a, "0%", pos=3, xpd=NA, cex=texcex)
  text(x+a, x, "25%", pos=4, xpd=NA, cex=texcex)
  text(x, x-a, "50%", pos=1, xpd=NA, cex=texcex)
  text(x-a, x, "75%", pos=2, xpd=NA, cex=texcex)
}


#' Plot LBSPR model estimates
#'
#' A function that plots the estimates of the LBSPR with a smoother line
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param pars a character vectors specifying which plots to create
#' @param Lwd line width
#' @param axCex size of the axis
#' @param labCex size of axis label
#' @param doSmooth apply the smoother?

#' @author A. Hordyk
#' @importFrom graphics abline axis hist legend lines mtext par plot points text
#' @importFrom plotrix plotCI
#' @export
plotEsts <- function(LB_obj=NULL, pars=c("Sel", "FM", "SPR"), Lwd=2.5,  axCex=1.45, labCex=1.55, doSmooth=TRUE) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use LBSPRfit")
  if (length(LB_obj@Ests) < 1) stop("No estimates found. Use LBSPRfit")
  pars <- match.arg(pars, several.ok=TRUE)
  rawEsts <- data.frame(SL50=LB_obj@SL50, SL95=LB_obj@SL95, FM=LB_obj@FM, SPR=LB_obj@SPR)
  if (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer") {
    warning("Years must be numeric values")
	message("Attempting to convert to numeric values")
	options(warn=-1)
    LB_obj@Years <-  gsub("X", "", LB_obj@Years)
	LB_obj@Years <- as.numeric(LB_obj@Years)
	options(warn=1)
    if (all(is.na(LB_obj@Years))) LB_obj@Years <- 1:length(LB_obj@Years)
  }

  rawEsts$Years <-  LB_obj@Years
  if (length(LB_obj@Years) < 2) message("This plot doesn't make much sense with only 1 year. But here it is anyway")
  smoothEsts <- data.frame(LB_obj@Ests)
  smoothEsts$Years <- LB_obj@Years
  
  ## 95% CIs ##
  CIlower <- as.matrix(rawEsts[,1:4] - 1.96 * sqrt(LB_obj@Vars))
  CIupper <- as.matrix(rawEsts[,1:4] + 1.96 * sqrt(LB_obj@Vars))
  
  # correct bounded parameters - dodgy I know!
  CIlower[CIlower[,3]<0,3] <- 0
  CIlower[CIlower[,4]<0,4] <- 0
  CIupper[CIupper[,4]>1,4] <- 1 
  CIlower[!is.finite(CIlower)] <- NA
  CIupper[!is.finite(CIupper)] <- NA
	
  scol <- "darkgray"
  
  at <- seq(from=min(LB_obj@Years)-1, to=max(LB_obj@Years)+1, by=1)
  nplots <- 0
  doSel <- doFM <- doSPR <- FALSE
  if ("Sel" %in% pars) {
    doSel <- TRUE
    nplots <- nplots + 1
  }
  if ("FM" %in% pars) {
    nplots <- nplots + 1
	doFM <- TRUE
  }
  if ("SPR" %in% pars) {
    nplots <- nplots + 1
	doSPR <- TRUE
  }
  par(mfrow=c(1,nplots), bty="l", las=1, mar=c(2,3,2,2), oma=c(2,2,0,0))
  # Selectivity
  if (doSel) {
    YLim <- c(max(CIlower[,1], na.rm=TRUE) * 0.95, max(CIupper[,2], na.rm=TRUE) * 1.05)
	YLim <- range(pretty(YLim))
    plot(rawEsts$Years,  rawEsts$SL50, ylim=YLim, pch=19, xlab="", ylab="", axes=FALSE, type="n")
	myLeg <- legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95]),
	  expression(L[50])), lty=c(1,2,1), lwd=Lwd, col=c("black", "black", "gray"),
	  cex=1.75, xpd=NA, plot=FALSE)

    YLim[2] <- 1.04*(YLim[2]+myLeg$rect$h)
	par(mfrow=c(1,nplots), bty="l", las=1, mar=c(2,3,2,2), oma=c(2,2,0,0))
	plot(rawEsts$Years,  rawEsts$SL50, ylim=YLim, pch=19, xlab="", ylab="", axes=FALSE)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SL50, ui=CIupper[,1], li=CIlower[,1], add=TRUE, scol=scol)
	
	axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(YLim), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SL50, lwd=Lwd)
   
    # points(rawEsts$Years,  rawEsts$SL95, pch=17)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SL95, ui=CIupper[,2], li=CIlower[,2], add=TRUE, pch=17, scol=scol)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SL95, lwd=Lwd, lty=2)
    abline(h=LB_obj@L50, col="gray", lwd=0.5)
	mtext(side=2, line=3, "Selectivity", cex=labCex, las=3)
	legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95]),
	  expression(L[50])), lty=c(1,2,1), lwd=Lwd, col=c("black", "black", "gray"),
	  cex=1.75, xpd=NA)
  }
  # Relative Fishing Mortality
  if (doFM) {
    YMax <- max(CIupper[,3], na.rm=TRUE) * 1.05
    YMin <- min(CIlower[,3], na.rm=TRUE) * 0.95
	YLim <- round(c(YMin, YMax),2)
	YLim <- range(pretty(YLim))
    plot(rawEsts$Years,  rawEsts$FM, ylim=YLim,pch=19, xlab="", ylab="", cex.axis=axCex, axes=FALSE)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$FM, ui=CIupper[,3], li=CIlower[,3], add=TRUE, scol=scol)
    axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(YLim), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$FM, lwd=Lwd)
	mtext(side=2, line=3, "F/M", cex=labCex, las=3)
  }
  # SPR
  if (doSPR) {
    plot(rawEsts$Years,  rawEsts$SPR, ylim=c(0,1), pch=19, xlab="", ylab="", cex.axis=axCex, axes=FALSE)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SPR, ui=CIupper[,4], li=CIlower[,4], add=TRUE, scol=scol)
	axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(c(0,1)), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SPR, lwd=Lwd)
	mtext(side=2, line=3, "SPR", cex=labCex, las=3)
  }
  mtext(outer=TRUE, side=1, line=1, "Years", cex=labCex)
}

#' Report the location of the Data Files
#'
#' A function that returns the location of the example CSV files
#'
#' @author A. Hordyk modified (i.e., stolen) from T. Carruthers' code (DLMtool package)
#' @export
DataDir<-function(){
    return(paste(searchpaths()[match("package:LBSPR",search())],"/",sep=""))
}
