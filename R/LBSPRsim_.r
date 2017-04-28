#' Internal LBSPR Simulation Model
#'
#' A internal function that generates the expected equilbrium size composition given biological parameters, and fishing mortality and selectivity pattern.  Typically only used by other functions in the package.
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history information
#' @param Control a list of control options for the LBSPR model.
#' @param verbose display messages?
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
LBSPRsim_ <- function(LB_pars=NULL, Control=list(), verbose=TRUE, doCheck=TRUE) {
  # if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars'. Use: new('LB_pars')")

  # Error Checks
  # if (length(LB_pars@Species) < 1) {
    # if (verbose) message("No species name provided - using a default")
	# LB_pars@Species <- "My_Species"
  # }

  if (doCheck) check_LB_pars(LB_pars)
  if (doCheck) if(class(LB_pars) == "LB_pars") validObject(LB_pars)

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
    if (verbose) message("Walpha not set. Model not sensitive to this parameter - using default")
	LB_pars@Walpha <- Walpha <- 0.001
  }
  Wbeta <- LB_pars@Wbeta
  if (is.null(Wbeta) | length(Wbeta) < 1) {
    if (verbose) message("Wbeta not set. Model not sensitive to this parameter - using default")
	LB_pars@Wbeta <- Wbeta <- 3
  }
  FecB <- LB_pars@FecB
  if (is.null(FecB) | length(FecB) < 1) {
    if (verbose) message("FecB (Fecundity-at-length allometric parameter) not set. Using default - check value")
	LB_pars@FecB <- FecB <- 3
  }
  Steepness <- LB_pars@Steepness
  if (is.null(Steepness) | length(Steepness) < 1) {
      if (verbose) message("Steepness not set. Only used for yield analysis. Not sensitive if per-recruit. Using default of 1 (per-recruit model)")
	LB_pars@Steepness <- Steepness <- 0.99
  }
  if (Steepness <= 0.2 | Steepness >= 1) stop("Steepness must be greater than 0.2 and less than 1.0", call. = FALSE)

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
    if (verbose) message("BinMax not set. Using default of 1.3 Linf")
	  MaxL <- LB_pars@BinMax <- 1.3 * Linf
  }
  MinL <- LB_pars@BinMin
  if (is.null(MinL) | length(MinL) < 1) {
    if (verbose) message("BinMin not set. Using default value of 0")
	  MinL <- LB_pars@BinMin <- 0
  }
  BinWidth <- LB_pars@BinWidth
  if (is.null(BinWidth) | length(BinWidth) < 1) {
    if (verbose) message("BinWidth not set. Using default value of 1/20 Linf")
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
  if (MaxL < Linf) stop(paste0("Maximum length bin (", MaxL, ") can't be smaller than asymptotic size (", Linf ,"). Increase size of maximum length class ['maxL']"), call. = FALSE)
  # Control Parameters
  con <- list(maxsd=2, modtype=c("GTG","absel"), ngtg=13, P=0.01, Nage=101,
    maxFM=4, method="BFGS")
  nmsC <- names(con)
  con[(namc <- names(Control))] <- Control
  if (length(noNms <- namc[!namc %in% nmsC]))
        warning("unknown names in Control: ", paste(noNms, collapse = ", "), call. = FALSE)
  maxsd <- con$maxsd # maximum number of standard deviations from the mean for length-at-age distributions
  if (maxsd < 1) warning("maximum standard deviation is too small. See the help documentation", call. = FALSE)
  modType <- match.arg(arg=con$modtype, choices=c("GTG", "absel"))

  # Model control parameters
  P <- con$P
  if (P > 0.1 | P < 0.0001) warning("P parameter may be set to high or too low. See the help documentation", call. = FALSE)
  Nage <- con$Nage
  if (Nage < 90) warning("Nage should be higher. See the help documentation", call. = FALSE)
  maxFM <- con$maxFM
  ngtg <- con$ngtg
  Yield <- vector()

  newngtg <- max(ngtg, ceiling((2*maxsd*SDLinf + 1)/BinWidth))
  if (newngtg != ngtg) {
    if(verbose) message("ngtg increased to ", newngtg, " because of small bin size")
	ngtg <- newngtg
  }
  B0 <- SSB0 <- NA # hack
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

    # Minimum legal length
    plegal2 <- rep(1, length(LMids))
    if (length(LB_pars@MLL)>0) {
      plegal <- 1/(1+exp(-(LBins-LB_pars@MLL)/LB_pars@sdLegal))
      plegal2 <- 1/(1+exp(-(LMids-LB_pars@MLL)/LB_pars@sdLegal))
      FKL <- FKL * (plegal + (1-plegal) * LB_pars@fDisc)
    }
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
    B0 <- sum(NatLUF * Weight)
    SSB0 <- sum(NatLUF * Weight * MatLengtg)
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
    YPR <- sum(NatLC  * Weight * SelLen2 * plegal2) * FM


    Yield <- YPR * RelRec

	  # Spawning Stock Biomass
	  SSB <- sum(NatLF  * RelRec * Weight * MatLengtg)

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
	SSB <- sum(Ns * RelRec * rLens^Wbeta * Ma)

    # RelRec/R0 - relative recruitment
    YPR <- sum(Nc  * LMids^FecB ) * FM
    Yield <- YPR * RelRec

	# Simulated length data
    LenOut <- cbind(mids=LMids, n=Nc)
    LenOut <- LenOut[LenOut[,1] >= MinL,]
    LenOut[,2] <- LenOut[,2]/sum(LenOut[,2])
  }

  # if (FM > maxFM) {
    # if (verbose) message("F/M (", round(FM,2), ") greater than max F/M parameter (", maxFM, ")")
	# if (verbose) message("setting F/M to maxFM (see Control in documentation)")
    # FM <- maxFM
  # }
  LBobj <- new("LB_obj", verbose=verbose)
  Slots <- slotNames(LB_pars)
  for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
  LBobj@SPR <- SPR
  LBobj@Yield <- Yield
  LBobj@YPR <- YPR
  LBobj@SSB <- SSB
  LBobj@SSB0 <- SSB0
  LBobj@B0 <- B0
  LBobj@LMids <- LenOut[,1]
  LBobj@pLCatch <- matrix(LenOut[,2])
  LBobj@RelRec <- RelRec
  LBobj@pLPop <- round(array(c(LMids, PopUF, PopF, VulnUF, Nc),
    dim=c(length(PopUF), 5), dimnames=list(NULL, c("LMids", "PopUF", "PopF", "VulnUF", "VulnF")))
	, 6)
  LBobj@maxFM <- maxFM
  LBobj
}
