#' Simulation model for the equilibrium length-based SPR Model. 
#' @name LBSPRSim
#' @title Length-Based Spawning Potential Ratio Equilibrium Model
#' @param currEggProd Current egg production - total number of eggs or spawning biomass at the 

#' @return  
#' @author Adrian Hordyk
#' @export
LBSPRSim <- function(stock=NULL, fleet=NULL, 
  lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), 
  mod=c("GTG", "age"), xtra=NULL) {
  mod <- match.arg(mod) 

  # Default Biological Parameters 
  stockdefaults <- c(Linf=100, CVLinf=0.1, MaxSD=2, MK=1.5, L50=66, L95=70, 
                Walpha=0.001, Wbeta=3, FecB=3, Steepness=1, Mpow=0, 
				R0=10000)	
  stock <- makedefaults(stock, stockdefaults)
  # Default Fleet Parameters
  fleetdefaults <- list(SL50=0.66*Linf, SL95=0.7*Linf, FM=1)
  fleet <- makedefaults(fleet, fleetdefaults)
  
  if (is.null(xtra)) { # Control parameters for the models
	xtra$NGTG <- 9 # Number of GTGs
	xtra$P <- 0.001 # % of cohort alive at maximum age 
	xtra$Nage <- 101 # maximum number of pseudo-age classes
  }
  if (mod=="GTG") return(lbspr_gtg(stock=stock, fleet=fleet, 
    lbins=lbins, xtra=xtra)) 
  if (mod=="age") return(lbspr_age(stock=stock, fleet=fleet, 
    lbins=lbins, xtra=xtra))	
}

lbspr_gtg <- function(stock=NULL, fleet=NULL, 
  lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL, warn=TRUE) {
  if (is.null(stock)) stop("stock object must be supplied")
  if (is.null(fleet)) stop("fleet object must be supplied")
  if (is.null(xtra)) xtra$NGTG <- 9
  # Assign Variables #
  # Biological Parameters 
  Linf <- stock$Linf
  CVLinf <- stock$CVLinf 
  MaxSD <- stock$MaxSD 
  SDLinf <- CVLinf * Linf # Standard Deviation of Length-at-Age # Assumed constant CV here
  MK <- stock$MK 
  L50 <- stock$L50
  L95 <- stock$L95 
  Walpha <- stock$Walpha 
  Wbeta <- stock$Wbeta 
  FecB <- stock$FecB 
  Steepness <- stock$Steepness
  Mpow <- stock$Mpow
  R0 <- stock$R0 
  
  # Fishing Fleet Parameters 
  SL50 <- fleet$SL50
  SL95 <- fleet$SL95 
  FM <- fleet$FM 
  
  # Model control parameters
  NGTG <- xtra$NGTG 
  # Length Bins 
  if (is.null(lbins$mids)) { # length mids aren't supplied 
    Max <- lbins$max 
	Min <- lbins$min 
    By <- lbins$by
    lenc <- seq(from=0, by=By, to=Max) 
    lenm <- seq(from=By/2, by=By, length.out=(length(lenc)-1))
  } else {
	By <- lbins$mids[2] - lbins$mids[1]
    lenm <- lbins$mids
	lenc <- seq(from=min(lenm)-0.5*By, by=By, length.out=length(lenm)+1)
	Max <- max(lenc) 
	Min <- min(lenc) 
	if (Min > 0) {
	  zeros <- rep(0, length(seq(from=0, to=Min-By, by=By)))
	  lenc <- c(zeros, lenc)
	  zeros2 <- zeros[1:(length(zeros)-1)]
	  lenm <- c(zeros2, lenm)
	}
	if (all(diff(lbins$mids) != diff(lbins$mids)[1])) 
	  stop("Length classes don't appear to be of equal size")
  }
  if (warn) if (Max < Linf) stop(paste0("Maximum length bin (", Max, ") can't be smaller than asymptotic size (", Linf ,")"))
  
  # Linfs of the GTGs   
  gtgLinfs <- seq(from=Linf-MaxSD*SDLinf, to=Linf+MaxSD*SDLinf, length=NGTG)
  dLinf <- gtgLinfs[2] - gtgLinfs[1]
  
  # Distribute Recruits across GTGS 
  recP <- dnorm(gtgLinfs, Linf, sd=SDLinf) / 
	sum(dnorm(gtgLinfs, Linf, sd=SDLinf)) 
  
  Weight <- Walpha * lenm^Wbeta
  # Maturity and Fecundity for each GTG 
  L50GTG <- L50/Linf * gtgLinfs # Maturity at same relative size
  L95GTG <- L95/Linf * gtgLinfs # Assumes maturity age-dependant 
  DeltaGTG <- L95GTG - L50GTG
  MatLenGTG <- sapply(seq_along(gtgLinfs), function (X) 
	1.0/(1+exp(-log(19)*(lenm-L50GTG[X])/DeltaGTG[X])))
  FecLenGTG <- MatLenGTG * lenm^FecB # Fecundity across GTGs 
  
  # Selectivity - asymptotic only at this stage - by careful with knife-edge
  SelLen <- 1.0/(1+exp(-log(19)*(lenc-(SL50+0.5*By))/ 
	((SL95+0.5*By)-(SL50+0.5*By))))
	
   # Life-History Ratios 
  MKL <- MK * (Linf/(lenc+0.5*By))^Mpow # M/K ratio for each length class
  # Matrix of MK for each GTG
  MKMat <- matrix(rep(MKL, NGTG), nrow=length(MKL), byrow=FALSE)
  FK <- FM * MK # F/K ratio 
  FKL <- FK * SelLen # F/K ratio for each length class
  ZKLMat <- MKMat + FKL # Z/K ratio (total mortality) for each GTG

  # Set Up Empty Matrices 
  # number-per-recruit at length
  NPRFished <- NPRUnfished <- matrix(0, nrow=length(lenc), ncol=NGTG)  
  NatLUF <- matrix(0, nrow=length(lenm), ncol=NGTG) # N at L unfished 
  NatLF <- matrix(0, nrow=length(lenm), ncol=NGTG) # N at L fished
  FecGTG <- matrix(0, nrow=length(lenm), ncol=NGTG) # fecundity of GTG 
  
   # Distribute Recruits into first length class
  NPRFished[1, ] <- NPRUnfished[1, ] <- recP * R0 
  for (L in 2:length(lenc)) { # Calc number at each size class
    NPRUnfished[L, ] <- NPRUnfished[L-1, ] * ((gtgLinfs-lenc[L])/(gtgLinfs-lenc[L-1]))^MKMat[L-1, ]
    NPRFished[L, ] <- NPRFished[L-1, ] * ((gtgLinfs-lenc[L])/(gtgLinfs-lenc[L-1]))^ZKLMat[L-1, ]
	ind <- gtgLinfs  < lenc[L]
	NPRFished[L, ind] <- 0
	NPRUnfished[L, ind] <- 0
  } 
  NPRUnfished[is.nan(NPRUnfished)] <- 0
  NPRFished[is.nan(NPRFished)] <- 0
  NPRUnfished[NPRUnfished < 0] <- 0
  NPRFished[NPRFished < 0] <- 0

  for (L in 1:length(lenm)) { # integrate over time in each size class
    NatLUF[L, ] <- (NPRUnfished[L,] - NPRUnfished[L+1,])/MKMat[L, ]
    NatLF[L, ] <- (NPRFished[L,] - NPRFished[L+1,])/ZKLMat[L, ]  
	FecGTG[L, ] <- NatLUF[L, ] * FecLenGTG[L, ]
  }
  
  SelLen2 <- 1.0/(1+exp(-log(19)*(lenm-SL50)/(SL95-SL50))) # Selectivity-at-Length 
  NatLV <- NatLUF * SelLen2 # Unfished Vul Pop
  NatLC <- NatLF * SelLen2 # Catch Vul Pop  
  
  # Aggregate across GTGs  
  expNatLC <- apply(NatLC, 1, sum)/sum(apply(NatLC, 1, sum))
  expNatLVUF <- apply(NatLV, 1, sum)/sum(apply(NatLV, 1, sum))
  expNatLUF <- apply(NatLUF, 1, sum)/sum(apply(NatLUF, 1, sum))
  expNatLF <- apply(NatLF, 1, sum)/sum(apply(NatLF, 1, sum))
  
  # Calc SPR
  EPR0 <- sum(NatLUF * FecLenGTG) # Eggs-per-recruit Unfished
  EPRf <- sum(NatLF * FecLenGTG) # Eggs-per-recruit Fished
  SPR <- EPRf/EPR0 
  
  # Equilibrium Relative Recruitment
  recK <- (4*Steepness)/(1-Steepness) # Goodyear compensation ratio 
  reca <- recK/EPR0
  recb <- (reca * EPR0 - 1)/(R0*EPR0)
  RelRec <- max(0, (reca * EPRf-1)/(recb*EPRf))
  if (!is.finite(RelRec)) RelRec <- 1 
  # RelRec/R0 - relative recruitment 
  YPR <- sum(NatLC  * Weight * SelLen2) * FM 
  Yield <- YPR * RelRec
  
  # Calc Unfished Fitness - not used here  
  Fit <- apply(FecGTG, 2, sum, na.rm=TRUE) # Total Fecundity per Group
  FitPR <- Fit/recP # Fitness per-recruit
  FitPR <- FitPR/median(FitPR)  
 
  # Calculate spawning-per-recruit at each size class
  SPRatsize <- cumsum(rowSums(NatLUF * FecLenGTG))
  SPRatsize <- SPRatsize/max(SPRatsize) 
  
  # Simulated length data 
  lendat <- cbind(mids=lenm, n=expNatLC)
  lendat <- lendat[lendat[,1] >= Min,]
  lendat[,2] <- lendat[,2]/sum(lendat[,2])
  
  Output <- NULL 
  Output$SPR <- stock$SPR <- SPR 
  Output$FM <- FM
  Output$wFM <- mean(FM * SelLen2)
  Output$YPR <- YPR 
  Output$Yield <- Yield 
  Output$stock <- stock 
  Output$fleet <- fleet
  Output$size <- NULL
  Output$size$cal <- expNatLC
  Output$size$palv <- expNatLVUF
  Output$size$palUF <- expNatLUF
  Output$size$palF <- expNatLF
  Output$lendat <- lendat
  Output$lenm <- lenm 
  Output$lenc <- lenc 
  Output$Winf <- Walpha * Linf^Wbeta
  Output$SelLen <- SelLen 
  Output$SelLen2 <- SelLen2 
  Output$other <- NULL
  Output$other$MKMat <- MKMat 
  Output$other$ZKLMat <- ZKLMat
  Output$other$MKL <- MKL 
  Output$other$FKL <- FKL 
  Output
}

# FIX SIZE BINS HERE 
lbspr_age <- function(stock=NULL, fleet=NULL, 
  lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL) {
  if (is.null(stock)) stop("stock object must be supplied")
  if (is.null(fleet)) stop("fleet object must be supplied")
  if (is.null(xtra)) {
	xtra$P <- 0.001 # % of cohort alive at maximum age 
	xtra$Nage <- 101 # maximum number of pseudo-age classes
  }
  # Assign Variables #
  # Biological Parameters 
  Linf <- stock$Linf
  CVLinf <- stock$CVLinf 
  MaxSD <- stock$MaxSD 
  SDLinf <- CVLinf * Linf # Standard Deviation of Length-at-Age # Assumed constant CV here
  MK <- stock$MK 
  L50 <- stock$L50
  L95 <- stock$L95 
  Walpha <- stock$Walpha 
  Wbeta <- stock$Wbeta 
  FecB <- stock$FecB 
  Steepness <- stock$Steepness
  Mpow <- stock$Mpow
  R0 <- stock$R0 
  
  # Fishing Fleet Parameters 
  SL50 <- fleet$SL50
  SL95 <- fleet$SL95 
  FM <- fleet$FM 
  
  # Model control parameters
  P <- xtra$P
  Nage <- xtra$Nage 
  
  # Length Bins 
  if (is.null(lbins$mids)) { # length mids aren't supplied 
    Max <- lbins$max 
	Min <- lbins$min 
    By <- lbins$by  
  } else {
	By <- lbins$mids[2] - lbins$mids[1]
	Max <- max(lbins$mids) + 0.5 * By 
	Min <- min(lbins$mids) 
	if (all(diff(lbins$mids) != diff(lbins$mids)[1])) 
	  stop("Length classes don't appear to be of equal size")
  }
  lenc <- seq(from=0, by=By, to=Max) 
  lenm <- seq(from=By/2, by=By, length.out=(length(lenc)-1))
  if (Max < Linf) stop("Maximum length bin can't be smaller than asymptotic size")  

  # LBSPR model with pseudo-age classes 
  x <- seq(from=0, to=1, length.out=Nage) # relative age vector
  EL <- (1-P^(x/MK)) * Linf # length at relative age 
  rLens <- EL/Linf # relative length 
  SDL <- EL * CVLinf # standard deviation of length-at-age
  Nlen <- length(lenm) 
  Prob <- matrix(NA, nrow=Nage, ncol=Nlen)
  Prob[,1] <- pnorm((lenc[2] - EL)/SDL, 0, 1) # probablility of length-at-age
  for (i in 2:(Nlen-1)) {
    Prob[,i] <- pnorm((lenc[i+1] - EL)/SDL, 0, 1) - 
		pnorm((lenc[i] - EL)/SDL, 0, 1)
  }
  Prob[,Nlen] <- 1 - pnorm((lenc[Nlen] - EL)/SDL, 0, 1)
  
  # Truncate normal dist at MaxSD 
  mat <- array(1, dim=dim(Prob))
  for (X in 1:Nage) {
    ind <- which(abs((lenm - EL[X]) /SDL[X]) >= MaxSD)
    mat[X,ind] <- 0
  }
  Prob <- Prob * mat
  SL <- 1/(1+exp(-log(19)*(lenm-SL50)/(SL95-SL50))) # Selectivity at length
  Sx <- apply(t(Prob) * SL, 2, sum) # Selectivity at relative age 
  MSX <- cumsum(Sx) / seq_along(Sx) # Mean cumulative selectivity for each age 
  Ns <- (1-rLens)^(MK+(MK*FM)*MSX) # number at relative age in population
  
  Cx <- t(t(Prob) * SL) # Conditional catch length-at-age probablilities  
  Nc <- apply(Ns * Cx, 2, sum) # 
  Pop <- apply(Ns * Prob, 2, sum)
  
  Ml <- 1/(1+exp(-log(19)*(lenm-L50)/(L95-L50))) # Maturity at length
  Ma <-  apply(t(Prob) * Ml, 2, sum) # Maturity at relative age 
  
  N0 <- (1-rLens)^MK # Unfished numbers-at-age 
  SPR <- sum(Ma * Ns * rLens^FecB)/sum(Ma * N0 * rLens^FecB)
  
  # Simulated length data 
  lendat <- cbind(mids=lenm, n=Nc)
  lendat <- lendat[lendat[,1] >= Min,]
  lendat[,2] <- lendat[,2]/sum(lendat[,2])
  
  Output <- NULL 
  Output$SPR <- stock$SPR <- SPR 
  Output$FM <- FM
  Output$wFM <- mean(FM * Sx)
  Output$stock <- stock 
  Output$fleet <- fleet
  Output$size <- NULL
  Output$size$cal <- Nc
  Output$size$palv <- apply(N0 * Cx, 2, sum) #
  Output$size$palUF <- apply(N0 * Prob, 2, sum) #
  Output$size$palF <- apply(Nc * Prob, 2, sum) #
  Output$lendat <- lendat
  Output$lenm <- lenm 
  Output$lenc <- lenc 
  Output$Winf <- Walpha * Linf^Wbeta
  Output$SelLen <- 1/(1+exp(-log(19)*(lenc-SL50)/(SL95-SL50))) 
  Output$SelLen2 <- SL 
  Output$other <- NULL
  Output
}

makedefaults <- function(inputs, defaults) {
  output <- inputs 
  innm <- names(output)
  dfnm <- names(defaults)
  extrain <- which(innm %in% dfnm == FALSE)
  missin <- which(dfnm %in% innm == FALSE)
  if (length(missin) == 0) return(output)
  if (length(missin) > 0) {
    for (X in seq_along(missin)) {
	  output[dfnm[missin[X]]] <- NA
	}
  }
  output <- data.frame(t(output))
  output <- output[,match(names(defaults), names(output))]
  NAs <- which(is.na(output))
  if (length(NAs) > 0) {
    for (X in NAs) {
	  dfval <- defaults[dfnm[X]]
	  message("Value for ", dfnm[X], " not found. Using default value of ", dfval)
	  flush.console()
	  output[dfnm[X]] <- dfval
	}
  }
  if (length(extrain) > 0) {
    message("Extra parameters supplied that will be ignored:")
	print(innm[extrain])
	flush.console()
  }
  output 
}
    
optfun <- function(trypars, LenDat, stock, mod=c("GTG", "age"), xtra=NULL) {
  mod <- match.arg(mod) 
  mids <- LenDat[,1]
  ldat <- LenDat[,2]
  fleet <- NULL
  fleet$SL50 <- exp(trypars[1]) * stock$Linf
  fleet$SL95 <- fleet$SL50  + (exp(trypars[2]) * stock$Linf)
  fleet$FM <- exp(trypars[3])
  
  if (mod == "GTG") 
    runMod <- lbspr_gtg(stock, fleet, lbins=list(mids=mids), xtra)
  if (mod == "age") 
    runMod <- lbspr_age(stock, fleet, lbins=list(mids=mids), xtra)
  
  ldat <- ldat + 1E-15 # add tiny constant for zero catches
  LenProb <- ldat/sum(ldat)
  predProb <- runMod$lendat[,2] 
  predProb <- predProb + 1E-15 # add tiny constant for zero catches
  NLL <- -sum(ldat * log(predProb/LenProb))
  # add penalty for SL50 
  trySL50 <- exp(trypars[1])
  PenVal <- NLL
  Pen <- dbeta(trySL50, shape1=5, shape2=0.01) * PenVal
  if (Pen == 0) Pen <- PenVal * trySL50
  # plot(xx, dbeta(xx, shape1=5, shape2=0.01) )
  NLL <- NLL+Pen 
     # PARs <<- (c(logL50=trySL50, SL50=fleet$SL50, SL95=fleet$SL95, FM=fleet$FM,
	 # NLL=NLL, Pen=Pen))
   # print(PARs)
  return(NLL)
}
 
doopt <- function(stock, LenDat, mod=c("GTG", "age"), 
  lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL) {
  mod <- match.arg(mod)
  if(class(LenDat) != "matrix") LenDat <- as.matrix(LenDat)

  Ncol <- ncol(LenDat)
  if (Ncol > 2) stop("Too many columns in LenDat")
  if (is.null(LenDat)) stop("Length data required")
  if (Ncol == 1) {
    Min <- lbins$min 
	Max <- lbins$max 
	By <- lbins$by 
	if (Max < stock$Linf) stop("Maximum size bin must be higher than Linf")
	Max <- ceiling(Max/By) * By 
	lbins$max <- Max 
	lenc <- seq(from=0, by=By, to=Max)
	temp <- hist(LenDat, breaks=lenc, plot=FALSE)
	ldat <- temp$counts 
	lenm <- temp$mids
  } 
  if (Ncol == 2) {
    lenm <- LenDat[,1]
	ldat <- LenDat[,2]
  }

  sSL50 <- lenm[which.max(ldat)]/stock$Linf # Starting guesses
  sDel <- 0.2 * lenm[which.max(ldat)]/stock$Linf
  sFM <- 0.5 
  Start <- log(c(sSL50, sDel, sFM))
  opt <- nlminb(Start, optfun, stock=stock, LenDat=cbind(lenm, ldat), 
	mod=mod, control= list(iter.max=300, eval.max=400, abs.tol=1E-20))
  
  newFleet <- NULL 
  newFleet$FM <- exp(opt$par[3])
  newFleet$SL50 <- exp(opt$par[1]) * stock$Linf 
  newFleet$SL95 <- newFleet$SL50 + exp(opt$par[2]) * stock$Linf
  
  
  if (mod=="GTG") runMod <- lbspr_gtg(stock=stock, fleet=newFleet, 
    lbins=lbins, xtra=xtra) 
  if (mod=="age") runMod <- lbspr_age(stock=stock, fleet=newFleet, 
    lbins=lbins, xtra=xtra)
  
  Out <- NULL 
  Out$ests <- c(FM=newFleet$FM, wFM=runMod$wFM, SL50=newFleet$SL50, 
    SL95=newFleet$SL95, SPR=runMod$SPR)
  Out$predlen <- runMod$lendat[,2] * sum(ldat)
  Out$oblen <- ldat
  Out$lenm <- lenm
  return(Out)
}





# MCMC 
 
 
 # optfun2 <- function(trypars, LenDat, stock, mod=c("GTG", "age"), xtra=NULL) {
  # -optfun( trypars, LenDat, stock, mod=mod, xtra)
 # }
 
 
# CheckServerLoad <- function() {
  # sys <- Sys.info()
  # LoadOK <- TRUE 
  # if (sys["sysname"] == "Linux") {
    # tmp <- system("uptime", intern=TRUE)
	# tmp <- " 22:26:00 up 5 days, 3:52, 1 user, load average: 0.00, 0.01, 0.05"
    # n <- nchar(tmp) 
	# Load <- as.numeric(substr(tmp, n-3, n))
	# if (Load > 0.7) LoadOK <- FALSE
  # }
  # LoadOK 
# }

# mcmcNLL <- function(modpars, stock, LenDat, mod=c("GTG", "age"), xtra=NULL) {
  # # nms <- which(names(stock) %in% names(modpars))
  # # modnms <- names(modpars)
  # modpars <- as.numeric(modpars)
  # stock$MK <- exp(modpars[1])
  # stock$Linf <- exp(modpars[2])
  # trypars <- NULL 
  # trypars[1] <- exp(modpars[3])
  # trypars[2] <- exp(modpars[4])
  # trypars[3] <- exp(modpars[5]) 

  # mod <- match.arg(mod) 
  # mids <- LenDat[,1]
  # ldat <- LenDat[,2]
  # fleet <- NULL
  # fleet$SL50 <- (trypars[1]) * stock$Linf
  # fleet$SL95 <- fleet$SL50  + ((trypars[2]) * stock$Linf)
  # fleet$FM <- (trypars[3])
  
   # # add penalty for SL50 
  # trySL50 <- (trypars[1])
  # PenVal <- 1E3
  # Pen <- dbeta(trySL50, shape1=5, shape2=0.01) * PenVal
  # if (!is.finite(Pen)) {
    # Pen <- PenVal
  # }
  # if (Pen == 0) Pen <- PenVal * trySL50
  
  # if (stock$Linf >= max(mids)) {
    # # print('this happened') 
    # return(1/stock$Linf * 1E12)
  # }
  # if (mod == "GTG") 
    # runMod <- lbspr_gtg(stock, fleet, lbins=list(mids=mids), xtra, warn=FALSE)
  # if (mod == "age") 
    # runMod <- lbspr_age(stock, fleet, lbins=list(mids=mids), xtra)
  
  # ldat <- ldat + 1E-15 # add tiny constant for zero catches
  # LenProb <- ldat/sum(ldat)
  # predProb <- runMod$lendat[,2] 
  # predProb <- predProb + 1E-15 # add tiny constant for zero catches
 
  # if (any(!(is.finite(predProb)))) {
    # print(exp(modpars))
    # stop()
  # }
  # # if (any(!(is.finite(predProb)))) {
    # # print("**")
	# # print(predProb)
	# # print(round(exp(modpars), 2))
	# # print("****")
	# # flush.console()
	# # return(PenVal)
  # # }
  # NLL <- -sum(ldat * log(predProb/LenProb))
  # NLL2 <- NLL + Pen 
  # # plot(xx, dbeta(xx, shape1=5, shape2=0.01) )
  
  # # print(c(exp(modpars), NLL=NLL))  
     # # PARs <<- (c(logL50=trySL50, SL50=fleet$SL50, SL95=fleet$SL95, FM=fleet$FM,
	 # # NLL=NLL, Pen=Pen))
   # # print(PARs)
   
  # # Priors
  # MKmu <- log(1.5) 
  # MKsd <- 0.1  
  # MKpen <- -dlnorm(stock$MK, MKmu, MKsd, log=TRUE) #* 1E2
  
  # LinfBounds <- c(165, 180)
  # Linfpen <- 0 
  # if (stock$Linf > max(LinfBounds) | stock$Linf < min(LinfBounds)) {
    # Linfpen <- max(abs(stock$Linf - LinfBounds)) * 1E2 
  # }
  # # Linfmu <- log(170)
  # # Linfsd <- 0.025
  # # # hist(rlnorm(1000, Linfmu, Linfsd))
  # # Linfpen <- -dlnorm(stock$Linf, Linfmu, Linfsd, log=TRUE) #* 1E2
  
  # out <- NLL2 + Linfpen + MKpen
  # # print(c(NLL=NLL, Pen=Pen, trySL50=trySL50, Linfpen=Linfpen, MKpen=MKpen, out=out))
  # flush.console()
  
  # return(-out) 
# }
 
 
# modpars <- NULL 
# modpars$MK <- log(1.25)
# modpars$Linf <- log(172)
# modpars$SL50 <- log(100)/stock$Linf
# modpars$del <- log(0.6)
# modpars$FM <- log(1)

 
# Start <- as.numeric(modpars )
# Start

# lower <- log(c(0.4, 165, 0.1, 0.01, 0.01))
# upper <- log(c(3.0, 180, 0.8, 0.6, 10))
# rm(rr)
# one <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # tune=1, logfun=TRUE, LenDat=LenDat, stock=stock, mod="GTG", xtra=NULL,
  # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # optim.method="L-BFGS-B", seed=101)
# two <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # tune=1, logfun=TRUE, LenDat=LenDat, stock=stock, mod="GTG", xtra=NULL,
  # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # optim.method="L-BFGS-B", seed=102)
# three <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # tune=1, logfun=TRUE, LenDat=LenDat, stock=stock, mod="GTG", xtra=NULL,
  # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # optim.method="L-BFGS-B", seed=103)
  
# rr <- rbind(one, two, three)
# rm(out)
# out <- exp(rr)
# mk <- out[,1]
# linf <- out[,2]
# SL50 <- out[,3] * linf
# SL95 <- SL50 + out[,4] * linf
# FM <- out[,5]
# par(mfrow=c(3,2))
# hist(SL50)
# hist(SL95)
# hist(FM)
# hist(mk)
# hist(linf)

# plot(out)

# plot(one)
# plot(two)
# plot(three)

# optfun(log(PARs)[1:3], LenDat, stock, mod="GTG")
  
# tt <- seq(0, 1, 0.001)
# plot(tt,dbeta(tt, shape1=5, shape2=0.01))
# stock <- NULL
# stock$Linf <- 170 
# stock$CVLinf <- 0.1
# stock$MaxSD <- 2
# stock$MK <- 1.5
# stock$L50 <- 90 
# stock$L95 <- 114
# stock$Walpha <- 0.01
# stock$Wbeta <- 3 
# stock$FecB <- 3
# stock$Steepness <- 1 
# stock$Mpow <- 0
# stock$R0 <- 1000

# rawdat <- read.csv("E:/Dropbox/Projects/SriLanka_BSC/LengthData/13CWFOnlyDEPSeaEstuaryLagoonFG141024.csv")
# LenDat <- rawdat
# tryopt <- doopt(stock, rawdat, lbins=list(min=0, max=max(rawdat), by=5), mod="age")
# plot(tryopt$lenm, tryopt$oblen)
# lines(tryopt$lenm, tryopt$predlen)
# tryopt$ests

# tryopt2 <- doopt(stock, rawdat, lbins=list(min=0, max=max(rawdat), by=5), mod="GTG")
# plot(tryopt2$lenm, tryopt2$oblen)
# lines(tryopt2$lenm, tryopt2$predlen)
# tryopt2$ests


# rm(fleet)
# rm(stock)
# t1 <- LBSPRSim(mod="GTG") 
# t2 <- lbspr_age(stock, fleet)

# t1$SPR 
# t2$SPR

# fleet <- FleetPars





# plot(t1$lenm,t1$size$cal )
# lines(t2$lenm,t2$size$cal )


# lines(t3$lendat)
# t3$SPR

# stock <- stock 
# stock$Beta <- 3 
# LBSPRSim(stock, fleet)$SPR

# stock$NGTG <- 9 
# GTGLBSPRSim(stock, fleet)$SPR

