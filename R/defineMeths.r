check_pars <- function(object) {
  errors <- character()
  if (object@L50 >= object@L95) {
    msg <- paste("L50 is larger than L95")
    errors <- c(errors, msg)
  }
  if (object@L50 >= object@Linf) {
    msg <- paste("L50 is larger than Linf")
    errors <- c(errors, msg)
  }
  if (object@SL50 >= object@SL95) {
    msg <- paste("SL50 is larger than SL95")
    errors <- c(errors, msg)
  }
  if (object@SL50 >= object@Linf) {
    msg <- paste("SL50 is larger than Linf")
    errors <- c(errors, msg)
  }
  if (length(object@SPR) > 0) {
    if (object@SPR > 1 | object@SPR < 0) {
      msg <- paste("invalid value for SPR (must be between 0 and 1")
      errors <- c(errors, msg)
    }
  }
  if (object@Linf < 0 | object@SL50 < 0 | object@SL95 < 0 | object@MK < 0 |
    object@L50 < 0 | object@SL95 < 0 | object@CVLinf < 0 | object@FM < 0) {
	msg <- paste("negative values for some parameters")
    errors <- c(errors, msg)
  }
 if (length(errors) == 0) TRUE else errors
}

check_LB_pars <- function(object) {
  errors <- 0
  if (length(object@Linf) < 1) {
    message("Linf not set")
	errors <- errors + 1
  }
  if (length(object@MK) < 1)  {
    message("MK not set")
	errors <- errors + 1
  }
  if (length(object@L50) < 1) {
    message("L50 not set")
	errors <- errors + 1
  }
  if (length(object@L95) < 1) {
    message("L95 not set")
	errors <- errors + 1
  }
  if (length(object@SL50) < 1) {
    message("SL50 not set")
	errors <- errors + 1
  }
  if (length(object@SL95) < 1) {
    message("SL95 not set")
	errors <- errors + 1
  }
  if (length(object@FM) < 1 & length(object@SPR) < 1) {
    message("Required values for either FM or SPR")
	errors <- errors + 1
  }
  if (errors == 0) TRUE else stop(paste(errors, "errors in LB_pars"), call. = FALSE)
}


# --- LB_pars object ----
#' An S4 class containing life history and other parameters
#'
#' @slot Species Character vector of species name
#' @slot MK A length-one numeric vector for M/K ratio
#' @slot M An optional value for natural mortality (M)
#' @slot Linf A length-one numeric vector for Linf
#' @slot L_units Character describing units of length parameters
#' @slot CVLinf A length-one numeric vector for CV of length-at-age
#' @slot L50 A length-one numeric vector for length at 50\% maturity
#' @slot L95 A length-one numeric vector for length at 95\% maturity
#' @slot Walpha A length-one numeric vector for alpha parameter of length-weight relationship
#' @slot Walpha_units Character describing units for weight scaling parameter
#' @slot Wbeta A length-one numeric vector for beta parameter of length-weight relationship
#' @slot FecB A length-one numeric vector for beta parameter of length-fecundity relationship
#' @slot Steepness A length-one numeric vector for steepness of SRR
#' @slot Mpow A length-one numeric vector for M at length
#' @slot R0 A length-one numeric vector for initial number of recruits (1 for per-recruit)
#' @slot SL50 A length-one numeric vector for length at 50\% selectivity
#' @slot SL95 A length-one numeric vector for length at 95\% selectivity
#' @slot MLL Minimum legal length (inflection point)
#' @slot sdLegal Standard deviation of MLL curve
#' @slot fDisc Fraction discarded that die
#' @slot FM A length-one numeric vector for F/M ratio (note this is apical F)
#' @slot SPR A length-one numeric vector for SPR
#' @slot BinMin A length-one numeric vector for minimum length bin
#' @slot BinMax A length-one numeric vector for maximum length bin
#' @slot BinWidth A length-one numeric vector for width of length bins
#' @export
setClass("LB_pars", representation(
  Species = "character",
  MK = "numeric",
  M = "numeric",
  Linf = "numeric",
  L_units = "character",
  CVLinf = "numeric",
  L50 = "numeric",
  L95 = "numeric",
  Walpha = "numeric",
  Walpha_units = "character",
  Wbeta = "numeric",
  FecB = "numeric",
  Steepness = "numeric",
  Mpow = "numeric",
  R0 = "numeric",
  SL50 = "numeric",
  SL95 = "numeric",
  MLL = "numeric",
  sdLegal = "numeric",
  fDisc = "numeric",
  FM = "numeric",
  SPR = "vector",
  BinMin = "numeric",
  BinMax = "numeric",
  BinWidth = "numeric"
), validity=check_pars)

#' Create a new LB_pars object
#'
#' Function
#'
#' @param .Object class of object to be created
#' @param file use 'example' to create example LB_pars object. File path and name to CSV containing parameters. Import CSV currently not working
#' @param defaults use defaults for some parameters?
#' @param verbose display a message?
#' @return a object of class \code{'LB_pars'}
#' @author A. Hordyk
#' @importFrom utils read.csv
setMethod("initialize", "LB_pars", function(.Object, file="none", defaults=TRUE, verbose=TRUE){

 if (file != "none" & file.exists(file)) {
   # check that it is CSV
   # Add code for file input here
   return(.Object)
 }
 if (file == "none" & verbose)  message("A blank LB_pars object created")
 if (file == "example" & verbose)  {
    message("An example LB_pars object created")
    .Object@MK <- 1.5
    .Object@Linf <- 100
    .Object@L50 <- 66
    .Object@L95 <- 70
    .Object@SL50 <- 60
    .Object@SL95 <- 65
    .Object@FM <- 1
    .Object@CVLinf <- 0.1
    .Object@Walpha <- 0.0001
    .Object@Wbeta <- 3
    .Object@FecB <- 3
    .Object@Steepness <- 0.7
    .Object@Mpow <- 0
    .Object@R0 <- 10000
    return(.Object)
  }

 if (file != "none" && file != "example" & !(file.exists(file)) & verbose)  message("Couldn't file specified CSV file: ", file, ".  A blank LB_pars object created")
 if (!defaults)  return(.Object)
 if (defaults) {
   if (verbose) message("Default values have been set for some parameters")
   .Object@CVLinf <- 0.1
   .Object@Walpha <- 0.0001
   .Object@Wbeta <- 3
   .Object@FecB <- 3
   .Object@Steepness <- 0.7
   .Object@Mpow <- 0
   .Object@R0 <- 10000
    }
 .Object
})

# --- LB_lengths object ----
#' An S4 class containing length data
#'
#' @slot LMids A numeric vector containing the mid-points of the length bins
#' @slot LData A numeric matrix containing length data
#' @slot L_units Character descrbing units of the length measurements
#' @slot Years A numeric vector containing the year indices
#' @slot NYears A length-one numeric vector for number of years
#' @slot Elog A error log
#' @export
setClass("LB_lengths", representation(
  LMids = "vector",
  LData = "matrix",
  L_units = "character",
  Years = "vector",
  NYears = "numeric",
  Elog = "numeric"
))

#' Create a new LB_lengths object
#'
#' Function
#'
#' @param .Object class of object to be created
#' @param file file path and name to CSV containing parameters. Alternatively it can be a matrix or vector of length data
#' @param LB_pars a object of class LB_pars
#' @param dataType is the length data individual measurements (raw) or a length frequency (freq)?
#' @param header is there a header?
#' @param verbose display a message?
#' @param ... optional additional arguments passed to read.csv
#' @return a object of class \code{'LB_lengths'}
#' @author A. Hordyk
#' @importFrom utils read.csv
setMethod("initialize", "LB_lengths", function(.Object, file="none", LB_pars=NULL,
  dataType=c("raw", "freq"), header=FALSE, verbose=TRUE, ...) {

  if (class(file)== "character") {
    if(!file.exists(file)) {
      if (verbose) message("File not found. A blank LB_lengths object created")
	  .Object@Elog <- 0
      return(.Object)
    }

  if (class(LB_pars) != "LB_pars") stop("Must use a valid LB_pars object", call. = FALSE)
	if (length(LB_pars@SL50) == 0) LB_pars@SL50 <- 1
	if (length(LB_pars@SL95) == 0) LB_pars@SL95 <- 2
	if (length(LB_pars@FM) == 0) LB_pars@FM <- 1
    validObject(LB_pars)
    check_LB_pars(LB_pars)
    if (file != "none"  & file.exists(file)) {
      dat <- read.csv(file, header=header, stringsAsFactors=FALSE,check.names=FALSE, ...)
	  if (any(apply(dat, 1, class) == "character")) stop("Text in data file. Do you have header?", call. = FALSE)
	  # dat <- as.data.frame(dat)
	  # remove NAs
	  if (inherits(dat,"data.frame") | inherits(dat, "matrix")) {
	    if (ncol(dat) > 1) {
		  chkNAs <- apply(dat, 2, is.na) # check NAs
		  dat <- dat[!apply(chkNAs, 1, prod),, drop=FALSE]
		  dat <- dat[,!apply(chkNAs, 2, prod), drop=FALSE]
		}
	  }
	  if (class(dat) == "numeric" | class(dat) == "integer") {
  	    dat <- dat[!is.na(dat)]
	  }

      dataType <- match.arg(dataType)
      if (dataType == "freq") {
        .Object@LMids <- LMids <- dat[,1]
        chk <- all(diff(LMids) == median(diff(LMids)))
        if (!chk) stop("Intervals in length mid-points (first column in data file) are not consistent. Perhaps use dataType='raw'?", call. = FALSE)
        .Object@Years <- names(dat[2:ncol(dat)])
		options(warn=-1)
        .Object@Years <-  gsub("X", "", .Object@Years)
		.Object@Years <-  gsub("V", "", .Object@Years)
	    .Object@Years <- as.numeric(.Object@Years)
		if (any(is.na(.Object@Years))) .Object@Years <- 1:length(.Object@Years)
		if (.Object@Years[1] == 2) .Object@Years <- 1:length(.Object@Years)
	    options(warn=1)
        .Object@NYears <- ncol(dat) - 1
	    .Object@LData <- as.matrix(dat[, 2:ncol(dat)])
      }
      if (dataType == "raw") {
        .Object@Years <- names(dat[1:ncol(dat)])
		options(warn=-1)
        .Object@Years <-  gsub("X", "", .Object@Years)
		.Object@Years <-  gsub("V", "", .Object@Years)
	    .Object@Years <- as.numeric(.Object@Years)
		if (.Object@Years[1] == 2) .Object@Years <- 1:length(.Object@Years)
	    options(warn=1)
        .Object@NYears <- ncol(dat)
    	if (length(LB_pars@BinMax) < 1) {
          if (verbose) message("Length bin parameters (BinMax) must be set for raw data. Using defaults")
          LB_pars@BinMax <- max(LB_pars@Linf * 1.1, 1.1 * max(dat, na.rm=TRUE))
	    }
	    if (length(LB_pars@BinMin) < 1) {
	      if (verbose) message("Length bin parameters (BinMin) must be set for raw data. Using defaults")
		  LB_pars@BinMin <- min(dat, na.rm=TRUE) * 0.9
	    }
	    if (length(LB_pars@BinWidth) < 1) {
	      if (verbose) message("Length bin parameters (BinWidth) must be set for raw data. Using defaults")
		  LB_pars@BinWidth <- 1/20 * LB_pars@BinMax
	    }
	    chk <- all(diff(dat[,1]) == median(diff( dat[,1]), na.rm=TRUE))
		if (is.na(chk)) stop("There is a problem with the data file. Is there a header row?", call. = FALSE)
	    if (chk) { # a length frequency file has been uploaded
	      if (verbose) warning("It looks like you may have uploaded a length frequency file? Perhaps use dataType='freq'?")
	  	  .Object@Elog <- 2
	  	  return(.Object)
	    }
        maxL <- LB_pars@BinMax
	    minL <- LB_pars@BinMin
	    LBins <- seq(from=minL, by=LB_pars@BinWidth, to=maxL)
		if (max(LBins) < maxL) {
		  maxL <- LB_pars@BinMax <- maxL + LB_pars@BinWidth
		  LBins <- seq(from=minL, by=LB_pars@BinWidth, to=maxL)
		}
	    maxDat <- max(dat, na.rm=TRUE)
	    minDat <- min(dat, na.rm=TRUE)
	    if (maxL < maxDat) {
	      if (verbose) warning("BinMax must equal or larger than largest observation (", maxDat, ")")
	  	.Object@Elog <- 3
	  	return(.Object)
	    }
	     if (minL > minDat) {
	      if (verbose) warning("BinMin must equal or less than smallest observation (", minDat, ")")
	  	.Object@Elog <- 4
	  	return(.Object)
	    }
	    if (maxL < LB_pars@Linf) {
	      if (verbose) warning("BinMax must equal or larger than Linf")
	  	.Object@Elog <- 5
	  	return(.Object)
	    }
        .Object@LData <- sapply(1:ncol(dat), function(x) hist(dat[,x], breaks=LBins, plot=FALSE)$counts)
	    .Object@LMids <- hist(dat[,1], breaks=LBins, plot=FALSE)$mids
      }
	  .Object@Elog <- 0
	  return(.Object)
      message(file, " loaded")
    }
  }
  if(inherits(file,"matrix") | inherits(file,"numeric") | inherits(file,"integer")) {
    dat <- file
	dat <- as.data.frame(dat)
    dataType <- match.arg(dataType)
    if (dataType == "freq") {
      .Object@LMids <- LMids <- dat[,1]
      chk <- all(diff(LMids) == median(diff(LMids)),na.rm=TRUE)
      if (!chk) stop("Intervals in length mid-points (first column in data file) are not consistent. Perhaps use dataType='raw'?", call. = FALSE)
      .Object@Years <- names(dat[2:ncol(dat)])
      .Object@NYears <- ncol(dat) - 1
	  .Object@LData <- as.matrix(dat[, 2:ncol(dat)])
    }
    if (dataType == "raw") {
      .Object@Years <- names(dat[1:ncol(dat)])
      .Object@NYears <- ncol(dat)
	  if (length(LB_pars@BinMax) < 1) {
        if (verbose) message("Length bin parameters (BinMax) must be set for raw data. Using defaults")
        LB_pars@BinMax <- ceiling((max(LB_pars@Linf * 1.1, 1.1 * max(dat, na.rm=TRUE)))/0.5)*0.5
	  }
	  if (length(LB_pars@BinMin) < 1) {
	    if (verbose) message("Length bin parameters (BinMin) must be set for raw data. Using defaults")
		LB_pars@BinMin <- floor((min(dat, na.rm=TRUE) * 0.9)/0.5)*0.5
	  }
	  if (length(LB_pars@BinWidth) < 1) {
	    if (verbose) message("Length bin parameters (BinWidth) must be set for raw data. Using defaults")
		LB_pars@BinWidth <- floor((1/20 * LB_pars@BinMax)/5)*5
	  }
	  chk <- all(diff(dat[,1]) == median(diff( dat[,1]), na.rm=TRUE))
	  if (chk) { # a length frequency file has been uploaded
	    if (verbose) warning("It looks like you may have uploaded a length frequency file? Perhaps use dataType='freq'?")
		.Object@Elog <- 2
		return(.Object)
	  }
      maxL <- LB_pars@BinMax
	  minL <- LB_pars@BinMin
	  LBins <- seq(from=minL, by=LB_pars@BinWidth, to=maxL)
	  maxDat <- max(dat, na.rm=TRUE)
	  minDat <- min(dat, na.rm=TRUE)
	  if (maxL < maxDat) {
	    if (verbose) warning("BinMax must equal or larger than largest observation (", maxDat, ")")
		.Object@Elog <- 3
		return(.Object)
	  }
	   if (minL > minDat) {
	    if (verbose) warning("BinMin must equal or less than smallest observation (", minDat, ")")
		.Object@Elog <- 4
		return(.Object)
	  }
	  if (maxL < LB_pars@Linf) {
	    if (verbose) warning("BinMax must equal or larger than Linf")
		.Object@Elog <- 5
		return(.Object)
	  }
      .Object@LData <- sapply(1:ncol(dat), function(x) hist(dat[,x], breaks=LBins, plot=FALSE)$counts)
	  .Object@LMids <- hist(dat[,1], breaks=LBins, plot=FALSE)$mids
    }
    .Object@Elog <- 0
	if (length(LB_pars@L_units) > 0) .Object@L_units <- LB_pars@L_units
	return(.Object)
  }

  .Object@Elog <- 0
  .Object
})


# -- LB_obj object ----
#' An S4 class containing all parameters for the LBSPR model
#' @slot SPR The Spawning Potential Ratio
#' @slot Yield Relative yield
#' @slot YPR Yield per recruit
#' @slot SSB Spawning stock biomass (relative only)
#' @slot SSB0 Unfished spawning stock biomass
#' @slot B0 Unfished biomass
#' @slot LMids A numeric vector containing the mid-points of the length bins
#' @slot pLCatch A numeric vector containg expected proportion for each length class in the catch
#' @slot pLPop A numeric vector containg expected proportion for each length class in the population
#' @slot RelRec Relative recruitment
#' @slot Ests A matrix of estimated values
#' @slot Vars A vector of estimated variance for SL50, SL95, F/M and SPR
#' @slot NLL A numeric NLL values
#' @slot maxFM A numeric of maximum estimated F/M value (note this is apical F)
#' @slot SPRatsize A vector of cumulative SPR at length (currently only works for GTG model)
#' @slot Select Selectivities - gear, MLL, and realized (with discard mortality)
#' @slot fitLog A vector of error logs for each fit. 0 means everything is okay.
#' @export
setClass("LB_obj", representation(
  SPR = "vector",
  Yield = "vector",
  YPR = "vector",
  SSB = "vector",
  SSB0 = "vector",
  B0 = "vector",
  LMids = "vector",
  pLCatch = "matrix",
  pLPop = "array",
  RelRec = "vector",
  Ests = "matrix",
  Vars = "matrix",
  NLL = "vector",
  maxFM = "numeric",
  SPRatsize = "vector",
  Select='data.frame',
  fitLog = "vector"
  ), contains=c("LB_pars", "LB_lengths"))

#' Create a new LB_obj object
#'
#' Function
#'
#' @param .Object class of object to be created
#' @param defaults use defaults?
#' @param verbose display a message?
#' @return a object of class \code{'LB_obj'}
#' @author A. Hordyk
setMethod("initialize", "LB_obj", function(.Object, defaults=FALSE, verbose=FALSE){
  .Object
})











