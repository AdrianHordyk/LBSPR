#' Plot the Relative Yield, YPR, SPR, SSB, and Recruitment curves
#'
#' A function that plots the Relative Yield, YPR, SPR, SSB, and Recruitment curves
#'
#' @param LB_obj An object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param X a character value indicating what to plot on the x-axis: F/M (\code{"FM"}) or SSB (\code{"SSB"})
#' @param Y a character value indicating what to plot on the y-axis: \code{SPR}, \code{SSB}, \code{Yield}, \code{YPR} or \code{Rec}(multiple okay)
#' @param size.axtex size of the axis text
#' @param size.title size of axis title
#' @param size.leg size of legend text 
#' @param inc.pts Include points on the plots?
#' @param size.pt size of the points on the plots
#' @return a ggplot object 
#' @author A. Hordyk
#' @export

plotCurves <- function(LB_obj, X=c("FM", "SSB", "SPR"), 
  Y=c("SPR", "SSB", "Yield"), size.axtex=12, size.title=14, size.leg=12,
  size.pt=4, inc.pts=TRUE) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'", call. = FALSE)  
  X <- match.arg(X)
  Y <- match.arg(Y, choices=c("SPR", "SSB", "Yield", "YPR", "Rec"), several.ok=TRUE)
  if (all(X == Y)) stop("X is identical to Y", call. = FALSE)
  Y <- Y[!Y %in% X]
  if (length(LB_obj@SPR) > 1) {
    if (inc.pts) message("Multiple values in input object. Using last parameter values. \n Not plotting points")
    LB_obj@SPR <- LB_obj@SPR[length(LB_obj@SPR)] 
	LB_obj@FM <- LB_obj@FM[length(LB_obj@FM)] 
	LB_obj@SL50 <- LB_obj@SL50[length(LB_obj@SL50)]
	LB_obj@SL95 <- LB_obj@SL95[length(LB_obj@SL95)]
	inc.pts <- FALSE
  }

  Vals <- calcCurves(LB_obj)
  
  if (length(LB_obj@FM) > 0) {
    if (max(Vals$FM) >= LB_obj@FM) tempVal <- min(which(Vals$FM >= LB_obj@FM)) 
	if (max(Vals$FM) < LB_obj@FM) {
	  tempVal <- NULL
	  inc.pts <- FALSE 
	}
	xF <- min(LB_obj@FM, LB_obj@maxFM)
  }
  if (length(LB_obj@FM) == 0) { 
    tempVal <- NULL
	inc.pts <- FALSE 
	xF <- NULL
  }
  
  yieldp <- Vals[tempVal,"Yield"]
  YPRp <- Vals[tempVal,"YPR"]
  SSBp <- Vals[tempVal,"SSB"]
  Recp <- Vals[tempVal,"Rec"]  
  
  t1 <- c("SPR", "SSB", "Yield", "YPR", "Rec")
  t2 <- c("SPR", "SSB/SSB0", "Relative Yield", "Relative Yield-per-Recruit", "Recruitment")
  t3 <- t2[t1 %in% Y]
  t4 <- t1[t1 %in% Y]

  if (length(LB_obj@Steepness) < 1) inc.pts <- FALSE 
  if (length(LB_obj@Steepness) == 1) pointdat2 <- data.frame(X=0.2, Y=LB_obj@Steepness, Type2="Recruitment") 
	
  xdat <- dplyr::select(Vals, dplyr::starts_with(X))  
  dropN <- names(Vals)[!names(Vals) %in% X & !names(Vals) %in% Y]
  keepN <- names(Vals)[!names(Vals) %in% dropN]
  Vals2 <- dplyr::select(Vals, dplyr::one_of(keepN))
  
  T1 <- value <- key2 <- Type <- Type2 <- NULL # hack from CRAN Check
  if (length(c(xF, LB_obj@SPR, SSBp, yieldp, YPRp, Recp)) != 6) inc.pts <- FALSE 
  if (length(c(xF, LB_obj@SPR, SSBp, yieldp, YPRp, Recp)) == 6) {
    SPRp <- LB_obj@SPR
	if (SPRp < min(Vals$SPR)) inc.pts <- FALSE
    pointdat <- data.frame(FM=xF, 
      Y=c(SPRp, SSBp, yieldp, YPRp, Recp),
      Type=t2, T1=t1)	
    pointdat <- dplyr::filter(pointdat, T1 %in% keepN)	  
  }

  ydat <- dplyr::select(Vals2, -dplyr::starts_with(X))
  ydat2 <- tidyr::gather(ydat)
  dat <- data.frame(X=xdat, ydat2)
  
  dat$key <- factor(dat$key, levels=Y)
  dat$key2 <- factor(t3, levels=t3)
  for (tt in 1:length(t1)) {
   if (t1[tt] %in% Y) dat$key2[dat$key == t1[tt]] <- t2[tt]
  }	
  
  if (X == "FM") XLab <- "Relative Fishing Mortality \n(F/M)"
  if (X == "SSB") XLab <- "Spawning Stock Biomass"
  if (X == "SPR") XLab <- "Spawning Potential Ratio"
  if (X == "FM") xmax <- LB_obj@maxFM
  if (X != "FM") xmax <- 1 
  xmin <- 0 
  Pout <- ggplot(dat, aes(x=get(X), y=value, color=key2)) + geom_line(size=1.5) +
     theme_bw() +
     guides(color=guide_legend(title="")) +
     theme(axis.text=element_text(size=size.axtex),
          axis.title=element_text(size=size.title,face="bold"), 
		  legend.position="top", legend.text=element_text(size=size.leg)) +
     xlab(XLab) + ylab("Proportion") + ggplot2::ylim(0, 1) + ggplot2::xlim(xmin, xmax)
  if (inc.pts & X == "FM") Pout <- Pout + geom_point(data=pointdat, aes(x=get(X), 
    y=Y, color=Type), size=size.pt)
  if (inc.pts & X == "SSB" & "Rec" %in% Y) Pout <- Pout + geom_point(data=pointdat2, aes(x=X, 
    y=Y, color=Type2), size=size.pt)	
  Pout

  # out <- list()
  # out$data <- dat
  # if (exists("pointdat")) out$points <- pointdat
  # if (exists("pointdat2")) out$rec_point <- pointdat2
  # invisible(out)
}

