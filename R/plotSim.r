#' General plotting function for simulated data
#'
#' A general function that plots the simulation object. Includes four different plots: 
#' equilbrium size structure, maturity and selectivity curves, growth curves, and relative Yield, YPR, SPR, SSB, and Recruitment curves.
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param type a character value indicating which plots to include: "all", "len.freq", "growth", "maturity.select", "yield.curve"
#' @param lf.type a character value indicating if the \code{catch} or \code{pop} (population) should be plotted for the length frequency
#' @param growth.type should growth be plotted as length-at-age (\code{"LAA"}) or weight-at-age (\code{"WAA"})
#' @param y.type what curves should be plotted on y-axis? \code{"SPR"}, \code{"SSB"}, \code{"Yield"}, \code{"YPR"}
#' @param x.type what curves should be plotted on x-axis? \code{"FM"}, \code{"SSB"}, \code{"SPR"}
#' @param perRec a logical to indicate if plot should be per-recruit (ignore steepness) or not (zero recruitment if SPR below replacement level)
#' @param inc.SPR a logical to indicate if SPR value should be printed in top right corner of plot
#' @param Cols optional character vector of colours for the plot
#' @param size.axtex size of the axis text
#' @param size.title size of axis title
#' @param size.SPR size of SPR text
#' @param size.leg size of legend text
#' @param inc.pts Include points on the plots?
#' @param size.pt size of the points on the plots
#' @return a ggplot object
#' @author A. Hordyk
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_bar scale_color_manual guides guide_legend xlab ylab theme theme_bw element_text scale_fill_manual scale_fill_discrete ggtitle
#' @importFrom gridExtra arrangeGrob
#' @export
plotSim <- function(LB_obj=NULL, type=c("all", "len.freq", "growth", "maturity.select", "yield.curve"),
  lf.type=c("catch", "pop"), growth.type=c("LAA", "WAA"), y.type=c("SPR", "SSB", "Yield", "YPR"), x.type=c("FM", "SSB", "SPR"),
  perRec=FALSE, inc.SPR=TRUE,
  Cols=NULL, size.axtex=12, size.title=14, size.SPR=4, size.leg=12, inc.pts=TRUE, size.pt=4) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use: LBSPRsim", call. = FALSE)
  type <- match.arg(type, several.ok=TRUE)
  growth.type <- match.arg(growth.type)
  lf.type <- match.arg(lf.type)
  LMids <- LB_obj@LMids

  pLCatch <- LB_obj@pLCatch # predicted size comp of catch
  pLPop <- LB_obj@pLPop # predicted size comp of population

  if (length(pLPop) < 1) stop("No simulated population data", call. = FALSE)
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

  if (lf.type == "catch") {
    ind <- match(round(LMids,2), round(PopSizeDat[,1],2))
    Dat <- data.frame(LMids=LMids, VulnUF=PopSizeDat[ind, "VulnUF"], pLCatch=pLCatch)
	  longDat <- gather(Dat, "PopType", "PLength", 2:ncol(Dat))
	  Title <- "Catch"
	  Leg <- c("Fished", "Unfished")
  }
  if (lf.type == "pop") {
    longDat <- gather(PopSizeDat, "PopType", "PLength", 2:ncol(PopSizeDat))
    longDat <- dplyr::filter(longDat, PopType == "PopUF" | PopType == "PopF")
	  Title <- "Population"
	  Leg <- c("Fished", "Unfished")
  }
  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- "Length"

  PopType <- PLength <- NULL # hack to get past CRAN
  LF.Plot <- ggplot(longDat, aes(x=LMids, y=PLength, fill=PopType)) +
	geom_bar(stat="identity", position = "identity") +
	xlab(XLab) +
    ylab("Relative Number") +
	theme_bw() +
	theme(axis.text=element_text(size=size.axtex),
        axis.title=element_text(size=size.title,face="bold"), 
		legend.position="top", legend.text=element_text(size=size.leg),
		legend.title=element_text(size=size.leg))
 if (all(is.null(Cols))) LF.Plot <- LF.Plot + scale_fill_discrete(Title, labels = Leg)
 if (!all(is.null(Cols))) LF.Plot <- LF.Plot + scale_fill_manual(Title, labels = Leg, values=Cols)
 if (inc.SPR) {
    LF.Plot <- LF.Plot + annotate("text", x = 0.8*max(longDat$LMids),
	  y = 0.95*max(longDat$PLength), label = paste("SPR =", round(LB_obj@SPR,2)), size=size.SPR)
 }


 # Maturity & Selectivity
 MatSel.Plot <- plotMat(LB_obj, size.axtex=size.axtex, size.title=size.title, 
   size.leg=size.leg, useSmooth=TRUE, Title=NULL)
 
 Age <- Length <- Weight <- Y <- Reference <- FM <- Value <- Type  <- NULL # hack to get past CRAN check
 # Length at Age
 P <- 0.01
 x <- seq(from=0, to=1, length.out=200) # relative age vector
 EL <- (1-P^(x/LB_obj@MK )) *  LB_obj@Linf # length at relative age

 MaxAge <- 1
 if (length(LB_obj@M) > 0) MaxAge <- ceiling(-log(P)/LB_obj@M)

 A50 <- x[min(which(EL >= LB_obj@L50))] * MaxAge
 SA50 <- x[min(which(EL >= LB_obj@SL50))] * MaxAge

 matdat <- data.frame(X=c(A50, SA50),
   Y=c(LB_obj@L50, LB_obj@SL50), Reference=c("Maturity", "Selectivity"))
 matdat2 <- data.frame(X=c(A50, SA50),
   Y=c(LB_obj@Walpha*LB_obj@L50^LB_obj@Wbeta, LB_obj@Walpha*LB_obj@SL50^LB_obj@Wbeta),
   Reference=c("Maturity", "Selectivity"))

 lendat <- data.frame(Age=x*MaxAge, Length=EL)
 lendat2 <- data.frame(Age=x*MaxAge, Weight=LB_obj@Walpha*EL^LB_obj@Wbeta)

 if (MaxAge == 1) XLab <- "Relative Age\n"
 if (MaxAge != 1) XLab <- "Age"
 if (growth.type=="LAA") {
   if (length(LB_obj@L_units) > 0) {
     YLab <- paste0("Length (", LB_obj@L_units, ")")
    } else YLab <- "Length"
 }
 if (growth.type=="WAA") {
   if (length(LB_obj@Walpha_units) > 0) {
     YLab <- paste0("Weight (", LB_obj@Walpha_units, ")")
    } else YLab <- "Weight"
 }

 LaA.Plot1 <- ggplot(lendat, aes(x=Age, y=Length)) + geom_line(size=1.5) +
   theme_bw() +
   guides(color=guide_legend(title="")) +
   theme(axis.text=element_text(size=size.axtex),
        axis.title=element_text(size=size.title,face="bold"), 
		legend.position="top", legend.text=element_text(size=size.leg)) +
   xlab(XLab) + ylab(YLab)
  if (inc.pts) LaA.Plot1 <-  LaA.Plot1 +  geom_point(data=matdat, aes(x=X, y=Y, colour=Reference), size=size.pt) 
 WaA.Plot1 <- ggplot(lendat2, aes(x=Age, y=Weight)) + geom_line(size=1.5) +
   theme_bw() +
   guides(color=guide_legend(title="")) +
   theme(axis.text=element_text(size=size.axtex),
        axis.title=element_text(size=size.title,face="bold"), legend.position="top", 
		legend.text=element_text(size=size.leg)) +
   xlab(XLab) + ylab(YLab)
  if (inc.pts) WaA.Plot1 <- WaA.Plot1 +  geom_point(data=matdat2, aes(x=X, y=Y, colour=Reference), size=size.pt) 
  if (growth.type=="LAA") LaA.Plot <- LaA.Plot1
  if (growth.type=="WAA") LaA.Plot <- WaA.Plot1

 # SPR versus F &  # Yield versus F
 if ("yield.curve" %in% type | "all" %in% type) {
   if ("Yield" %in% y.type & perRec) y.type <- y.type[!y.type == "Yield"]
   if ("YPR" %in% y.type & !perRec) y.type <- y.type[!y.type == "YPR"]
   Yield.Plot <- plotCurves(LB_obj, X=x.type, 
     Y=y.type, size.axtex=size.axtex, size.title=size.title, 
     size.pt=size.pt, inc.pts=inc.pts, size.leg=size.leg)
 }

 
 L <- list()
 if ("all" %in% type) {
   L[[1]] <- LF.Plot
   L[[2]] <- LaA.Plot
   L[[3]] <- MatSel.Plot
   L[[4]] <- Yield.Plot
   plot(arrangeGrob(grobs=L, layout_matrix=matrix(1:length(L), ncol=2, nrow=2)))
 } else {
   for (X in seq_along(type)) {
     if ("len.freq" %in% type[X]) L[[X]] <- LF.Plot
	 if ("growth" %in% type[X]) L[[X]] <- LaA.Plot
	 if ("maturity.select" %in% type[X]) L[[X]] <- MatSel.Plot
	 if ("yield.curve" %in% type[X]) L[[X]] <- Yield.Plot
   }

   plot(arrangeGrob(grobs=L, layout_matrix=matrix(1:length(L), ncol=length(L), nrow=1)))
 }

}
