#' Plot LBSPR model estimates
#'
#' A function that plots the estimates of the LBSPR with a smoother line
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param pars a character vectors specifying which plots to create
#' @param Lwd line width
#' @param ptCex size of plotted points
#' @param axCex size of the axis
#' @param labCex size of axis label
#' @param doSmooth apply the smoother?
#' @param incL50 include L50 line?
#' @param CIcol colour of the confidence interval bars
#' @param L50col colour of L50 line (if included)
#' @author A. Hordyk
#' @importFrom graphics abline axis hist legend lines mtext par plot points text
#' @importFrom plotrix plotCI
#' @export
plotEsts <- function(LB_obj=NULL, pars=c("Sel", "FM", "SPR"), Lwd=2.5, ptCex=1.25,
  axCex=1.45, labCex=1.55, doSmooth=TRUE, incL50=FALSE, CIcol="darkgray", L50col="gray") {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use LBSPRfit", call. = FALSE)
  if (length(LB_obj@Ests) < 1) stop("No estimates found. Use LBSPRfit", call. = FALSE)
  pars <- match.arg(pars, several.ok=TRUE)
  rawEsts <- data.frame(SL50=LB_obj@SL50, SL95=LB_obj@SL95, FM=LB_obj@FM, SPR=LB_obj@SPR)
  if (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer") {
    warning("Years must be numeric values", call. = FALSE)
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
  CIlower <- rawEsts[,1:4] - 1.96 * sqrt(LB_obj@Vars)
  CIupper <- rawEsts[,1:4] + 1.96 * sqrt(LB_obj@Vars)

  # correct bounded parameters - dodgy I know!
  CIlower[CIlower[,3]<0,3] <- 0
  CIlower[CIlower[,4]<0,4] <- 0
  CIupper[CIupper[,4]>1,4] <- 1

  CIlower[!apply(CIlower, 2, is.finite)] <- NA
  CIupper[!apply(CIupper, 2, is.finite)] <- NA
  # CIlower[!is.finite(CIlower)] <- NA
  # CIupper[!is.finite(CIupper)] <- NA

  scol <- CIcol

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
  par(mfrow=c(1,nplots), bty="l", las=1, mar=c(3,4,2,2), oma=c(2,2,0,0))
  # Selectivity
  if (doSel) {
    YLim <- c(min(CIlower[,1], na.rm=TRUE) * 0.95, max(CIupper[,2], na.rm=TRUE) * 1.05)
	YLim <- range(pretty(YLim))
    # plot(rawEsts$Years,  rawEsts$SL50, ylim=YLim, xlab="", ylab="", axes=FALSE, type="n")
	# myLeg <- legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95]),
	  # expression(L[50])), lty=c(1,2,1), lwd=Lwd, col=c("black", "black", "gray"),
	  # cex=1.75, xpd=NA, plot=FALSE)

    # YLim[2] <- 1.04*(YLim[2]+myLeg$rect$h)
    par(mfrow=c(1,nplots), bty="l", las=1, mar=c(3,4,2,2), oma=c(2,2,0,0))
	plot(rawEsts$Years,  rawEsts$SL50, ylim=YLim, xlab="", ylab="", axes=FALSE, type="n")
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SL50, ui=CIupper[,1], li=CIlower[,1], add=TRUE, scol=scol,
	   pch=19, cex=ptCex)

	axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(YLim), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SL50, lwd=Lwd)

    # points(rawEsts$Years,  rawEsts$SL95, pch=17)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SL95, ui=CIupper[,2], li=CIlower[,2], add=TRUE, pch=17, scol=scol,
	  cex=ptCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SL95, lwd=Lwd, lty=2)
    if (incL50) abline(h=LB_obj@L50, col=L50col, lwd=0.5)
	mtext(side=2, line=4, "Selectivity", cex=labCex, las=3)
	if (incL50 & doSmooth)
	  legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95]),
	  expression(L[50])), lty=c(1,2,1), lwd=Lwd, col=c("black", "black", "gray"),
	  cex=1.75, xpd=NA)
	if (!incL50 & doSmooth)
	  legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95])),
	  lty=c(1,2), lwd=Lwd, col=c("black"),  cex=1.75, xpd=NA)
	if (incL50 & !doSmooth)
	  legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95]),
	  expression(L[50])), pch=c(17, 19, 15), col=c("black", "black", L50col),
	  cex=ptCex, xpd=NA)
	if (!incL50 & !doSmooth)
	  legend("topright", bty="n", legend=c(expression(S[L50]), expression(S[L95])),
	  pch=c(19, 17), col=c("black"), cex=ptCex, xpd=NA)
  }
  # Relative Fishing Mortality
  if (doFM) {
    YMax <- max(CIupper[,3], na.rm=TRUE) * 1.05
    YMin <- min(CIlower[,3], na.rm=TRUE) * 0.95
	YLim <- round(c(YMin, YMax),2)
	YLim <- range(pretty(YLim))
    plot(rawEsts$Years,  rawEsts$FM, ylim=YLim, type="n", xlab="", ylab="", cex.axis=axCex, axes=FALSE)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$FM, ui=CIupper[,3], li=CIlower[,3], add=TRUE, scol=scol,
	  cex=ptCex, pch=19)
    axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(YLim), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$FM, lwd=Lwd)
	mtext(side=2, line=4, "F/M", cex=labCex, las=3)
  }
  # SPR
  if (doSPR) {
    plot(rawEsts$Years,  rawEsts$SPR, ylim=c(0,1), type="n", xlab="", ylab="", cex.axis=axCex, axes=FALSE)
	plotrix::plotCI(x=rawEsts$Years, y=rawEsts$SPR, ui=CIupper[,4], li=CIlower[,4], add=TRUE, scol=scol,
	 cex=ptCex, pch=19)
	axis(side=1, at=at, cex.axis=axCex)
	axis(side=2, at=pretty(c(0,1)), cex.axis=axCex)
    if(doSmooth) lines(smoothEsts$Years,  smoothEsts$SPR, lwd=Lwd)
	mtext(side=2, line=4, "SPR", cex=labCex, las=3)
  }
  mtext(outer=TRUE, side=1, line=1, "Years", cex=labCex)
}
