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
#' @param limcol colour for SPR Limit (hex; default is red)
#' @param targcol colour for SPR target (hex; default is orange)
#' @param abtgcol colour for above SPR target (hex; default is green)
#' @param labcol optional fixed colour for estimated SPR label
#' @param bgcol colour for the background
#' @param labcex size for the estimated SPR label
#' @param texcex size for estimated other labels
#' @author A. Hordyk
#' @importFrom plotrix draw.circle draw.ellipse draw.radial.line radialtext
#' @export
plotSPRCirc <- function(LB_obj=NULL, SPRTarg=0.4, SPRLim=0.2, useSmooth=TRUE,
  Title=FALSE, Leg=TRUE, limcol="#ff1919", targcol="#ffb732", abtgcol="#32ff36",
  labcol=NULL, bgcol="#FAFAFA", labcex=2, texcex=1.3) {
  if (class(LB_obj) != "LB_obj") stop("LB_obj must be of class 'LB_obj'. Use LBSPRfit", call. = FALSE)

  par(mfrow=c(1,1), mar=c(1,2,3,2), oma=c(1,1,1,1))
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
  # limcol <- "#ff1919"
  # targcol <- "#ffb732"
  # abtgcol <- "#32ff36"
  nv <- 200
  # texcex <- 1.3
  # texcex2 <- 2
  # Circle

  draw.circle(x=x, y=x, radius=a, border=bgcol, col=bgcol, nv=nv)
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
  if (class(labcol) == "character") textcol <- labcol
  radialtext(paste0(round(spr,2)*100, "%"),
    center=c(x,x), start=x-0.2, middle=1, end=NA, deg=ang,  expand=0, stretch=1,
	nice=TRUE, cex=labcex, xpd=NA, col=textcol)

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
