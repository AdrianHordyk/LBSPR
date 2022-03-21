#' Plot the maturity-at-length and selectivity-at-length curves
#'
#' A function that plots the maturity-at-length and selectivity-at-length curves
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param size.axtex size of the axis text
#' @param size.title size of axis title
#' @param size.leg size of legend text
#' @param useSmooth use the smoothed estimates?
#' @param Title optional character string for plot title
#' @return a ggplot object
#' @author A. Hordyk
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom tidyr gather
#' @importFrom RColorBrewer brewer.pal
#' @export
plotMat <- function(LB_obj=NULL, size.axtex=12, size.title=14, size.leg=12, useSmooth=TRUE, Title=NULL) {
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_pars") stop("LB_obj must be of class 'LB_obj' or class 'LB_pars'", call. = FALSE)

  if ("LMids" %in% slotNames(LB_obj)) Lens <- seq(from=LB_obj@LMids[1], to=LB_obj@LMids[length(LB_obj@LMids)], by=1)
  if (!("LMids" %in% slotNames(LB_obj))) Lens <- seq(from=0, to=LB_obj@Linf, by=1)
  # Length at Maturity
  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- "Length"
  LenMat <- 1.0/(1+exp(-log(19)*(Lens-LB_obj@L50)/(LB_obj@L95-LB_obj@L50)))
  DF <- data.frame(Lens=Lens, Dat=LenMat, Line="Maturity")
  Dat <- Proportion <- Line <- SelDat <- Year <- NULL # hack to get past CRAN check
  mplot <- ggplot(DF, aes(x=Lens, y=Dat)) +
    geom_line(aes(color="Maturity"), size=1.5) +
	scale_color_manual(values="black") +
    guides(color=guide_legend(title="")) +
	xlab(XLab) +
    ylab("Proportion") +
	theme_bw() +
	theme(axis.text=element_text(size=size.axtex),
    axis.title=element_text(size=size.title,face="bold"), legend.position="top",
	plot.title = element_text(lineheight=.8, face="bold"),
	legend.text=element_text(size=size.leg),
	legend.title=element_text(size=size.leg))

  if (class(LB_obj) == "LB_obj") {
    if (length(LB_obj@Ests)>0 & (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer")) {
      warning("Years must be numeric values", call. = FALSE)
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

    colourCount <- length(years)
    getPalette <- colorRampPalette(brewer.pal(12, "Set3"))
    cols <- rep(getPalette(min(12, colourCount)),5)[1:colourCount]

    if (length(SL50) > 0 & (length(years) == 1 | length(SL50) < 2)) {
      LenSel <- 1.0/(1+exp(-log(19)*(Lens-(SL50))/((SL95)-(SL50))))
      longSel <- data.frame(Lens=Lens, Selectivity=LenSel, Maturity=LenMat)
      longSel <- gather(longSel, "Line", "Proportion", 2:3)
      longSel$Line <- factor(longSel$Line, levels=c('Selectivity', 'Maturity'), ordered=TRUE)
      mplot <- ggplot(longSel, aes(x=Lens, y=Proportion)) +
        geom_line(aes(color=Line), size=1.5) +
        xlab(XLab) +
        ylab("Proportion") +
        guides(color=guide_legend(title="")) +
        theme_bw() +
        scale_color_manual(values = c(cols, "black")) +
        theme(axis.text=element_text(size=size.axtex),
              axis.title=element_text(size=size.title,face="bold"), legend.position="top",
              plot.title = element_text(lineheight=.8, face="bold"),
              legend.text=element_text(size=size.leg),
              legend.title=element_text(size=size.leg))
    }
    if (length(SL50) > 0 & (length(years) > 1 | length(SL50) > 1)) { # Multiple years exist
      LenSel <- sapply(1:length(years), function(X)
        1.0/(1+exp(-log(19)*(Lens-(SL50[X]))/((SL95[X])-(SL50[X])))))
      LenSel <- data.frame(LenSel, check.names=FALSE)
      colnames(LenSel) <- years
      longSel <- gather(LenSel, "Year", "SelDat")
      colourCount <- length(years)
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
