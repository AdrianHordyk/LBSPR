#' Plot the size data and model fits
#'
#' A function that plots size data and the fitted LBSPR model
#'
#' @param LB_obj an object of class \code{'LB_obj'} that contains the life history and fishing information
#' @param size.axtex size of the axis text
#' @param size.title size of axis title
#' @param Title optional character string for plot title
#' @param sclae argument to ggplot2 function. Are scales shared across all facets
#' (the default, "fixed"), or do they vary across rows ("free_x"), columns ("free_y"),
#' or both rows and columns ("free")
#' @return a ggplot object
#' @author A. Hordyk
#'
#' @importFrom ggplot2 facet_wrap geom_text
#' @export
plotSize <- function(LB_obj=NULL, size.axtex=12, size.title=14, Title=NULL,
                     scales=c("fixed", "free_x", "free_y")) {
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_lengths") stop("Require LB_lengths or LB_obj object", call. = FALSE)

  if (class(LB_obj@Years) != "numeric" & class(LB_obj@Years) != "integer") {
    warning("Years must be numeric values", call. = FALSE)
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
  if (length(Ldat) < 1) stop("No length data found", call. = FALSE)
  LMids <- LB_obj@LMids
  Ldat <- data.frame(Ldat, check.names=FALSE)
  colnames(Ldat) <- as.character(Years)
  longDat <- gather(Ldat, "Year", "LBSPR_len")
  longDat$LMids <- LMids
  longDat$Year <- factor(longDat$Year, levels=colnames(Ldat))
  NCol <- ceiling(sqrt(NYrs))
  NRow <- ceiling(NYrs/NCol)
  LBSPR_len <- lab <- NULL # hack to get past CRAN check
  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- "Length"
  bplot <- ggplot(longDat, aes(x=LMids, y=LBSPR_len)) +
   facet_wrap(~Year, ncol=NCol, scales=scales) +
   geom_bar(stat="identity") +
   xlab(XLab) +
   ylab("Count") +
   theme_bw() +
   theme(axis.text=element_text(size=size.axtex),
   axis.title=element_text(size=size.title,face="bold"),
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
	  # Didn't converge
	  yrs <- unique(longDat$Year)[which(fitLog == 1)]
	  if (length(yrs) > 0) {
	    text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
	      LMids=longDat$LMids[0.5*length(longDat$LMids)],
		  LBSPR_len=0.99 * max(longDat$LBSPR_len), lab="Model didn't converge")
        bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=6)
	  }
	  # High Selectivity
	  yrs <- unique(longDat$Year)[which(fitLog == 2)]
	  if (length(yrs) > 0) {
	    text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
	      LMids=longDat$LMids[0.5*length(longDat$LMids)],
		  LBSPR_len=0.99 * max(longDat$LBSPR_len),
		  lab="Estimated selectivity\n may be unrealistically high")
        bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=6)
	  }
	  # High F/M
	  yrs <- unique(longDat$Year)[which(fitLog == 3)]
	  if (length(yrs) > 0) {
	    text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
	      LMids=longDat$LMids[0.5*length(longDat$LMids)],
		  LBSPR_len=0.99 * max(longDat$LBSPR_len),
		  lab="Estimated F/M appears\n be unrealistically high")
        bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=6)
	  }
	  # High F/M & Selectivity
	  yrs <- unique(longDat$Year)[which(fitLog == 4)]
	  if (length(yrs) > 0) {
	    text_dat <- data.frame(Year=factor(yrs, levels=levels(longDat$Year)),
	      LMids=longDat$LMids[0.5*length(longDat$LMids)],
		  LBSPR_len=0.99 * max(longDat$LBSPR_len),
		  lab="Estimated selectivity\n and F/M may be unrealistically high")
        bplot <- bplot + geom_text(data=text_dat, aes(label=lab), size=6)
	  }
	}
  }

  bplot
}
