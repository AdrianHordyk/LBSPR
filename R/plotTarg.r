#' Plot sampled length structure against target simulated size composition
#'
#' A function that plots the observed size structure against the expected size composition at the target SPR
#'
#' @param LB_pars an object of class \code{'LB_pars'} that contains the life history and fishing information
#' @param LB_lengths an object of class \code{'LB_lengths'} that contains the observed size data
#' @param yr index for sampled length data (defaults to 1)
#' @param Cols optional character vector of colours for the plot
#' @param title character - optional title for plot
#' @param targtext logical - should the SPR target text be displayed as a subtitle?
#' @param size.axtex size of the axis text
#' @param size.title size of axis title
#' @param scales argument to ggplot2 function. Are scales shared across all facets
#' (the default, "fixed"), or do they vary across rows ("free_x"), columns ("free_y"),
#' or both rows and columns ("free")
#' @return a ggplot object
#' @author A. Hordyk
#' @importFrom ggplot2 ggplot aes geom_line geom_bar scale_color_manual guides guide_legend xlab ylab theme theme_bw element_text scale_fill_manual scale_fill_discrete ggtitle scale_alpha_manual annotate
#' @importFrom stats optimize quantile
#' @export
plotTarg <- function(LB_pars=NULL, LB_lengths=NULL, yr=1, Cols=NULL, title=NULL,
                     targtext=TRUE,
                     size.axtex=12, size.title=14,
                     scales=c("fixed", "free_x", "free_y", "free")) {
  if (class(LB_pars) != "LB_pars") stop("LB_pars must be of class 'LB_pars' Use: new('LB_lengths')", call. = FALSE)
  if (class(LB_lengths) != "LB_lengths") stop("LB_lengths must be of class 'LB_lengths'. Use: new('LB_lengths')", call. = FALSE)

  if (length(LB_pars@SPR) < 1) stop("Must supply SPR target (LB_pars@SPR)", call. = FALSE)
  if (length(LB_pars@SL50) < 1) stop("Must supply SL50 (LB_pars@SL50)", call. = FALSE)
  if (length(LB_pars@SL95) < 1) stop("Must supply SL95 (LB_pars@SL95)", call. = FALSE)

  scales <- match.arg(scales)
  LMids <- LB_lengths@LMids
  LB_pars@BinWidth <- LMids[2] - LMids[1]
  LB_pars@BinMin <- min(LMids) - 0.5 * LB_pars@BinWidth
  LB_pars@BinMax <- max(LMids) + 0.5 * LB_pars@BinWidth

  # scale predicted to sample
  ScaleCatch <- function(Scale, Sample, PredCatch) {
    ind <- which.max(Sample)
    if (ind < 1) ind <- 1
    wght <- Sample[1:ind]
    sum((((PredCatch[1:ind] * Scale) -  Sample[1:ind]) * wght)^2)
  }

  nyr <- length(yr)
  years <- as.numeric(colnames(LB_lengths@LData[,yr]))
  if (any(is.na(years))) years <- 1:nyr
  if (length(years) <1) years <- 1:nyr

  pLCatch <- matrix(NA, nrow=length(LMids), ncol=nyr)
  pLSample <- matrix(NA, nrow=length(LMids), ncol=nyr)
  for (x in 1:nyr) {
    LB_pars1 <- LB_pars
    LB_pars1@SL50 <- LB_pars@SL50[x]
    LB_pars1@SL95 <- LB_pars@SL95[x]
    LB_obj <- LBSPRsim(LB_pars1, verbose=FALSE)
    pLCatch[,x] <- LB_obj@pLCatch # predicted size comp of catch - target
    pLSample[,x] <- LB_lengths@LData[,x] #
    Scale <- optimize(ScaleCatch, interval=c(1, 1E10), Sample=pLSample[,x], PredCatch=pLCatch[,x])$minimum
    pLCatch[,x] <- pLCatch[,x] * Scale
  }

  Dat <- data.frame(LMids=LMids, pLCatch=pLCatch, Sample=pLSample)
  longDat <- tidyr::gather(Dat, "PopType", "PLength", !! 2:ncol(Dat))


  longDat[grepl("pLCatch",longDat[,2]),2] <- "pLCatch"
  longDat[grepl("Sample",longDat[,2]),2] <- "Sample"

  Title <- "Size Structure"
  Leg <- c("Target", "Sample")
  longDat$alphayr <- c(rep(1, length(LMids)*nyr), rep(0.6, length(LMids)*nyr))
  longDat$Year <-  rep(sort(rep(1:nyr, length(LMids))),2)

  longDat$Year <- factor(longDat$Year)
  levels(longDat$Year) <- years


  SPRtarg <- LB_pars@SPR
  if (SPRtarg <= 1) SPRtarg <- SPRtarg * 100

  targ <- paste0("SPR Target: ", SPRtarg, "%")

  if (length(LB_obj@L_units) > 0) {
    XLab <- paste0("Length (", LB_obj@L_units, ")")
  } else XLab <- "Length"
  PopType <- PLength <- alphayr <- NULL # hack to get past CRAN
  Plot <- ggplot(longDat, aes(x=LMids, y=PLength, fill=PopType, alpha=factor(alphayr))) +
    facet_wrap(~Year, scales=scales) +
    geom_bar(stat="identity", position = "identity") +
    xlab(XLab) +
    ylab("Frequency") +
    scale_alpha_manual(values = c("0.6"=0.6, "1"=1), guide='none') +
    theme_bw() +
    theme(axis.text=element_text(size=size.axtex),
          axis.title=element_text(size=size.title,face="bold"))
  if (all(is.null(Cols))) Plot <- Plot + scale_fill_discrete(Title, labels = Leg)
  if (!all(is.null(Cols))) Plot <- Plot + scale_fill_manual(Title, labels = Leg,
                                                            values=Cols)
  if (!is.null(title)) Plot <- Plot + ggtitle(label=title)
  if (targtext) Plot <- Plot + ggtitle(label=title, subtitle=targ)

  Plot
  
}
