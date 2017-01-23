#' Kalman filter and Rauch-Tung-Striebel smoother
#'
#' A function that applies a filter and smoother to estimates
#'
#' @param RawEsts a vector of estimated values
#' @param R variance of sampling noise
#' @param Q variance of random walk increments
#' @param Int covariance of initial uncertainty
#' @return a vector of smoothed values
#'
#' @export
FilterSmooth <- function(RawEsts, R=1, Q=0.1, Int=100) {
  # Modified from \url{"http://read.pudn.com/downloads88/ebook/336360/Kalman%20Filtering%20Theory%20and%20Practice,%20Using%20MATLAB/CHAPTER4/RTSvsKF.m__.htm"}
  Ppred <-  rep(Int, length(RawEsts))
  nNA <- sum(is.na(RawEsts))
  while(nNA > 0) { # NAs get replaced with last non-NA
    RawEsts[is.na(RawEsts)] <- RawEsts[which(is.na(RawEsts))-1]
    nNA <- sum(is.na(RawEsts))
  }
  Pcorr <- xcorr <- xpred <- rep(0, length(RawEsts))
  # Kalman Filter
  for (X in 1:length(Ppred)) {
    if (X !=1) {
	  Ppred[X] <- Pcorr[X-1] + Q
	  xpred[X] <- xcorr[X-1]
	}
	W <- Ppred[X]/(Ppred[X] + R)
	xcorr[X] <- xpred[X] + W * (RawEsts[X] - xpred[X]) # Kalman filter estimate
	Pcorr[X] <- Ppred[X] - W * Ppred[X]
  }
  # Smoother
  xsmooth <- xcorr
  for (X in (length(Pcorr)-1):1) {
    A <- Pcorr[X]/Ppred[X+1]
	xsmooth[X] <- xsmooth[X] + A*(xsmooth[X+1] - xpred[X+1])
  }
  return(xsmooth)
}
