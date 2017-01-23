#' Calculate Relative Yield, YPR, SPR, SSB, and Recruitment curves for F/M 
#'
#' A function that takes a LB_pars or LB_obj object and returns a data frame of values 
#' for relative Yield, YPR, SPR, SSB, and Recruitment at different values of F/M  
#' @param LB_obj An object of class \code{'LB_obj'} or class \code{'LB_pars'} that contains the life history and fishing information
#' @return a dataframe with YPR, Yield, SSB, Rec, and FM
#' @author A. Hordyk
#' @export
calcCurves <- function(LB_obj) {
  if (class(LB_obj) != "LB_obj" & class(LB_obj) != "LB_pars") 
    stop("LB_obj must be of class 'LB_obj' or class 'LB_pars'", call. = FALSE)
  LB_obj@SPR <- numeric()
  if (class(LB_obj) == "LB_pars") maxFM <- 4 
  if (class(LB_obj) == "LB_obj") maxFM <- LB_obj@maxFM

  FMVec <- seq(from=0, to=maxFM, by=0.05) 
  nsim <- length(FMVec)
  
  Vals <- vapply(1:nsim, function(X) {
    LB_obj@FM <- FMVec[X]
	sim <- LBSPRsim(LB_obj, verbose=FALSE)
    c(SPR=sim@SPR,
    sim@YPR,
    sim@Yield,
    sim@SSB,
    sim@RelRec)
   }, FUN.VALUE=array(0,dim=5))
 
  Vals <- t(Vals)
  colnames(Vals) <- c("SPR", "YPR", "Yield", "SSB", "Rec")
  
  Vals[,"YPR"] <- Vals[,"YPR"]/max(Vals[,"YPR"])
  Vals[,"Yield"] <- Vals[,"Yield"]/max(Vals[,"Yield"])
  Vals[,"SSB"] <- Vals[,"SSB"]/max(Vals[,"SSB"])
  Vals[,"Rec"] <- Vals[,"Rec"]/max(Vals[,"Rec"]) 
  DF <- data.frame(Vals)
  # DF$SPR[DF$SSB == 0] <- 0 
  DF$FM <- FMVec 
  DF
}
