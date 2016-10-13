library(LBSPR)
LB_pars <- new("LB_pars")
LB_pars@MK <- 1.5
LB_pars@Linf <- 100
LB_pars@L50 <- 50
LB_pars@L95 <- 55 
LB_pars@FM <- 0.02
LB_pars@SL50 <- 0
LB_pars@SL95 <- 1

tt <- LBSPRsim(LB_pars)
Fpop
