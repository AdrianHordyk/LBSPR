varSPR <- function(MLEs, varcov, LB_pars) {
  var <- diag(varcov)
  vars <- c("lSL50", "ldL", "lFM")
  p1 <- 0
  for (i in seq_along(MLEs)) p1 <- p1 + derivative(dSPR, x=MLEs[i], var=vars[i],
    LB_pars=LB_pars)^2 * var[i]

  p2 <- derivative(dSPR, x=MLEs[1], var=vars[1],  LB_pars=LB_pars) *
    derivative(dSPR, x=MLEs[2], var=vars[2],  LB_pars=LB_pars) * varcov[1,2]

  p3 <- derivative(dSPR, x=MLEs[1], var=vars[1],  LB_pars=LB_pars) *
    derivative(dSPR, x=MLEs[3], var=vars[3],  LB_pars=LB_pars) * varcov[1,3]

  p4 <- derivative(dSPR, x=MLEs[3], var=vars[3],  LB_pars=LB_pars) *
    derivative(dSPR, x=MLEs[2], var=vars[2],  LB_pars=LB_pars) * varcov[3,2]
  p1 + p2 + p3 + p4
}

dSPR <- function(x, LB_pars, var=c("lSL50", "ldL", "lFM"), Control=NULL) {
  lvar <- match.arg(var)
  ex <- exp(x)
  if (lvar == "lFM") myslot <- "FM"
  if (lvar == "lSL50") {
    myslot <- "SL50"
	ex <- ex * LB_pars@Linf
  }
  if (lvar == "ldL") {
    myslot <- "SL95"
	ex <-  ex * LB_pars@Linf + LB_pars@L50
  }
  slot(LB_pars, myslot) <- ex
  temp <- LBSPRsim_(LB_pars, Control=Control, verbose=FALSE, doCheck=FALSE)
  temp@SPR
}

# From http://blog.quantitations.com/tutorial/2013/02/12/numerical-derivatives-in-r/
derivative <- function(f, x, ..., order = 1, delta = 0.01, sig = 6) {
    # Numerically computes the specified order derivative of f at x
    vals <- matrix(NA, nrow = order + 1, ncol = order + 1)
    grid <- seq(x - delta/2, x + delta/2, length.out = order + 1)
    vals[1, ] <- sapply(grid, f, ...) - f(x, ...)
    for (i in 2:(order + 1)) {
        for (j in 1:(order - i + 2)) {
            stepsize <- grid[i + j - 1] - grid[i + j - 2]
            vals[i, j] <- (vals[i - 1, j + 1] - vals[i - 1, j])/stepsize
        }
    }
    return(signif(vals[order + 1, 1], sig))
}
