library(LBSPR)

context("import length data")

test_that("create new pars file", {
 expect_equal(class(new("LB_pars", msg=FALSE))[1], "LB_pars")
})

test_that("import raw length data but specify freq", {
  MyPars <- new("LB_pars", msg=FALSE)
  MyPars@Linf <- 100
  MyPars@L50 <- 66
  MyPars@L95 <- 70
  MyPars@MK <- 1.5

  MyPars@SL50 <- 50
  MyPars@SL95 <- 65
  MyPars@SPR <- 0.4
  MyPars@BinWidth <- 5

  expect_error(new("LB_lengths", LB_pars=MyPars, file="../../inst/LRaw_SingYr.csv", dataType="freq"))
})

test_that("import freq length data but specify raw", {
  MyPars <- new("LB_pars", msg=FALSE)
  MyPars@Linf <- 100
  MyPars@L50 <- 66
  MyPars@L95 <- 70
  MyPars@MK <- 1.5

  MyPars@SL50 <- 50
  MyPars@SL95 <- 65
  MyPars@SPR <- 0.4
  MyPars@BinWidth <- 5
  expect_warning(new("LB_lengths", LB_pars=MyPars, file="../../inst/LFreq_SingYr.csv", dataType="raw"))
})

test_that("import raw length data but specify freq - multiyear", {
  MyPars <- new("LB_pars", msg=FALSE)
  MyPars@Linf <- 100
  MyPars@L50 <- 66
  MyPars@L95 <- 70
  MyPars@MK <- 1.5

  MyPars@SL50 <- 50
  MyPars@SL95 <- 65
  MyPars@SPR <- 0.4
  MyPars@BinWidth <- 5
  expect_error(new("LB_lengths", LB_pars=MyPars, file="../../inst/LRaw_MultiYr.csv", dataType="freq"))
})

test_that("import freq length data but specify raw - multiyear", {
  MyPars <- new("LB_pars", msg=FALSE)
  MyPars@Linf <- 100
  MyPars@L50 <- 66
  MyPars@L95 <- 70
  MyPars@MK <- 1.5

  MyPars@SL50 <- 50
  MyPars@SL95 <- 65
  MyPars@SPR <- 0.4
  MyPars@BinWidth <- 5
  expect_warning(new("LB_lengths", LB_pars=MyPars, file="../../inst/LFreq_MultiYr.csv", dataType="raw"))
})


# Header
test_that("import freq file with header", {
  MyPars <- new("LB_pars", msg=FALSE)
  MyPars@Linf <- 100
  MyPars@L50 <- 66
  MyPars@L95 <- 70
  MyPars@MK <- 1.5

  MyPars@SL50 <- 50
  MyPars@SL95 <- 65
  MyPars@SPR <- 0.4
  MyPars@BinWidth <- 5
  expect_error(new("LB_lengths", LB_pars=MyPars, file="../../inst/LFreq_SingYrHead.csv", dataType="freq"))
})



