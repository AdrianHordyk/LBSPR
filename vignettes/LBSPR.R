## ---- eval=FALSE---------------------------------------------------------
#  install.packages("LBSPR")

## ---- eval=FALSE---------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("AdrianHordyk/LBSPR")

## ------------------------------------------------------------------------
library(LBSPR)

## ------------------------------------------------------------------------
MyPars <- new("LB_pars")

## ------------------------------------------------------------------------
slotNames(MyPars)

## ---- eval=FALSE---------------------------------------------------------
#  class?CLASSNAME
#  
#  class?LB_pars

## ------------------------------------------------------------------------
MyPars@Linf <- 100 
MyPars@L50 <- 66 
MyPars@L95 <- 70
MyPars@MK <- 1.5 

MyPars@SL50 <- 50 
MyPars@SL95 <- 65
MyPars@SPR <- 0.4
MyPars@BinWidth <- 5

## ------------------------------------------------------------------------
MySim <- LBSPRsim(MyPars)

## ------------------------------------------------------------------------
MyPars@BinMax <- 150
MyPars@BinMin <- 0

## ------------------------------------------------------------------------
MyPars@L_units <- "mm"

## ------------------------------------------------------------------------
MySim@SPR 

## ------------------------------------------------------------------------
MySim@FM 

## ------------------------------------------------------------------------
MyPars@SPR <- numeric() # remove value for SPR 
MyPars@FM <- 1 # set value for FM
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR at F/M = 1 

## ------------------------------------------------------------------------
MyPars@MK <- 2.0 
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR 

MyPars@MK <- 0.5
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR 

MyPars@Linf <- 120
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR 

## ------------------------------------------------------------------------
MyPars@MK <- 1.5 
MyPars@SL50 <- 10
MyPars@SL95 <- 15 
MyPars@FM <- 1 
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR 

MyPars@SL50 <- 80
MyPars@SL95 <- 85 
MySim <- LBSPRsim(MyPars)
round(MySim@SPR, 2) # SPR 

## ------------------------------------------------------------------------
MyPars@Linf <- 100 
MyPars@SL50 <- 50
MyPars@SL95 <- 55 
MyPars@FM <- numeric()
MyPars@SPR <- 0.4
MySim <- LBSPRsim(MyPars, Control=list(modtype="absel"))
MySim@FM 

MySim <- LBSPRsim(MyPars, Control=list(modtype="GTG"))
MySim@FM  # lower F/M for the GTG model

## ---- fig.width = 6, fig.height=6----------------------------------------
plotSim(MySim) 

## ---- fig.width = 6, fig.height=6----------------------------------------
plotSim(MySim, lf.type="pop")

## ---- fig.width = 4, fig.height=4----------------------------------------
plotSim(MySim, type="len.freq")

## ------------------------------------------------------------------------
MyLengths <- new("LB_lengths")
slotNames(MyLengths)

## ---- eval=FALSE---------------------------------------------------------
#  datdir <- DataDir()

## ---- include=FALSE------------------------------------------------------
datdir <- DataDir()
if (datdir == "E:\\GitRepos\\LBSPR/") # during build
  datdir <- "E:/GitRepos/LBSPR/inst"

## ------------------------------------------------------------------------
list.files(datdir, pattern=".csv")

## ------------------------------------------------------------------------
MyPars <- new("LB_pars")
MyPars@Species <- "MySpecies"
MyPars@Linf <- 100 
MyPars@L50 <- 66 
MyPars@L95 <- 70
MyPars@MK <- 1.5 
MyPars@L_units <- "mm"

## ------------------------------------------------------------------------
Len1 <- new("LB_lengths", LB_pars=MyPars, file=paste0(datdir, "/LFreq_MultiYr.csv"), 
            dataType="freq")

## ------------------------------------------------------------------------
Len2 <- new("LB_lengths", LB_pars=MyPars, file=paste0(datdir, "/LFreq_MultiYrHead.csv"), 
            dataType="freq", header=TRUE)

## ------------------------------------------------------------------------
Len3 <- new("LB_lengths", LB_pars=MyPars, file=paste0(datdir, "/LRaw_MultiYr.csv"), 
            dataType="raw")

