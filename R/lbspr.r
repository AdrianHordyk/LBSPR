# library(LBSPR)


# library(LBSPR)
# LB_pars <- MyPars <- new("LB_pars")
# slotNames(MyPars)


# LB_pars@Linf <- 100 
# LB_pars@L50 <- 66 
# LB_pars@L95 <- 70
# LB_pars@MK <- 1.5 

# LB_pars@SL50 <- 50 
# LB_pars@SL95 <- 65
# LB_pars@SPR <- 0.4
# LB_pars@BinWidth <- 5

# MySim <- LBSPRsim(LB_pars, Control=list(modtype="GTG"))
# plotSim(MySim)

# plotMat(MySim)


# Years <- 2010:(2010+NYears-1)
# ### Length Measurements ###
# # 1 Year with Header 
# lstYr <- LenRaw[,NYears]
# lstYr <- lstYr[!is.na(lstYr)]
# Dat <- data.frame("2015"=lstYr)
# write.table(Dat, file="LRaw_SingYrHead.csv", sep=",", row.names=FALSE, col.names=TRUE)
# # 1 Year no header
# write.table(Dat, file="LRaw_SingYr.csv", sep=",", row.names=FALSE, col.names=FALSE)
# # Multi Year with Header
# Dat <- LenRaw
# Dat <- as.data.frame(Dat)
# colnames(Dat) <- Years
# write.table(Dat, file="LRaw_MultiYrHead.csv", sep=",", row.names=FALSE, col.names=TRUE)

# ### Length Frequencies ###
# # 1 Year with Header 
# lstYr <- LenFreq[,c(1,NYears)]
# Dat <- as.data.frame(lstYr)
# colnames(Dat) <- c("Length", "2015")
# write.table(Dat, file="LFreq_SingYrHead.csv", sep=",", row.names=FALSE, col.names=TRUE)

# # 1 Year no header
# write.table(Dat, file="LFreq_SingYr.csv", sep=",", row.names=FALSE, col.names=FALSE)

# # Multi Year with Header

# Dat <- as.data.frame(LenFreq)
# colnames(Dat) <- c("Length", Years)
# write.table(Dat, file="LFreq_MultiYrHead.csv", sep=",", row.names=FALSE, col.names=TRUE)





# LB_pars@BinWidth <- 5
# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, 
  # file="E:/Dropbox/Projects/SriLanka_BSC/LengthData/13CWFOnlyDEPSeaEstuaryLagoonFG141024.csv", 
  # dataType="raw", header=FALSE)

# plotSize(LB_lengths)
# rr <- LBSPRfit(LB_pars, LB_lengths)
# plotSize(rr)

# rr

# # library(ggplot2)
# # library(tidyr)
# # library(RColorBrewer)
# LB_pars <- new("LB_pars")
# LB_pars@Linf <- 100 
# LB_pars@L50 <- 66 
# LB_pars@L95 <- 70
# LB_pars@MK <- 1.5 


# read.csv(paste0(DataDir(), "LFreq_MultiYr.csv"))

# datdir <- DataDir()
# files <- list.files(datdir, pattern=".csv")

# new("LB_lengths", file=files[1], dataType="raw")

# .Object <-  new("LB_lengths")

# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="lendat.csv", dataType="freq")
# file <- cbind(LB_lengths@LMids, LB_lengths@LData)
# lendat <- new("LB_lengths", file=file, LB_pars, dataType="freq")

# plotSize(lendat)
# LB_obj <- LBSPRfit(LB_pars, LB_lengths)
# plotSize(LB_obj)

# plotMat(LB_pars)

# LB_pars@BinMax <- 140
# LB_pars@BinMin <- 0 
# LB_pars@BinWidth <- 5 
# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="rand.csv", dataType="raw")
# file <- cbind(runif(100, 0 ,120))
# mydat <- new("LB_lengths", file=file, LB_pars, dataType="raw")
# plotSize(mydat)

# LB_obj <- LBSPRsim(LB_pars, Control=list(modtype="GTG"))
# plotMat(LB_obj)

# plotSim(LB_obj)
# plotSim(LB_obj, type="Pop")


# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="lendat.csv", dataType="freq")
# plotSize(LB_lengths)

# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="rand.csv", dataType="raw")
# plotSize(LB_lengths)

# LB_lengths@Years <- 1990:1999
# plotSize(LB_lengths)

 # LB_lengths@LMids  <- LB_lengths@LMids[10:length(LB_lengths@LMids)]
 # LB_lengths@LData <- LB_lengths@LData[10:nrow(LB_lengths@LData),]


# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="lendat.csv", dataType="freq")
# LB_obj <- LBSPRfit(LB_pars, LB_lengths)
# plotSize(LB_obj)
# plotEsts(LB_obj)


# plotMat(LB_obj)
# plotMat(LB_obj, useSmooth=FALSE)
# plotSize(LB_obj)
# plotEsts(LB_obj)
# plotSPRCirc(LB_obj)

# # LBSPR Functions 

# library(ggplot2)
# library(tidyr)
# library(gridExtra)
# library(RColorBrewer)
# library(plotrix)
# library(grid)
# library(gridBase)
# library(dplyr)
# library(formattable)

# LB_pars <- new("LB_pars")
# LB_pars@Linf <- 100 
# LB_pars@L50 <- 66 
# LB_pars@L95 <- 70
# LB_pars@MK <- 1.5 
# LB_pars@FM <- 1 
# LB_pars@SL50 <- 50
# LB_pars@SL95 <- 55 
# LB_pars@SPR <- 0.5 

# ## Simulate Size Composition 
# LB_pars@BinWidth <- 5
# LB_obj <- LBSPRsim(LB_pars, Control=list(modtype="GTG"))
# plotMat(LB_obj)

# plotSim(LB_obj)
# plotSim(LB_obj, type="Pop")
# LB_obj@SPR 

# LB_obj <- LBSPRsim(LB_pars, Control=list(modtype="absel"))
# plotSize(LB_lengths)
# plotSize(LB_obj)


# plotSim(LB_obj)
# plotSim(LB_obj, type="Pop")
# LB_obj@SPR 

# # add plotting functions for simulations 

# ## Assessment 
# # Import length data 
# new("LB_lengths", LB_pars=LB_pars)

# LB_pars@BinMax <- 140 
# LB_pars@BinMin <- 0
# LB_pars@BinWidth <- 5
# LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="lendat.csv", dataType="freq")
# # LB_lengths <- new("LB_lengths", LB_pars=LB_pars, file="rand.csv", dataType="raw")


# # Next - fit model to the size data 
# LBSPRfit(LB_pars, LB_lengths, Control=list(modtype="absel"))
# LB_lengths@Years <- 1990:1999
# LB_obj <- LBSPRfit(LB_pars, LB_lengths)
# plotMat(LB_obj)
# plotSize(LB_obj)

# LB_obj <- LB_pars

# LB_obj <- LBSPRfit(LB_pars, LB_lengths, yrs=5:1)
# plotEsts(LB_obj)
# plotEsts(LB_obj, pars=c("Sel", "FM"))
# plotEsts(LB_obj, pars=c("SPR"), labCex=2)


# plot(fit@SL50, ylim=c(55, 65))
# lines(fit@Ests[,"SL50"])

# LB_obj@Ests[,"SPR"] <- 0.8
# plotSPRCirc(LB_obj, SPRTarg=0.4)


# plot(fit@SPR, ylim=c(0,1))
# lines(fit@Ests[,"SPR"])


# LBSPRfit(LB_pars, LB_lengths, yrs=5)


# mylendat <- new("LBSPR_len", file="LBSPR_len.csv", dataType="freq")
# mylendat <- new("LBSPR_len", file="rand.csv", dataType="raw")

# ests <- matrix(c(fit@SPR, fit@FM, fit@SL50, fit@SL95), ncol=4)

# plot(ests[,1])
# smoths <- apply(ests, 2, FilterSmooth)
# lines(smoths[,1])












# LB_pars@BinMax <- 125 
# LB_pars@BinWidth 

# # Things to do 
# - .LBSPRSim - add check for name - add default if nothing there 


 # if (file == "example") {
  # if(msg) message("An example LBSPR_bio object created") 
  # .Object@Name <- "Example Species"
  # .Object@MK <- 1.5 
  # .Object@Linf <- 100 
  # .Object@CVLinf <- 0.1 
  # .Object@L50 <- 66
  # .Object@L95 <- 70 
  # .Object@Walpha = 0.001
  # .Object@Wbeta = 3
  # .Object@FecB <- 3 
  # .Object@Steepness <- 0.7 
  # .Object@Mpow <- 0 
  # .Object@R0 <- 1000
 # }
 
# LBSPR_bio <- new("LBSPR_bio", "example")
# LBSPR_len <- new("LBSPR_len", "example")
# LBSPR_sim <- new("LBSPR_sim", "example")

# LBSPR_obj <- new("LBSPR_obj", LBSPR_bio)
# LBSPR_obj <- new("LBSPR_obj", LBSPR_bio, LBSPR_len, LBSPR_sim)

# LBSPRsim(LBSPR_obj)

# rr <- LBSPRfit(LBSPR_obj)

# # Plotting functions 
# # validations 





  # validObject(LBSPR_bio)
  # validObject(LBSPR_sim) # add these later to LBSPR_obj 

 # # Example - take biological parameters and simulate with those 

# # Simulate 
# mysim <- LBSPR_sim(LBSPR_bio, myexploit, LBSPR_len)
# LBSPR_mod <- LBSPR_fit(LBSPR_bio, LBSPR_len, yrs=NA)
# LBSPR_plot(mybio, LBSPR_len)

# LBSPR_plot(mybio, LBSPR_len, LBSPR_mod)


# df <- data.frame(
  # group = c("Male", "Female", "Child"),
  # value = c(25, 25, 50)
  # )
# df

# df <- data.frame(
  # group = c("Limit", "Target", "Estimate", "rest"),
  # value = c(20, 20, 30, 30)
  # )
# df
# bp <-  ggplot(df, aes(x="", y=value, fill=group))+ geom_bar(width = 1, stat = "identity")
# pie <- bp + coord_polar("y", start=0)
# pie


# circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    # r = diameter / 2
    # tt <- seq(0,2*pi,length.out = npoints)
    # xx <- center[1] + r * cos(tt)
    # yy <- center[2] + r * sin(tt)
    # return(data.frame(x = xx, y = yy))
# }


# dat <- circleFun(c(0,0), 2, npoints = 5000)

# limit <- 0.05

# lim <- (limit*360) * pi / 180
# x1 <- sin(lim)
# y1 <- cos(lim)

# Plot <- ggplot(dat,aes(x,y)) + geom_path()
# limdat <- dat 
# if (limit <=0.50) {
  # limdat <- limdat[limdat$y >=y1 & limdat$x >= 0,] 
  # limdat <- rbind(limdat, cbind(x=c(0), y=c(0)))
  # limdat$SPR <- "Limit"
  # Plot <- Plot + geom_polygon(data=limdat, aes(fill=SPR))
# }
# if (limit > 0.50) {
  # limdat <- limdat[limdat$y >=-1 & limdat$x >= 0,] 
  # limdat$SPR <- "Limit"
  # limdat2 <- dat[dat$y <=y1 & dat$x <= 0,] 
  # limdat2 <- rbind(limdat2, cbind(x=c(0), y=c(0)))
  # limdat2$SPR <- "Limit"
  # Plot <- Plot + geom_polygon(data=limdat, aes(fill=SPR)) + geom_polygon(data=limdat2, aes(fill=SPR))
# }

# Plot


# limdat2$SPR <- "Limit"
 # + geom_polygon(data=limdat, aes(fill=SPR)) +
  # geom_polygon(data=limdat2, aes(fill=SPR))



# geom_polygon(data=dat3, aes(fill=type)) + theme_bw() 
  

# dat$type <- "rest"
# dat2 <- rbind(dat, data.frame(x=c(0, 0.5, 0), y=c(0, 0, 0.5)))
# dat2$type <- "limit"
# dat2 <- dat2[dat2$x>=0 & dat2$y<=0.5 & dat2$y>=0,]

# dat3 <- rbind(dat, data.frame(x=c(0, -0.5, 0), y=c(0, 0, -0.5), type=rep("rest", 3)))
# dat3$type <- "target"
# dat3 <- dat3[dat3$x>=0 & dat3$y>=-0.5 & dat3$y<=0,]

# #geom_path will do open circles, geom_polygon will do filled circles
# ggplot(dat,aes(x,y)) + geom_path()+ geom_polygon(data=dat2, aes(fill=type)) +
  # geom_polygon(data=dat3, aes(fill=type)) + theme_bw() 
  
  
  
  
  # +
    # theme(axis.line=element_blank(),axis.text.x=element_blank(),
          # axis.text.y=element_blank(),axis.ticks=element_blank(),
          # axis.title.x=element_blank(),
          # axis.title.y=element_blank(),
          # panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          # panel.grid.minor=element_blank(),plot.background=element_blank())


		  
		  
		  
# a <- 6.5
# draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(max(lim, ang), ang2),
  	    # col=limcol, arc.only=FALSE, border=FALSE, nv=nv)
		
# draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(lim, ang2),
  	    # col=limcol, arc.only=FALSE, border=FALSE, nv=nv)
		
		
# draw.radial.line(0, a, center=c(x, x), angle=ang2, 
        # expand=FALSE, col="darkgray", lwd=1, lty=1)
		
# draw.radial.line(0, a, center=c(x, x), deg=tg, 
        # expand=FALSE, col="darkgray", lwd=1, lty=1)			



# SPRCircFun <- function(LBSPR_mod, target=0.4, limit=0.2){
	# plot(1:10, asp = 1,main="", type="n", bty="n", axes=FALSE, 
	  # xlim=c(0,10), ylim=c(0,10), xlab="", ylab="")
    # a <- 4.5 
    # x <- 5
	# # if(values$ShowLine) {
      # spr <- LBSPR_mod$Ests$SPR[1] # values$spr# runOpt$ests[5]
	  # mtext(side=3, paste0("Estimated SPR = ", round(spr,2)), cex=1.25, line=-2)
      # ang <- 90 - (spr*360) 
      # ang2 <- 90
	  # tg  <- 90 - (target*360)
	  # lim <- 90 - (limit*360) 
	  # limcol <- "#ff1919"
	  # targcol <- "#ffb732"
	  # abtgcol <- "#32ff36"
	  # nv <- 200 
	  # # Circle
	  # draw.circle(x=x, y=x, radius=a, border=FALSE, nv=nv)
	  # # Limit Ellipse 
	  # draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(max(lim, ang), ang2),
  	    # col=limcol, arc.only=FALSE, border=FALSE, nv=nv)
	  # if (spr > limit) { 
	  # draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(max(tg, ang), lim), 
	    # col=targcol, arc.only=FALSE, border=FALSE, nv=nv)	
	  # }	
	  # if (spr > target) { 	
	    # draw.ellipse(x=x, y=x, a=a, b=a, angle=0, segment=c(min(360, ang), tg), 
	      # col=abtgcol, arc.only=FALSE, border=FALSE, nv=nv)		
	  # }	 	
	  # # radialtext(as.character(round(spr,2)), center=c(x,x), start=NA, 
	    # # middle=a/2, end=NA, deg=ang, expand=FALSE, stretch=1, nice=TRUE, 
		# # cex=1, xpd=NA)	
	  # draw.radial.line(0, a, center=c(x, x), deg=lim, 
        # expand=FALSE, col="darkgray", lwd=1, lty=1)
	  # draw.radial.line(0, a, center=c(x, x), deg=tg, 
        # expand=FALSE, col="darkgray", lwd=1, lty=1)	
    # # output$plotDone <<- renderUI({tags$input(type="hidden", value="TRUE")})
	# # }  
# }























# LBSPR_sim <- new("LBSPR_sim", TRUE)
# LBSPR_sim@SPR <- 0.3 
# tt <- LBSPR_sim(mybio, LBSPR_sim, mylendat)



# mybio <- new("LBSPR_bio", "example")

# mylendat <- new("LBSPR_len", file="example")
# mylendat <- new("LBSPR_len", file="LBSPR_len.csv", dataType="freq")
# mylendat <- new("LBSPR_len", file="rand.csv", dataType="raw")

# mymod <- LBSPR_fit(LBSPR_bio=mybio, LBSPR_len=mylendat, yrs=NA, Control=list())




  # N <- sum(LBSPR_mod$LenPred$Input)
  # ggplot(LBSPR_mod$LenPred, aes(x=LMids, y=Input)) +
    # geom_bar(stat='identity') +
	# geom_line(aes(x=LMids, y=Predict*N), colour="blue", size=2) +
	 # theme_bw() 
	# + 
    # xlab("Year") + 
    # ylab("Effort (trips per year)") + 
    # theme_bw() +
    # scale_y_continuous(labels = comma)



# }


# rr <- sapply(1:10, LBSPR_fit, LBSPR_bio=mybio, LBSPR_len=mylendat, Control=list())
# rr[1,]






# write.csv(cbind(LBSPR_len@LMids, LBSPR_len@LData), file="LBSPR_len.csv")
# LBSPR_sim1(example=TRUE)

# LBSPR_fit(example=TRUE)

# LBSPR_plot





# # Age-based Model

# # Example Length Data

# # Example LBSPR_bio 

# # Convert Raw to Frequency 

# # Multi Years 

# # Uncertainty 






# #' Simulation model for the equilibrium length-based SPR Model. 
# #' @name LBSPRSim
# #' @title Length-Based Spawning Potential Ratio Equilibrium Model
# #' @param currEggProd Current egg production - total number of eggs or spawning biomass at the 

# #' @return  
# #' @author Adrian Hordyk
# #' @export
# LBSPRSim <- function(stock=NULL, fleet=NULL, 
  # lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), 
  # mod=c("GTG", "age"), xtra=NULL) {
  # mod <- match.arg(mod) 

  # # Default Biological Parameters 
  # stockdefaults <- c(Linf=100, CVLinf=0.1, MaxSD=2, MK=1.5, L50=66, L95=70, 
                # Walpha=0.001, Wbeta=3, FecB=3, Steepness=1, Mpow=0, 
				# R0=10000)	
  # stock <- makedefaults(stock, stockdefaults)
  # # Default Fleet Parameters
  # fleetdefaults <- list(SL50=0.66*Linf, SL95=0.7*Linf, FM=1)
  # fleet <- makedefaults(fleet, fleetdefaults)
  
  # if (is.null(xtra)) { # Control parameters for the models
	# xtra$ngtg <- 9 # Number of GTGs
	# xtra$P <- 0.001 # % of cohort alive at maximum age 
	# xtra$Nage <- 101 # maximum number of pseudo-age classes
  # }
  # if (mod=="GTG") return(lbspr_gtg(stock=stock, fleet=fleet, 
    # lbins=lbins, xtra=xtra)) 
  # if (mod=="age") return(lbspr_age(stock=stock, fleet=fleet, 
    # lbins=lbins, xtra=xtra))	
# }

# lbspr_gtg <- function(stock=NULL, fleet=NULL, 
  # lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL, warn=TRUE) {
  # if (is.null(stock)) stop("stock object must be supplied")
  # if (is.null(fleet)) stop("fleet object must be supplied")
  # if (is.null(xtra)) xtra$ngtg <- 9
  # # Assign Variables #
  # # Biological Parameters 
  # Linf <- stock$Linf
  # CVLinf <- stock$CVLinf 
  # MaxSD <- stock$MaxSD 
  # SDLinf <- CVLinf * Linf # Standard Deviation of Length-at-Age # Assumed constant CV here
  # MK <- stock$MK 
  # L50 <- stock$L50
  # L95 <- stock$L95 
  # Walpha <- stock$Walpha 
  # Wbeta <- stock$Wbeta 
  # FecB <- stock$FecB 
  # Steepness <- stock$Steepness
  # Mpow <- stock$Mpow
  # R0 <- stock$R0 
  
  # # Fishing Fleet Parameters 
  # SL50 <- fleet$SL50
  # SL95 <- fleet$SL95 
  # FM <- fleet$FM 
  
  # # Model control parameters
  # ngtg <- xtra$ngtg 
  # # Length Bins 
  # if (is.null(lbins$mids)) { # length mids aren't supplied 
    # Max <- lbins$max 
	# Min <- lbins$min 
    # By <- lbins$by
    # LBins <- seq(from=0, by=By, to=Max) 
    # LMids <- seq(from=By/2, by=By, length.out=(length(LBins)-1))
  # } else {
	# By <- lbins$mids[2] - lbins$mids[1]
    # LMids <- lbins$mids
	# LBins <- seq(from=min(LMids)-0.5*By, by=By, length.out=length(LMids)+1)
	# Max <- max(LBins) 
	# Min <- min(LBins) 
	# if (Min > 0) {
	  # zeros <- rep(0, length(seq(from=0, to=Min-By, by=By)))
	  # LBins <- c(zeros, LBins)
	  # zeros2 <- zeros[1:(length(zeros)-1)]
	  # LMids <- c(zeros2, LMids)
	# }
	# if (all(diff(lbins$mids) != diff(lbins$mids)[1])) 
	  # stop("Length classes don't appear to be of equal size")
  # }
  # if (warn) if (Max < Linf) stop(paste0("Maximum length bin (", Max, ") can't be smaller than asymptotic size (", Linf ,")"))
  
  # # Linfs of the GTGs   
  # gtgLinfs <- seq(from=Linf-MaxSD*SDLinf, to=Linf+MaxSD*SDLinf, length=ngtg)
  # dLinf <- gtgLinfs[2] - gtgLinfs[1]
  
  # # Distribute Recruits across GTGS 
  # recP <- dnorm(gtgLinfs, Linf, sd=SDLinf) / 
	# sum(dnorm(gtgLinfs, Linf, sd=SDLinf)) 
  
  # Weight <- Walpha * LMids^Wbeta
  # # Maturity and Fecundity for each GTG 
  # L50GTG <- L50/Linf * gtgLinfs # Maturity at same relative size
  # L95GTG <- L95/Linf * gtgLinfs # Assumes maturity age-dependant 
  # DeltaGTG <- L95GTG - L50GTG
  # MatLengtg <- sapply(seq_along(gtgLinfs), function (X) 
	# 1.0/(1+exp(-log(19)*(LMids-L50GTG[X])/DeltaGTG[X])))
  # FecLengtg <- MatLengtg * LMids^FecB # Fecundity across GTGs 
  
  # # Selectivity - asymptotic only at this stage - by careful with knife-edge
  # SelLen <- 1.0/(1+exp(-log(19)*(LBins-(SL50+0.5*By))/ 
	# ((SL95+0.5*By)-(SL50+0.5*By))))
	
   # # Life-History Ratios 
  # MKL <- MK * (Linf/(LBins+0.5*By))^Mpow # M/K ratio for each length class
  # # Matrix of MK for each GTG
  # MKMat <- matrix(rep(MKL, ngtg), nrow=length(MKL), byrow=FALSE)
  # FK <- FM * MK # F/K ratio 
  # FKL <- FK * SelLen # F/K ratio for each length class
  # ZKLMat <- MKMat + FKL # Z/K ratio (total mortality) for each GTG

  # # Set Up Empty Matrices 
  # # number-per-recruit at length
  # NPRFished <- NPRUnfished <- matrix(0, nrow=length(LBins), ncol=ngtg)  
  # NatLUF <- matrix(0, nrow=length(LMids), ncol=ngtg) # N at L unfished 
  # NatLF <- matrix(0, nrow=length(LMids), ncol=ngtg) # N at L fished
  # FecGTG <- matrix(0, nrow=length(LMids), ncol=ngtg) # fecundity of GTG 
  
   # # Distribute Recruits into first length class
  # NPRFished[1, ] <- NPRUnfished[1, ] <- recP * R0 
  # for (L in 2:length(LBins)) { # Calc number at each size class
    # NPRUnfished[L, ] <- NPRUnfished[L-1, ] * ((gtgLinfs-LBins[L])/(gtgLinfs-LBins[L-1]))^MKMat[L-1, ]
    # NPRFished[L, ] <- NPRFished[L-1, ] * ((gtgLinfs-LBins[L])/(gtgLinfs-LBins[L-1]))^ZKLMat[L-1, ]
	# ind <- gtgLinfs  < LBins[L]
	# NPRFished[L, ind] <- 0
	# NPRUnfished[L, ind] <- 0
  # } 
  # NPRUnfished[is.nan(NPRUnfished)] <- 0
  # NPRFished[is.nan(NPRFished)] <- 0
  # NPRUnfished[NPRUnfished < 0] <- 0
  # NPRFished[NPRFished < 0] <- 0

  # for (L in 1:length(LMids)) { # integrate over time in each size class
    # NatLUF[L, ] <- (NPRUnfished[L,] - NPRUnfished[L+1,])/MKMat[L, ]
    # NatLF[L, ] <- (NPRFished[L,] - NPRFished[L+1,])/ZKLMat[L, ]  
	# FecGTG[L, ] <- NatLUF[L, ] * FecLengtg[L, ]
  # }
  
  # SelLen2 <- 1.0/(1+exp(-log(19)*(LMids-SL50)/(SL95-SL50))) # Selectivity-at-Length 
  # NatLV <- NatLUF * SelLen2 # Unfished Vul Pop
  # NatLC <- NatLF * SelLen2 # Catch Vul Pop  
  
  # # Aggregate across GTGs  
  # expNatLC <- apply(NatLC, 1, sum)/sum(apply(NatLC, 1, sum))
  # expNatLVUF <- apply(NatLV, 1, sum)/sum(apply(NatLV, 1, sum))
  # expNatLUF <- apply(NatLUF, 1, sum)/sum(apply(NatLUF, 1, sum))
  # expNatLF <- apply(NatLF, 1, sum)/sum(apply(NatLF, 1, sum))
  
  # # Calc SPR
  # EPR0 <- sum(NatLUF * FecLengtg) # Eggs-per-recruit Unfished
  # EPRf <- sum(NatLF * FecLengtg) # Eggs-per-recruit Fished
  # SPR <- EPRf/EPR0 
  
  # # Equilibrium Relative Recruitment
  # recK <- (4*Steepness)/(1-Steepness) # Goodyear compensation ratio 
  # reca <- recK/EPR0
  # recb <- (reca * EPR0 - 1)/(R0*EPR0)
  # RelRec <- max(0, (reca * EPRf-1)/(recb*EPRf))
  # if (!is.finite(RelRec)) RelRec <- 1 
  # # RelRec/R0 - relative recruitment 
  # YPR <- sum(NatLC  * Weight * SelLen2) * FM 
  # Yield <- YPR * RelRec
  
  # # Calc Unfished Fitness - not used here  
  # Fit <- apply(FecGTG, 2, sum, na.rm=TRUE) # Total Fecundity per Group
  # FitPR <- Fit/recP # Fitness per-recruit
  # FitPR <- FitPR/median(FitPR)  
 
  # # Calculate spawning-per-recruit at each size class
  # SPRatsize <- cumsum(rowSums(NatLUF * FecLengtg))
  # SPRatsize <- SPRatsize/max(SPRatsize) 
  
  # # Simulated length data 
  # LBSPR_len <- cbind(mids=LMids, n=expNatLC)
  # LBSPR_len <- LBSPR_len[LBSPR_len[,1] >= Min,]
  # LBSPR_len[,2] <- LBSPR_len[,2]/sum(LBSPR_len[,2])
  
  # Output <- NULL 
  # Output$SPR <- stock$SPR <- SPR 
  # Output$FM <- FM
  # Output$wFM <- mean(FM * SelLen2)
  # Output$YPR <- YPR 
  # Output$Yield <- Yield 
  # Output$stock <- stock 
  # Output$fleet <- fleet
  # Output$size <- NULL
  # Output$size$cal <- expNatLC
  # Output$size$palv <- expNatLVUF
  # Output$size$palUF <- expNatLUF
  # Output$size$palF <- expNatLF
  # Output$LBSPR_len <- LBSPR_len
  # Output$LMids <- LMids 
  # Output$LBins <- LBins 
  # Output$Winf <- Walpha * Linf^Wbeta
  # Output$SelLen <- SelLen 
  # Output$SelLen2 <- SelLen2 
  # Output$other <- NULL
  # Output$other$MKMat <- MKMat 
  # Output$other$ZKLMat <- ZKLMat
  # Output$other$MKL <- MKL 
  # Output$other$FKL <- FKL 
  # Output
# }

# # FIX SIZE BINS HERE 
# lbspr_age <- function(stock=NULL, fleet=NULL, 
  # lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL) {
  # if (is.null(stock)) stop("stock object must be supplied")
  # if (is.null(fleet)) stop("fleet object must be supplied")
  # if (is.null(xtra)) {
	# xtra$P <- 0.001 # % of cohort alive at maximum age 
	# xtra$Nage <- 101 # maximum number of pseudo-age classes
  # }
  # # Assign Variables #
  # # Biological Parameters 
  # Linf <- stock$Linf
  # CVLinf <- stock$CVLinf 
  # MaxSD <- stock$MaxSD 
  # SDLinf <- CVLinf * Linf # Standard Deviation of Length-at-Age # Assumed constant CV here
  # MK <- stock$MK 
  # L50 <- stock$L50
  # L95 <- stock$L95 
  # Walpha <- stock$Walpha 
  # Wbeta <- stock$Wbeta 
  # FecB <- stock$FecB 
  # Steepness <- stock$Steepness
  # Mpow <- stock$Mpow
  # R0 <- stock$R0 
  
  # # Fishing Fleet Parameters 
  # SL50 <- fleet$SL50
  # SL95 <- fleet$SL95 
  # FM <- fleet$FM 
  
  # # Model control parameters
  # P <- xtra$P
  # Nage <- xtra$Nage 
  
  # # Length Bins 
  # if (is.null(lbins$mids)) { # length mids aren't supplied 
    # Max <- lbins$max 
	# Min <- lbins$min 
    # By <- lbins$by  
  # } else {
	# By <- lbins$mids[2] - lbins$mids[1]
	# Max <- max(lbins$mids) + 0.5 * By 
	# Min <- min(lbins$mids) 
	# if (all(diff(lbins$mids) != diff(lbins$mids)[1])) 
	  # stop("Length classes don't appear to be of equal size")
  # }
  # LBins <- seq(from=0, by=By, to=Max) 
  # LMids <- seq(from=By/2, by=By, length.out=(length(LBins)-1))
  # if (Max < Linf) stop("Maximum length bin can't be smaller than asymptotic size")  

  # # LBSPR model with pseudo-age classes 
  # x <- seq(from=0, to=1, length.out=Nage) # relative age vector
  # EL <- (1-P^(x/MK)) * Linf # length at relative age 
  # rLens <- EL/Linf # relative length 
  # SDL <- EL * CVLinf # standard deviation of length-at-age
  # Nlen <- length(LMids) 
  # Prob <- matrix(NA, nrow=Nage, ncol=Nlen)
  # Prob[,1] <- pnorm((LBins[2] - EL)/SDL, 0, 1) # probablility of length-at-age
  # for (i in 2:(Nlen-1)) {
    # Prob[,i] <- pnorm((LBins[i+1] - EL)/SDL, 0, 1) - 
		# pnorm((LBins[i] - EL)/SDL, 0, 1)
  # }
  # Prob[,Nlen] <- 1 - pnorm((LBins[Nlen] - EL)/SDL, 0, 1)
  
  # # Truncate normal dist at MaxSD 
  # mat <- array(1, dim=dim(Prob))
  # for (X in 1:Nage) {
    # ind <- which(abs((LMids - EL[X]) /SDL[X]) >= MaxSD)
    # mat[X,ind] <- 0
  # }
  # Prob <- Prob * mat
  # SL <- 1/(1+exp(-log(19)*(LMids-SL50)/(SL95-SL50))) # Selectivity at length
  # Sx <- apply(t(Prob) * SL, 2, sum) # Selectivity at relative age 
  # MSX <- cumsum(Sx) / seq_along(Sx) # Mean cumulative selectivity for each age 
  # Ns <- (1-rLens)^(MK+(MK*FM)*MSX) # number at relative age in population
  
  # Cx <- t(t(Prob) * SL) # Conditional catch length-at-age probablilities  
  # Nc <- apply(Ns * Cx, 2, sum) # 
  # Pop <- apply(Ns * Prob, 2, sum)
  
  # Ml <- 1/(1+exp(-log(19)*(LMids-L50)/(L95-L50))) # Maturity at length
  # Ma <-  apply(t(Prob) * Ml, 2, sum) # Maturity at relative age 
  
  # N0 <- (1-rLens)^MK # Unfished numbers-at-age 
  # SPR <- sum(Ma * Ns * rLens^FecB)/sum(Ma * N0 * rLens^FecB)
  
  # # Simulated length data 
  # LBSPR_len <- cbind(mids=LMids, n=Nc)
  # LBSPR_len <- LBSPR_len[LBSPR_len[,1] >= Min,]
  # LBSPR_len[,2] <- LBSPR_len[,2]/sum(LBSPR_len[,2])
  
  # Output <- NULL 
  # Output$SPR <- stock$SPR <- SPR 
  # Output$FM <- FM
  # Output$wFM <- mean(FM * Sx)
  # Output$stock <- stock 
  # Output$fleet <- fleet
  # Output$size <- NULL
  # Output$size$cal <- Nc
  # Output$size$palv <- apply(N0 * Cx, 2, sum) #
  # Output$size$palUF <- apply(N0 * Prob, 2, sum) #
  # Output$size$palF <- apply(Nc * Prob, 2, sum) #
  # Output$LBSPR_len <- LBSPR_len
  # Output$LMids <- LMids 
  # Output$LBins <- LBins 
  # Output$Winf <- Walpha * Linf^Wbeta
  # Output$SelLen <- 1/(1+exp(-log(19)*(LBins-SL50)/(SL95-SL50))) 
  # Output$SelLen2 <- SL 
  # Output$other <- NULL
  # Output
# }

# makedefaults <- function(inputs, defaults) {
  # output <- inputs 
  # innm <- names(output)
  # dfnm <- names(defaults)
  # extrain <- which(innm %in% dfnm == FALSE)
  # missin <- which(dfnm %in% innm == FALSE)
  # if (length(missin) == 0) return(output)
  # if (length(missin) > 0) {
    # for (X in seq_along(missin)) {
	  # output[dfnm[missin[X]]] <- NA
	# }
  # }
  # output <- data.frame(t(output))
  # output <- output[,match(names(defaults), names(output))]
  # NAs <- which(is.na(output))
  # if (length(NAs) > 0) {
    # for (X in NAs) {
	  # dfval <- defaults[dfnm[X]]
	  # message("Value for ", dfnm[X], " not found. Using default value of ", dfval)
	  # flush.console()
	  # output[dfnm[X]] <- dfval
	# }
  # }
  # if (length(extrain) > 0) {
    # message("Extra parameters supplied that will be ignored:")
	# print(innm[extrain])
	# flush.console()
  # }
  # output 
# }
    
# optfun <- function(trypars, LBSPR_len, stock, mod=c("GTG", "age"), xtra=NULL) {
  # mod <- match.arg(mod) 
  # mids <- LBSPR_len[,1]
  # ldat <- LBSPR_len[,2]
  # fleet <- NULL
  # fleet$SL50 <- exp(trypars[1]) * stock$Linf
  # fleet$SL95 <- fleet$SL50  + (exp(trypars[2]) * stock$Linf)
  # fleet$FM <- exp(trypars[3])
  
  # if (mod == "GTG") 
    # runMod <- lbspr_gtg(stock, fleet, lbins=list(mids=mids), xtra)
  # if (mod == "age") 
    # runMod <- lbspr_age(stock, fleet, lbins=list(mids=mids), xtra)
  
  # ldat <- ldat + 1E-15 # add tiny constant for zero catches
  # LenProb <- ldat/sum(ldat)
  # predProb <- runMod$LBSPR_len[,2] 
  # predProb <- predProb + 1E-15 # add tiny constant for zero catches
  # NLL <- -sum(ldat * log(predProb/LenProb))
  # # add penalty for SL50 
  # trySL50 <- exp(trypars[1])
  # PenVal <- NLL
  # Pen <- dbeta(trySL50, shape1=5, shape2=0.01) * PenVal
  # if (Pen == 0) Pen <- PenVal * trySL50
  # # plot(xx, dbeta(xx, shape1=5, shape2=0.01) )
  # NLL <- NLL+Pen 
     # # PARs <<- (c(logL50=trySL50, SL50=fleet$SL50, SL95=fleet$SL95, FM=fleet$FM,
	 # # NLL=NLL, Pen=Pen))
   # # print(PARs)
  # return(NLL)
# }
 
# doopt <- function(stock, LBSPR_len, mod=c("GTG", "age"), 
  # lbins=list(min=0, max=1.25*stock$Linf, by=5, mids=NULL), xtra=NULL) {
  # mod <- match.arg(mod)
  # if(class(LBSPR_len) != "matrix") LBSPR_len <- as.matrix(LBSPR_len)

  # Ncol <- ncol(LBSPR_len)
  # if (Ncol > 2) stop("Too many columns in LBSPR_len")
  # if (is.null(LBSPR_len)) stop("Length data required")
  # if (Ncol == 1) {
    # Min <- lbins$min 
	# Max <- lbins$max 
	# By <- lbins$by 
	# if (Max < stock$Linf) stop("Maximum size bin must be higher than Linf")
	# Max <- ceiling(Max/By) * By 
	# lbins$max <- Max 
	# LBins <- seq(from=0, by=By, to=Max)
	# temp <- hist(LBSPR_len, breaks=LBins, plot=FALSE)
	# ldat <- temp$counts 
	# LMids <- temp$mids
  # } 
  # if (Ncol == 2) {
    # LMids <- LBSPR_len[,1]
	# ldat <- LBSPR_len[,2]
  # }

  # sSL50 <- LMids[which.max(ldat)]/stock$Linf # Starting guesses
  # sDel <- 0.2 * LMids[which.max(ldat)]/stock$Linf
  # sFM <- 0.5 
  # Start <- log(c(sSL50, sDel, sFM))
  # opt <- nlminb(Start, optfun, stock=stock, LBSPR_len=cbind(LMids, ldat), 
	# mod=mod, control= list(iter.max=300, eval.max=400, abs.tol=1E-20))
  
  # newFleet <- NULL 
  # newFleet$FM <- exp(opt$par[3])
  # newFleet$SL50 <- exp(opt$par[1]) * stock$Linf 
  # newFleet$SL95 <- newFleet$SL50 + exp(opt$par[2]) * stock$Linf
  
  
  # if (mod=="GTG") runMod <- lbspr_gtg(stock=stock, fleet=newFleet, 
    # lbins=lbins, xtra=xtra) 
  # if (mod=="age") runMod <- lbspr_age(stock=stock, fleet=newFleet, 
    # lbins=lbins, xtra=xtra)
  
  # Out <- NULL 
  # Out$ests <- c(FM=newFleet$FM, wFM=runMod$wFM, SL50=newFleet$SL50, 
    # SL95=newFleet$SL95, SPR=runMod$SPR)
  # Out$predlen <- runMod$LBSPR_len[,2] * sum(ldat)
  # Out$oblen <- ldat
  # Out$LMids <- LMids
  # return(Out)
# }





# # MCMC 
 
 
 # # optfun2 <- function(trypars, LBSPR_len, stock, mod=c("GTG", "age"), xtra=NULL) {
  # # -optfun( trypars, LBSPR_len, stock, mod=mod, xtra)
 # # }
 
 
# # CheckServerLoad <- function() {
  # # sys <- Sys.info()
  # # LoadOK <- TRUE 
  # # if (sys["sysname"] == "Linux") {
    # # tmp <- system("uptime", intern=TRUE)
	# # tmp <- " 22:26:00 up 5 days, 3:52, 1 user, load average: 0.00, 0.01, 0.05"
    # # n <- nchar(tmp) 
	# # Load <- as.numeric(substr(tmp, n-3, n))
	# # if (Load > 0.7) LoadOK <- FALSE
  # # }
  # # LoadOK 
# # }

# # mcmcNLL <- function(modpars, stock, LBSPR_len, mod=c("GTG", "age"), xtra=NULL) {
  # # # nms <- which(names(stock) %in% names(modpars))
  # # # modnms <- names(modpars)
  # # modpars <- as.numeric(modpars)
  # # stock$MK <- exp(modpars[1])
  # # stock$Linf <- exp(modpars[2])
  # # trypars <- NULL 
  # # trypars[1] <- exp(modpars[3])
  # # trypars[2] <- exp(modpars[4])
  # # trypars[3] <- exp(modpars[5]) 

  # # mod <- match.arg(mod) 
  # # mids <- LBSPR_len[,1]
  # # ldat <- LBSPR_len[,2]
  # # fleet <- NULL
  # # fleet$SL50 <- (trypars[1]) * stock$Linf
  # # fleet$SL95 <- fleet$SL50  + ((trypars[2]) * stock$Linf)
  # # fleet$FM <- (trypars[3])
  
   # # # add penalty for SL50 
  # # trySL50 <- (trypars[1])
  # # PenVal <- 1E3
  # # Pen <- dbeta(trySL50, shape1=5, shape2=0.01) * PenVal
  # # if (!is.finite(Pen)) {
    # # Pen <- PenVal
  # # }
  # # if (Pen == 0) Pen <- PenVal * trySL50
  
  # # if (stock$Linf >= max(mids)) {
    # # # print('this happened') 
    # # return(1/stock$Linf * 1E12)
  # # }
  # # if (mod == "GTG") 
    # # runMod <- lbspr_gtg(stock, fleet, lbins=list(mids=mids), xtra, warn=FALSE)
  # # if (mod == "age") 
    # # runMod <- lbspr_age(stock, fleet, lbins=list(mids=mids), xtra)
  
  # # ldat <- ldat + 1E-15 # add tiny constant for zero catches
  # # LenProb <- ldat/sum(ldat)
  # # predProb <- runMod$LBSPR_len[,2] 
  # # predProb <- predProb + 1E-15 # add tiny constant for zero catches
 
  # # if (any(!(is.finite(predProb)))) {
    # # print(exp(modpars))
    # # stop()
  # # }
  # # # if (any(!(is.finite(predProb)))) {
    # # # print("**")
	# # # print(predProb)
	# # # print(round(exp(modpars), 2))
	# # # print("****")
	# # # flush.console()
	# # # return(PenVal)
  # # # }
  # # NLL <- -sum(ldat * log(predProb/LenProb))
  # # NLL2 <- NLL + Pen 
  # # # plot(xx, dbeta(xx, shape1=5, shape2=0.01) )
  
  # # # print(c(exp(modpars), NLL=NLL))  
     # # # PARs <<- (c(logL50=trySL50, SL50=fleet$SL50, SL95=fleet$SL95, FM=fleet$FM,
	 # # # NLL=NLL, Pen=Pen))
   # # # print(PARs)
   
  # # # Priors
  # # MKmu <- log(1.5) 
  # # MKsd <- 0.1  
  # # MKpen <- -dlnorm(stock$MK, MKmu, MKsd, log=TRUE) #* 1E2
  
  # # LinfBounds <- c(165, 180)
  # # Linfpen <- 0 
  # # if (stock$Linf > max(LinfBounds) | stock$Linf < min(LinfBounds)) {
    # # Linfpen <- max(abs(stock$Linf - LinfBounds)) * 1E2 
  # # }
  # # # Linfmu <- log(170)
  # # # Linfsd <- 0.025
  # # # # hist(rlnorm(1000, Linfmu, Linfsd))
  # # # Linfpen <- -dlnorm(stock$Linf, Linfmu, Linfsd, log=TRUE) #* 1E2
  
  # # out <- NLL2 + Linfpen + MKpen
  # # # print(c(NLL=NLL, Pen=Pen, trySL50=trySL50, Linfpen=Linfpen, MKpen=MKpen, out=out))
  # # flush.console()
  
  # # return(-out) 
# # }
 
 
# # modpars <- NULL 
# # modpars$MK <- log(1.25)
# # modpars$Linf <- log(172)
# # modpars$SL50 <- log(100)/stock$Linf
# # modpars$del <- log(0.6)
# # modpars$FM <- log(1)

 
# # Start <- as.numeric(modpars )
# # Start

# # lower <- log(c(0.4, 165, 0.1, 0.01, 0.01))
# # upper <- log(c(3.0, 180, 0.8, 0.6, 10))
# # rm(rr)
# # one <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # # tune=1, logfun=TRUE, LBSPR_len=LBSPR_len, stock=stock, mod="GTG", xtra=NULL,
  # # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # # optim.method="L-BFGS-B", seed=101)
# # two <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # # tune=1, logfun=TRUE, LBSPR_len=LBSPR_len, stock=stock, mod="GTG", xtra=NULL,
  # # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # # optim.method="L-BFGS-B", seed=102)
# # three <- MCMCmetrop1R(mcmcNLL, theta.init=Start, burnin=100000, mcmc=500000, thin=200,
  # # tune=1, logfun=TRUE, LBSPR_len=LBSPR_len, stock=stock, mod="GTG", xtra=NULL,
  # # force.samp=TRUE  , optim.lower=lower, optim.upper=upper, 
  # # optim.method="L-BFGS-B", seed=103)
  
# # rr <- rbind(one, two, three)
# # rm(out)
# # out <- exp(rr)
# # mk <- out[,1]
# # linf <- out[,2]
# # SL50 <- out[,3] * linf
# # SL95 <- SL50 + out[,4] * linf
# # FM <- out[,5]
# # par(mfrow=c(3,2))
# # hist(SL50)
# # hist(SL95)
# # hist(FM)
# # hist(mk)
# # hist(linf)

# # plot(out)

# # plot(one)
# # plot(two)
# # plot(three)

# # optfun(log(PARs)[1:3], LBSPR_len, stock, mod="GTG")
  
# # tt <- seq(0, 1, 0.001)
# # plot(tt,dbeta(tt, shape1=5, shape2=0.01))
# # stock <- NULL
# # stock$Linf <- 170 
# # stock$CVLinf <- 0.1
# # stock$MaxSD <- 2
# # stock$MK <- 1.5
# # stock$L50 <- 90 
# # stock$L95 <- 114
# # stock$Walpha <- 0.01
# # stock$Wbeta <- 3 
# # stock$FecB <- 3
# # stock$Steepness <- 1 
# # stock$Mpow <- 0
# # stock$R0 <- 1000

# # rawdat <- read.csv("E:/Dropbox/Projects/SriLanka_BSC/LengthData/13CWFOnlyDEPSeaEstuaryLagoonFG141024.csv")
# # LBSPR_len <- rawdat
# # tryopt <- doopt(stock, rawdat, lbins=list(min=0, max=max(rawdat), by=5), mod="age")
# # plot(tryopt$LMids, tryopt$oblen)
# # lines(tryopt$LMids, tryopt$predlen)
# # tryopt$ests

# # tryopt2 <- doopt(stock, rawdat, lbins=list(min=0, max=max(rawdat), by=5), mod="GTG")
# # plot(tryopt2$LMids, tryopt2$oblen)
# # lines(tryopt2$LMids, tryopt2$predlen)
# # tryopt2$ests


# # rm(fleet)
# # rm(stock)
# # t1 <- LBSPRSim(mod="GTG") 
# # t2 <- lbspr_age(stock, fleet)

# # t1$SPR 
# # t2$SPR

# # fleet <- FleetPars





# # plot(t1$LMids,t1$size$cal )
# # lines(t2$LMids,t2$size$cal )


# # lines(t3$LBSPR_len)
# # t3$SPR

# # stock <- stock 
# # stock$Beta <- 3 
# # LBSPRSim(stock, fleet)$SPR

# # stock$ngtg <- 9 
# # GTGLBSPRSim(stock, fleet)$SPR

