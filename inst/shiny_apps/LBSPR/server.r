library(shiny)
library(shinyBS)
library(LBSPR)
library(Hmisc)
library(xtable)

shinyServer(function(input, output, clientData, session) {

  values <- reactiveValues(useDF=FALSE, default=NULL,useExamp=FALSE,
    ShowResults=FALSE, AssessReady=FALSE, DoneAssess=FALSE)

  observeEvent(input$binswidth, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  # If any biological parameters are changed, the assessment is reset
  observeEvent(input$MK, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$relLinf, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$SetLinf, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$L50, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$L95, {
    values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })

  # Observe Events
  observeEvent(input$defPars, {
    values$useDF <- TRUE
    values$default <- c(1.5, 100, 66, 70, 0.66)
  })
  observeEvent(input$exampData, {
	values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$exmpData, {
    values$useExamp <- TRUE
	values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })
  observeEvent(input$file1, {
    values$useExamp <- FALSE
	values$ShowResults <- FALSE
	values$AssessReady <- FALSE
	values$DoneAssess <- FALSE
  })

  observeEvent(input$assessReady, {
    if(input$assessReady == 0) return(NULL)
    if (!is.null(data())) values$AssessReady <- TRUE
  })

  observeEvent(input$goAssess, {
    if(input$goAssess == 0) return(NULL)
    values$ShowResults <- TRUE
  })

  # Tool Tips
  addTooltip(session, id = "file1", title = "CSV or text file only",
          placement = "right", trigger = "hover")
  addTooltip(session, id="header", title = "Does the first row contain labels?",
           placement = "right", trigger = "hover")
  addTooltip(session, id="smooth", title = "Only used if more than one year",
           placement = "right", trigger = "hover")
  ## Alerts ##
  observe({
    # Pars <- Lens <- NULL
	# ParsOK <- FALSE
    # if(chkFileUp() & is.null(UpLoadMSG())) Pars <- getLB_pars()
	# if(chkFileUp() & is.null(UpLoadMSG())) Lens <- getLB_lens()
	# if (class(Pars)!="NULL") {
	  # if(length(Pars@Linf)>0 & is.finite(Pars@Linf)) ParsOK <- TRUE
	# }
	# if (ParsOK & class(Lens) !="NULL") {
	  # if (Pars@Linf < max(Lens@LMids)) {
	    # createAlert(session,  "LMidsErr", "lmidserr", title = "Error",
	      # content=HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")),
		  # " must be larger than maximum length bin (", max(Lens@LMids), ")")), append=FALSE)
	  # } else {
	    # closeAlert(session, "lmidserr")
	  # }
	# }
	# if (!values$ShowResults) return(NULL)
    MK <- as.numeric(input$MK)
    Linf <- getLinf() # as.numeric(input$L50) / as.numeric(input$relLinf)
    # Linf <- getLinf() # as.numeric(input$Linf)
	L50 <- as.numeric(input$L50)
	L95 <- as.numeric(input$L95)
	relLinf <- as.numeric(input$relLinf)
	if (length(MK) <1) MK <- NA
	if (length(Linf)<1) Linf <- NA
	if (length(L50)<1) L50 <- NA
	if (length(L95)<1) L95 <- NA
	if (length(relLinf)<1) relLinf <- NA
	if (is.null(MK)) MK <- NA
	if (is.null(Linf)) Linf <- NA
	if (is.null(L50)) L50 <- NA
	if (is.null(L95)) L95 <- NA
	if (is.null(relLinf)) relLinf <- NA

	# Errors #
	if (input$sprtarg <= input$sprlim) {
      createAlert(session,  "refalert", "refs", title = "Error",
        content = "SPR Limit must be less than SPR Target", append = FALSE)
    } else {
	  closeAlert(session, "refs")
	}
	chk <- any(c(MK, Linf, L50, L95, relLinf)<0)
	if (all(is.na(chk))) doChk <- FALSE
	if (!all(is.na(chk))) {
	  doChk <- chk
	}
	if (doChk) {
	  createAlert(session,  "NegVals", "negvals", title = "Error",
	    content="Negative values don't make much sense!", append=FALSE)
	} else {
	  closeAlert(session, "negvals")
	}

	if (!is.na(L95) & !is.na(L50)) {
   	  if (L95 <= L50) {
        createAlert(session,  "lmalert", "lmpar", title = "Error",
          content = HTML(paste0(tags$i("L", tags$sub("50")), " must be less than ",
			tags$i("L", tags$sub("95")))),
			append = FALSE)
      } else {
	    closeAlert(session, "lmpar")
	  }
	}
	# Warnings
	if (!is.na(L95) & !is.na(L50) & !is.na(Linf)) {
	  if (L50 >= Linf | L95 >= Linf) {
        createAlert(session,  "lmalert2", "lmpar2", title = "Error",
          content = HTML(paste0("Maturity parameters are higher than ",  tags$i("L"), tags$sub(HTML("&infin;")))),
	  	append = FALSE)
      } else {
	    closeAlert(session, "lmpar2")
	  }
	}
	if (!is.na(Linf) & Linf < 10) {
      createAlert(session,  "linfalert", "linf1", title = "Warning",
        content = HTML(paste0("Are you sure ", tags$i("L"), tags$sub(HTML("&infin;")), " is so low?")),
		append = FALSE)
    } else {
	  closeAlert(session, "linf1")
	}
	if (!is.na(Linf) & is.null(UpLoadMSG()) & input$dataType == "freq") {
	  Lens <- getLB_lens()
	  if (Linf > max(Lens@LMids)) {
        createAlert(session,  "linfalert3", "linf2", title = "Error",
          content = HTML(paste0("Maximum length bin (", round(max(Lens@LMids),2), 
		    ") must be greater than ", tags$i("L"), tags$sub(HTML("&infin;")))),
		  append = FALSE)
      } else {
	    closeAlert(session, "linf2")		
	  }	
	}
	if (!is.na(MK) & (MK < 0.2 | MK > 6)) {
      createAlert(session,  "mkalert", "mk1", title = "Warning",
        content = HTML(paste0("Are you sure of the ",  tags$i("M/K"), " ratio? Model may not perform well
		  at extreme values")),
		append = FALSE)
    } else {
	  closeAlert(session, "mk1")
	}
    if (!is.na(relLinf) & (relLinf >=1 | relLinf <=0)) {
      createAlert(session,  "RelLinferr", "relLinferr", title = "Error",
        content = HTML(paste0("Relative size at maturity must be between 0 and 1")),
        append=FALSE)
	} else {
	  closeAlert(session, "relLinferr")
	}
	templen <- NULL
	if (chkPars()) templen <- getLB_lens()
	if (class(templen) != "NULL") {
	if (!is.na(Linf) & Linf > max(templen@LMids)) {
      createAlert(session,  "linfalert2", "linf2", title = "Error",
        content = HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")), "(", Linf, ") must be lower than the largest length bin (", max(templen@LMids), ")")),
		append = FALSE)
    } else {
	  closeAlert(session, "linf2")
	}
	}
    # Add checks for all parameters
  })

  #############################
  ### Read in CSV file      ###
  ### Check that data is ok ###
  #############################

  ExampleDataFile <- reactive({
    switch(input$exampData,
	  rawSingHead = "../../LRaw_SingYrHead.csv",
	  rawSing = "../../LRaw_SingYr.csv",
	  rawMultiHead = "../../LRaw_MultiYrHead.csv",
	  rawMulti = "../../LRaw_MultiYr.csv",
	  freqSingHead = "../../LFreq_SingYrHead.csv",
	  freqSing = "../../LFreq_SingYr.csv",
	  freqMultiHead = "../../LFreq_MultiYrHead.csv",
	  freqMulti = "../../LFreq_MultiYr.csv")
  })

  output$downloadExample <- renderUI({
    if (!is.null(data()) & values$useExamp) {
	  fluidRow(
	    h5(strong("Download Example File")),
	    downloadButton("dnlData", label = "Download", class = NULL)
	  , style="padding: 5px 15px;")
	}
  })

  output$dnlData <- downloadHandler(
    filename = function() {
	     nm <- ExampleDataFile()
		 nm <- gsub("data/", "", nm)
		 nm <- gsub('.csv', "", nm)
		 paste(nm, '.csv', sep='')
    },
    content = function(file) {
	  write.table(data(), file, sep=",", row.names=FALSE, col.names=FALSE)
	}
  )

  data <- reactive({
    if (values$useExamp) {
	  read.csv(ExampleDataFile(), header = input$header,
               sep = input$sep,
	  		  stringsAsFactors=FALSE, check.names=FALSE)
	} else {
      file1 <- input$file1
	  if (is.null(file1)) return(NULL)
	  dat <- read.csv(file1$datapath, header = input$header,
               sep = input$sep, stringsAsFactors=FALSE, check.names=FALSE)
	  if (class(dat) == "data.frame" | class(dat) == "matrix") {
	    if (ncol(dat) > 1) {
	      chkNAs <- apply(dat, 2, is.na) # check NAs
	      dat <- dat[!apply(chkNAs, 1, prod),, drop=FALSE]
	      dat <- dat[,!apply(chkNAs, 2, prod), drop=FALSE]
	    }
	  }
	  if (class(dat) == "numeric" | class(dat) == "integer") {
	    dat <- dat[!is.na(dat)]
	  }
	  dat

	}
	})

  # Check - has file been uploaded?
  chkFileUp <- reactive({
    if(is.null(data())) return(FALSE)
	return(TRUE)
  })
  chkSep <- reactive({
    if(is.null(data())) return(TRUE)
  	lendat <- as.matrix(data())
	ind1 <- any(grepl(";", lendat))
	ind2 <- any(grepl(",", lendat))
	if (ind1) return(FALSE)
	if (ind2) return(FALSE)
	return(TRUE)
  })
  
  chkText <- reactive({
  if(is.null(data())) return(FALSE)
	if(chkFileUp() == FALSE) return(FALSE)
    if(!chkSep()) return(TRUE)
	if(class(data()) == "character") return(TRUE)
	if(class(data()[1,1]) == "character") return(TRUE)
	FALSE
  })

  UpLoadMSG <- reactive({
    msg1 <- msg2 <- msg3 <- msg4 <- msg5 <- msg6 <- NULL
    if(chkFileUp() == FALSE) msg1 <- "Please upload a CSV data file"
	if(!chkSep())  msg6 <- "Check File Separator" 
	if(chkFileUp() == TRUE & chkSep()) { 
	  if(chkFreq() & input$dataType == "raw") {
	    msg2 <- "It looks like you've uploaded length frequencies? Please change Data Type"
	  }
	  if(!chkFreq() & input$dataType == "freq") {
	    msg3 <- "It looks like you've uploaded length measurements? Please change Data Type"
	  }
	  if(chkHeader() & !input$header) {
	    msg4 <- "It looks like the file has a header row? Please check Header box"
	  }
	  if(chkText()) {
	    msg5 <- "Text in the data file. Do you have a header?"
	  }

	}
	out <- c(msg1, msg2, msg3, msg4, msg5, msg6)
	out
  })

  output$UpLoadText <- renderUI({
	out <- UpLoadMSG()
	out <- paste(out, collapse="<br/>")
	HTML(out)
  })

  chkFreq <- reactive({ # Check if data appears to be length frequencies
    if(!chkFileUp()) return(NULL)
	if(!chkSep()) return(NULL)
    lendat <- as.matrix(data())
	if (ncol(lendat) == 1) return (FALSE)
	fst <- lendat[,1]
    fst <- fst[is.finite(fst)]
	if (all(diff(fst) == median(diff(fst)))) return(TRUE)
	FALSE
  })

  chkHeader <- reactive({ # Check if there appears to be a header
  if(!chkFileUp()) return(NULL)
  if(!chkSep()) return(NULL)
	if(input$header) return(TRUE)
    lendat <- as.matrix(data())
  topRow <- lendat[1,, drop=FALSE]
	if (class(topRow) == "character") return(TRUE)
	if (chkFreq() & is.na(topRow[1])) return(TRUE)
	lendat <- as.matrix(lendat)
	topRow <- as.numeric(lendat[1,, drop=FALSE])
	if (!chkFreq()) {
	  if (ncol(lendat) > 1) {
	    if (all(diff(topRow) == 1))  return(TRUE)
	    if (all(topRow > 1900 & topRow < 2100)) return(TRUE)
	  }
	  if (ncol(lendat) == 1) {
	    if (topRow[1] > 1900 & topRow[1] < 2100) return(TRUE)
	  }
	}
    FALSE
  })

  chkMulitYear <- reactive({ # Check if there are multiple years
    if(!chkFileUp()) return(NULL)
    if(!chkSep()) return(NULL)
	lendat <- as.matrix(data())
	Ncol <- ncol(lendat)
    if(chkFreq() & Ncol > 2) return(TRUE)
	if(!chkFreq() & Ncol > 1) return(TRUE)
	FALSE
  })

  output$FileTable <- renderDataTable({
	if(!chkFileUp()) return(NULL)
    if(values$useExamp) {
	  DF <- data.frame(Filename=ExampleDataFile(),
	    DataType=ifelse(chkFreq(), "Frequency", "Raw"),
	    Header=chkHeader(),
	    MultiYear=chkMulitYear())
	} else {
	  DF <- data.frame(Filename=input$file1$name,
	    DataType=ifelse(chkFreq(), "Frequency", "Raw"),
	    Header=chkHeader(),
	    MultiYear=chkMulitYear())
    }
    return(DF)
  }, options=list(pageLength=-1, searching = FALSE, paging = FALSE,
     ordering=FALSE, info=FALSE, rowCallback = I(
    'function(row, data) {
        $("td", row).css("text-align", "center");
      }'
  )))

  output$metadata <- renderUI({
    if(values$useExamp) return()
	if(!chkFileUp()) return()
	HTML(paste(h3("Check the Uploaded File"),
	p("Does everything look right?"),
	h4("File Metadata")))
  })
  output$topdata <- renderDataTable({
    # Print out first 6 observations
    if(!chkFileUp()) return(NULL)
	dat <- data()
	if (input$header == TRUE) {
	  innames <- colnames(dat)
	  if (input$dataType == "freq") {
	    if (ncol(dat) >1) {
	      # innames[1] <- "Length.Bins"
		  # innames[2:ncol(dat)] <- gsub("X", "", innames[2:ncol(dat)])
		  # colnames(dat) <- innames
		}
	  }
	  if (input$dataType == "raw") {
	    innames <- gsub("X", "", innames)
	    colnames(dat) <- innames
	  }
	}
	if (input$header == FALSE & input$dataType == "freq") {
	  # if (ncol(dat) >1) {
	    # colnames(dat)[1] <- "Length.Bins"
	    # colnames(dat)[2:length(colnames(dat))] <- 1:(length(colnames(dat))-1)
	  # }
    }
	head(dat)
  }, options=list(pageLength=-1, searching = FALSE, paging = FALSE,
     ordering=FALSE, info=FALSE)
  )

  output$fileContents <- renderUI({
    if(!chkFileUp()) return()
    HTML(paste(h4("File contents"),
	  p("This shows the first six rows of your data file. All numbers below the bold black heading should be your length data. If you have multiple years of data, they should appear in seperate columns in the table below. ")
	))
  })

  ###################
  ## Fit Model Tab ##
  ###################
  # observeEvent(input$relLinf, {
  # observe({
    # myval <- as.numeric(input$L50) / as.numeric(input$relLinf)
    # # myval <- round(myval, 5)
	# if (length(myval)>0) updateTextInput(session, "Linf", value = myval)

  # })

  # observeEvent(input$Linf, {
      # if (length(as.numeric(input$relLinf)<1) & length(as.numeric(input$Linf))>0) {
      # myval <- as.numeric(input$L50) / as.numeric(input$Linf)
      # # myval <- round(myval, 2)
	  # if (!is.na(myval)) {
	    # updateTextInput(session, "relLinf", value = myval)
	  # }
	# }
  # }
  # # )

  getLinf <- reactive({
    if (input$dorelLinf == TRUE) {
	  tryLinf <- as.numeric(input$L50) / as.numeric(input$relLinf)
	  if (length(tryLinf)>0) {
       if(is.na(tryLinf)) return(NULL)
	   return(tryLinf)
	   # return(round(tryLinf,2))
      }
	} else {
	  return(as.numeric(input$SetLinf))
	}
  })
  # output$CurrLinf <- renderText(paste0("Linf = ", getLinf()))
  output$CurrLinf <- renderUI({
    myLinf <- getLinf()
	if(!is.numeric(myLinf)) return("")
    myLinf <- round(getLinf(),2)
    HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;"))), "=", myLinf)
  })
  output$InputPars <- renderUI({
    times <- input$defPars
	# MKVal <- 1.5 #""
	# relLinfVal <- 0.66 #""
	# LinfVal <- ""
	# L50Val <- 66 #""
	# L95Val <- 70 #""
	# disLinf <- getLinf() # round(as.numeric(input$L50) / as.numeric(input$relLinf),2)
	# if (is.numeric(disLinf)) disLinf <- round(disLinf,2)
	# if (values$useDF) {
	  # MKVal <- values$default[1]
	  # LinfVal <- values$default[2]
	  # L50Val <- values$default[3]
      # L95Val <- values$default[4]
	  # relLinfVal <- values$default[5]
	# }
    div(id=letters[(times %% length(letters)) + 1],
	h4("Life history ratios"),
	fluidRow(
	column(6,
	  textInput("MK", label=tags$i("M/K ratio"), value=1.5)
	),
	conditionalPanel(condition="input.dorelLinf == 'TRUE'",
	  column(6,
	    textInput("relLinf", label = HTML(paste0(tags$i("L", tags$sub("50")), "/",tags$i("L"), tags$sub(HTML("&infin;")))), value=0.66))
	),
	   # sliderInput("relLinf", label = HTML(paste0(tags$i("L", tags$sub("50")), "/",tags$i("L"), tags$sub(HTML("&infin;")))),
	   # min=0.0, max=1, step=0.01, value=input$relLinf))),
	 column(6,
	 conditionalPanel(condition="input.dorelLinf == 'FALSE'",
	   textInput("SetLinf", label = HTML(paste0(tags$i("L"), tags$sub(HTML("&infin;")))), value=100))
	)),
	h4("Length-at-Maturity"),
	fluidRow(
	column(6,
	  textInput("L50", label = tags$i(HTML(paste0("L", tags$sub("50")))), value=66)
	),
	column(6,
	  textInput("L95", label = tags$i(HTML(paste0("L", tags$sub("95")))), value=70)
	))
	)
  })

  output$HistControl <- renderUI({
    if (!chkFileUp()) return(NULL)
	if (!chkFileUp()) return(NULL)
	if(!is.null(UpLoadMSG())) return(NULL)
	if (input$dataType != "raw") return(NULL)
	dat <- data()
	Min <- round(min(dat, na.rm=TRUE),0)
	Max <- ceiling(max(dat, na.rm=TRUE)/5)*5
	Start <- floor(max(1/20 * Max)/5) * 5
	Max <- ceiling((max(dat, na.rm=TRUE)/10)/5)*5
    sliderInput("binswidth","Width of length bins:", min = 1, max = Max, value = Start)
  })

  getLB_pars <- reactive({
    # print(values$ShowResults)
    # if (!values$ShowResults) return(NULL)
    LB_pars <- new("LB_pars", verbose=FALSE)
	linf <- getLinf()
	if (class(linf) == "NULL") return(NULL)
	LB_pars@Linf <- linf  # as.numeric(input$L50) / as.numeric(input$relLinf) # as.numeric(input$Linf)
    LB_pars@L50 <- as.numeric(input$L50)
    LB_pars@L95 <- as.numeric(input$L95)
    LB_pars@MK <- as.numeric(input$MK)
	LB_pars@Species <- input$Species
	LB_pars@L_units <- input$Lunits
	binwidth <- input$binswidth
	LB_pars@BinWidth <- ifelse(is.null(binwidth), 5, binwidth)
	LB_pars
  })

  getLB_lens <- reactive({
   if (!chkFileUp()) return(NULL)
   if (!chkSep()) return(NULL)
	 if(!is.null(UpLoadMSG())) return(NULL)
     dat <- data()
     dat <- as.matrix(data())
	 LB_pars <- getLB_pars()
	 if (class(LB_pars) == "NULL") return(NULL)
	 if (class(LB_pars) != "LB_pars") return(NULL)
     LB_lengths <- new("LB_lengths", file=dat, LB_pars=LB_pars, dataType=input$dataType, verbose=FALSE)
	 LB_lengths
  })

  chkPars <- reactive({ # Are all input parameters entered?
  	Linf <- getLinf() # as.numeric(input$L50) / as.numeric(input$relLinf)
    pars <- as.numeric(c(input$MK, Linf, input$L50, input$L95, input$relLinf))
	if(any(!is.finite(pars))) return(FALSE)
	if (any(pars <=0)) return(FALSE)
	if(length(pars)<1) return(FALSE)
	if (pars[4] <= pars[3]) return(FALSE)
	if (pars[5] <=0 | pars[5] >=1) return(FALSE)
    #Pars <- getLB_pars()
	Lens <- getLB_lens()
	if (class(Lens) != "LB_lengths") return(FALSE)
	if (pars[3] >= Linf | pars[4] >= Linf) return(FALSE)
	if (Linf > max(Lens@LMids)) return(FALSE)
    TRUE
  })

  output$ValidPars <- renderText({
    if(chkText()) return("")
    if (!chkPars()) return("Invalid input parameters")
	if (chkPars()) return("")
  })

  output$ValidData <- renderText({
    if(!is.null(UpLoadMSG())) return("No valid data file")
	if(is.null(UpLoadMSG())) return("")
  })

  MakeHist <- reactive({
	  if(chkText()) return(NULL)
    if(!chkFileUp()) return(NULL)
	  if(!chkPars()) return(NULL)
	  if(!is.null(UpLoadMSG())) return(NULL)
    if (values$AssessReady & !values$ShowResults) return(plotSize(getLB_lens()))
    if (values$AssessReady & values$ShowResults) return(plotSize(doAssess()))
  })
  output$DatHistPlot <- renderPlot({
	if(chkText()) return(NULL)
  if(!chkFileUp()) return(NULL)
	if(!chkPars()) return(NULL)
	if(!is.null(UpLoadMSG())) return(NULL)
  histdat <- MakeHist()
  # need to add check here if model fails to converge
  print(histdat)
  })

  MatSel <- reactive({
  	if(chkText()) return(NULL)
    if(!chkFileUp()) return(NULL)
	if(!chkPars()) return(NULL)
	if(!is.null(UpLoadMSG())) return(NULL)
	if (values$AssessReady & !values$ShowResults) {
      LBobj <- new("LB_obj")
	  LB_lengths <- getLB_lens()
	  LB_pars <- getLB_pars()
	  Slots <- slotNames(LB_lengths)
      for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_lengths, Slots[X])
      Slots <- slotNames(LB_pars)
      for (X in 1:length(Slots)) slot(LBobj, Slots[X]) <- slot(LB_pars, Slots[X])
      LBobj@LMids[1] <- 0 # hack to make maturity curve start at zero
	  return(plotMat(LBobj))
    }
	if (values$AssessReady & values$ShowResults) return(plotMat(doAssess()))
  })

  output$DatMat <- renderPlot({
  	if(chkText()) return(NULL)
    if(!chkFileUp()) return(NULL)
	if(!chkPars()) return(NULL)
	if(!is.null(UpLoadMSG())) return(NULL)
    try(print(MatSel()))
  })


  ## Fit the Model ##
  # Run the LBSPR assessment routine
  doAssess <- reactive({
    if(chkText()) return(NULL)
    if(!chkFileUp()) return(NULL)
	if (!values$ShowResults) return(NULL)
	if (values$ShowResults) {
	  values$DoneAssess <- TRUE
	  lens <- getLB_lens()
	  fitmod <-  try(LBSPRfit(getLB_pars(), getLB_lens(), useCPP = TRUE))
	  if (class(fitmod) == "LB_obj") return(fitmod)
	  if (class(fitmod) != "LB_obj") {
        values$DoneAssess <- FALSE
		  return(FALSE)
	  }
	}
  })

  output$clickAssess <- renderUI({
    # if (!values$AssessReady) return("")
    if(chkText()) return("")
	if (!chkPars()) return("")
	if(!is.null(UpLoadMSG())) return("")
	if (values$AssessReady) {
	  fluidRow(
	    h4("Ready to Fit Model"),
	    actionButton("goAssess", "Fit Model",  icon("line-chart"), style="color: #fff; background-color: #00B700; border-color: #006D00")
	  , style="padding: 15px 15px 15px 15px;")
	} else {
	  fluidRow(
	    h4("Plot the Data"),
	    actionButton("assessReady", "Plot Data", icon("area-chart"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
	  ,style="padding: 15px 15px 15px 15px;")
	}
  })

  output$Histogram <- renderUI({
    if(!is.null(UpLoadMSG())) return("")
	if(is.null(UpLoadMSG()) & values$AssessReady & chkPars()) {
	  fluidRow(
	   h4("Histogram of Length Data"),
       plotOutput("DatHistPlot"),
       downloadButton("dnloadSize", label = "Download", class = NULL),
	  style="padding-top: 25px;")
	}
  })

  output$dnloadSize <- downloadHandler(
    filename = function() {
	     nm <- input$Species
		 nm <- gsub(" ", "", nm)
		 if (nchar(nm) <1) nm <- "MySpecies"
		 paste0(nm, '_SizeDist.png')
    },
    content = function(file) {
      ggsave(file, plot = MakeHist(), device = "png")
    }
  )

  output$MatSelPlot <- renderUI({
  if(is.null(UpLoadMSG()) & values$AssessReady & chkPars()) {
	  fluidRow(
	   h4("Maturity-at-Length"),
       plotOutput("DatMat"),
       downloadButton("dnloadMat", label = "Download", class = NULL),
	  style="padding-top: 25px;")
	}
  })

  output$dnloadMat <- downloadHandler(
    filename = function() {
	     nm <- input$Species
		 nm <- gsub(" ", "", nm)
		 if (nchar(nm) <1) nm <- "MySpecies"
		 paste0(nm, '_MatSel.png')
    },
    content = function(file) {
      ggsave(file, plot = MatSel(), device = "png")
    }
  )

  ### Results Tab ###
  output$ResultsText <- renderUI({
    if (values$DoneAssess == FALSE) {
	  h4(HTML("Model hasn't been fitted"), style = "color:red")
	} else {
	  # fluidRow(
	    # h3("Heading"),
	    # p("Use the controls on the left to select ")
	  # , style="padding: 0px 0px 0px 15px;")
	}
  })

  ### Table of Estimates ###
  GetEstimates <- reactive({
    if (!values$DoneAssess) return("")
    if (!"table" %in% input$pTypes) return("")
	ModelFit <- doAssess()
	# Results <- round(ModelFit@Ests,2)
	Results <- matrix(c(ModelFit@SL50, ModelFit@SL95, ModelFit@FM, ModelFit@SPR),
	  ncol=4, byrow=FALSE)
	
	# 95% confidence intervals #
	CIlower <- Results[,1:4] - 1.96 * sqrt(ModelFit@Vars)
    CIupper <- Results[,1:4] + 1.96 * sqrt(ModelFit@Vars)
	CIlower[!is.finite(CIlower)] <- 0
	CIupper[!is.finite(CIupper)] <- 0
	CIlower[CIlower <0 ] <- 0
	CIupper[CIupper <0 ] <- 0

    # correct bounded parameters - dodgy I know!
    CIlower[CIlower[,3]<0,3] <- 0
    CIupper[CIupper[,4]>1,4] <- 1
    CIlower[CIlower[,4]<0,4] <- 0
	
	CIlower <- round(CIlower,2)
	CIupper <- round(CIupper,2)
	
	#chk <- is.finite(CIlower)
	#if (any(!chk)) CIlower[!chk] <- 0 
	
	DF <- data.frame(Years=ModelFit@Years,
  	  SPR=paste0(round(ModelFit@SPR, 2), " (", CIlower[,4], " - ", CIupper[,4], ")"),
	  SL50=paste0(round(ModelFit@SL50, 2), " (", CIlower[,1], " - ", CIupper[,1], ")"),
	  SL95=paste0(round(ModelFit@SL95, 2), " (", CIlower[,2], " - ", CIupper[,2], ")"),
	  FM=paste0(round(ModelFit@FM, 2), " (", CIlower[,3], " - ", CIupper[,3], ")"))
	
	rownames(DF) <- 1:nrow(DF)# ModelFit@Years
	names(DF) <- c('Years',
	  # 'M/K',
	  # 'Linf',
	  # 'L50',
	  # 'L95',
	  'SPR',
	  'SL50',
	  'SL95',
	  'F/M')
	  # 'Above Target?',
	  # 'Above Limit?')
	if (input$smooth == "TRUE" & length(ModelFit@Years) > 1) {
	  Results <- as.data.frame(round(ModelFit@Ests,2))
	  DF$SPR <- Results$SPR
	  DF$SL50 <- Results$SL50
	  DF$SL95 <- Results$SL95
	  DF[,5] <- Results$FM
	}
	fitLog <- ModelFit@fitLog
	if (any(fitLog > 0)) {
	  DF$Note <- rep("", nrow(DF))
	  ind <- which(names(DF) == "Note")
	  DF[which(fitLog == 1),ind] <- "Model did not converge"
	  DF[which(fitLog == 2),ind] <- "Estimated selectivity may be unrealistically high"
	  DF[which(fitLog == 3),ind] <- "Estimated F/M may be unrealistically high"
	  DF[which(fitLog == 4),ind] <- "Estimated selectivity and F/M may be unrealistically high"
    }
	DF
  })

  output$Estimates <- renderDataTable({
    if (!values$DoneAssess) return("")
    if (!"table" %in% input$pTypes) return("")
	GetEstimates()
  }, options=list(pageLength=-1, searching = FALSE, paging = FALSE,
     ordering=FALSE, info=FALSE)
  # }, options=list(pageLength=-1, searching = FALSE, paging = FALSE,
     # ordering=FALSE, info=FALSE, rowCallback = I(
    # 'function(row, data) {
        # $("td", row).css("text-align", "center");
      # }'))
	)

  output$downloadEsts <- renderUI({
    if (!values$DoneAssess) return("")
    if (!"table" %in% input$pTypes) return("")
     downloadButton("dwnbuttonEsts", label = "Download", class = NULL)
  })

  AllPars <- reactive({
    if (!values$DoneAssess) return("")
    if (!"table" %in% input$pTypes) return("")
	Linf <- getLinf() # as.numeric(input$L50) / as.numeric(input$relLinf)
    DFAll <- GetEstimates()
	DFAll$MK <- input$MK
	DFAll$Linf <- Linf # getLinf() # input$Linf
	DFAll$L50 <- input$L50
	DFAll$L95 <- input$L95
	DFAll$CVLinf <- getLB_pars()@CVLinf
	DFAll$FecB <- getLB_pars()@FecB
	DFAll$Mpow <- getLB_pars()@Mpow
	DFAll$Smooth <- input$smooth
    DFAll
  })
  output$dwnbuttonEsts <- downloadHandler(
    filename = function() {
	     nm <- input$Species
		 nm <- gsub(" ", "", nm)
		 if (nchar(nm) <1) nm <- "MySpecies"
		 paste0(nm, '_Ests.csv')
    },
    content = function(file) {
	  write.table(AllPars(), file, sep=",", row.names=FALSE, col.names=TRUE)
	}
  )
  output$TableHeader <- renderUI({
    if (!values$DoneAssess) return("")
    if (!"table" %in% input$pTypes) return("")
     # HTML("Parameters: Input and Estimates")
	 HTML("Model Estimates (95% confidence intervals)")
  })

  ### Plot SPR Circle ####
  output$SPRCircle <- renderPlot({
    if (!values$DoneAssess) return("")
	if (!"spr" %in% input$pTypes) return("")
	if (input$smooth == "TRUE") smooth=TRUE
	if (input$smooth != "TRUE") smooth=FALSE
	labcol <- input$labcol
	if (labcol=="#FFFFFF") labcol <- NULL
    plotSPRCirc(doAssess(), SPRTarg=input$sprtarg, SPRLim=input$sprlim, 
	  useSmooth=smooth, bgcol=input$bgcol, limcol=input$limcol, 
	  targcol=input$targcol, abtgcol=input$abtgcol,
      labcol=labcol, labcex=input$labcex, texcex=input$texcex)
  })
  
  output$PSPRCirc <- renderUI({
    if (!values$DoneAssess) return("")
	if (!"spr" %in% input$pTypes) return("")

	if (input$smooth == "TRUE") smooth=TRUE
	if (input$smooth != "TRUE") smooth=FALSE
    fluidRow(
	  h4("Estimated Spawning Potential and Reference Points"),
	  h5("Note: if multiple years, only the estimate from the last year is shown"),
      plotOutput("SPRCircle"),
	  downloadButton("downloadSPRcirc2", label = "Download", class = NULL),
	  style="padding-top: 25px;")
  })

  plotOut1 <- function(){
  	if (input$smooth == "TRUE") smooth=TRUE
	if (input$smooth != "TRUE") smooth=FALSE
	labcol <- input$labcol
	if (labcol=="#FFFFFF") labcol <- NULL
    plotSPRCirc(doAssess(), SPRTarg=input$sprtarg, SPRLim=input$sprlim, 
	  useSmooth=smooth, bgcol=input$bgcol, limcol=input$limcol, 
	  targcol=input$targcol, abtgcol=input$abtgcol,
      labcol=labcol, labcex=input$labcex, texcex=input$texcex)
  }

  output$downloadSPRcirc2 <- downloadHandler(
    filename = function() {
	     nm <- input$Species
		 nm <- gsub(" ", "", nm)
		 if (nchar(nm) <1) nm <- "MySpecies"
		 paste0(nm, '_SPREst.png')
    },
    content = function(file) {
        png(file)
		plotOut1()
		dev.off()
    }
  )


  ### Estimates over Time ###
  output$YrEsts <- renderPlot({
	plotOut2()
  })

  output$EstsByYear <- renderUI({
    if (!values$DoneAssess) return("")
	if (!"ests" %in% input$pTypes) return("")
	if (getLB_lens()@NYears < 2) {
      return(
	    fluidRow(
	    h4("Estimates by Year: only one year - plot not shown"),
	    style="padding-top: 25px;")
	  )
	}
    fluidRow(
	  h4("Estimates by Year (with 95% confidence intervals)"),
      plotOutput("YrEsts", height="220px"),
	  downloadButton("dnloadEsts", label = "Download", class = NULL),
	  style="padding-top: 25px;")
  })

  plotOut2 <- function(){
    if (!values$DoneAssess) return("")
	if (!"ests" %in% input$pTypes) return("")
	if (input$smooth == "TRUE") smooth <- TRUE
	if (input$smooth != "TRUE") smooth <- FALSE
	if (input$incL50 == "TRUE") incL50 <- TRUE
	if (input$incL50 != "TRUE") incL50 <- FALSE	
    plotEsts(doAssess(), doSmooth=smooth, CIcol=input$CIcol, axCex=input$axCex, 
	  labCex=input$labCex, ptCex=input$ptCex, incL50=incL50, L50col=input$L50col)
  }
  output$dnloadEsts <- downloadHandler(
    filename = function() {
	     nm <- input$Species
		 nm <- gsub(" ", "", nm)
		 if (nchar(nm) <1) nm <- "MySpecies"
		 paste0(nm, '_YrEsts.png')
    },
    content = function(file) {
        png(file, width=900, height=550)
		plotOut2()
		dev.off()
    }
  )


})
