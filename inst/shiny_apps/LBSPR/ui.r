library(shiny)
library(shinyBS)
library(colourpicker)


shinyUI(tagList(tags$head(includeScript("google-analytics.js")),
  navbarPage("LBSPR Application", id="navbar",
  tabPanel("Instructions",
	  h3("Instructions"),
	  p("To use the LBSPR Application you will need: 1) length composition data from your fishery (either raw measurements or counts), and 2) estimates of the life history parameters. The following paragraphs outline the steps to use the LBSPR Application.  Each heading refers to a tab on the menu."),
	  h4("Upload Data"),
	  p("The first step is to upload a CSV (comma separated variable) file containing length data.  The file must be in CSV format and contain only numeric values except for the header row which can contain labels.  Multiple years of data should be placed in seperate columns."),
	  p("Length frequency data must have the midpoints of the length classes (the length bins) in the first column, and numeric values for all counts (i.e., all columns are the same length). Length measurements should be raw numbers, each column representing a different year."),
	  p("A number of example data files have been included. Download the CSV files to see the contents of these files"),
	  h4("Fit Model"),
	  p("Enter the life history parameters for your species, and check that the length frequency distribution looks correct.  If  everything is correct, run the LBSPR model."),
	  p("Use the example life history parameters for the example data files"),
	  h4("Examine Results"),
	  p("The estimated parameters of the LBSPR model in tabular and graphical format. All figures can be downloaded. The estimated parameters can be downloaded in CSV format.")
  ),
  tabPanel("Upload Data",
     # titlePanel("Upload Data File"),
	   sidebarLayout(
	     sidebarPanel(
	       fileInput('file1', "Choose CSV file to upload",
            accept = c(
                'text/csv',
                'text/comma-separated-values',
                'text/tab-separated-values',
                'text/plain',
                '.csv',
                '.tsv'
            )
          ),
		  bsTooltip(id = "file1", title = "CSV or text file only",
            placement = "right", trigger = "hover"),
		  tags$hr(),
	      checkboxInput('header', 'Header', FALSE),
		  bsTooltip(id = "header", title = "Does the first row contain labels?",
            placement = "right", trigger = "hover"),
	      radioButtons('dataType', 'Data Type',
                    c(Lengths='raw',
                    Frequency='freq'),
                    'raw'),
          radioButtons('sep', 'Separator',
                    c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                    ','),
		  tags$br(style="padding:20px 0px 0px 0px;"),
		  # h4("Load Example Data File", style="padding:20px 0px 0px 0px;"),
		  selectInput('exampData', label="Example Data",
		    choices=c("Lengths - 1 year no header" = "rawSing",
			          "Lengths - 1 year header" = "rawSingHead",
					  "Lengths - multi year no header" = "rawMulti",
					  "Lengths - multi year header" = "rawMultiHead",
					  "Frequency - 1 year no header" = "freqSing",
					  "Frequency - 1 year header" = "freqSingHead",
					  "Frequency - multi year no header" = "freqMulti",
					  "Frequency - multi year header" = "freqMultiHead")
					  ),
		  actionButton("exmpData", "Load Example Data", icon("cloud-upload")),
		  p(),
		  uiOutput("downloadExample")
		 ),
		mainPanel(
		  h3("Upload a Length Data File"),
		  # p("Sed id nunc ut nisl convallis tincidunt. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Suspendisse eu aliquet ex. Vivamus at ipsum vel felis dictum accumsan. In hac habitasse platea dictumst. Integer pellentesque tortor a scelerisque imperdiet. Suspendisse tempor scelerisque egestas. Maecenas pretium, erat posuere blandit iaculis, est felis ornare sem, sed commodo sem neque non augue. In faucibus laoreet sapien, at cursus ligula pharetra ut. Cras dictum pulvinar leo, in ultrices elit mollis id. In molestie, massa a mattis dapibus, ipsum lacus pellentesque justo, in auctor lectus purus eget urna. Duis ac neque quis sapien malesuada convallis. Nulla feugiat risus nisl, vitae pulvinar sapien placerat vitae. Morbi diam mi, facilisis eget sagittis vitae, maximus vel lacus."),
		h4("Steps:"),
		tags$ol(
		  tags$li("Upload a CSV data file or select an example data set from the menu on the lower left"),
		  tags$li("Check that the details to ensure that the data has been read in correctly. The length data must be numeric values.")
		),
		  p(htmlOutput("metadata")),
		  h4(htmlOutput("UpLoadText"), style = "color:red"),
		  dataTableOutput("FileTable"),
		  p(htmlOutput("fileContents")),
		  dataTableOutput("topdata")
		)
	  )
	),
  tabPanel("Plot Data and Fit Model",
    sidebarLayout(
	  sidebarPanel(
	  	radioButtons("dorelLinf", "Asymptotic Length",
		  choices=c("Absolute" = FALSE,
		           "Relative" = TRUE), inline=TRUE),
	    textInput("Species", label = "Species", placeholder="My Species"),
		uiOutput("InputPars"),
		conditionalPanel(condition="input.dorelLinf == 'TRUE'",
		  h4("Asymptotic Length"),
	      uiOutput("CurrLinf", style="padding-bottom:25px;")
		),
    radioButtons("Lunits", "Length units", 
                 choices=c("mm", "cm", "inch"), inline=TRUE),
		actionButton("defPars", "Reset Example Parameters", icon("gear")),
		p(),
	    uiOutput("HistControl"),
		uiOutput("clickAssess"),
		
		# actionButton("assessReady", "Plot Data", icon("area-chart"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
	
		tags$hr(),
		
		# bsCollapse(id = "extraPars",
                  # bsCollapsePanel("Additional Parameters", open=NULL,
				    # sliderInput ("beta", label=HTML("&beta;:"),  min=0, max=4, value=3, step=0.1), style="primary")
		# Alerts
        bsAlert("NegVals"),
		bsAlert("lmalert"),
	    bsAlert("lmalert2"),
	    bsAlert("linfalert"),
		bsAlert("linfalert2"),
		bsAlert("linfalert3"),
	    bsAlert("mkalert"),
		bsAlert("RelLinferr"),
		h5(a(paste("LBSPR Version: ", packageVersion("LBSPR")), href="https://github.com/AdrianHordyk/LBSPR"))
      ),
	  mainPanel(
		h3("Enter Life History Parameters"),
		h4(textOutput("ValidPars"), style = "color:red"),
		h4(textOutput("ValidData"), style = "color:red"),
		h4("The steps to fit the LBSPR model to the data are as follows:"),
		tags$ol(
		 tags$li("Enter the life history parameters for the species. Use the example parameters for the 'Example Data' from the 'Upload Data' tab.  The Species name input is only used for downloads and plots. The model and parameters are not influenced by the Species name."),
		  tags$li("Click 'Plot Data' and check that everything looks right"),
		  tags$li("If you're happy with everything, click 'Fit Model' to fit the LBSPR model to your data"),
		  tags$li("Visually inspect the fit. Has the model fitted the data? Remember: a good fit doesn't imply a good estimate, a terrible fit means something is wrong!"),
		  tags$li("Does the estimated selectivity curve make sense?"),
		  tags$li("If you are happy with everything, go to the 'Examine Results' tab")
		),
		uiOutput("Histogram"),
	    uiOutput("MatSelPlot")
	  )
	)
  ),
  tabPanel("Examine Results",
    sidebarLayout(
	  sidebarPanel(
		checkboxGroupInput("pTypes", "Display Results:",
                   c("Table of Estimates" = "table",
                     "SPR and Reference Points" = "spr",
					 "Estimates by Year" = "ests"),
				   selected=c("table", "ests", "spr")),
		radioButtons("smooth", "Apply smoother?",
		  choices=c("No" = FALSE,
		           "Yes" = TRUE), inline=TRUE),
        bsTooltip(id = "smooth", title = "Only used if more than one year",
            placement = "right", trigger = "hover"),
		h4("SPR Reference Points"),
	    sliderInput("sprlim",
                  "SPR Limit",
                  min = 0.05,
                  max = 0.95,
                  value = 0.20,
				  step=0.05),
        sliderInput("sprtarg",
                  "SPR Target",
                  min = 0.05,
                  max = 0.95,
                  value = 0.40,
				  step=0.05),
        bsAlert("refalert"),
		bsCollapse(id = "imgcontrol", 
                bsCollapsePanel("Edit SPR Circle Plot", 
				  h4("SPR and Reference Points"), 
				  colourInput("bgcol", "Background colour", value = "#FAFAFA", 
				    showColour = "background"), 
				  colourInput("limcol", "Limit colour", value = "#ff1919", 
				    showColour = "background"), 
				  colourInput("targcol", "Target colour", value = "#ffb732", 
				    showColour = "background"), 
				  colourInput("abtgcol", "Above target colour", value = "#32ff36", 
				    showColour = "background"), 
				  colourInput("labcol", "Label colour", value = "#FFFFFF", 
				    showColour = "background"),
 				  sliderInput("labcex", "Estimated SPR Size",
				    min=1, max=3, step=0.1, value=2),
 				  sliderInput("texcex", "Other Labels",
				    min=1, max=3, step=0.1, value=1.3),style = "info"),
               	bsCollapsePanel("Edit Model Estimates Plot", 				
				  h4("Estimates by Year"),
				  colourInput("CIcol", "CI bar colour", value = "darkgray", 
				    showColour = "background"), 
 				  sliderInput("ptCex", "Point Size",
				    min=1, max=3, step=0.1, value=1.5),
 				  sliderInput("axCex", "Axis Size",
				    min=1, max=3, step=0.1, value=1.45),
 				  sliderInput("labCex", "Label Size",
				    min=1, max=3, step=0.1, value=1.55),
				  radioButtons('incL50', "Include L50 line?",
				    choices=c("No"=FALSE, "Yes"=TRUE), inline=TRUE),
				  colourInput("L50col", "L50 colour", value = "gray", 
				    showColour = "background"), 					
				  style = "info"))
	  ),
	  mainPanel(
	    h3("LBSPR Results"),
	    uiOutput("ResultsText"),

		h4(uiOutput("TableHeader")),
		dataTableOutput("Estimates"),
		uiOutput("downloadEsts"),

		uiOutput("PSPRCirc"),

		uiOutput("EstsByYear")


	  )
    )
  )

  # tabPanel("Help",

  # )
)))
