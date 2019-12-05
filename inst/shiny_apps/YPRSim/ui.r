library(shiny)
library(shinyBS)

shinyUI(tagList(tags$head(includeScript("google-analytics.js")),
  fluidPage(titlePanel("Simulation Modeling"),
  # navbarPage("LBSPR Simulation Modeling", id="navbar",
  # tabPanel("Instructions",
	  # h3("Instructions")
  # ),
  # tabPanel("Simulation Model",
     # titlePanel("Upload Data File"),
    sidebarLayout(
	  sidebarPanel(
		uiOutput("InputPars"),
		# conditionalPanel(condition="input.dorelLinf == 'TRUE'",
		  # h4("Asymptotic Length"),
	      # uiOutput("CurrLinf", style="padding-bottom:25px;")
		# ),
		uiOutput("FishingPars"),
		actionButton("defPars", "Reset Default Parameters", icon("gear")),
		tags$hr(),
		h5(a(paste("LBSPR Version: ", packageVersion("LBSPR")), href="https://github.com/AdrianHordyk/LBSPR"))
      ),
   	  mainPanel(
	  	h4(textOutput("Loading"), style = "color:red"),
		column(12, plotOutput("SizeComp", height=700, width="100%")),
		fluidRow(downloadButton("dnloadImage", label = "Download", class = NULL), 
		  style="padding-bottom: 25px; padding-left: 30px;"),
	    column(12,
	      wellPanel(
		    h4("Controls"), 
			fluidRow(column(3,
	          radioButtons("Ltype", "Catch or Population?", 
		        choices=c("Catch" = TRUE, "Population" = FALSE), inline=FALSE)
			),
            column(3, 				   
    	      radioButtons("perRec", "Per-Recruit?", 
		        choices=c("No"=FALSE, "Yes"=TRUE), inline=FALSE)
			),
			column(2,
			  radioButtons("x.type", "X-Axis",
			    choices=c("SPR"="SPR", "SSB"="SSB", "F/M"="FM"), selected="FM")
			),
			column(4,
			  checkboxGroupInput("y.type", "Y-Axis",
			    c("Spawning Potential Ratio"="SPR", "Spawning Stock Biomass"="SSB", "Relative Yield"="yield", "Recruitment"="Rec"),
				selected=c("SPR", "SSB", "yield"))	    
			)
			
			
			),
            fluidRow(
			 column(3,
    	      radioButtons("plots", "Plots", 
		        choices=c("All"="all", "Length Frequency"="len.freq", 
				"Maturity/Selectivty"="maturity.select",
				 "Growth"="growth", "Yield Curve"="yield.curve"))
			  ),
			 column(3, radioButtons("LorW", "Length or Weight?",
			                        c("Length"="LAA", "Weight"="WAA"), inline=FALSE)),			 
			 column(3, radioButtons("inc.SPR", "Show SPR?",
			    c("Yes"=TRUE, "No"=FALSE), inline=FALSE)),
			 column(3, radioButtons("inc.pts", "Show Points?",
			    c("Yes"=TRUE, "No"=FALSE), inline=FALSE))			 	  
			), fluidRow(
			column(3, sliderInput("size.SPR", label="SPR Size", value=5, min=1, max=10, step=1)),
			column(3, sliderInput("size.pt", label="Point Size", value=5, min=1, max=10, step=1)),
			column(3, sliderInput("size.axtex", label="Axis Size", value=12, min=1, max=20, step=2)),	  
			column(3, sliderInput("size.title", label="Label Size", value=14, min=1, max=20, step=2)),
			column(3, sliderInput("size.leg", label="Legend Size", value=12, min=1, max=20, step=2))
			)
          ), tags$hr()			
		)
	)
  )
 )
))



