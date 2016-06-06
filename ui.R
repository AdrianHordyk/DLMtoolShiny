
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyBS)

source("Functions.r")

  Dev <- TRUE
 #Dev <- FALSE # Set Dev <- FALSE for live version

if (Dev) { # development mode 
  cond <- "input.see!=1"
  Selected <- 1 
  Selected2 <- "Not Overfished (probability Biomass > 0.5 BMSY)"
  Selected3 <- "Long-Term Yield (yrs 26-30, relative to fishing @ FMSY)"
}
if (!Dev) { # normal mode
  cond <- "input.see>=1"
  Selected <- NULL
  Selected2 <- NULL
  Selected3 <- NULL
}


##Actual UI
shinyUI(fluidPage(
  fluidRow(
    br(),
    column(6, h5("")),
    column(6,actionButton("see","Start the Demo")),
	br()
  ),
 
  conditionalPanel(cond,
  # conditionalPanel("input.see!=1",
                   sidebarPanel( 
                     ##Stock Type
                     # fluidRow(
                       # # column(6, h5("Start Here")),
                       # column(6, tags$a(href="javascript:window.open('Glossary.htm', '_blank','width=600,height=400')", h5("Glossary")),align = "left")
                     # ),
                     wellPanel(
                     h4("Operating Model"),
                     #drop-down
                     fluidRow(column(12,selectInput("Stock",label="Stock Type:",
                                                    c(" "=0,
                                                      "Albacore"=1,
                                                      "Snapper"=2,
                                                      "Mackerel"=3,
													  "Blue Shark"=4,
													  "Sole"=5, 
													  "Rockfish"=6
													  ), selected=Selected), style="margin-bottom:-18px"),
                              column(12,actionLink("stock","See operating model parameter table for your stock"))
                              , style="margin-bottom:20px"),
                     bsModal("StockTable", textOutput("StockName"), "stock", dataTableOutput("StockLink")),                                     
                     ##
                     
                     ##Fleet Type 
                     #dropdown
                     fluidRow(column(12,selectInput("Fleet",label="Fleet Type:" ,
                                                    c(" "=0,
                                                      "Stable Effort"=1,
                                                      "Stable Effort & Targetting Small Fish"=2,
                                                      "Increasing Effort"=3,
													  "Increasing Effort & Targetting Small Fish"=4
													  ), selected=Selected), style="margin-bottom:-18px"),
                              column(12,actionLink("fleet","See operating model parameter table for your fleet"))
                     ),
                     bsModal("FleetTable", textOutput("FleetName"), "fleet", dataTableOutput("FleetLink")),
					                                    
                     ##
                     ### X Axis - wellpanel provides visual separation
                     br(),
                     h4("Performance Metrics and Risk Thresholds"),             
                       ##Performance Metric 
                       #Title with help button    
                       #dropdown
                       selectInput("Xchoice", label="Biological Performance:",
                                   c(" ",
								     "Not Overfished (probability Biomass > 0.5 BMSY)"="Biomass:BMSY" ,
									 "Not Severely Overfished (probability Biomass > 0.2 BMSY)"="Biomass:B0",
                                     "Not Overfishing (probability of F < FMSY)"="Overfishing"
                                    ), selected=Selected2),
                       ##
                       ##Threshold
                       sliderInput("XThresh",label=NULL,min=20, max=95, value=75), #,style="padding: 1px;margin-top:-3px;"),
                     ##
                     ###
                     ### Y Axis - wellpanel provides visual separation
                     
                       ##Performance Metric 
                       #Title with help button
                       #Drop-down
                       selectInput("Ychoice", label="Yield Performance:",
                                   c(" ",
                                     "Long-Term Yield (yrs 26-30, relative to fishing @ FMSY)"="Long-term Yield",
                                     "Short-Term Yield (10 yrs, relative to fishing @ FMSY)"="Short-term Yield" ,
                                     "Inter-Annual Variability in Yield (probability < 15%)"="AnnualVar"), selected=Selected3),
                       #Threshold
                       sliderInput("YThresh",label=NULL,min=20, max=80, value=20)
					   ), #                     ,style="padding: 1px;margin-top:-3px;"),
                     
                     ##Available Data
                     #Title with help button
					 wellPanel(
                     h4("Available Data Types"),
                                      
                     #checkboxes - separated for numeric vectoring ease
					 actionLink("selectall", "Select All"),
					 checkboxGroupInput("availdat", "Select Available Data Types:", choices=DataTypes, selected = NULL),					 
					 actionLink("AvailableDataMock","See Example Data Object"),
				     p("Note: this is example data, and the amount of data will change with the selected check boxes"),
                     bsModal("AvailableDataTable", "Data Input Table (Mock Data)", "AvailableDataMock", 
                        tags$head(
                         tags$style(HTML(".modal-dialog {width: 75%; }"))
                        ),
                        tableOutput("AvailableDataMockup"))
					 )
					 #textOutput("pckV") # print DLMTool package version
                     ##
                     # style="padding: 0px;margin-left:-30px;margin-right:-20px",  width=3)
					 ,style="padding: 0px;margin-left:-30px;margin-right:-20px; background-color: #ffffff;",  width=3),
                   
                   mainPanel(
                     tabsetPanel(id="tabsetpanel",
                                 tabPanel(htmlOutput("instructions"), value="Instructions",
                                   br(),								 
								   wellPanel(
								   h3("Overview"),
								   p("Please read the instructions for each of the three features below (each correspond to the three tabs above) then select your inputs in the left column for the Operating Model, Performance Metrics/Risk Thresholds, and Available Data Types."),
								   p("Once the inputs are populated, you can review the results by selecting the tabs above for Simulation Testing (MSE), Method Application, and Value of Information.  You can change the inputs at any time and the results under each tab will update based on your new selections.")
								   , style="background-color: #ffffff;"),
								   wellPanel(
								   h3("Management Strategy Evaluation"),							
								   h4("1. Select an Operating Model:"),
								   p("Use the drop-down menus to select a", strong(em("Stock Type")), "and a", strong(em("Fleet Type")), "from the list provided."),
                                   p("Note that these stock and fleet types are examples for demonstration purposes. 
								   The DLMtool allows complete customization of stock and fleet types, as well the observation 
								   model for generating the data."), 								   
								   
								   h4("2. Choose Performance Metrics"),
								   p("Use the drop-down menus to select a", strong(em("Biological Performance Metric")), "and a", strong(em("Yield Performance Metric")),
								     "from the list provided."),	   
								   p("Note that these are example performance metrics for demonstration purposes. 
								   Additional performance metrics are available in the full version of the DLMtool, 
								   and all performance metrics are fully customizable."),
								   
								   h4("3. Set Risk Thresholds"),
								   p("Use the slider bars to set the risk thresholds (probabilities and relative performance) for the Performance Metrics.  
								    The risk thresholds determine the minimum performance criteria."), 
                                   p("Any methods that do not meet the minimum performance criteria (i.e., probability of meeting performance metric is lower than risk threshold) are not marked as", strong(em("not acceptable."))),	
									
								   h4("4. Select Available Data Types"),
								   p("Use the checkboxes to specify what data-types are available. 
								   The available data determines which methods are", strong(em("available."))),
								   p("Note that these are example data types. The DLMtool includes a wide range of data types"),
								   
								   h4("5. MSE Results"),
								   p("Select the", strong(em("Simulation Testing (MSE)")), "tab to see the output of the MSE"),
								   p("The graphs will automatically update if you change the Stock, Fleet, Performance Metrics, 
								   Risk Thresholds, or Available Data."), 
								   p("The DLMtool has a range of plotting and summary functions to examine the output of the MSE, 
								   and users can customize their own plots.")
								   , style="background-color: #ffffff;"),
								   wellPanel(
								   h3("Application to Real Data"),
								   p("Select the", strong(em("Method Application")), "tab to see the (example) management recommendations 
								   from the", strong(em("available")), "and", strong(em("acceptable")), "methods determined by the MSE."),
								   p("Note that the data in these examples are entirely fictitious. The DLMtool uses a data object that includes all data from the fishery.") 
								   , style="background-color: #ffffff;"),
								   wellPanel(
								   h3("Value of Information"),
								   p("Select the", strong(em("Value of Information")), "tab to learn from the MSE results which operating model and observation parameters are most important.")
					               , style="background-color: #ffffff;"),
                                   wellPanel(
								   h4("Glossary"),
								   p("Click", tags$a(href="javascript:window.open('Glossary.htm', '_blank','width=600,height=400')", "here"), "for a glossary of the terms used in this demo."), style="background-color: #ffffff;")					   
								 ),
                                 tabPanel(htmlOutput("MSEtab"), value="MSE",
								   conditionalPanel(condition="output.display !=1", 
                                          h3("Select Stock, Fleet, and Performance Metrics"),
										  h4("See", em("Instructions"), "Tab for more details"),
										  tags$style(type='text/css', '#display {color: white;}'),
										  textOutput("display")
								   ),
								   conditionalPanel(condition="output.display ==1",
                                     wellPanel(								   
										  h4("Use Management Strategy Evaluation (MSE) to identify appropriate data-limited management methods for your fishery"),
										  p("This plot shows the relative performance of the data-limited management methods
										    with respect to the", em("Biological"), "and", em("Yield"), "performance metrics."),
										  p("Each point represents a different management method. Click a point for details of the management procedure.  Note that this is a subset of the methods in the DLMtool, and users can develop and add their own management procedures."),				  
										  h5(strong("Key:")),
										  tags$ul(
										    tags$li("Green:", strong(em("Acceptable")),  "(meets minimum performance metrics) and", strong(em("Available")), "(meets data requirements)"),
											tags$li("Black:", strong(em("Acceptable")), "and", strong(em("Not Available")), "(insufficient data)"),
											tags$li("Gray:", strong(em("Not Acceptable")), "(does not meet minimum performance metrics)")
										  ),
										 
                                          #Tradeplot
										  fluidRow(column(12,
										    plotOutput("tplot", click = "plot_click", height="100%", width="100%")
										  )),
										  fluidRow(column(12,
										    htmlOutput("info", style="padding:0px 0px 0px 0px")
										  ))
																  
                                          # fluidRow(p(strong("Performance Trade-Offs Among Management Procedures"),align = "center"),
                                                   # plotOutput("tplot", click = "plot_click")                                                   
                                          # )
                                        , style="background-color: #ffffff;"),  
                                        #Pplots
                                        wellPanel(
										  h4(HTML(paste0("Fishing Mortality (F/F", tags$sub("MSY"), ") and Biomass Projections (B/B", tags$sub("MSY"), ") for the Best-Performing Methods"))),
										  p("The MSE results can be used to determines the conditions that influence the performance of a method. For example, some methods perform well (i.e., maintain stock at healthy levels) only if the stock is in a healthy initial state (i.e., biomass is at or greater than BMSY). "),
																			   
										  p("The DLMtool includes many plotting functions to examine the performance of data-limited methods.  These plots compare the performance of two Data-Limited Management Methods with
										  respect to the relative fishing mortality and stock biomass."),
										  p("Use the drop-down menu to select management methods to compare:"),										 
										  fluidRow(
										    column(4, align="center", htmlOutput("selectMP")),
										    column(4, align="center", htmlOutput("selectMP2"))
										  ), 
										  textOutput("SameMPs"),
										  fluidRow(column(10, align="center", plotOutput("pplot"))),
										  fluidRow(column(10, align="center", plotOutput("kplot")))	
                                        , style="background-color: #ffffff;")										
                                 )),
                                 tabPanel(htmlOutput("MPtab"),  value="MP",
								   conditionalPanel(condition="output.display2 !=1", 
                                          h3("Select Stock, Fleet, and Performance Metrics"),
										  h4("See", em("Instructions"), "Tab for more details"),
										  tags$style(type='text/css', '#display2 {color: white;}'),
										  textOutput("display2")
								   ),
								   conditionalPanel(condition="output.display2 ==1",
                                          h4("Apply a management procedure that meets your performance metrics and is available with current data"),
                                          p(strong("Important note:"), "this is only a demonstration using example data; download the full version of the Toolkit to input real fisheries data, apply management procedures, and obtain recommended levels of input or output controls for your fishery.",style="margin-left:5%;margin-right:5%"),
                                          #Spacer
                                          h1(""),
                                          
                                          #visual separation
                                          wellPanel(
                                            htmlOutput("MPselector"),
                                            h5("Management Recommendation:"),
                                            wellPanel(textOutput("mprecommendation"), style="background-color: white;")                                            
                                          )
								)),
                                 tabPanel(htmlOutput("VOItab"), value="VOI",
								  conditionalPanel(condition="output.display3 !=1", 
                                          h3("Select Stock, Fleet, and Performance Metrics"),
										  h4("See", em("Instructions"), "Tab for more details"),
										  tags$style(type='text/css', '#display3 {color: white;}'),
										  textOutput("display3")
								   ),
								   conditionalPanel(condition="output.display3 ==1",
                                          h4("Learn what data are most valuable to improve the performance of currently available management procedures"),
										  p("The MSE results can be used to examine which operating model and observation parameters are most influential in determining the 
										  performance of a data-limited management method.  This information can used to decide on research priorities for improving 
										  the management of the fishery."),
										  p("This plot shows how the yield is influenced by variability or uncertainty in various parameters for the top four performing methods. 
										  The black lines show how the expected yield is influenced by each
										  parameter.  A steep slope indicates that yield is strongly determined
										  by the variability or range in the parameter. A flat line suggests
										  that the yield is not greatly influenced by variability in the
										  parameter."),
                                          p("The Operating Model parameters show which life-history or
										  population parameters most influence the performance of a management
										  method, and can be used to determine the situations where methods are
										  likeky to be most useful.  The Observation parameters show how
										  bias and error in the data sources influence the
										  expected yield.  This information can be used to identify the data
										  sources that are most important in the performance of a method, and 
										  assist in prioritizing future research efforts."),
										  p("Green names indicate methods that are", strong(em("Available and Acceptable.")), "Black names are", strong(em("Acceptable")), "but", strong(em("Not Available")), "and Grey names are", strong(em("Not Acceptable."))),
										  
										  
                                          # textOutput("voi2message"),
										  radioButtons("ptype", "Parameter Type:",
										    choices = c("Operating Model" = "OM", "Observation" = "Obs"), 
											inline = FALSE, width = NULL),
										  p("Note: not all management methods use data and may not appear in the 'Observation Parameters' plot."),
										  plotOutput("voi2plot", height="600px")
										  
                                 ))    
                     )
                   ))
				   
	# Debugging 
	# ,	   
	# dataTableOutput("checkfease"),
	# textOutput("printfease")
)
)
