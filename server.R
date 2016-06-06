
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com

library(DLMtool)
library(shiny)
library(shinyBS)

source("Functions.r")

###Server
shinyServer(function(input, output, session) {
  ## Sidebar 
  #Set the available data to the numeric vector required
  availData<-reactive({
    availdat <- input$availdat
	if (length(availdat) <= 0) return(rep(0, length(DataTypes)))
	as.numeric(DataTypes %in% availdat)
  })
  
  #PickMSE
  MSE <- reactive({ChooseSFOb(input$Stock, input$Fleet)})
  
  #Pick feasability object and resulting feasable MPs
  Feasefun <- reactive({ModFease(availData(), AllFeaseObj)})
  feaseMPs <- reactive({FeaseMPs(availData(), MSE(), Feasefun())})
  
  #Pick DLM Data object
  OurData <- reactive({ModDataObj(availData(), DLMData)})
  
  # ## Debugging ##
  # output$checkfease <- renderDataTable({
    # FeaseObj <- ModFease(availData(), AllFeaseObj)
    # tt<- as.list(FeaseObj)
	# data.frame(unlist(tt ))
  # })
  # output$printfease <- renderText({
    # tt <- capture.output(Fease)
	# print(tt)
    # HTML(tt)
  # })
  output$pckV <- renderText({ 
    paste0("DLMtool Package Version: ", as.character(packageVersion("DLMtool")))
  })
  
  #################
  
  #Pick Stock Name and Link
  Stock <- reactive({
     switch(as.numeric(input$Stock), 
	  "Albacore", 
	  "Snapper", 
	  "Mackerel",
	  "Blue_shark",
	  "Sole",
	  "Rockfish"
	 )
  })
  
  output$StockName<-renderText({ if(input$Stock==0){"Please Select a Stock"} else{ colnames(Stock())[[2]]}})
  
  output$StockLink<-renderDataTable({
    if(input$Stock==0) output<-matrix(,15,)
    else{ 
      read.csv(paste0(switch(as.numeric(input$Stock), 
	  "Albacore", 
	  "Snapper", 
	  "Mackerel",
	  "Blue_shark",
	  "Sole",
	  "Rockfish"
	  ), ".csv"))  
    }	
  }, options = list(searching = FALSE, paging = TRUE))
  
  #Pick Fleet Name and Link
  Fleet<-reactive({
    Name <- paste0("Fleet", input$Fleet)
    Fleet<- get(Name)
    return(Fleet)
  })
  
  output$FleetName<-renderText({ switch(input$Fleet,
                                        "0"="Please Select a Fleet",
                                        "1"="Stable Effort",
                                        "2"="table Effort & Targetting Small Fish",
                                        "3"="Increasing Effort",
										"4"="Increasing Effort & Targetting Small Fish"
  )})
  
  output$FleetLink<-renderDataTable({
    if(input$Fleet==0) output<-matrix(,15,)
	else {
	  dat <- read.csv(paste0(switch(as.numeric(input$Fleet), "FlatE_NDom", "FlatE_Dom", "IncE_NDom", "IncE_HDom"), ".csv"))
	  dat
	}
  }, options = list(searching = FALSE, paging = TRUE))
  
  observe({ # select all checkboxes 
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"availdat","Select Available Data Types::",choices=DataTypes)
    }
    else
    {
      updateCheckboxGroupInput(session,"availdat","Select Available Data Types:",choices=DataTypes,selected=DataTypes)
    }
  })
  
  ## end Sidebar 
  ## Tabs
  #Formatting for Tab  names
  output$instructions<-renderUI({if(input$tabsetpanel=="Instructions") {strong(h4("Instructions"))} else {p(h5("Instructions"))}})
  output$MSEtab<-renderUI({if(input$tabsetpanel=="MSE") {strong(h4("Simulation Testing (MSE)"))} else {p(h5("Simulation Testing (MSE)"))}})
  output$MPtab<-renderUI({if(input$tabsetpanel=="MP") {strong(h4("Method Application"))} else {p(h5("Method Application"))}})
  output$VOItab<-renderUI({if(input$tabsetpanel=="VOI") {strong(h4("Value of Information"))} else {p(h5("Value of Information"))}})
  
  #display anything?
  showresults<- reactive({
    # if (input$Stock==0 || input$Fleet==0 ||input$Xchoice==" "||input$Ychoice==" "|| is.element(1,availData())==FALSE)
	if (input$Stock==0 || input$Fleet==0 ||input$Xchoice==" "||input$Ychoice==" ")
    {0}
    else {1}
  })
  output$display <- renderText(showresults())
  output$display2 <- renderText(showresults())
  output$display3 <- renderText(showresults()) # this is dodgy, but it works...
  
  ##Tab 1
  #create the Trade Plot
  # Data frame of performance of methods 
  Perf <- reactive({
    PerfStats(MSE())
  })
   
  PerfMetrics <- reactive({
    TradePerf(
	  Perf(),
      XAxis=c(input$Xchoice), 
      YAxis=c(input$Ychoice), 
      XThresh=input$XThresh, 
      YThresh=input$YThresh,
      maxVar=15)
  })
  
  clickedMP <- reactiveValues()
  clickedMP$clicked <- NULL
    
  observeEvent(input$plot_click,{
    tempDF <- PerfMetrics()[[1]]
    temp <- nearPoints(tempDF, input$plot_click, xvar="x", yvar="y", threshold=15, addDist=TRUE)
	clickedMP$clicked <- head(temp,1)[,1]
  })
  
  #output the trade plot 
  output$tplot <- renderPlot({ 
    TradePlot2(PerfMetrics(),
	  AvailMPs=feaseMPs(),
	  clickedMP=clickedMP$clicked,
	  XThresh=input$XThresh, 
      YThresh=input$YThresh)
  }, height = 500, width = 500)
  
  # output the pop-in information
  output$info <- renderUI({
    # if (length(input$plot_click)==0||showresults()==0 ) return("")
      # #don't show anything if no plot point's been clicked
    if(length(clickedMP$clicked)==0) 
	  return(p( HTML("<i>","Click a point for details","</i>", sep="")
       , class = "well"))
    
    #actual info
    p(HTML("<font size=\"6\" color=\"red\">&#9679;</font><i>",MPname(clickedMP$clicked),"</i>", sep=""), class = "well")
  })
  
  ## Projection Plots ##
  # Choose MPs 

  output$selectMP <- renderUI({ 
    selectInput("MP1", "Method 1",   as.character(PerfMetrics()[[1]][,1]),
	selected = NULL)
  })
  
   GetMP2 <- reactive({
    MPList <- as.character(PerfMetrics()[[1]][,1])
	ind <- MPList %in% input$MP1
	MPList[!ind]
  })
  
  output$selectMP2 <- renderUI({ 
    # selectInput("MP2", "Method 2", GetMP2(),
	MPList <- as.character(PerfMetrics()[[1]][,1])
	selectInput("MP2", "Method 2", MPList,
	selected = MPList[2])
  })   
  
  output$pplot <- renderPlot({ 
    # Top4 <- head(PerfMetrics()[[1]],4)
	# subMPs <- as.character(Top4[,1])
	if (length(input$MP1) > 0) if (input$MP1 == input$MP2) return("")
	subMPs <- c(input$MP1, input$MP2)
	MSEtemp <- MSE()
	FMSYr<-quantile(MSEtemp@F_FMSY,c(0.001,0.90),na.rm=T)
	BMSYr<-quantile(MSEtemp@B_BMSY,c(0.001,0.975),na.rm=T)
	subMSE <- Sub(MSEtemp, MPs=subMPs, sims=1:60)
	subMSE@Name <- "Projections"
    Pplot2(subMSE, nam="", YLim1=FMSYr, YLim2=BMSYr)
  })
   output$kplot <- renderPlot({ 
    # Top4 <- head(PerfMetrics()[[1]],4)
	# subMPs <- as.character(Top4[,1])
	if (length(input$MP1) > 0) if (input$MP1 == input$MP2) return("")
	subMPs <- c(input$MP1, input$MP2)
	MSEtemp <- MSE()
	subMSE <- Sub(MSEtemp, MPs=subMPs, sims=1:60)
	subMSE@Name <- "Kobe Plot"
    Kplot2(subMSE, nam="")
  }) 
  ##
  output$SameMPs <- renderText({
    if (length(input$MP1) > 0) if (input$MP1 == input$MP2) {
	  return("Please select two different methods")
	}
  })
  
  
  ##Tab 2
  #Get the MPs that meet the performance criteria
  AvailAcceptMPs <- reactive({
    perf <- PerfMetrics()[[1]]
	accept <- perf[,4]
	mpnames <- as.character(perf[,1])
	mpnames[mpnames %in% feaseMPs() & accept]
  })
  
  #Output the MP selector
  output$MPselector<-renderUI({
    if (showresults()==0) {selectInput("MP", label=h5(strong(em("Acceptable")), "and", strong(em("Available")), "Management Procedure(s) (select one):"), c("None Available or Acceptable"),width="100%")}
    else{
      #get the performing MPs
      performingMPs<-AvailAcceptMPs()
            
      if(length(performingMPs)==0) {performingMPs<-c("No Available Methods Meet Your Performance Metrics",performingMPs)}
        else {
    #remove all MPs with nas and add blank
          performingMPs<-c(" ",performingMPs)
    #name  the MPs for greater comprehension in the UI
    for (X in seq_along(performingMPs)){
      names(performingMPs)[X]<-MPname(performingMPs[X])
    }
    }
  
    #actual drop-down
    selectInput("MP", label=h5("Available Management Procedure(s): (select one)"), performingMPs,width="100%",selected=performingMPs[1])
  }}) 
  
  #Output the data table mock
  output$AvailableDataMockup <- renderTable({
    if (!all(availData() ==0))  DataTable(OurData())
  })

  #Output the recommendation
  output$mprecommendation<-renderText({
    MP<-input$MP  
    if(length(MP)==0 || MP==" " || MP=="None Available" || MP=="No Available Methods Meet Your Performance Metrics")  return("")
    Name<-MPname(MP)
    Class <- class(get(MP))
    if (Class == "DLM_output") Class <- "output control"
    if (Class == "DLM_input") Class <- "input control"
	dat <- OurData()
    test<-RunMP(1,OurData(),MP)
    paste(gsub("ton","tons",gsub("TAC","Total Allowable Catch",test[1])),sep="")
    
  })    
  ##
  ##Tab 3
 #Output the "No Available Plots" message
 # output$voi2message<-renderText({
   # if (showresults()==0) {return("")}
   # #get the performing MPs
   # #performingMPs<-PerformingMPs()
  # # if (length(performingMPs)==0) {return ("No Management Procedures Meet Your Performance Metrics.")}
   # else {
     # Top4 <- head(PerfMetrics()[[1]],4)
	 # subMPs <- as.character(Top4[,1])
	 # MSEtemp <- MSE()
	 # subMSE <- Sub(MSEtemp, MPs=subMPs)
	
      # MSEtemp<-Sub(MSEtemp, MPs=feaseMPs())
      # # voi<-VOI2(MSEtemp,lay=T)
      # # voiplots<-voi[[5]]
      # # # voiplots<-Filter(length,voiplot)
	  # nmps <- MSEtemp@nMPs
      # if(nmps < 2) {return("No Value of Information plots are available with the selected Available Data. Please select more Available Data to use this functionality.")
      # #if(length(voiplots)==0) {return("No Value of Information plots are available above the Performance Metric thresholds with the selected Available Data. Please select more Available Data to use this functionality.")
   # }
      # else{
    # return ("")}
   # }
 # })
 
 
  #Output the VOI2 plot
  output$voi2plot<-renderPlot({
    # if (showresults()==0) {
      # plot(1, type="n", axes=F, xlab="", ylab="")}
    # else{
     Perf <- PerfMetrics()[[1]]
	 Accept <- Perf[,4]
	 AvailMPs <- feaseMPs()
	 Perf$Avail <- FALSE 
	 Perf$Avail[Perf$Names %in% AvailMPs] <- TRUE
	 
	 AcceptMPs <- as.character(Perf$Names[Perf$Accept])
	 AcceptAvailMPs <- as.character(Perf$Names[Perf$Avail & Perf$Accept])
	 
	 myMPs <- AcceptAvailMPs[1:4] # Top 4 methods
	 if (any(is.na(myMPs))) { # add some unavailable methods 
	   naind <- which(is.na(myMPs))
	   Nna <- length(naind)
	   myMPs[naind] <- as.character(Perf$Names[Perf$Accept & !Perf$Avail][1:Nna])
	 }
	 if (any(is.na(myMPs))) { # add some unacceptable methods 
	   naind <- which(is.na(myMPs))
	   Nna <- length(naind)
	   myMPs[naind] <- as.character(Perf$Names[!Perf$Accept][1:Nna])
	 }	 
	 
	 MSEtemp <- MSE()	 
	 subMSE <- Sub(MSEtemp, MPs=myMPs)
	 
	 VOIplot(subMSE, availMP=AvailMPs, acceptMP=AcceptMPs, Par=input$ptype)
     # voi<-VOI2(MSEtemp,lay=T)
     # voiplots<-voi[[5]]
    # voiplots<-Filter(length,voiplots)
       # if(length(voiplots)==0) {plot(1, type="n", axes=F, xlab="", ylab="")} else {
         # MSEtemp@nMPs<-length(voiplots)
         # VOI2(MSEtemp,lay=T)
    # }
   })
  
})

