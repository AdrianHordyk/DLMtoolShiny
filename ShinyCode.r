setwd("E:/Dropbox/CAProject/DLMtoolkit/Shiny") # Path to read files
# setwd("~/Dropbox/CAProject/DLMtoolkit/Shiny")

# Install dev DLMtool package from GitHub
library(devtools)
install_github("adrianhordyk/DLMtooldev") # Install dev package from GitHub   
library(DLMtool) # use plotting functions from dev package

MSEGrid <- expand.grid(Stock=c(1:3), Fleet=c(1:3))
SFObs <- paste0("Stock", MSEGrid[,1], "_Fleet", MSEGrid[,2])
SFFiles <- paste0(SFObs, ".Rdata")

for (X in 1:length(SFFiles)) load(SFFiles[X]) # Load all MSE objects 

load("DLMData.Rdata") # Load data object

source("E:/Dropbox/CAProject/DLMtoolkit/Shiny/Functions.r")

####################################
# Create objects used in shiny app #
####################################

# Broad Data Types for Check-Boxes
# These may be modified in the future 
# These match the index position in AvailData used in ModFease function - so order is important 
DataTypes <- c("Historical Catches", "Index of Abundance", 
	"Life History Information",	"Fishing Fleet Selectivity", 
	"Time-series of Catch-at-Age", "Time-series of Catch-at-Length")

# Create Feasibility Object with all elements set to 1 	
AllFeaseObj <- new('DLM_fease')
for (X in 3:length(slotNames(AllFeaseObj)))  slot(AllFeaseObj, slotNames(AllFeaseObj)[X]) <- 1 	

#############################
# shiny user interface code #
#############################

# User Choose Stock and Fleet from dropdown menu 
Stock <- 1 
Fleet <- 1 
MSEObj <- ChooseSFOb(Stock=Stock, Fleet=Fleet) # Parameters set from dropdown menus


# User Check or Uncheck Available Data Types
# AvailData is a vector of length(DataTypes) with 0 and 1 values if checkbox unchecked or checked
# Examples below 

AvailData <- rep(1, length(DataTypes)) # all data available
AvailData <- c(1, 1, 1, 0, 0, 0) # only 'Historical Catches' and 'Index of Abundance' and ' Life History Infromation' available
AvailData <- c(1, 1, 1, 1, 1, 0) # everying except 'Catch-at-Length' available

AvailData <- c(1, 1, 1, 0, 0, 0) # Example choice 

AvailDataChoices <- expand.grid(Catch=0:1, Index=0:1, LHI=0:1, FishSel=0:1, 
	CaA=0:1, CaL=0:1)

feaseMPs <- FeaseMPs(AvailData, MSEObj, AllFeaseObj)

# Dropdown Menu with 6 Different Performance Metrics (PMs)  
# User selects 2 PMs for X-Axis and 2 PMs for Y-Axis		
PMs # These may change or more may be added 

# PM choice from dropdown menu 
Xchoice <- c(3, 4) # Choose two PMs for X-Axis
Ychoice <- c(1, 6) # Choose two PMs for Y-Axis 

XThresh <- c(50, 50) # Threshold values from sliders for X-Axis(say between 0% and 80%)
YThresh <- c(0, 50) # Threshold values from sliders for X-Axis(say between 0% and 80%)

# Parameters we can probably leave fixed in the demo 
maxVar <- 15 
ShowLabs <- FALSE # Print MP labels? - maybe better to use mouse-over text if possible?
ShowCols <- TRUE # Show background colors?

# TradePlot is function in the DLMtool dev package.  May need to modify this function 
# as necessary for the shiny app - particularly colours and sizes 
# Code for TradePlot and supporting functions below 
TradePlot(MSEObj, XAxis=PMs[[1]][Xchoice], YAxis=PMs[[1]][Ychoice], 
	XThresh=XThresh, YThresh=YThresh, maxVar=maxVar, AvailMPs=feaseMPs, 
	ShowLabs=ShowLabs, ShowCols=ShowCols)

# Grey Circle - Management Procedure (MP) does not meet minimum performance metrics
# Black Circle - MP meets minimum PM, but not available due to insufficient data
# Green Circle - MP meets minimum PM and sufficient data is available

##################
# All User Input #
##################

Stock <- 3 # 1 2 3 
Fleet <- 3 # 1 2 3 
AvailData <- as.numeric(AvailDataChoices[1,]) # Available Data 

XAxis <- PMs[[1]][c(3, 4)] # Choose two PMs for X-Axis
YAxis <- PMs[[1]][c(1, 6)] # Choose two PMs for Y-Axis 

XThresh <- c(50, 50) # Risk Thresholds 
YThresh <- c(0, 0) 

TradePlot(ChooseSFOb(Stock=Stock, 
	Fleet=Fleet), XAxis=XAxis, YAxis=YAxis, 
	XThresh=XThresh, YThresh=YThresh, maxVar=maxVar, 
	AvailMPs=FeaseMPs(AvailData, 
	ChooseSFOb(Stock=Stock, Fleet=Fleet), AllFeaseObj), 
	ShowLabs=ShowLabs, ShowCols=ShowCols)

# Show MP labels 
TradePlot(ChooseSFOb(Stock=Stock, 
	Fleet=Fleet), XAxis=PMs[[1]][Xchoice], YAxis=PMs[[1]][Ychoice], 
	XThresh=XThresh, YThresh=YThresh, maxVar=maxVar, 
	AvailMPs=FeaseMPs(AvailData, 
	ChooseSFOb(Stock=Stock, Fleet=Fleet), AllFeaseObj), 
	ShowLabs=TRUE, ShowCols=ShowCols)
	
out <- TradePlot(ChooseSFOb(Stock=Stock, Fleet=Fleet))
out # MP names can be written out and used for mouse-over text etc
# other info, like x y position could also be returned from function 

# No background colours	
TradePlot(ChooseSFOb(Stock=Stock, 
	Fleet=Fleet), XAxis=PMs[[1]][Xchoice], YAxis=PMs[[1]][Ychoice], 
	XThresh=XThresh, YThresh=YThresh, maxVar=maxVar, 
	AvailMPs=FeaseMPs(AvailData, 
	ChooseSFOb(Stock=Stock, Fleet=Fleet), AllFeaseObj), 
	ShowLabs=TRUE, ShowCols=FALSE)

#############
# Feature 2 #
#############

# Vector of available MPs 
AvailMPs <- FeaseMPs(AvailData, ChooseSFOb(Stock=Stock, Fleet=Fleet), AllFeaseObj)

OurData <- ModDataObj(AvailData, DLMData) # Data object 

# Choose From Available MPs from Dropdown list 
X <- 2 # Element X of Avail 

AvailData <- as.numeric(AvailDataChoices[64,]) # Change amount of Available Data 
AvailMPs <- FeaseMPs(AvailData, ChooseSFOb(Stock=Stock, Fleet=Fleet), AllFeaseObj)

AvailMPs
RunMP(X=2, OurData, AvailMPs)
RunMP(X=3, OurData, AvailMPs)
RunMP(X=5, OurData, AvailMPs)

# look at different options 
for (X in seq_along(AvailMPs)) {
  print(paste("MP =", AvailMPs[X]))
  print(RunMP(X=X, OurData, AvailMPs))
}














