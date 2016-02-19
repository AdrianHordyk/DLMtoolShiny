setwd("E:/Dropbox/CAProject/DLMtoolkit/DLMtoolShiny") # Path to read files
# setwd("~/Dropbox/CAProject/DLMtoolkit/DLMtoolShiny")

# Install dev DLMtool package from GitHub
library(devtools)
install_github("adrianhordyk/DLMtooldev") # Install dev package from GitHub (only once)   
library(DLMtool) # use plotting functions from dev package

MSEGrid <- expand.grid(Stock=c(1:3), Fleet=c(1:3))
SFObs <- paste0("Stock", MSEGrid[,1], "_Fleet", MSEGrid[,2])
SFFiles <- paste0(SFObs, ".Rdata")
URLs <- paste0("https://github.com/AdrianHordyk/DLMtoolShiny/raw/master/", SFFiles)

for (X in 1:length(SFFiles)) load(SFFiles[X]) # Load all MSE objects from local
# for (X in 1:length(SFFiles)) load(URLs[X]) # Alternative load MSE objects from github

load("DLMData.Rdata") # Load data object from local 
# load(url("https://github.com/AdrianHordyk/DLMtoolShiny/raw/master/DLMData.Rdata") # load data from github

#############
# Functions #
#############

# Function to get MSE object given stock and fleet number
# Stock and Fleet number from dropdown menus with descriptive labels
ChooseSFOb <- function(Stock, Fleet) {
  Name <- paste0("Stock", Stock, "_Fleet", Fleet)
  MSE <- get(Name)
  return(MSE)
}

# Function to create feasiblity object depending on what checkboxes are selected
ModFease <- function(AvailData, FeaseObj) {
  FeaseObj@Catch <- ifelse (AvailData[1] == 1, 1, 0)
  FeaseObj@Index <- ifelse (AvailData[2] == 1, 1, 0)
  FeaseObj@Natural_mortality_rate <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Maturity_at_length <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Growth <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Length_weight_conversion <- ifelse (AvailData[3] == 1, 1, 0)
  FeaseObj@Fleet_selectivity <- ifelse (AvailData[4] == 1, 1, 0)
  FeaseObj@Catch_at_length <- ifelse (AvailData[6] == 1, 1, 0)
  FeaseObj@Catch_at_age <- ifelse (AvailData[5] == 1, 1, 0)
  FeaseObj@Stock_recruitment_relationship <- ifelse (AvailData[3] == 1, 1, 0)
  # May need to add data types for below - otherwise just leave them 1s for Demo
  # FeaseObj@Recruitment_index <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_catch <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_biomass <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Target_index <- ifelse (AvailData[1] == 1, 1, 0)
  # FeaseObj@Abundance <- ifelse (AvailData[1] == 1, 1, 0)
  return(FeaseObj)
} 

  
# Function to get list of 'feasible' MPs given checked/unchecked data types 
FeaseMPs <- function(AvailData, MSEObj, AllFeaseObj) {
  FeaseObj <- ModFease(AvailData, AllFeaseObj)
  chkFease <- Fease(FeaseObj)
  allfeaseMPs <- rownames(chkFease)[which(chkFease[,1]=="Yes")]
  feaseMPs <- MSEObj@MPs[which(MSEObj@MPs %in% allfeaseMPs)]
  return(feaseMPs)
}  

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

Stock <- 1 # 1 2 3 
Fleet <- 1 # 1 2 3 
AvailData <- as.numeric(AvailDataChoices[64,]) # Available Data 

XAxis <- PMs[[1]][c(3, 4)] # Choose two PMs for X-Axis
YAxis <- PMs[[1]][c(1, 6)] # Choose two PMs for Y-Axis 

XThresh <- c(50, 50) # Risk Thresholds 
YThresh <- c(0, 0) 

maxVar <- 15 
ShowLabs <- FALSE # Print MP labels?
ShowCols <- TRUE # Show background colors?

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

################################################
# TradePlot function - generic trade-off plots #
################################################

# Trade-Off Plot Function ------------------------------------------------------
TradePlot <- function(MSEobj, XAxis=c("Overfishing", "Biomass:BMSY"), 
	YAxis=c("Long-term Yield", "AnnualVar"), XThresh=c(30, 80), YThresh=c(0,50),
	maxVar=15, BmsyRef=0.5, B0Ref=0.2, AvailMPs=NULL, ShowLabs=FALSE, 
	ShowCols=TRUE) {
  PMs <- c("Long-term Yield", "Short-term Yield", "Overfishing", "Biomass:BMSY",
	"Biomass:B0", "AnnualVar")
  # Error Checks 	
  if (prod(XAxis %in% PMs)!=1) {
    message("Available Performance Metrics")
    print(PMs)
    stop("Invalid XAxis Performance Metrics")
  }	
  if (prod(YAxis %in% PMs)!=1) {
    message("Available Performance Metrics")
    print(PMs)
    stop("Invalid YAxis Performance Metrics")
  }	
  if (length(XAxis) > 4) stop("Too many Performance Metrics (max 4)")
  if (length(YAxis) > 4) stop("Too many Performance Metrics (max 4)")
  if (length(XAxis) != length(YAxis)) stop("XAxis must be of same length as YAxis")
  if (length(XThresh) != length(XAxis) | length(YThresh) != length(XAxis)) 
	warning("Risk Threshold not same length as number of PMs")
  
  Yd<-rep(NA,MSEobj@nMPs)
  BMSYref<-rep(NA,MSEobj@nMPs)
  B0ref<-rep(NA,MSEobj@nMPs)
  PNOF<-rep(NA,MSEobj@nMPs)
  LTY<-rep(NA,MSEobj@nMPs)
  STY<-rep(NA,MSEobj@nMPs)
  VY<-rep(NA,MSEobj@nMPs)

  y1 <- 1:(MSEobj@proyears-1)
  y2 <- 2:MSEobj@proyears
  
  ystart<-1:5
  yend<-max(MSEobj@proyears-4,1):MSEobj@proyears
  
  RefYd<-MSEobj@OM$RefY
  if (maxVar < 1) maxVar <- maxVar * 100
  
  for(mm in 1:MSEobj@nMPs){  
    PNOF[mm]<-round(sum(MSEobj@F_FMSY[,mm,]<1,na.rm=T)/prod(dim(MSEobj@F_FMSY[,mm,]),na.rm=T)*100,1)
    BMSYref[mm]<-round(sum(MSEobj@B_BMSY[,mm,]>BmsyRef,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
	B0ref[mm]<-round(sum((MSEobj@B[,mm,]/MSEobj@B[,mm,1] * MSEobj@OM$Depletion) >B0Ref,na.rm=T)/prod(dim(MSEobj@B_BMSY[,mm,]))*100,1)
    LTY[mm]<-round(sum(MSEobj@C[,mm,yend]/RefYd>0.5,na.rm=T)/(MSEobj@nsim*length(yend)),3)*100
	STY[mm]<-round(sum(MSEobj@C[,mm,ystart]/RefYd>0.5,na.rm=T)/(MSEobj@nsim*length(ystart)),3)*100
	# LTY[mm]<-round(mean(apply(MSEobj@C[,mm,yend],1,mean,na.rm=T)/RefYd,na.rm=T)*100,1)
	# STY[mm]<-round(mean(apply(MSEobj@C[,mm,ystart],1,mean,na.rm=T)/RefYd,na.rm=T)*100,1)
    AAVY<-apply((((MSEobj@C[,mm,y1]-MSEobj@C[,mm,y2])/MSEobj@C[,mm,y2])^2)^0.5,1,mean,na.rm=T) 
    VY[mm]<-round(sum(AAVY<(maxVar/100),na.rm=T)/MSEobj@nsim,3)*100
  }
  
  for (xx in seq_along(XAxis)) {
    name <- paste0("X", xx)
	name1 <- paste0("XLab", xx)
    assign(name, GetStat(XAxis[xx], LTY, STY, PNOF, BMSYref, B0ref, VY))
	assign(name1, StatLab(XAxis[xx], maxVar, BmsyRef, B0Ref))
	name <- paste0("Y", xx)
	name1 <- paste0("YLab", xx)
	assign(name, GetStat(YAxis[xx], LTY, STY, PNOF, BMSYref, B0ref, VY))
	assign(name1, StatLab(YAxis[xx], maxVar, BmsyRef, B0Ref))
  }
  
  Nplot <- length(XAxis)
  if (Nplot == 1) par(mfrow=c(1,1), mar=c(4,4.5,1,1), oma=c(1,1,0,0))
  if (Nplot == 2) par(mfrow=c(1,2), mar=c(4,4.5,1,1), oma=c(1,1,0,0))
  if (Nplot == 3) par(mfrow=c(1,3), mar=c(4,4.5,1,1), oma=c(1,1,0,0))
  if (Nplot == 4) par(mfrow=c(2,2), mar=c(4,4.5,1,1), oma=c(1,1,0,0))
  
  OutList <- list()
  for (xx in seq_along(XAxis)) {
    Xname <- paste0("X", xx)
	XLab <- paste0("XLab", xx)
	Yname <- paste0("Y", xx)
	YLab <- paste0("YLab", xx)
    rr <- tradeoffplot4(x=get(Xname), y=get(Yname), get(XLab), get(YLab), 
		labs=MSEobj@MPs[1:MSEobj@nMPs],vl=XThresh[xx],hl=YThresh[xx], 
		ShowLabs=ShowLabs,  ShowCols=ShowCols, AvailMPs=AvailMPs)
	
	labs <- MSEobj@MPs[1:MSEobj@nMPs]
	ind <- which(labs %in% rr)
    tempDF <- data.frame(MP=rr, X=get(Xname)[ind], Y=get(Yname)[ind])
	Dist <- NULL # calculate distance from corner
    for (X in 1:length(tempDF[,2])) Dist[X] <- euc.dist(c(tempDF[X,2], tempDF[X,3]), c(100, 100))
	tempDF <- tempDF[order(Dist),]
	rownames(tempDF) <- 1:nrow(tempDF)
	OutList[[xx]] <- tempDF
  }
 
  print(OutList)
  invisible(OutList)
  
}

# Supporting functions 
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

GetStat <- function(PM, LTY, STY, PNOF, BMSYref, B0ref, VY) {
  switch(PM,
    "Long-term Yield" = LTY,
	"Short-term Yield" = STY,
	"Overfishing" = PNOF,
	"Biomass:BMSY" = BMSYref,
	"Biomass:B0" = B0ref,
    "AnnualVar" = VY)
}

StatLab <- function(PM, maxVar, BmsyRef, B0Ref) {
  switch(PM,
    "Long-term Yield" = "Long-term Yield",
	"Short-term Yield" = "Short-term Yield",
	"Overfishing" = "Prob. of Not Overfishing (%)",
	"Biomass:BMSY" = paste0("Prob. Biomass >",BmsyRef, "BMSY (%)"),
	"Biomass:B0" = paste0("Prob. Biomass >",B0Ref, "B0 (%)"),,
    "AnnualVar" = paste0("Prob. AAVY <", maxVar, "%")
	)
}

tradeoffplot4<-function(x,y,xlab,ylab,labs,cex,vl,hl, 
	ShowLabs=FALSE,  ShowCols=FALSE, AvailMPs=NULL){
   adjj<-c(0.7,1.3)
   XLim <- c(min(c(-10, min(x,na.rm=T)*adjj)), max(c(max(x,na.rm=T)*adjj, 110)))
   YLim <- c(min(c(-10, min(y,na.rm=T)*adjj)), max(c(max(y,na.rm=T)*adjj, 110)))
   
   # Which MPs meet minimum PMs 
   ind <- which(x >= vl & y >=hl)
   coly <- rep("darkgray", length(labs)) 
   coly[ind] <- "black" 
   # coly[labs%in%c("AvC","curE","FMSYref")]<-'black'
   
   Pch <- rep(21, length(labs))
   # Pch[labs%in%c("AvC","curE","FMSYref")] <- 17
   coly[grep("FMSY", labs)]<-'black'
   Pch[grep("FMSY", labs)] <- 24
   if (!is.null(AvailMPs)) Pch[labs%in%AvailMPs] <- 21
   if (!is.null(AvailMPs)) coly[labs%in%AvailMPs & (x >= vl & y >=hl)] <- "green"
   # coly<-rep(c('#0000ff95','#ff000095','#20ff1095'),50)[1:length(labs)]

   plot(NA,xlim=XLim,ylim=YLim,xlab=xlab,ylab=ylab, bty="l", las=1)
   abline(v=vl,col="#99999940",lwd=2)
   abline(h=hl,col="#99999940",lwd=2)
   
   Alpha <- 30
   # polygons 
   LeftCol <- rgb(red=255, green=0, blue=0, alpha=Alpha, names = NULL, 
	maxColorValue = 255)
   RightCol <- rgb(red=0, green=255, blue=0, alpha=Alpha, names = NULL, 
	maxColorValue = 255)   

   if(ShowCols) {
     polygon(x=c(0, vl,  vl, 0), y=c(0, 0, hl, hl), col=LeftCol, border=NA)
     polygon(x=c(0, vl,  vl, 0), y=c(0, 0, 100, 100), col=LeftCol, border=NA)
     polygon(x=c(vl,  100, 100, vl), y=c(0, 0, 100, 100), col=RightCol, border=NA)
     polygon(x=c(vl, 100,  100, vl), y=c(hl, hl, 100, 100), col=RightCol, border=NA)
    }
   
    Cex <- 1.5
   if(!ShowLabs) points(x,y, bg=coly, pch=Pch, cex=Cex, col="black" )
   if(ShowLabs) text(x,y,labs,font=2,col=coly,cex=1)
   # if(IdPoints) {
    # message("Click points on plot to display MP name")
	# message("Click Stop to finish")
	# flush.console()
	# identify(x,y, labels=labs)
   # }	
   
   labs[ind]
   
}


