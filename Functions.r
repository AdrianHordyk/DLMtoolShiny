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

# Function to modify data object depending on what data types are selected
ModDataObj <- function(AvailData, DLMData) {
  DLMDataOut <- DLMData
  if(AvailData[1] != 1) DLMDataOut@Cat[1,] <- NA
  if(AvailData[2] != 1) DLMDataOut@Ind[1,] <- NA
  # if(AvailData[1] != 1) DLMDataOut@Rec[1,]  
  # if(AvailData[1] != 1) DLMDataOut@t 
  # if(AvailData[1] != 1) DLMDataOut@AvC 
  # if(AvailData[1] != 1) DLMDataOut@Dt
  # if(AvailData[1] != 1) DLMDataOut@Mort 
  # if(AvailData[1] != 1) DLMDataOut@FMSY_M 
  # if(AvailData[1] != 1) DLMDataOut@BMSY_B0 
  # if(AvailData[1] != 1) DLMDataOut@Cref 
  # if(AvailData[1] != 1) DLMDataOut@Bref
  # if(AvailData[1] != 1) DLMDataOut@Iref
  if(AvailData[3] != 1) DLMDataOut@L50 <- NA
  if(AvailData[3] != 1) DLMDataOut@L95 <- NA
  if(AvailData[4] != 1) DLMDataOut@LFC <- NA
  if(AvailData[4] != 1) DLMDataOut@LFS <- NA
  if(AvailData[5] != 1) DLMDataOut@CAA[1,,] <- NA
  # if(AvailData[1] != 1) DLMDataOut@Dep
  if(AvailData[2] != 1) DLMDataOut@Abun[1,] <- NA
  if(AvailData[3] != 1) DLMDataOut@vbK <- NA
  if(AvailData[3] != 1) DLMDataOut@vbLinf <- NA
  if(AvailData[3] != 1) DLMDataOut@vbt0 <- NA
  if(AvailData[3] != 1) DLMDataOut@wla <- NA
  if(AvailData[3] != 1) DLMDataOut@wlb <- NA
  if(AvailData[3] != 1) DLMDataOut@steep <- NA
  if(AvailData[3] != 1) DLMDataOut@MaxAge <- NA
  if(AvailData[6] != 1) DLMDataOut@CAL_bins <- 0
  if(AvailData[6] != 1) DLMDataOut@CAL[1,,] <- NA
  if(AvailData[6] != 1) DLMDataOut@ML[1,] <- NA
  if(AvailData[6] != 1) DLMDataOut@Lbar[1,] <- NA
  if(AvailData[6] != 1) DLMDataOut@Lc[1,] <- NA

  return(DLMDataOut)
} 
 
# Run a MP 
RunMP <- function(X, OurData, AvailMPs) {
  fun <- get(AvailMPs[X])
  Class <- class(fun)
  if (Class == "DLM_output") {
    # rr <- TAC(DLM_data=OurData, MPs=AvailMPs[X], reps=1)
	TAC <- fun(x=1, DLM_data=OurData, reps=1)
	TAC <- round(TAC,0)
	return(paste("Set TAC to", TAC, "ton")) 
  }
  if (Class == "DLM_input") {
    rr <- runInMP(OurData, MPs=AvailMPs[X], reps=1)[[1]][,1,1]
    if (!is.na(rr[5])) return(paste("Set size limit to", round(rr[5],0), "mm")) 
	if(length(grep("MR", AvailMPs[X])>0)) return("Close spatial area") 
    if(length(grep("E", AvailMPs[X])>0)) return(paste("Change fishing effort by a factor of", round(rr[2],2))) 
  }
}


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


