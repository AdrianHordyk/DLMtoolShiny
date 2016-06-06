setwd("E:/Dropbox/CAProject/ShinyDemo") # Path to save output files
# setwd("D:/Dropbox/CAProject/ShinyDemo") # Path to save output files
# setwd("~/Dropbox/CAProject/DLMtoolkit/DLMtoolShiny")

# Install dev DLMtool package from GitHub
 # library(devtools)
 # install_github("adrianhordyk/DLMtool") # Install dev package from GitHub   

library(DLMtool)

# Assign objects using Tom's naughty trick 
for (i in 1:length(DLMdat))  assign(DLMdat[[i]]@Name,DLMdat[[i]])

# Set up Parallel Processing
NCPU <- detectCores() - 1 # Can set this to all cores 
sfInit(parallel=TRUE,cpus=NCPU)
sfExportAll()

# Look at available Stock, Fleet, and Observation Objects
avail("Stock") # Built-in Stocks 
avail("Fleet") # Built-in Fleets
avail("Observation") # Built-in Observation models 

# Define 3 Stock Parameter Sets
Stock1 <- Albacore
Stock2 <- Snapper
Stock3 <- Mackerel 
Stock4 <- Blue_shark
Stock5 <- Sole
Stock6 <- Rockfish

# Define 3 Fleet Parameter Sets
Fleet1 <- FlatE_NDom
Fleet2 <- FlatE_Dom
Fleet3 <- IncE_NDom
Fleet4 <- IncE_HDom

# Define 1 Observation Parameter Set
Obs <- Generic_obs

# List all available MPs 
list(Output=avail("DLM_output"), Input=avail("DLM_input"))
AllMPs <- c(avail("DLM_output"), avail("DLM_input"))

# Choose some MPs to test in the Demo - random selection here - can decide on others or more/less
MPs <- c("AvC", "DBSRA", "DD", "DCAC", "EDCAC", 
         "Islope1", "IT5", "Itarget1", "Ltarget1",
		 "MCD", "YPR", "CC1",
		 "ItargetE4", "LstepCE1",		 
		 "matlenlim", "curE", "MRreal") 

# Create list Operating Model object for all combinations of Stock and Fleet 
MSEGrid <- expand.grid(Stock=c(1:6), Fleet=c(1:4))
OMList <- list()
for (X in 1:nrow(MSEGrid)) {
  OMList[[X]] <- new("OM",  
    Stock=get(paste0("Stock", MSEGrid[X,1])),  
	Fleet=get(paste0("Fleet", MSEGrid[X,2])),  
	Observation=Obs) 
}

# Other MSE Parameters 
nsim <- 60		# Number of independent simulations 
reps <- 1  		# Number of repititions for stochastic MPs 
proyears <- 30  # Number of years to project the model 
interval <- 5   # Interval (in years) when the MP is applied 

pstar <- 0.5    # Quantile to sample TAC from stochastic methods - 0.5 is median
maxF <- 0.8     # Ceiling F value so it doesn't run off to super high values
timelimit <- 10 # timelimit in seconds for a MP to be included - LBSPR methods are slooooow

for (X in 1:nrow(MSEGrid)) {
  Name <- paste0("Stock", MSEGrid[X,1], "_Fleet", MSEGrid[X,2])
  MSERun <- runMSE(OM=OMList[[X]], MPs=MPs, nsim=nsim, proyears=proyears, 
	interval=interval, pstar=pstar, maxF=maxF, timelimit=timelimit, reps=reps)
  assign(Name, MSERun) 	
  save(list=Name, file=paste0(Name, ".Rdata"))
}


load("Stock6_Fleet3.Rdata")
MSEobj <- Stock6_Fleet3


## Test Results ##
MSEGrid <- expand.grid(Stock=c(1:6), Fleet=c(1:4))
SFObs <- paste0("Stock", MSEGrid[,1], "_Fleet", MSEGrid[,2])
SFFiles <- paste0(SFObs, ".Rdata")
for (X in 1:length(SFFiles)) load(SFFiles[X]) # Load all MSE objects 
MSEobj <- Stock1_Fleet1

for (St in 1:6) {
for (Fl in 1:4) {
  mse <- get(paste0("Stock", St, "_Fleet", Fl))
  TradePlot(mse, ShowLabs=TRUE, XAxis = "Biomass:BMSY", 
  YAxis="Long-term Yield",  XThresh=0, YThresh=0)
  print(mse@Name)
  readline("press enter")
}
}

