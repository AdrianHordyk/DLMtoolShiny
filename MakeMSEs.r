setwd("E:/Dropbox/CAProject/DLMtoolkit/DLMtoolShiny") # Path to save output files
# setwd("~/Dropbox/CAProject/DLMtoolkit/DLMtoolShiny")

# Install dev DLMtool package from GitHub
library(devtools)
install_github("adrianhordyk/DLMtooldev") # Install dev package from GitHub   
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

# Define 3 Fleet Parameter Sets
Fleet1 <- DecE_Dom
Fleet2 <- FlatE_NDom
Fleet3 <- IncE_HDom

# Define 1 Observation Parameter Set
Obs <- Generic_obs

# List all available MPs 
list(Output=avail("DLM_output"), Input=avail("DLM_input"))
AllMPs <- c(avail("DLM_output"), avail("DLM_input"))

# Choose some MPs to test in the Demo - random selection here - can decide on others or more/less
MPs <- c("AvC", "CC1", "DD", "DCAC", "matlenlim", "curE", "MRreal") 

# Create list Operating Model object for all combinations of Stock and Fleet 
MSEGrid <- expand.grid(Stock=c(1:3), Fleet=c(1:3))
OMList <- list()
for (X in 1:nrow(MSEGrid)) 
OMList[[X]] <- new("OM",  Stock=get(paste0("Stock", MSEGrid[X,1])),  Fleet=get(paste0("Fleet", MSEGrid[X,2])),  Observation=Obs) 

# Other MSE Parameters 
nsim <- 50		# Number of independent simulations 
reps <- 1  		# Number of repititions for stochastic MPs 
proyears <- 60  # Number of years to project the model 
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


# load("Stock1_Fleet1.Rdata")



