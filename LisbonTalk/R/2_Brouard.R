
# Author: tim
###############################################################################

setwd("/home/tim/workspace/LisbonTalk/")

# load some helpful functions
source("R/Functions.R")

# read in useful data!
pop <- read("Data/Cleaned/pop2.Rdata")
dVV <- read("Data/Cleaned/dVV2.Rdata")
dRR <- read("Data/Cleaned/dRR2.Rdata")

Pyf  <- getPy(pop,
	         dRR,
	         dVV,
	         geo = "PT",
	         sex = "F",
	         year = 2013)
Pym  <- getPy(pop,
			    dRR,
			    dVV,
			    geo = "PT",
			    sex = "M",
			    year = 2013)

	 # take a quick look
PyrSetup(xlim = c(-1,1))
PyramidOutline(Pym$Exp, Pyf$Exp, scale = 100, col = "#00000050")
PyramidOutline(Pym$Py, Pyf$Py, scale = 100,col = "#00DDAA50")
axis(2)


# how has this changed? How far back to the stats go anyway?
# let's animate it.
years <- 1990:2014
for (yr in years){
	Pyf  <- getPy(pop,
			dRR,
			dVV,
			geo = "PT",
			sex = "F",
			year = yr)
	Pym  <- getPy(pop,
			dRR,
			dVV,
			geo = "PT",
			sex = "M",
			year = yr)
	
	# take a quick look
	PyrSetup(xlim = c(-1,1))
	title(yr,cex=4)
	PyramidOutline(Pym$Exp, Pyf$Exp, scale = 100, col = "#00000050")
	PyramidOutline(Pym$Py, Pyf$Py, scale = 100,col = "#00DDAA50")
	axis(2)
	
}


