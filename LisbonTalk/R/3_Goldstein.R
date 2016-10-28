# Author: tim
###############################################################################

setwd("/home/tim/workspace/LisbonTalk/")

# load some helpful functions
source("R/Functions.R")

# read in useful data!
pop  <- read("Data/Cleaned/pop2.Rdata")
dVV  <- read("Data/Cleaned/dVV2.Rdata")
dRR  <- read("Data/Cleaned/dRR2.Rdata")
fert <- read("Data/Cleaned/fert2.Rdata")


getLotkar(pop, dVV, dRR, fert, geo = "PT", year = 2013, SRB = 1.05)$r
# what if this is due to young portuguese moving abroad in reproductive ages?
# what percentage of PT fertility might be happening abroad? Can we do a counterfactual?

getLotkar(pop, dVV, dRR, fert, geo = "EU28", year = 2013, SRB = 1.05)$r
getLotkar(pop, dVV, dRR, fert, geo = "FR", year = 2013, SRB = 1.05)$r
getLotkar(pop, dVV, dRR, fert, geo = "IS", year = 2013, SRB = 1.05)$r
getLotkar(pop, dVV, dRR, fert, geo = "SE", year = 2013, SRB = 1.05)$r

#-------------

# calculate turnover for PT
getTurnover(pop, dVV, dRR, fert, 
            geo = "PT", 
            year = 2013, 
            SRB = 1.05, 
            r = NULL)

#
years <- 1993:2014
turn <- rep(0,length(years))
turn2 <- rep(0,length(years))

for (i in 1:length(years)){
	turn[i] <- getTurnover(pop, dVV, dRR, fert, geo = "PT", year = years[i], SRB = 1.05)
}
graphics.off()
plot(years, turn, type = 'l')
#lines(years, turn2, col="blue")