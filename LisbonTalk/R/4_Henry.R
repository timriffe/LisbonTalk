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

# need materials for R0
opt1 <- getLotkar(pop, dVV, dRR, fert, 
                 geo = "EU28", year = 2013, 
                 SRB = 1.05)
Dat  <- opt1$Dat
R0   <- sum(Dat$Lx * Dat$Fxf)

r    <- opt1$r

# exp(r * MeanGeneration) = R0
# r *MeanGeneration = log(R0)
(MeanGeneration <- log(R0) / r)
# 
# we can either do assumptions or load in a long historical series from HMD

# let's make the strong period assumption. Since this series is so long,
# the main downside to the period assumption is erratic movements.
# More direct would be fltcoh_1x1, however, then we'd also need cohort
# fertility for a long long series. It's available, but not quite long
# enough. Would need to produce it with indirect approximations.
fltper <- read("Data/SWEfltper_1x1.Rdata")

# life expectancy over 250+ years in Sweden
plot(unique(fltper$Year), fltper$ex[fltper$Age == 0])

e0             <- fltper$ex[fltper$Age == 0]
MeanGeneration <- 30 # chosen from above

# daughters / mothers' e0
ratios         <- e0[-(1:MeanGeneration)] /    # daughters
                      e0[1:(length(e0)-MeanGeneration)]  # mothers

# this would be the R0 inflation (but note this is period e0, arg,
# actually you can just download fltcoh_1x1 and get that, but it'd be a shorter series)
# noise at start due to period data. Broad trend is the interesting part.
plot(ratios)

# so the inverse of this tells us 'how low' R0 can get while still yielding
# stationarity in years-of-life reproduction.
R0stationarysize  <- 1 / ratios

# the inverse, just mentioned.
plot(R0stationarysize, xlab = "Time since 1751", 
     ylab = "e0 ratio, flipped", 
     main = "Swedish females")
# 1 just means mothers and daughters had same e0. 
abline(h = 1)
# if the R0 line is among the points, then we're in good company
abline(h = R0, col = "red")

# what would the adjusted R0 be?
plot(ratios * R0, xlab = "Time since 1751", 
     ylab = "adjusted R0", 
     main = "Swedish females")
abline(h = 1) # stationary years.

# And despite all this, recall the various kinds of rosy findings
# about aged and shrinking populations.
# further, one could compare with cohort, just to be sure. Heck, let's do it:
# BONUS:
library(HMDHFDplus)
# (I have the variables us and pw stored on my system, you can change those
# to your own login credentials as character strings)
fltcoh <- readHMDweb("SWE","fltcoh_1x1",username=us, password=pw)

# get cohort e0
ce0             <- fltcoh$ex[fltcoh$Age == 0]
years           <- sort(unique(fltcoh$Year))
MeanGeneration <- 30 # chosen from above

# daughters / mothers' e0
cratios         <- ce0[-(1:MeanGeneration)] /    # daughters
		ce0[1:(length(ce0)-MeanGeneration)]  # mothers

plot(years[1:(length(ce0)-MeanGeneration)],cratios, xlab = "mothers' generation", ylab = "e0 ratio")
# But then to match the R0 to this, we'd also want a time series of cohort R0. Perfectly doable,
# but that'll have to wait for another time.
#-----------------------------------------------------------------



