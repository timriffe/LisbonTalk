
# Author: tim
###############################################################################

# set the working directory (change this to where your folder is)
setwd("/home/tim/workspace/LisbonTalk/")

# load some helpful functions
source("R/Functions.R")


# read in useful data!
pop <- read("Data/Cleaned/pop2.Rdata")
ex  <- read("Data/Cleaned/ex2.Rdata")



# Sanderson function, for adjusted old-age dependency ratio
Sanderson(pop,ex,geo="PT",sex = "F",year=2014, cutoff = 15)

# box
years  <- 1990:2014
soa    <- rep(0,length(years))
sr     <- soa
for (i in 1:length(years)){
	soa[i] <- Sanderson(pop,              # Eurostat population object
			            ex,               # life expectancy object
						geo = "PT",       # selectors
						sex = "F", 
						year = years[i], 
						cutoff = 15)      # e(x) cutoff
				
	# classic old age support.
	sr[i]  <- OASR(pop, 
			       geo = "PT", 
				   sex = "F", 
				   year = years[i], 
				   lower = 20, 
				   upper = 65)
}

# standardize to 1990:

plot(years, 
     sr, 
	 type = 'l', 
	 ylim = c(0,.4), 
	 main = "Support ratios of PT", 
	 ylab = "support")
lines(years, soa,col = "blue")
legend("bottomright", lty=1,col=c("black","blue"),legend=c("OASR","Sanderson"),bty="n")


# what about other countries? The EU? how far back does Eurostat EU28 series go?

