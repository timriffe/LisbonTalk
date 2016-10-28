
# Author: tim
###############################################################################
setwd("/home/tim/workspace/LisbonTalk/")
library(data.table)

read <- function(X){
	local(get(load(X)))
}

# take care of EUROSTAT age categories

fixages <- function(X){
	X <- X[X$age != "TOTAL", ]
	if (any(X$age == "Y_OPEN")){
		
		X <- data.table(X)
		f <- function(.SD){
			agein         <- .SD$age
			
			openi         <- agein == "Y_OPEN"
			ageint        <- gsub("Y","",agein)
			ageint        <- suppressWarnings(as.integer(ageint))
			if(all(is.na(ageint))){
				return(NULL)
			}
			maxA                  <- max(ageint, na.rm = TRUE)
			ageint[openi] 		  <- maxA + 1
			agein[!is.na(ageint)] <- ageint[!is.na(ageint)]
			.SD$age               <- as.character(agein)
			.SD
		}
		X                         <- X[, f(.SD), by = c("geo", "time", "sex")]
		
	}
	
	X$age                     <- gsub("Y_GE", "", X$age)
	X$age[X$age == "Y_LT1"]   <- 0
	X$age                     <- gsub("Y","",X$age)
	if (any(X$age == "20-24")){
		X$age                 <- unlist(lapply(X$age, function(xx)
							strsplit(xx, split = "-")[[1]][1]
				))		
	}
	X
}

# redistribute UNK ages proportionally
distUNK <- function(X){
	UNKs <- X$age == "UNK"
    X <- X[(!UNKs) | (UNKs & X$values > 0), ]
	if (any(X$age == "UNK")){
		
		f <- function(.SD){
			
			UNKi <- .SD$age == "UNK"
			if (sum(UNKi) == 1){
				UNK <- .SD$values[UNKi]
				.SD <- .SD[!UNKi, ]
				vals <- .SD$values
				.SD$values <- vals + UNK * (vals / sum(vals,na.rm=TRUE))
			}
			.SD
		}
		
		if ("agedef" %in% colnames(X)){
			XL             <- split(X, list(X$geo,X$sex,X$time,X$agedef),drop=TRUE)
		} else {		
			XL             <- split(X, list(X$geo,X$sex,X$time),drop=TRUE)
		}
		XXL            <- lapply(XL,f)
		X              <- do.call(rbind, XXL)
		rownames(X) <- NULL
		
	}
	X
}

# call the latter two functions X <- emm
clean <- function(X){
	X <- fixages(X)
	X <- distUNK(X)
	X$age <- as.integer(X$age)
	as.data.frame(X)
}


# deciding to throw out fert in 5-year age groups
#0.02058
#(0.00169 + 0.00508 + 0.01118 + 0.03051 + 0.04955) / 5
#head(fert[fert$age == "Y20-24",])
#X <- fert[fert$geo == "AL" & fert$time == 2014,]
# Now we need to graduate fertility rates given in 5-year age groups...
# ahhhh. why? How about instead we just assume a step function

imm   <- read("Data/imm.Rdata")
emm   <- read("Data/emm.Rdata")
fert  <- read("Data/fert.Rdata")
pop   <- read("Data/pop.Rdata")
dRR   <- read("Data/dRR.Rdata")
dVV   <- read("Data/dVV.Rdata")
proj  <- read("Data/proj.Rdata")

# 1) do initial clean
proj  <- clean(proj)
dVV   <- clean(dVV)
dRR   <- clean(dRR)
pop   <- clean(pop)
imm   <- clean(imm)
emm   <- clean(emm)
fert  <- fert[!grepl("-",fert$age),]
fert  <- clean(fert)

ex    <- read("Data/ex.Rdata")
ex    <- clean(ex)

save(imm, file = "Data/Cleaned/imm2.Rdata")
save(emm, file = "Data/Cleaned/emm2.Rdata")
save(fert, file = "Data/Cleaned/fert2.Rdata")
save(pop, file = "Data/Cleaned/pop2.Rdata")
save(dRR, file = "Data/Cleaned/dRR2.Rdata")
save(dVV, file = "Data/Cleaned/dVV2.Rdata")
save(proj, file = "Data/Cleaned/proj2.Rdata")
save(ex, file = "Data/Cleaned/ex2.Rdata")


#e0    <- e0[e0$age == 0,]
#
#e0$values[e0$geo == "EU28" & e0$sex == "F" & e0$time == 2014]
#e0$values[e0$geo == "EU28" & e0$sex == "M" & e0$time == 2014]
#hist(e0$values[e0$time == 2014 & e0$sex == "F"])
#hist(e0$values[e0$time == 2014 & e0$sex == "M"])




AgesChar <- unique(c(
				unique(proj$age),
				unique(dVV$age),
				unique(dRR$age),
				unique(pop$age),
				unique(imm$age),
				unique(emm$age)#,
				#unique(fert$age)
		))

# ohhhhh- includes both five-year and single year!!!
head(fert[fert$geo == "AL",])
head(fert)
# 2) get deaths in triangles
# 3) create exposures using MPv5 formula
# 4) get mortality rates
# 5) emigration rates

# does emigration from Portugal add up to totals from Guy Abel?
# adjust so that it adds up? hmmm hard to say.

dim(pop)
head(pop)

unique(pop$geo)
EU <- pop[pop$geo == "EU28",]
head(EU)
