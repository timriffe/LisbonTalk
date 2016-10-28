setwd("/home/tim/workspace/LisbonTalk/")

#install.packages("eurostat")
#install.packages("rvest")
#library(eurostat)
#library(rvest)
#
#
#toc <- get_eurostat_toc()
#dim(toc)
#id   <- search_eurostat("immigration")$code
#
## immigration by sex and age
#imm  <- get_eurostat("migr_imm8", time_format = "num")
## emigration by sex and age (usually underestimate)
#emm  <- get_eurostat("migr_emi2", time_format = "num")
## fertility rates by sex and age
#fert <- get_eurostat("demo_frate", time_format = "num")
## pop Jan 1
#pop  <- get_eurostat("demo_pjan", time_format = "num")
## just take exposures as avg btwn years.
#dRR  <- get_eurostat("demo_magec", time_format = "num") # AP
#dVV  <- get_eurostat("demo_mager", time_format = "num") # PC
## Eurostat projection, main
#proj  <- get_eurostat("proj_13npms", time_format = "num")
#
#save(imm, file = "Data/imm.Rdata")
#save(emm, file = "Data/emm.Rdata")
#save(fert, file = "Data/fert.Rdata")
#save(pop, file = "Data/pop.Rdata")
#save(dRR, file = "Data/dRR.Rdata")
#save(dVV, file = "Data/dVV.Rdata")
#save(proj, file = "Data/proj.Rdata")
#

unique(proj$age)
unique(dVV$age)
unique(dRR$age)
unique(pop$age)
unique(imm$age)
unique(emm$age)

# write age converter function, seems to be standard

#dim(emm[emm$geo == "PT",])
#unique(emm$time[emm$geo == "PT"])
#

#install.packages("migest")
#install.packages("circlize")
#library(migest)
#library(circlize)
#demo(cfplot_reg2, package = "migest", ask = FALSE)
#
#migest
#install.packages("googlesheets")
#
#

setwd("/home/tim/workspace/LisbonTalk")
# --------------
# deprecated working with zip files, needed more than 8GB RAM...
# --------------
# modified from:
# http://stackoverflow.com/questions/18186357/importing-csv-file-with-multiple-character-separator-to-r
#read <- function(fileName, separators) {
#	data                <- readLines(con <- file(fileName))
#	close(con)
#	records             <- sapply(data, strsplit, split=separators)
#	header              <- records[1]
#	header              <- gsub(pattern=" ",replacement= "", sapply(header,c))
#	header[header == "geo\\time"] <- "geo"
#	dataFrame           <- data.frame(t(sapply(records[-1],c)))
#	rownames(dataFrame) <- 1: nrow(dataFrame)
#	colnames(dataFrame) <- header
#	return(as.data.frame(dataFrame, stringsAsFactors = FALSE))
#}
## -------------------------------------------
#
#hm                <- read("/home/tim/Data/Eurostat/emigration.tsv", "\t|,")
#hm                <- apply(hm,2,function(x){
#			            gsub(" ", "", x)
#		              })
#hm                <- apply(hm,2,function(x){
#			            x[x == ":"] <- NA
#			            x
#		              })
#integercols       <- !is.na(as.integer(colnames(hm)))
#hm[, integercols] <- apply(hm[, integercols], 2, function(x){
#			            gsub("[^0-9]", "", x)
#		              })
#hm                <- as.data.frame(hm)
#hm[integercols]   <- lapply(hm[integercols], as.integer)
#
#save(hm, file = "Data/emig.Rdata")
#rm(hm);gc()
## -------------------------------------------
#hm                <- read("/home/tim/Data/Eurostat/immigration.tsv", "\t|,")
#hm                <- apply(hm,2,function(x){
#			           gsub(" ", "", x)
#		             })
#hm                <- apply(hm,2,function(x){
#			           x[x == ":"] <- NA
#			x
#		             })
#integercols       <- !is.na(as.integer(colnames(hm)))
#hm[, integercols] <- apply(hm[, integercols], 2, function(x){
#			           gsub("[^0-9]", "", x)
#		             })
#hm                <- as.data.frame(hm)
#hm[integercols]   <- lapply(hm[integercols], as.integer)
#head(hm)
#save(hm, file = "Data/inmig.Rdata")
#
