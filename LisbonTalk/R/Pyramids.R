# Author: tim
###############################################################################
setwd("/home/tim/workspace/LisbonTalk/")
read <- function(X){
	local(get(load(X)))
}
#library(HMDHFDplus)
#
#mlt 	<- readHMDweb("SWE", "mltper_1x1", username = us, password = pw)
#flt 	<- readHMDweb("SWE", "fltper_1x1", username = us, password = pw) 
#
#save(mlt, file = "Data/SWEmltper_1x1.Rdata")
#save(flt, file = "Data/SWEfltper_1x1.Rdata")

mlt 	<- read("Data/SWEmltper_1x1.Rdata")
flt 	<- read("Data/SWEmltper_1x1.Rdata")

yr1 	<- 1900
yr2 	<- 2014
rgrow   <- .01
rshrink <- -.01
SRB     <- 1.05


Lxm1 	<- mlt$Lx[mlt$Year == yr1] / 1e5 * (SRB / (1 + SRB))
Lxm2 	<- mlt$Lx[mlt$Year == yr2] / 1e5 * (SRB / (1 + SRB))

Lxf1 	<- flt$Lx[flt$Year == yr1] / 1e5 * (1 / (1 + SRB))
Lxf2 	<- flt$Lx[flt$Year == yr2] / 1e5 * (1 / (1 + SRB))

a <- 0:110 + .5

m1 <- Lxm1 * exp(-a * rgrow)
f1 <- Lxf1 * exp(-a * rgrow)

m2 <- Lxm2 * exp(-a * rshrink)
f2 <- Lxf2 * exp(-a * rshrink)

library(Pyramid)

pdf("Figures/growingshort.pdf")
par(mai=c(0,0,0,0), xpd=TRUE,xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(-1.3, 1.3), ylim = c(0, 110), axes = FALSE, xlab = "", ylab = "")
PyramidOutline(m1, f1, scale = 100, col = "#00000050")
dev.off()

pdf("Figures/shrinkinglong.pdf")
par(mai=c(0,0,0,0), xpd=TRUE,xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(-1.3, 1.3), ylim = c(0, 110), axes = FALSE, xlab = "", ylab = "")
PyramidOutline(m2, f2, scale = 100, col = "#00000050")
dev.off()

pdf("Figures/popcompare.pdf")
par(mai=c(0,0,0,0), xpd=TRUE,xaxs="i",yaxs="i")
plot(NULL, type = "n", xlim = c(-1.3, 1.3), ylim = c(0, 110), axes = FALSE, xlab = "", ylab = "")
PyramidOutline(m1, f1, scale = 100, col = "#00000050")
PyramidOutline(m2, f2, scale = 100, col = "#00000050")
dev.off()

# now years lived vs left
p1 <- m1 + f1
sum(a * p1)/sum(p1) # 29.5

p2 <- m2 + f2
sum(a * p2)/sum(p2) # 47.5

p1s     <- p1 / sum(p1)
p2s     <- p2 / sum(p2)

exm1 	<- mlt$ex[mlt$Year == yr1] 
exm2 	<- mlt$ex[mlt$Year == yr2] 

exf1 	<- flt$ex[flt$Year == yr1] 
exf2 	<- flt$ex[flt$Year == yr2] 

# years left avg:
sum(m1 * exm1 + f1 * exf1) / sum(m1+f1) # 37.7
sum(m2 * exm2 + f2 * exf2) / sum(m2+f2) # 36.3

