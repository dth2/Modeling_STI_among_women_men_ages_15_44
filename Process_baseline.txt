# Process condom intervention output.

library(EpiModelHPC)
setwd("/homes/dth2/CAMP-HET-STI/scenarios/model")


library(EpiModel)
library(EpiModelHPC)
library(ggplot2)
library(openxlsx)



# Set-up data
## Load simulation results

sim_base <- merge_simfiles(1000, indir = "data/", truncate.at = 6241,ftype="max")
sim_base$attr <- sim_base$el <- sim_base$temp <-NULL
#save(sim_base, file = "~/CAMP-HET-STI/scenarios/model/data/sim_base.rda")
scenarios <- c(
  "sim_base")


level <- c("","_hi","_low")
nsims<-500
steps<-520
hi.cut <- round(.95*nsims)
low.cut <- round(.05*nsims)



for(i in 1:length(scenarios)){

  x<-get(scenarios[i])


  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".diag100K.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc", level[j])
    assign(fn,rep(NA,steps))


  }}

for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$diag100K.ct[k,1:nsims]))
  sim_base.diag100K.ct[k]<-mean(x)
  sim_base.diag100K.ct_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct_low[k]<-x[low.cut]


}


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$diag100K.gc[k,1:nsims]))
  sim_base.diag100K.gc[k]<-mean(x)
  sim_base.diag100K.gc_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc_low[k]<-x[low.cut]


}



##Plots
observed.2011.ct <- rep(453.4,steps)
observed.2019.ct <- rep(551,steps)
observed.2011.gc <- rep(106.7,steps)
observed.2019.gc <- rep(187.8,steps)

#Diagnosis - Chlamydia baseline
#tiff(filename = "out/Baseline Chlamydia Diagnosis.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,5,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE,
     xlab = "Years", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=.8)
axis(1, at = xticks, labels = c("0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.8)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.8)

xx <- c(1:(length(sim_base.diag100K.ct)), (length(sim_base.diag100K.ct)):1)
yy <- c(sim_base.diag100K.ct_low, rev(sim_base.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("black", alpha = 0.2), border = NA)
lines(sim_base.diag100K.ct, lwd = 1, col = "black")

#############################################################
# ADD LINES for 2011 observed and 2019 observed #############
lines(observed.2011.ct, lwd = 1, col = "black", lty=4)
lines(observed.2019.ct, lwd = 1, lty=2, col = "grey")

legend("topleft", c("Baseline"),
       col = c("black"),lty=1, cex = .8)

#dev.off()
#Diagnosis - Gonorrhea baseline
#tiff(filename = "out/Baseline Gonorrhea Diagnosis.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,5,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,300,25)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 300), lwd = 3, col="black", axes=FALSE,
     xlab = "Years", ylab = "Annual gonorrhea diagnosis per 100,000",  cex.main=.8)
axis(1, at = xticks, labels = c("0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.8)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.8)

xx <- c(1:(length(sim_base.diag100K.gc)), (length(sim_base.diag100K.gc)):1)
yy <- c(sim_base.diag100K.gc_low, rev(sim_base.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("grey", alpha = 0.2), border = NA)
lines(sim_base.diag100K.gc, lwd = 1, col = "black")

#############################################################
# ADD LINES for 2011 observed and 2019 observed #############
lines(observed.2011.gc, lwd = 1, col = "black", lty=4)
lines(observed.2019.gc, lwd = 1, col = "gray", lty=2)


legend("topleft", c("Baseline 2011"),
       col = c("red"),lty=1, cex = .8)

#dev.off()
