## Select the best-fitting simulation from burnin

library("EpiModelHIV")
library("EpiModelHPC")
library("tidyverse")

setwd("~/CAMP-HET-STI/scenarios/burn")

list.files("data", pattern = "min.rda$")
fn <- list.files("data",
                 full.names = TRUE, pattern = "min.rda$")

tdf <- data.frame(batch = NA, dist.1 = NA, dist.2 = NA, dist.3 = NA,
                  dist.4 = NA, dist.5 = NA, dist.6 = NA,
                  dist.7 = NA, dist.8 = NA, dist.9 = NA,
                  dist.10 = NA, dist.11 = NA, dist.12 = NA,
                  dist.13 = NA, dist.14 = NA, dist.c = NA, dist.ratio.CT = NA, dist.ratio.GC = NA)


tdf2 <- data.frame(batch = NA, dist2.1 = NA, dist2.2 = NA, dist2.3 = NA,
                  dist2.4 = NA, dist2.5 = NA, dist2.6 = NA,
                  dist2.7 = NA, dist2.8 = NA, dist2.9 = NA,
                  dist2.10 = NA, dist2.11 = NA, dist2.12 = NA,
                  dist2.13 = NA, dist2.14 = NA, dist2.c = NA, dist2.ratio.CT = NA, dist2.ratio.GC = NA)

tdf3 <- data.frame(batch = NA, val.1 = NA, val.2 = NA, val.3 = NA,
                   val.4 = NA, val.5 = NA, val.6 = NA,
                   val.7 = NA, val.8 = NA, val.9 = NA,
                   val.10 = NA, val.11 = NA, val.12 = NA,
                   val.13 = NA, val.14 = NA, val.15 = NA, val.16 = NA)


for (i in 1:length(fn)) {
  load(fn[i])

  mod1<-sim

  for (j in 1:mod1$control$nsims) {
    batch <- paste(paste(strsplit(fn[i], "[.]")[[1]][2:3], collapse = "."), j, sep = ".")
    df <- as.data.frame(x = mod1, sim = j)
    df <- select(df, c(diag100K.ct, diag100K.ct.BF, diag100K.ct.BM, diag100K.ct.HF, diag100K.ct.HM, diag100K.ct.WF, diag100K.ct.WM, diag100K.ct.M, diag100K.ct.F,
                       diag100K.gc, diag100K.gc.BF, diag100K.gc.BM, diag100K.gc.HF, diag100K.gc.HM, diag100K.gc.WF, diag100K.gc.WM, diag100K.gc.M, diag100K.gc.F) )

    #CHLAMYDIA Diagnosis
    diag100K.ct <- mean(df$diag100K.ct)
    diag100K.ct.ratio <- mean(df$diag100K.ct.M) / mean(df$diag100K.ct.F)

    #GC Diagnosis
    diag100K.gc <- mean(df$diag100K.gc)
    diag100K.gc.ratio <- mean(df$diag100K.gc.M) / mean(df$diag100K.gc.F)

    # Calculate mean stats for the 2 months
    means <- c(mean(tail(df$diag100K.ct,8)),mean(tail(df$diag100K.ct.BF,8)),mean(tail(df$diag100K.ct.BM,8)),mean(tail(df$diag100K.ct.HF,8)),
               mean(tail(df$diag100K.ct.HM,8)),mean(tail(df$diag100K.ct.WF,8)),mean(tail(df$diag100K.ct.WM,8)),mean(tail(df$diag100K.gc,8)),
               mean(tail(df$diag100K.gc.BF,8)),mean(tail(df$diag100K.gc.BM,8)),mean(tail(df$diag100K.gc.HF,8)),mean(tail(df$diag100K.gc.HM,8)),
               mean(tail(df$diag100K.gc.WF,8)),mean(tail(df$diag100K.gc.WM,8)),mean(tail(df$diag100K.ct.M,8)) / mean(tail(df$diag100K.ct.F,8)),
               mean(tail(df$diag100K.ct.M,8)) / mean(tail(df$diag100K.ct.F,8)))


    diag.targ <- c(453.4, 1542.4, 786, 555.7, 190.8, 254.7, 94.4, 106.7, 427.2, 447.4, 56.9, 62.3, 32.1, 28.5, 0.5084, 0.9741 )




    # get the mean distance from each target
    dist.1 <- sum(means[1] - diag.targ[1])
    dist.2 <- sum(means[2] - diag.targ[2])
    dist.3 <- sum(means[3] - diag.targ[3])
    dist.4 <- sum(means[4] - diag.targ[4])
    dist.5 <- sum(means[5] - diag.targ[5])
    dist.6 <- sum(means[6] - diag.targ[6])
    dist.7 <- sum(means[7] - diag.targ[7])
    dist.8 <- sum(means[8] - diag.targ[8])
    dist.9 <- sum(means[9] - diag.targ[9])
    dist.10 <- sum(means[10] - diag.targ[10])
    dist.11 <- sum(means[11] - diag.targ[11])
    dist.12 <- sum(means[12] - diag.targ[12])
    dist.13 <- sum(means[13] - diag.targ[13])
    dist.14 <- sum(means[14] - diag.targ[14])
    dist.c <- sum(dist.1 , dist.2, dist.3, dist.4, dist.5, dist.6, dist.7, dist.8, dist.9, dist.10,
                  dist.11, dist.12, dist.13, dist.14)
    dist.ratio.CT <- sum(means[15] - diag.targ[15])
    dist.ratio.GC <- sum(means[16] - diag.targ[16])

    out <- c(batch, dist.1, dist.2, dist.3, dist.4, dist.5, dist.6, dist.7,
             dist.8, dist.9, dist.10, dist.11, dist.12, dist.13, dist.14, dist.c, dist.ratio.CT, dist.ratio.GC)


    # Take the sum of the percent difference between target and simulated mean
    dist2.1 <- (min(means[1],diag.targ[1])/max(means[1],diag.targ[1]))
    dist2.2 <- (min(means[2],diag.targ[2])/max(means[2],diag.targ[2]))
    dist2.3 <- (min(means[3],diag.targ[3])/max(means[3],diag.targ[3]))
    dist2.4 <- (min(means[4],diag.targ[4])/max(means[4],diag.targ[4]))
    dist2.5 <- (min(means[5],diag.targ[5])/max(means[5],diag.targ[5]))
    dist2.6 <- (min(means[6],diag.targ[6])/max(means[6],diag.targ[6]))
    dist2.7 <- (min(means[7],diag.targ[7])/max(means[7],diag.targ[7]))
    dist2.8 <- (min(means[8],diag.targ[8])/max(means[8],diag.targ[8]))
    dist2.9 <- (min(means[9],diag.targ[9])/max(means[9],diag.targ[9]))
    dist2.10 <- (min(means[10],diag.targ[10])/max(means[10],diag.targ[10]))
    dist2.11 <- (min(means[11],diag.targ[11])/max(means[11],diag.targ[11]))
    dist2.12 <- (min(means[12],diag.targ[12])/max(means[12],diag.targ[12]))
    dist2.13 <- (min(means[13],diag.targ[13])/max(means[13],diag.targ[13]))
    dist2.14 <- (min(means[14],diag.targ[14])/max(means[14],diag.targ[14]))
    dist2.c <- sum(dist2.1, dist2.2, dist2.3, dist2.4, dist2.5, dist2.6, dist2.7, dist2.8, dist2.9, dist2.10,
                  dist2.11, dist2.12, dist2.13, dist2.14)

    dist2.ratio.CT <- (min(means[15],diag.targ[15])/max(means[15],diag.targ[15]))
    dist2.ratio.GC <- (min(means[16],diag.targ[16])/max(means[16],diag.targ[16]))

    out2 <- c(batch, dist2.1, dist2.2, dist2.3, dist2.4, dist2.5, dist2.6, dist2.7,
             dist2.8, dist2.9, dist2.10, dist2.11, dist2.12, dist2.13, dist2.14, dist2.c, dist2.ratio.CT, dist2.ratio.GC)


    # get the observed values
    val.1 <- sum(means[1])
    val.2 <- sum(means[2])
    val.3 <- sum(means[3])
    val.4 <- sum(means[4])
    val.5 <- sum(means[5])
    val.6 <- sum(means[6])
    val.7 <- sum(means[7])
    val.8 <- sum(means[8])
    val.9 <- sum(means[9])
    val.10 <- sum(means[10])
    val.11 <- sum(means[11])
    val.12 <- sum(means[12])
    val.13 <- sum(means[13])
    val.14 <- sum(means[14])
    val.15 <- sum(means[15])
    val.16 <- sum(means[16])

    out3 <- c(batch, val.1, val.2, val.3, val.4, val.5, val.6, val.7,
              val.8, val.9, val.10, val.11, val.12, val.13, val.14, val.15, val.16)


    tdf <- rbind(tdf, out)
    tdf2 <- rbind(tdf2, out2)
    tdf3 <- rbind(tdf3, out3)
  }


  cat("\nFile", fn[i], "complete ...")

  if (i == length(fn)) {
    tdf <- tdf[-1, ]
  save(tdf, file = "data/tdf.rda")
  }

  if (i == length(fn)) {
    tdf2 <- tdf2[-1, ]
    save(tdf2, file = "data/tdf2.rda")

  }


  if (i == length(fn)) {
    tdf3 <- tdf3[-1, ]
    save(tdf3, file = "data/tdf3.rda")

  }

}


names<-c("diag100K.ct", "diag100K.ct.BF", "diag100K.ct.BM", "diag100K.ct.HF",
         "diag100K.ct.HM", "diag100K.ct.WF", "diag100K.ct.WM", "diag100K.gc",
         "diag100K.gc.BF", "diag100K.gc.BM", "diag100K.gc.HF", "diag100K.gc.HM", "diag100K.gc.WF", "diag100K.gc.WM","diag100K.ct.ratio", "diag100K.gc.ratio")




boxplot(as.numeric(tdf3$val.1),xlab = names[1])
abline(h=diag.targ[1])

boxplot(as.numeric(tdf3$val.2),xlab = names[2]) #BF CT
abline(h=diag.targ[2])
boxplot(as.numeric(tdf3$val.3),xlab = names[3]) #BM CT
abline(h=diag.targ[3])
boxplot(as.numeric(tdf3$val.4),xlab = names[4]) #HF
abline(h=diag.targ[4])
boxplot(as.numeric(tdf3$val.5),xlab = names[5]) #HM
abline(h=diag.targ[5])
boxplot(as.numeric(tdf3$val.6),xlab = names[6]) #WF
abline(h=diag.targ[6])
boxplot(as.numeric(tdf3$val.7),xlab = names[7]) #WM
abline(h=diag.targ[7])

boxplot(as.numeric(tdf3$val.8),xlab = names[8])
abline(h=diag.targ[8])

boxplot(as.numeric(tdf3$val.9),xlab = names[9]) #BF GC
abline(h=diag.targ[9])
boxplot(as.numeric(tdf3$val.10),xlab = names[10]) #BM GC
abline(h=diag.targ[10])
boxplot(as.numeric(tdf3$val.11),xlab = names[11]) #HF
abline(h=diag.targ[11])
boxplot(as.numeric(tdf3$val.12),xlab = names[12]) #HM
abline(h=diag.targ[12])
boxplot(as.numeric(tdf3$val.13),xlab = names[13]) #WF
abline(h=diag.targ[13])
boxplot(as.numeric(tdf3$val.14),xlab = names[14]) #WM
abline(h=diag.targ[14])





boxplot(as.numeric(tdf$dist.1),xlab = names[1])
abline(h=0)
boxplot(as.numeric(tdf$dist.2),xlab = names[2]) #BF CT
abline(h=0)
boxplot(as.numeric(tdf$dist.3),xlab = names[3]) #BM CT
abline(h=0)
boxplot(as.numeric(tdf$dist.4),xlab = names[4]) #HF
abline(h=0)
boxplot(as.numeric(tdf$dist.5),xlab = names[5]) #HM
abline(h=0)
boxplot(as.numeric(tdf$dist.6),xlab = names[6]) #WF
abline(h=0)
boxplot(as.numeric(tdf$dist.7),xlab = names[7]) #WM
abline(h=0)
boxplot(as.numeric(tdf$dist.8),xlab = names[8])
abline(h=0)
boxplot(as.numeric(tdf$dist.9),xlab = names[9]) #BF GC
abline(h=0)
boxplot(as.numeric(tdf$dist.10),xlab = names[10]) #BM GC
abline(h=0)
boxplot(as.numeric(tdf$dist.11),xlab = names[11]) #HF
abline(h=0)
boxplot(as.numeric(tdf$dist.12),xlab = names[12]) #HM
abline(h=0)
boxplot(as.numeric(tdf$dist.13),xlab = names[13]) #WF
abline(h=0)
boxplot(as.numeric(tdf$dist.14),xlab = names[14]) #WM
abline(h=0)



boxplot(as.numeric(tdf3$val.15),xlab = names[15]) #WM
abline(h=diag.targ[15])
boxplot(as.numeric(tdf3$val.16),xlab = names[16]) #WM
abline(h=diag.targ[16])


##PLOT on ART by dem class
tiff(filename = "out/ABC fits distance overall.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,1))

tdf$dist.1<-as.numeric(tdf$dist.1)
boxplot(as.numeric(tdf$dist.1),
        ylab="Difference between simulated and observed chlamydia diagnosis",xlab="5000 simulations")
abline(h=0)

boxplot(as.numeric(tdf$dist.8),ylab = "Difference between simulated and observed gonorrhea diagnosis",
        xlab="5000 simulations")
abline(h=0)


dev.off()


tiff(filename = "out/ABC fits distance CT.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))


boxplot(as.numeric(tdf$dist.2),xlab = "Black F",
        ylab="Diff sim vs obs ct diag") #BF CT
abline(h=0)
boxplot(as.numeric(tdf$dist.3),xlab = "Black M",
        ylab="Diff sim vs obs ct diag") #BM CT
abline(h=0)
boxplot(as.numeric(tdf$dist.4),xlab = "Hispanic F",
        ylab="Diff sim vs obs ct diag") #HF
abline(h=0)
boxplot(as.numeric(tdf$dist.5),xlab = "Hispanic M",
        ylab="Diff sim vs obs ct diag") #HM
abline(h=0)
boxplot(as.numeric(tdf$dist.6),xlab = "White/Other F",
        ylab="Diff sim vs obs ct diag") #WF
abline(h=0)
boxplot(as.numeric(tdf$dist.7),xlab = "White/Other M",
        ylab="Diff sim vs obs ct diag") #WM
abline(h=0)

dev.off()


tiff(filename = "out/ABC fits distance GC.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))


boxplot(as.numeric(tdf$dist.9),xlab = "Black F",
        ylab="Diff sim vs obs gc diag") #BF GC
abline(h=0)
boxplot(as.numeric(tdf$dist.10),xlab = "Black M",
        ylab="Diff sim vs obs gc diag") #BM GC
abline(h=0)
boxplot(as.numeric(tdf$dist.11),xlab = "Hispanic F",
        ylab="Diff sim vs obs gc diag") #HF
abline(h=0)
boxplot(as.numeric(tdf$dist.12),xlab = "Hispnic M",
        ylab="Diff sim vs obs gc diag") #HM
abline(h=0)
boxplot(as.numeric(tdf$dist.13),xlab = "White/Other F",
        ylab="Diff sim vs obs gc diag") #WF
abline(h=0)
boxplot(as.numeric(tdf$dist.14),xlab = "White/Other M",
        ylab="Diff sim vs obs gc diag") #WM
abline(h=0)

dev.off()

####################  RAW VALUES  ###########################



##PLOT on ART by dem class
tiff(filename = "out/ABC fits diagnosis overall.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,1))

tdf3$val.1<-as.numeric(tdf3$val.1)
boxplot(as.numeric(tdf3$val.1),
        ylab="Simulated and observed chlamydia diagnosis",xlab="5000 simulations")
abline(h=diag.targ[1])

boxplot(as.numeric(tdf3$val.8),ylab = "Simulated and observed gonorrhea diagnosis",
        xlab="5000 simulations")
abline(h=diag.targ[8])


dev.off()


tiff(filename = "out/ABC fits diagnosis CT.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))


boxplot(as.numeric(tdf3$val.2),xlab = "Black F",
        ylab="Diff sim vs obs ct diag") #BF CT
abline(h=diag.targ[2])
boxplot(as.numeric(tdf3$val.3),xlab = "Black M",
        ylab="Diff sim vs obs ct diag") #BM CT
abline(h=diag.targ[3])
boxplot(as.numeric(tdf3$val.4),xlab = "Hispanic F",
        ylab="Diff sim vs obs ct diag") #HF
abline(h=diag.targ[4])
boxplot(as.numeric(tdf3$val.5),xlab = "Hispanic M",
        ylab="Diff sim vs obs ct diag") #HM
abline(h=diag.targ[5])
boxplot(as.numeric(tdf3$val.6),xlab = "White/Other F",
        ylab="Diff sim vs obs ct diag") #WF
abline(h=diag.targ[6])
boxplot(as.numeric(tdf3$val.7),xlab = "White/Other M",
        ylab="Diff sim vs obs ct diag") #WM
abline(h=diag.targ[7])

dev.off()


tiff(filename = "out/ABC fits diagnosis GC.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))


boxplot(as.numeric(tdf3$val.9),xlab = "Black F",
        ylab="Diff sim vs obs gc diag") #BF GC
abline(h=diag.targ[9])
boxplot(as.numeric(tdf3$val.10),xlab = "Black M",
        ylab="Diff sim vs obs gc diag") #BM GC
abline(h=diag.targ[10])
boxplot(as.numeric(tdf3$val.11),xlab = "Hispanic F",
        ylab="Diff sim vs obs gc diag") #HF
abline(h=diag.targ[11])
boxplot(as.numeric(tdf3$val.12),xlab = "Hispnic M",
        ylab="Diff sim vs obs gc diag") #HM
abline(h=diag.targ[12])
boxplot(as.numeric(tdf3$val.13),xlab = "White/Other F",
        ylab="Diff sim vs obs gc diag") #WF
abline(h=diag.targ[13])
boxplot(as.numeric(tdf3$val.14),xlab = "White/Other M",
        ylab="Diff sim vs obs gc diag") #WM
abline(h=diag.targ[14])

dev.off()



#match overall
c<-.99
tdf2_OVERALL <- tdf2[which(tdf2$dist2.1 > c & tdf2$dist2.8 > c), ]

tdf2_OVERALL

c<-.5
tdf2_sel <- tdf2_OVERALL[which(tdf2_OVERALL$dist2.1 > c & tdf2_OVERALL$dist2.2 > c & tdf2_OVERALL$dist2.3 > (c/1.5) &
                                 tdf2_OVERALL$dist2.4 > c & tdf2_OVERALL$dist2.5 > c & tdf2_OVERALL$dist2.6 > c
                       & tdf2_OVERALL$dist2.7 > c & tdf2_OVERALL$dist2.8 > c & tdf2_OVERALL$dist2.9 > c & tdf2_OVERALL$dist2.10 > (c/1.5) & tdf2_OVERALL$dist2.11 > c
                       & tdf2_OVERALL$dist2.12 > c & tdf2_OVERALL$dist2.13 > c & tdf2_OVERALL$dist2.14 > c), ]

tdf2_sel



#n <- tdf2_sel[which(tdf2_sel$batch == "n2012.22.9"), ]
#n
#n2013.22.6 load("data/sim.n2013.22.20221104.1322.rda") maybe
#n2013.38.6 - no
#n2013.42.6 load("data/sim.n2013.42.20221104.1336.rda") -lock on GC not bad CT

#n2000.4.18 Current yes
#n2000.9.19 - testing
#n2013.38.6


#####################################################

# Save as best-fitting ----------------------------------------------------
load("data/sim.n1001.104.20230212.1504.rda")
sim <- get_sims(sim, sims = 8)
save(sim, file = "est/sim.burnin.1.rda")
save(sim, file = "data/sim.burnin.1.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.1.rda")

load("data/sim.n1001.110.20230212.1458.rda")
sim <- get_sims(sim, sims = 2)
save(sim, file = "est/sim.burnin.2.rda")
save(sim, file = "data/sim.burnin.2.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.2.rda")

load("data/sim.n1001.191.20230212.1506.rda")
sim <- get_sims(sim, sims = 13)
save(sim, file = "est/sim.burnin.3.rda")
save(sim, file = "data/sim.burnin.3.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.3.rda")

load("data/sim.n1001.51.20230212.1500.rda")
sim <- get_sims(sim, sims = 6)
save(sim, file = "est/sim.burnin.4.rda")
save(sim, file = "data/sim.burnin.4.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.4.rda")

load("data/sim.n1001.194.20230215.1258.rda")
sim <- get_sims(sim, sims = 18)
save(sim, file = "est/sim.burnin.5.rda")
save(sim, file = "data/sim.burnin.5.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.5.rda")

load("data/sim.n1001.214.20230215.1255.rda")
sim <- get_sims(sim, sims = 10)
save(sim, file = "est/sim.burnin.6.rda")
save(sim, file = "data/sim.burnin.6.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.6.rda")

load("data/sim.n1001.215.20230215.1317.rda")
sim <- get_sims(sim, sims = 4)
save(sim, file = "est/sim.burnin.7.rda")
save(sim, file = "data/sim.burnin.7.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.7.rda")

load("data/sim.n1001.39.20230215.1253.rda")
sim <- get_sims(sim, sims = 5)
save(sim, file = "est/sim.burnin.8.rda")
save(sim, file = "data/sim.burnin.8.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.8.rda")

load("data/sim.n1001.96.20230215.1258.rda")
sim <- get_sims(sim, sims = 12)
save(sim, file = "est/sim.burnin.9.rda")
save(sim, file = "data/sim.burnin.9.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.9.rda")

load("data/sim.n1000.104.20230212.1504.rda")
sim <- get_sims(sim, sims = 16)
save(sim, file = "est/sim.burnin.10.rda")
save(sim, file = "data/sim.burnin.10.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.10.rda")

load("data/sim.n1000.14.20230212.1505.rda")
sim <- get_sims(sim, sims = 16)
save(sim, file = "est/sim.burnin.11.rda")
save(sim, file = "data/sim.burnin.11.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.11.rda")

load("data/sim.n1000.144.20230212.1513.rda")
sim <- get_sims(sim, sims = 13)
save(sim, file = "est/sim.burnin.12.rda")
save(sim, file = "data/sim.burnin.12.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.12.rda")

load("data/sim.n1000.173.20230212.1838.rda")
sim <- get_sims(sim, sims = 16)
save(sim, file = "est/sim.burnin.13.rda")
save(sim, file = "data/sim.burnin.13.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.13.rda")

load("data/sim.n1000.57.20230212.1832.rda")
sim <- get_sims(sim, sims = 15)
save(sim, file = "est/sim.burnin.14.rda")
save(sim, file = "data/sim.burnin.14.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.14.rda")

load("data/sim.n1000.125.20230211.1833.rda")
sim <- get_sims(sim, sims = 10)
save(sim, file = "est/sim.burnin.15.rda")
save(sim, file = "data/sim.burnin.15.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.15.rda")


##########################################################################################

load("data/sim.n1000.103.20230211.1832.rda")
sim.sel <- get_sims(sim, sims = 8)

diag.targ <- c(453.4, 1542.4, 786, 555.7, 190.8, 254.7, 94.4, 106.7, 427.2, 447.4, 56.9, 62.3, 32.1, 28.5)


df <- as.data.frame(sim.sel)
df <- tail(df, 104)



#PLOT CT diagnosis
plot(1:104 , df$diag100K.ct, ylim=c(200,600))
lines(1:104,rep(diag.targ[1],104))

plot(1:104 , df$diag100K.ct.BF, ylim=c(800,1800))
lines(1:104,rep(diag.targ[2],104))

plot(1:104 , df$diag100K.ct.BM, ylim=c(400,900))
lines(1:104,rep(diag.targ[3],104))

plot(1:104 , df$diag100K.ct.HF, ylim=c(400,700))
lines(1:104,rep(diag.targ[4],104))

plot(1:104 , df$diag100K.ct.HM, ylim=c(50,300))
lines(1:104,rep(diag.targ[5],104))

plot(1:104 , df$diag100K.ct.WF, ylim=c(0,500))
lines(1:104,rep(diag.targ[6],104))

plot(1:104 , df$diag100K.ct.WM, ylim=c(0,250))
lines(1:104,rep(diag.targ[7],104))


#PLOT GC diagnosis
plot(1:104 , df$diag100K.gc, ylim=c(0,200))
lines(1:104,rep(diag.targ[8],104))

plot(1:104 , df$diag100K.gc.BF, ylim=c(200,700))
lines(1:104,rep(diag.targ[9],104))

plot(1:104 , df$diag100K.gc.BM, ylim=c(50,600))
lines(1:104,rep(diag.targ[10],104))

plot(1:104 , df$diag100K.gc.HF, ylim=c(0,200))
lines(1:104,rep(diag.targ[11],104))

plot(1:104 , df$diag100K.gc.HM, ylim=c(0,200))
lines(1:104,rep(diag.targ[12],104))

plot(1:104 , df$diag100K.gc.WF, ylim=c(0,250))
lines(1:104,rep(diag.targ[13],104))

plot(1:104 , df$diag100K.gc.WM, ylim=c(0,150))
lines(1:104,rep(diag.targ[14],104))







tiff(filename = "out/burn diag overall.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,1))

#PLOT CT diagnosis
plot(1:104 , df$diag100K.ct, ylim=c(200,600), xlab="Final two years of seed simulation",
     ylab = "Chlamydia diagnosis per 100,000")
lines(1:104,rep(diag.targ[1],104))


#PLOT GC diagnosis
plot(1:104 , df$diag100K.gc, ylim=c(0,200), xlab="Final two years of seed simulation",
     ylab = "gonorrhea diagnosis per 100,000")
lines(1:104,rep(diag.targ[8],104))


dev.off()


tiff(filename = "out/burn diag CT.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))

#PLOT CT diagnosis

plot(1:104 , df$diag100K.ct.BF, ylim=c(800,1800))
lines(1:104,rep(diag.targ[2],104))

plot(1:104 , df$diag100K.ct.BM, ylim=c(400,900))
lines(1:104,rep(diag.targ[3],104))

plot(1:104 , df$diag100K.ct.HF, ylim=c(400,700))
lines(1:104,rep(diag.targ[4],104))

plot(1:104 , df$diag100K.ct.HM, ylim=c(50,300))
lines(1:104,rep(diag.targ[5],104))

plot(1:104 , df$diag100K.ct.WF, ylim=c(0,500))
lines(1:104,rep(diag.targ[6],104))

plot(1:104 , df$diag100K.ct.WM, ylim=c(0,250))
lines(1:104,rep(diag.targ[7],104))



dev.off()



tiff(filename = "out/burn diag GC.tiff", height = 10, width = 8, units = "in", res = 250)


par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(2,3))

#PLOT GC diagnosis

plot(1:104 , df$diag100K.gc.BF, ylim=c(200,700))
lines(1:104,rep(diag.targ[9],104))

plot(1:104 , df$diag100K.gc.BM, ylim=c(50,600))
lines(1:104,rep(diag.targ[10],104))

plot(1:104 , df$diag100K.gc.HF, ylim=c(0,200))
lines(1:104,rep(diag.targ[11],104))

plot(1:104 , df$diag100K.gc.HM, ylim=c(0,200))
lines(1:104,rep(diag.targ[12],104))

plot(1:104 , df$diag100K.gc.WF, ylim=c(0,250))
lines(1:104,rep(diag.targ[13],104))

plot(1:104 , df$diag100K.gc.WM, ylim=c(0,150))
lines(1:104,rep(diag.targ[14],104))

dev.off()

# Save as best-fitting ----------------------------------------------------
load("data/sim.n2000.14.20230117.1831.rda")
sim <- get_sims(sim, sims = 13)
save(sim, file = "est/sim.burnin.1.rda")
save(sim, file = "data/sim.burnin.1.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.1.rda")

load("data/sim.n2000.17.20230117.1339.rda")
sim <- get_sims(sim, sims = 1)
save(sim, file = "est/sim.burnin.2.rda")
save(sim, file = "data/sim.burnin.2.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.2.rda")

load("data/sim.n3000.112.20230117.2349.rda")
sim <- get_sims(sim, sims = 3)
save(sim, file = "est/sim.burnin.3.rda")
save(sim, file = "data/sim.burnin.3.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.3.rda")

load("data/sim.n3000.148.20230118.1149.rda")
sim <- get_sims(sim, sims = 4)
save(sim, file = "est/sim.burnin.4.rda")
save(sim, file = "data/sim.burnin.4.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.4.rda")

load("data/sim.n3000.188.20230118.1152.rda")
sim <- get_sims(sim, sims = 12)
save(sim, file = "est/sim.burnin.5.rda")
save(sim, file = "data/sim.burnin.5.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.5.rda")

load("data/sim.n3000.200.20230118.1148.rda")
sim <- get_sims(sim, sims = 12)
save(sim, file = "est/sim.burnin.6.rda")
save(sim, file = "data/sim.burnin.6.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.6.rda")

load("data/sim.n3000.217.20230118.1149.rda")
sim <- get_sims(sim, sims = 7)
save(sim, file = "est/sim.burnin.7.rda")
save(sim, file = "data/sim.burnin.7.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.7.rda")

load("data/sim.n3000.224.20230117.1410.rda")
sim <- get_sims(sim, sims = 11)
save(sim, file = "est/sim.burnin.8.rda")
save(sim, file = "data/sim.burnin.8.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.8.rda")

load("data/sim.n3000.238.20230117.1408.rda")
sim <- get_sims(sim, sims = 20)
save(sim, file = "est/sim.burnin.9.rda")
save(sim, file = "data/sim.burnin.9.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.9.rda")

load("data/sim.n3000.93.20230118.0010.rda")
sim <- get_sims(sim, sims = 5)
save(sim, file = "est/sim.burnin.10.rda")
save(sim, file = "data/sim.burnin.10.rda")
save(sim, file = "~/CAMP-HET-STI/scenarios/model/est/sim.burnin.10.rda")

