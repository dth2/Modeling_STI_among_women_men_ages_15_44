# Process condom intervention output.

library(EpiModelHPC)
setwd("/homes/dth2/CAMP-HET-STI/scenarios/model")


library(EpiModel)
library(EpiModelHPC)
library(ggplot2)
#library(openxlsx)



# Set-up data
## Load simulation results

sim_base <- merge_simfiles(1000, indir = "data/", truncate.at = 6241,ftype="max")
sim_base$attr <- sim_base$el <- sim_base$temp <-NULL
save(sim_base, file = "~/CAMP-HET-STI/scenarios/model/data/sim_base.rda")

sim_acts <- merge_simfiles(1001, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_acts$attr <- sim_acts$el <- sim_acts$temp <-NULL
save(sim_acts, file = "~/CAMP-HET-STI/scenarios/model/data/sim_acts.rda")

sim_cond <- merge_simfiles(1002, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_cond$attr <- sim_cond$el <- sim_cond$temp <-NULL
save(sim_cond, file = "~/CAMP-HET-STI/scenarios/model/data/sim_cond.rda")

sim_tst <- merge_simfiles(1003, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_tst$attr <- sim_tst$el <- sim_tst$temp <-NULL
save(sim_tst, file = "~/CAMP-HET-STI/scenarios/model/data/sim_tst.rda")

sim_acts_cond <- merge_simfiles(1004, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_acts_cond$attr <- sim_acts_cond$el <- sim_acts_cond$temp <-NULL
save(sim_acts_cond, file = "~/CAMP-HET-STI/scenarios/model/data/sim_acts_cond.rda")

sim_acts_tst <- merge_simfiles(1005, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_acts_tst$attr <- sim_acts_tst$el <- sim_acts_tst$temp <-NULL
save(sim_acts_tst, file = "~/CAMP-HET-STI/scenarios/model/data/sim_acts_tst.rda")

sim_cond_tst <- merge_simfiles(1006, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_cond_tst$attr <- sim_cond_tst$el <- sim_cond_tst$temp <-NULL
save(sim_cond_tst, file = "~/CAMP-HET-STI/scenarios/model/data/sim_cond_tst.rda")

sim_acts_cond_tst <- merge_simfiles(1007, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_acts_cond_tst$attr <- sim_acts_cond_tst$el <- sim_acts_cond_tst$temp <-NULL
save(sim_acts_cond_tst, file = "~/CAMP-HET-STI/scenarios/model/data/sim_acts_cond_tst.rda")

sim_nets <- merge_simfiles(1008, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_nets$attr <- sim_nets$el <- sim_nets$temp <-NULL
save(sim_nets, file = "~/CAMP-HET-STI/scenarios/model/data/sim_nets.rda")

sim_acts_cond_tst_nets <- merge_simfiles(1009, indir = "data/" ,truncate.at = 6241, ftype="max")
sim_acts_cond_tst_nets$attr <- sim_acts_cond_tst_nets$el <- sim_acts_cond_tst_nets$temp <-NULL
save(sim_acts_cond_tst_nets, file = "~/CAMP-HET-STI/scenarios/model/data/sim_acts_cond_tst_nets.rda")

scenarios <- c(
  "sim_base",
  "sim_acts",
  "sim_cond",
  "sim_tst",
  "sim_acts_cond",
  "sim_acts_tst",
  "sim_cond_tst",
  "sim_acts_cond_tst",
  "sim_nets",
  "sim_acts_cond_tst_nets")


level <- c("","_hi","_low")
nsims<-600
steps<-520
hi.cut <- round(.95*nsims)
low.cut <- round(.05*nsims)


##Prevalence
for(i in 1:length(scenarios)){

  x<-get(scenarios[i])


  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".prev.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".prev.gc", level[j])
    assign(fn,rep(NA,steps))


  }}



##prev.gc
##prev.ct
for (k in seq_along(1:steps)) {


 #no int
  x<-sort(as.numeric(sim_base$epi$prev.ct[k,1:nsims]))
  sim_base.prev.ct[k]<-mean(x) * 100
  sim_base.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_base.prev.ct_low[k]<-x[low.cut] * 100


  #sim_acts
  x<-sort(as.numeric(sim_acts$epi$prev.ct[k,1:nsims]))
  sim_acts.prev.ct[k]<-mean(x) * 100
  sim_acts.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_acts.prev.ct_low[k]<-x[low.cut] * 100

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$prev.ct[k,1:nsims]))
  sim_cond.prev.ct[k]<-mean(x) * 100
  sim_cond.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_cond.prev.ct_low[k]<-x[low.cut] * 100


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$prev.ct[k,1:nsims]))
  sim_tst.prev.ct[k]<-mean(x) * 100
  sim_tst.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_tst.prev.ct_low[k]<-x[low.cut] * 100

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$prev.ct[k,1:nsims]))
  sim_acts_cond.prev.ct[k]<-mean(x) * 100
  sim_acts_cond.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_acts_cond.prev.ct_low[k]<-x[low.cut] * 100

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$prev.ct[k,1:nsims]))
  sim_acts_tst.prev.ct[k]<-mean(x) * 100
  sim_acts_tst.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_acts_tst.prev.ct_low[k]<-x[low.cut] * 100

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$prev.ct[k,1:nsims]))
  sim_cond_tst.prev.ct[k]<-mean(x) * 100
  sim_cond_tst.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_cond_tst.prev.ct_low[k]<-x[low.cut] * 100

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$prev.ct[k,1:nsims]))
  sim_acts_cond_tst.prev.ct[k]<-mean(x) * 100
  sim_acts_cond_tst.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_acts_cond_tst.prev.ct_low[k]<-x[low.cut] * 100

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$prev.ct[k,1:nsims]))
  sim_nets.prev.ct[k]<-mean(x) * 100
  sim_nets.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_nets.prev.ct_low[k]<-x[low.cut] * 100

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$prev.ct[k,1:nsims]))
  sim_acts_cond_tst_nets.prev.ct[k]<-mean(x) * 100
  sim_acts_cond_tst_nets.prev.ct_hi[k]<-x[hi.cut] * 100
  sim_acts_cond_tst_nets.prev.ct_low[k]<-x[low.cut] * 100


  }


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$prev.gc[k,1:nsims]))
  sim_base.prev.gc[k]<-mean(x) * 100
  sim_base.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_base.prev.gc_low[k]<-x[low.cut] * 100


  #sim_acts
  x<-sort(as.numeric(sim_acts$epi$prev.gc[k,1:nsims]))
  sim_acts.prev.gc[k]<-mean(x) * 100
  sim_acts.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_acts.prev.gc_low[k]<-x[low.cut] * 100

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$prev.gc[k,1:nsims]))
  sim_cond.prev.gc[k]<-mean(x) * 100
  sim_cond.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_cond.prev.gc_low[k]<-x[low.cut] * 100


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$prev.gc[k,1:nsims]))
  sim_tst.prev.gc[k]<-mean(x) * 100
  sim_tst.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_tst.prev.gc_low[k]<-x[low.cut] * 100

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$prev.gc[k,1:nsims]))
  sim_acts_cond.prev.gc[k]<-mean(x) * 100
  sim_acts_cond.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_acts_cond.prev.gc_low[k]<-x[low.cut] * 100

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$prev.gc[k,1:nsims]))
  sim_acts_tst.prev.gc[k]<-mean(x) * 100
  sim_acts_tst.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_acts_tst.prev.gc_low[k]<-x[low.cut] * 100

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$prev.gc[k,1:nsims]))
  sim_cond_tst.prev.gc[k]<-mean(x) * 100
  sim_cond_tst.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_cond_tst.prev.gc_low[k]<-x[low.cut] * 100

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$prev.gc[k,1:nsims]))
  sim_acts_cond_tst.prev.gc[k]<-mean(x) * 100
  sim_acts_cond_tst.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_acts_cond_tst.prev.gc_low[k]<-x[low.cut] * 100

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$prev.gc[k,1:nsims]))
  sim_nets.prev.gc[k]<-mean(x) * 100
  sim_nets.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_nets.prev.gc_low[k]<-x[low.cut] * 100

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$prev.gc[k,1:nsims]))
  sim_acts_cond_tst_nets.prev.gc[k]<-mean(x) * 100
  sim_acts_cond_tst_nets.prev.gc_hi[k]<-x[hi.cut] * 100
  sim_acts_cond_tst_nets.prev.gc_low[k]<-x[low.cut] * 100


}


##Network stats

for(i in 1:length(scenarios)){

  x<-get(scenarios[i])


  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".edges.main", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".NM.B.main", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".edges.casl", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".NM.B.casl", level[j])
    assign(fn,rep(NA,steps))


  }}



for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$edges.main[k,1:nsims]))
  sim_base.edges.main[k]<-mean(x)
  sim_base.edges.main_hi[k]<-x[hi.cut]
  sim_base.edges.main_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$NM.B.main[k,1:nsims]))
  sim_base.NM.B.main[k]<-mean(x)
  sim_base.NM.B.main_hi[k]<-x[hi.cut]
  sim_base.NM.B.main_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$edges.casl[k,1:nsims]))
  sim_base.edges.casl[k]<-mean(x)
  sim_base.edges.casl_hi[k]<-x[hi.cut]
  sim_base.edges.casl_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$NM.B.casl[k,1:nsims]))
  sim_base.NM.B.casl[k]<-mean(x)
  sim_base.NM.B.casl_hi[k]<-x[hi.cut]
  sim_base.NM.B.casl_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$edges.main[k,1:nsims]))
  sim_nets.edges.main[k]<-mean(x)
  sim_nets.edges.main_hi[k]<-x[hi.cut]
  sim_nets.edges.main_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$NM.B.main[k,1:nsims]))
  sim_nets.NM.B.main[k]<-mean(x)
  sim_nets.NM.B.main_hi[k]<-x[hi.cut]
  sim_nets.NM.B.main_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$edges.casl[k,1:nsims]))
  sim_nets.edges.casl[k]<-mean(x)
  sim_nets.edges.casl_hi[k]<-x[hi.cut]
  sim_nets.edges.casl_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$NM.B.casl[k,1:nsims]))
  sim_nets.NM.B.casl[k]<-mean(x)
  sim_nets.NM.B.casl_hi[k]<-x[hi.cut]
  sim_nets.NM.B.casl_low[k]<-x[low.cut]


}




for(i in 1:length(scenarios)){

  x<-get(scenarios[i])

  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".ir100.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc", level[j])
    assign(fn,rep(NA,steps))


  }}

networksims <-c(1,9)
for(i in networksims){

  x<-get(scenarios[i])

  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".ir100.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.BM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.BF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.HM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.HF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.WM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.ct.WF", level[j])
    assign(fn,rep(NA,steps))



    fn <- paste0(scenarios[i], ".ir100.gc", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.BM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.BF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.HM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.HF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.WM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100.gc.WF", level[j])
    assign(fn,rep(NA,steps))


  }}



for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$ir100.ct[k,1:nsims]))
  sim_base.ir100.ct[k]<-mean(x)
  sim_base.ir100.ct_hi[k]<-x[hi.cut]
  sim_base.ir100.ct_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.BM[k,1:nsims]))
  sim_base.ir100.ct.BM[k]<-mean(x)
  sim_base.ir100.ct.BM_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.BF[k,1:nsims]))
  sim_base.ir100.ct.BF[k]<-mean(x)
  sim_base.ir100.ct.BF_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.HM[k,1:nsims]))
  sim_base.ir100.ct.HM[k]<-mean(x)
  sim_base.ir100.ct.HM_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.HF[k,1:nsims]))
  sim_base.ir100.ct.HF[k]<-mean(x)
  sim_base.ir100.ct.HF_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.WM[k,1:nsims]))
  sim_base.ir100.ct.WM[k]<-mean(x)
  sim_base.ir100.ct.WM_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.ct.WF[k,1:nsims]))
  sim_base.ir100.ct.WF[k]<-mean(x)
  sim_base.ir100.ct.WF_hi[k]<-x[hi.cut]
  sim_base.ir100.ct.WF_low[k]<-x[low.cut]

  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$ir100.ct[k,1:nsims]))
  sim_acts.ir100.ct[k]<-mean(x)
  sim_acts.ir100.ct_hi[k]<-x[hi.cut]
  sim_acts.ir100.ct_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$ir100.ct[k,1:nsims]))
  sim_cond.ir100.ct[k]<-mean(x)
  sim_cond.ir100.ct_hi[k]<-x[hi.cut]
  sim_cond.ir100.ct_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$ir100.ct[k,1:nsims]))
  sim_tst.ir100.ct[k]<-mean(x)
  sim_tst.ir100.ct_hi[k]<-x[hi.cut]
  sim_tst.ir100.ct_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$ir100.ct[k,1:nsims]))
  sim_acts_cond.ir100.ct[k]<-mean(x)
  sim_acts_cond.ir100.ct_hi[k]<-x[hi.cut]
  sim_acts_cond.ir100.ct_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$ir100.ct[k,1:nsims]))
  sim_acts_tst.ir100.ct[k]<-mean(x)
  sim_acts_tst.ir100.ct_hi[k]<-x[hi.cut]
  sim_acts_tst.ir100.ct_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$ir100.ct[k,1:nsims]))
  sim_cond_tst.ir100.ct[k]<-mean(x)
  sim_cond_tst.ir100.ct_hi[k]<-x[hi.cut]
  sim_cond_tst.ir100.ct_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$ir100.ct[k,1:nsims]))
  sim_acts_cond_tst.ir100.ct[k]<-mean(x)
  sim_acts_cond_tst.ir100.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.ir100.ct_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$ir100.ct[k,1:nsims]))
  sim_nets.ir100.ct[k]<-mean(x)
  sim_nets.ir100.ct_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.BM[k,1:nsims]))
  sim_nets.ir100.ct.BM[k]<-mean(x)
  sim_nets.ir100.ct.BM_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.BF[k,1:nsims]))
  sim_nets.ir100.ct.BF[k]<-mean(x)
  sim_nets.ir100.ct.BF_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.HM[k,1:nsims]))
  sim_nets.ir100.ct.HM[k]<-mean(x)
  sim_nets.ir100.ct.HM_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.HF[k,1:nsims]))
  sim_nets.ir100.ct.HF[k]<-mean(x)
  sim_nets.ir100.ct.HF_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.WM[k,1:nsims]))
  sim_nets.ir100.ct.WM[k]<-mean(x)
  sim_nets.ir100.ct.WM_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.ct.WF[k,1:nsims]))
  sim_nets.ir100.ct.WF[k]<-mean(x)
  sim_nets.ir100.ct.WF_hi[k]<-x[hi.cut]
  sim_nets.ir100.ct.WF_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$ir100.ct[k,1:nsims]))
  sim_acts_cond_tst_nets.ir100.ct[k]<-mean(x)
  sim_acts_cond_tst_nets.ir100.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.ir100.ct_low[k]<-x[low.cut]

}


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$ir100.gc[k,1:nsims]))
  sim_base.ir100.gc[k]<-mean(x)
  sim_base.ir100.gc_hi[k]<-x[hi.cut]
  sim_base.ir100.gc_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.BM[k,1:nsims]))
  sim_base.ir100.gc.BM[k]<-mean(x)
  sim_base.ir100.gc.BM_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.BF[k,1:nsims]))
  sim_base.ir100.gc.BF[k]<-mean(x)
  sim_base.ir100.gc.BF_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.HM[k,1:nsims]))
  sim_base.ir100.gc.HM[k]<-mean(x)
  sim_base.ir100.gc.HM_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.HF[k,1:nsims]))
  sim_base.ir100.gc.HF[k]<-mean(x)
  sim_base.ir100.gc.HF_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.WM[k,1:nsims]))
  sim_base.ir100.gc.WM[k]<-mean(x)
  sim_base.ir100.gc.WM_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_base$epi$ir100.gc.WF[k,1:nsims]))
  sim_base.ir100.gc.WF[k]<-mean(x)
  sim_base.ir100.gc.WF_hi[k]<-x[hi.cut]
  sim_base.ir100.gc.WF_low[k]<-x[low.cut]


  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$ir100.gc[k,1:nsims]))
  sim_acts.ir100.gc[k]<-mean(x)
  sim_acts.ir100.gc_hi[k]<-x[hi.cut]
  sim_acts.ir100.gc_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$ir100.gc[k,1:nsims]))
  sim_cond.ir100.gc[k]<-mean(x)
  sim_cond.ir100.gc_hi[k]<-x[hi.cut]
  sim_cond.ir100.gc_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$ir100.gc[k,1:nsims]))
  sim_tst.ir100.gc[k]<-mean(x)
  sim_tst.ir100.gc_hi[k]<-x[hi.cut]
  sim_tst.ir100.gc_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$ir100.gc[k,1:nsims]))
  sim_acts_cond.ir100.gc[k]<-mean(x)
  sim_acts_cond.ir100.gc_hi[k]<-x[hi.cut]
  sim_acts_cond.ir100.gc_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$ir100.gc[k,1:nsims]))
  sim_acts_tst.ir100.gc[k]<-mean(x)
  sim_acts_tst.ir100.gc_hi[k]<-x[hi.cut]
  sim_acts_tst.ir100.gc_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$ir100.gc[k,1:nsims]))
  sim_cond_tst.ir100.gc[k]<-mean(x)
  sim_cond_tst.ir100.gc_hi[k]<-x[hi.cut]
  sim_cond_tst.ir100.gc_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$ir100.gc[k,1:nsims]))
  sim_acts_cond_tst.ir100.gc[k]<-mean(x)
  sim_acts_cond_tst.ir100.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.ir100.gc_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$ir100.gc[k,1:nsims]))
  sim_nets.ir100.gc[k]<-mean(x)
  sim_nets.ir100.gc_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.BM[k,1:nsims]))
  sim_nets.ir100.gc.BM[k]<-mean(x)
  sim_nets.ir100.gc.BM_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.BF[k,1:nsims]))
  sim_nets.ir100.gc.BF[k]<-mean(x)
  sim_nets.ir100.gc.BF_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.HM[k,1:nsims]))
  sim_nets.ir100.gc.HM[k]<-mean(x)
  sim_nets.ir100.gc.HM_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.HF[k,1:nsims]))
  sim_nets.ir100.gc.HF[k]<-mean(x)
  sim_nets.ir100.gc.HF_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.WM[k,1:nsims]))
  sim_nets.ir100.gc.WM[k]<-mean(x)
  sim_nets.ir100.gc.WM_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$ir100.gc.WF[k,1:nsims]))
  sim_nets.ir100.gc.WF[k]<-mean(x)
  sim_nets.ir100.gc.WF_hi[k]<-x[hi.cut]
  sim_nets.ir100.gc.WF_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$ir100.gc[k,1:nsims]))
  sim_acts_cond_tst_nets.ir100.gc[k]<-mean(x)
  sim_acts_cond_tst_nets.ir100.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.ir100.gc_low[k]<-x[low.cut]

}


for(i in 1:length(scenarios)){

  x<-get(scenarios[i])

  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".ir100K.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".ir100K.gc", level[j])
    assign(fn,rep(NA,steps))


  }}


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$ir100K.ct[k,1:nsims]))
  sim_base.ir100K.ct[k]<-mean(x)
  sim_base.ir100K.ct_hi[k]<-x[hi.cut]
  sim_base.ir100K.ct_low[k]<-x[low.cut]

  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$ir100K.ct[k,1:nsims]))
  sim_acts.ir100K.ct[k]<-mean(x)
  sim_acts.ir100K.ct_hi[k]<-x[hi.cut]
  sim_acts.ir100K.ct_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$ir100K.ct[k,1:nsims]))
  sim_cond.ir100K.ct[k]<-mean(x)
  sim_cond.ir100K.ct_hi[k]<-x[hi.cut]
  sim_cond.ir100K.ct_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$ir100K.ct[k,1:nsims]))
  sim_tst.ir100K.ct[k]<-mean(x)
  sim_tst.ir100K.ct_hi[k]<-x[hi.cut]
  sim_tst.ir100K.ct_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$ir100K.ct[k,1:nsims]))
  sim_acts_cond.ir100K.ct[k]<-mean(x)
  sim_acts_cond.ir100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond.ir100K.ct_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$ir100K.ct[k,1:nsims]))
  sim_acts_tst.ir100K.ct[k]<-mean(x)
  sim_acts_tst.ir100K.ct_hi[k]<-x[hi.cut]
  sim_acts_tst.ir100K.ct_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$ir100K.ct[k,1:nsims]))
  sim_cond_tst.ir100K.ct[k]<-mean(x)
  sim_cond_tst.ir100K.ct_hi[k]<-x[hi.cut]
  sim_cond_tst.ir100K.ct_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$ir100K.ct[k,1:nsims]))
  sim_acts_cond_tst.ir100K.ct[k]<-mean(x)
  sim_acts_cond_tst.ir100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.ir100K.ct_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$ir100K.ct[k,1:nsims]))
  sim_nets.ir100K.ct[k]<-mean(x)
  sim_nets.ir100K.ct_hi[k]<-x[hi.cut]
  sim_nets.ir100K.ct_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$ir100K.ct[k,1:nsims]))
  sim_acts_cond_tst_nets.ir100K.ct[k]<-mean(x)
  sim_acts_cond_tst_nets.ir100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.ir100K.ct_low[k]<-x[low.cut]

}


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$ir100K.gc[k,1:nsims]))
  sim_base.ir100K.gc[k]<-mean(x)
  sim_base.ir100K.gc_hi[k]<-x[hi.cut]
  sim_base.ir100K.gc_low[k]<-x[low.cut]

  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$ir100K.gc[k,1:nsims]))
  sim_acts.ir100K.gc[k]<-mean(x)
  sim_acts.ir100K.gc_hi[k]<-x[hi.cut]
  sim_acts.ir100K.gc_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$ir100K.gc[k,1:nsims]))
  sim_cond.ir100K.gc[k]<-mean(x)
  sim_cond.ir100K.gc_hi[k]<-x[hi.cut]
  sim_cond.ir100K.gc_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$ir100K.gc[k,1:nsims]))
  sim_tst.ir100K.gc[k]<-mean(x)
  sim_tst.ir100K.gc_hi[k]<-x[hi.cut]
  sim_tst.ir100K.gc_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$ir100K.gc[k,1:nsims]))
  sim_acts_cond.ir100K.gc[k]<-mean(x)
  sim_acts_cond.ir100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond.ir100K.gc_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$ir100K.gc[k,1:nsims]))
  sim_acts_tst.ir100K.gc[k]<-mean(x)
  sim_acts_tst.ir100K.gc_hi[k]<-x[hi.cut]
  sim_acts_tst.ir100K.gc_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$ir100K.gc[k,1:nsims]))
  sim_cond_tst.ir100K.gc[k]<-mean(x)
  sim_cond_tst.ir100K.gc_hi[k]<-x[hi.cut]
  sim_cond_tst.ir100K.gc_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$ir100K.gc[k,1:nsims]))
  sim_acts_cond_tst.ir100K.gc[k]<-mean(x)
  sim_acts_cond_tst.ir100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.ir100K.gc_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$ir100K.gc[k,1:nsims]))
  sim_nets.ir100K.gc[k]<-mean(x)
  sim_nets.ir100K.gc_hi[k]<-x[hi.cut]
  sim_nets.ir100K.gc_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$ir100K.gc[k,1:nsims]))
  sim_acts_cond_tst_nets.ir100K.gc[k]<-mean(x)
  sim_acts_cond_tst_nets.ir100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.ir100K.gc_low[k]<-x[low.cut]

}


for(i in 1:length(scenarios)){

  x<-get(scenarios[i])


  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".diag100K.ct", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc", level[j])
    assign(fn,rep(NA,steps))


  }}


networksims <-c(1,9)
for(i in networksims){

  x<-get(scenarios[i])


  for(j in 1:length(level)){

    fn <- paste0(scenarios[i], ".diag100K.ct.BM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.ct.BF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.ct.HM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.ct.HF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.ct.WM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.ct.WF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.BM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.BF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.HM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.HF", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.WM", level[j])
    assign(fn,rep(NA,steps))

    fn <- paste0(scenarios[i], ".diag100K.gc.WF", level[j])
    assign(fn,rep(NA,steps))


  }}




for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$diag100K.ct[k,1:nsims]))
  sim_base.diag100K.ct[k]<-mean(x)
  sim_base.diag100K.ct_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct_low[k]<-x[low.cut]

  #no int BM
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.BM[k,1:nsims]))
  sim_base.diag100K.ct.BM[k]<-mean(x)
  sim_base.diag100K.ct.BM_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.BM_low[k]<-x[low.cut]

  #no int BF
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.BF[k,1:nsims]))
  sim_base.diag100K.ct.BF[k]<-mean(x)
  sim_base.diag100K.ct.BF_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.BF_low[k]<-x[low.cut]

  #no int HM
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.HM[k,1:nsims]))
  sim_base.diag100K.ct.HM[k]<-mean(x)
  sim_base.diag100K.ct.HM_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.HM_low[k]<-x[low.cut]

  #no int HF
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.HF[k,1:nsims]))
  sim_base.diag100K.ct.HF[k]<-mean(x)
  sim_base.diag100K.ct.HF_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.HF_low[k]<-x[low.cut]

  #no int WM
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.WM[k,1:nsims]))
  sim_base.diag100K.ct.WM[k]<-mean(x)
  sim_base.diag100K.ct.WM_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.WM_low[k]<-x[low.cut]

  #no int WF
  x<-sort(as.numeric(sim_base$epi$diag100K.ct.WF[k,1:nsims]))
  sim_base.diag100K.ct.WF[k]<-mean(x)
  sim_base.diag100K.ct.WF_hi[k]<-x[hi.cut]
  sim_base.diag100K.ct.WF_low[k]<-x[low.cut]



  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$diag100K.ct[k,1:nsims]))
  sim_acts.diag100K.ct[k]<-mean(x)
  sim_acts.diag100K.ct_hi[k]<-x[hi.cut]
  sim_acts.diag100K.ct_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$diag100K.ct[k,1:nsims]))
  sim_cond.diag100K.ct[k]<-mean(x)
  sim_cond.diag100K.ct_hi[k]<-x[hi.cut]
  sim_cond.diag100K.ct_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$diag100K.ct[k,1:nsims]))
  sim_tst.diag100K.ct[k]<-mean(x)
  sim_tst.diag100K.ct_hi[k]<-x[hi.cut]
  sim_tst.diag100K.ct_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$diag100K.ct[k,1:nsims]))
  sim_acts_cond.diag100K.ct[k]<-mean(x)
  sim_acts_cond.diag100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond.diag100K.ct_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$diag100K.ct[k,1:nsims]))
  sim_acts_tst.diag100K.ct[k]<-mean(x)
  sim_acts_tst.diag100K.ct_hi[k]<-x[hi.cut]
  sim_acts_tst.diag100K.ct_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$diag100K.ct[k,1:nsims]))
  sim_cond_tst.diag100K.ct[k]<-mean(x)
  sim_cond_tst.diag100K.ct_hi[k]<-x[hi.cut]
  sim_cond_tst.diag100K.ct_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$diag100K.ct[k,1:nsims]))
  sim_acts_cond_tst.diag100K.ct[k]<-mean(x)
  sim_acts_cond_tst.diag100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.diag100K.ct_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$diag100K.ct[k,1:nsims]))
  sim_nets.diag100K.ct[k]<-mean(x)
  sim_nets.diag100K.ct_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.BM[k,1:nsims]))
  sim_nets.diag100K.ct.BM[k]<-mean(x)
  sim_nets.diag100K.ct.BM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.BF[k,1:nsims]))
  sim_nets.diag100K.ct.BF[k]<-mean(x)
  sim_nets.diag100K.ct.BF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.HM[k,1:nsims]))
  sim_nets.diag100K.ct.HM[k]<-mean(x)
  sim_nets.diag100K.ct.HM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.HF[k,1:nsims]))
  sim_nets.diag100K.ct.HF[k]<-mean(x)
  sim_nets.diag100K.ct.HF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.WM[k,1:nsims]))
  sim_nets.diag100K.ct.WM[k]<-mean(x)
  sim_nets.diag100K.ct.WM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.ct.WF[k,1:nsims]))
  sim_nets.diag100K.ct.WF[k]<-mean(x)
  sim_nets.diag100K.ct.WF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.ct.WF_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$diag100K.ct[k,1:nsims]))
  sim_acts_cond_tst_nets.diag100K.ct[k]<-mean(x)
  sim_acts_cond_tst_nets.diag100K.ct_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.diag100K.ct_low[k]<-x[low.cut]

}


for (k in seq_along(1:steps)) {


  #no int
  x<-sort(as.numeric(sim_base$epi$diag100K.gc[k,1:nsims]))
  sim_base.diag100K.gc[k]<-mean(x)
  sim_base.diag100K.gc_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc_low[k]<-x[low.cut]

  #no int BM
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.BM[k,1:nsims]))
  sim_base.diag100K.gc.BM[k]<-mean(x)
  sim_base.diag100K.gc.BM_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.BM_low[k]<-x[low.cut]

  #no int BF
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.BF[k,1:nsims]))
  sim_base.diag100K.gc.BF[k]<-mean(x)
  sim_base.diag100K.gc.BF_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.BF_low[k]<-x[low.cut]

  #no int HM
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.HM[k,1:nsims]))
  sim_base.diag100K.gc.HM[k]<-mean(x)
  sim_base.diag100K.gc.HM_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.HM_low[k]<-x[low.cut]

  #no int HF
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.HF[k,1:nsims]))
  sim_base.diag100K.gc.HF[k]<-mean(x)
  sim_base.diag100K.gc.HF_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.HF_low[k]<-x[low.cut]

  #no int WM
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.WM[k,1:nsims]))
  sim_base.diag100K.gc.WM[k]<-mean(x)
  sim_base.diag100K.gc.WM_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.WM_low[k]<-x[low.cut]

  #no int WF
  x<-sort(as.numeric(sim_base$epi$diag100K.gc.WF[k,1:nsims]))
  sim_base.diag100K.gc.WF[k]<-mean(x)
  sim_base.diag100K.gc.WF_hi[k]<-x[hi.cut]
  sim_base.diag100K.gc.WF_low[k]<-x[low.cut]


  #"sim_acts",
  x<-sort(as.numeric(sim_acts$epi$diag100K.gc[k,1:nsims]))
  sim_acts.diag100K.gc[k]<-mean(x)
  sim_acts.diag100K.gc_hi[k]<-x[hi.cut]
  sim_acts.diag100K.gc_low[k]<-x[low.cut]

  #"sim_cond",
  x<-sort(as.numeric(sim_cond$epi$diag100K.gc[k,1:nsims]))
  sim_cond.diag100K.gc[k]<-mean(x)
  sim_cond.diag100K.gc_hi[k]<-x[hi.cut]
  sim_cond.diag100K.gc_low[k]<-x[low.cut]


  #sim_tst
  x<-sort(as.numeric(sim_tst$epi$diag100K.gc[k,1:nsims]))
  sim_tst.diag100K.gc[k]<-mean(x)
  sim_tst.diag100K.gc_hi[k]<-x[hi.cut]
  sim_tst.diag100K.gc_low[k]<-x[low.cut]

  #sim_acts_cond
  x<-sort(as.numeric(sim_acts_cond$epi$diag100K.gc[k,1:nsims]))
  sim_acts_cond.diag100K.gc[k]<-mean(x)
  sim_acts_cond.diag100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond.diag100K.gc_low[k]<-x[low.cut]

  #sim_acts_tst
  x<-sort(as.numeric(sim_acts_tst$epi$diag100K.gc[k,1:nsims]))
  sim_acts_tst.diag100K.gc[k]<-mean(x)
  sim_acts_tst.diag100K.gc_hi[k]<-x[hi.cut]
  sim_acts_tst.diag100K.gc_low[k]<-x[low.cut]

  #sim_cond_tst
  x<-sort(as.numeric(sim_cond_tst$epi$diag100K.gc[k,1:nsims]))
  sim_cond_tst.diag100K.gc[k]<-mean(x)
  sim_cond_tst.diag100K.gc_hi[k]<-x[hi.cut]
  sim_cond_tst.diag100K.gc_low[k]<-x[low.cut]

  #sim_acts_cond_tst
  x<-sort(as.numeric(sim_acts_cond_tst$epi$diag100K.gc[k,1:nsims]))
  sim_acts_cond_tst.diag100K.gc[k]<-mean(x)
  sim_acts_cond_tst.diag100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst.diag100K.gc_low[k]<-x[low.cut]

  #sim_nets
  x<-sort(as.numeric(sim_nets$epi$diag100K.gc[k,1:nsims]))
  sim_nets.diag100K.gc[k]<-mean(x)
  sim_nets.diag100K.gc_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.BM[k,1:nsims]))
  sim_nets.diag100K.gc.BM[k]<-mean(x)
  sim_nets.diag100K.gc.BM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.BM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.BF[k,1:nsims]))
  sim_nets.diag100K.gc.BF[k]<-mean(x)
  sim_nets.diag100K.gc.BF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.BF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.HM[k,1:nsims]))
  sim_nets.diag100K.gc.HM[k]<-mean(x)
  sim_nets.diag100K.gc.HM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.HM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.HF[k,1:nsims]))
  sim_nets.diag100K.gc.HF[k]<-mean(x)
  sim_nets.diag100K.gc.HF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.HF_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.WM[k,1:nsims]))
  sim_nets.diag100K.gc.WM[k]<-mean(x)
  sim_nets.diag100K.gc.WM_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.WM_low[k]<-x[low.cut]

  x<-sort(as.numeric(sim_nets$epi$diag100K.gc.WF[k,1:nsims]))
  sim_nets.diag100K.gc.WF[k]<-mean(x)
  sim_nets.diag100K.gc.WF_hi[k]<-x[hi.cut]
  sim_nets.diag100K.gc.WF_low[k]<-x[low.cut]

  #sim_acts_cond_tst_nets
  x<-sort(as.numeric(sim_acts_cond_tst_nets$epi$diag100K.gc[k,1:nsims]))
  sim_acts_cond_tst_nets.diag100K.gc[k]<-mean(x)
  sim_acts_cond_tst_nets.diag100K.gc_hi[k]<-x[hi.cut]
  sim_acts_cond_tst_nets.diag100K.gc_low[k]<-x[low.cut]

}



##Plots
observed.2012.ct <- rep(453.4,steps)
observed.2019.ct <- rep(551,steps)
observed.2012.gc <- rep(106.7,steps)
observed.2019.gc <- rep(187.8,steps)

#Diagnosis - Chlamydia baseline
tiff(filename = "out/Baseline Chlamydia Diagnosis.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.diag100K.ct)), (length(sim_base.diag100K.ct)):1)
yy <- c(sim_base.diag100K.ct_low, rev(sim_base.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.ct, lwd = 1, col = "black")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.ct, lwd = 1, col = "gray", lty=4)
lines(observed.2019.ct, lwd = 1, lty=2, col = "gray")
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Mean diagnosed per 100K", "2012 rate", "2019 rate"),
       col = c("black", "gray", "gray"),lty=c(1,4,2), cex = .8)

dev.off()

#Diagnosis - Chlamydia Interventions
tiff(filename = "out/Chlamydia Diagnosis with changes.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.diag100K.ct, lwd = 1, lty = 1, col = "black")
lines(sim_cond.diag100K.ct, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.diag100K.ct, lwd = 1, lty = 2, col = "blue")
lines(sim_acts.diag100K.ct, lwd = 1, lty = 3, col = "blue")
lines(sim_acts_cond_tst.diag100K.ct, lwd = 1, lty = 1, col = "green")
lines(sim_nets.diag100K.ct, lwd = 1, lty = 1, col = "red")
lines(sim_acts_cond_tst_nets.diag100K.ct, lwd = 1, lty = 2, col = "red")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.ct, lwd = 1, col = "gray", lty=4)
lines(observed.2019.ct, lwd = 1,  lty=2, col = "gray")
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                       "Three above - 2019", "Partnership network - 2019", "All above"),
       col = c("black", "blue", "blue", "blue", "green", "red", "red"),lty=c(1,1,2,3,1,1,2), cex = .8)

dev.off()

#Diagnosis - Chlamydia Interventions - no nets
tiff(filename = "out/Chlamydia Diagnosis with behavior change.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.diag100K.ct, lwd = 1, lty = 1, col = "black")
lines(sim_cond.diag100K.ct, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.diag100K.ct, lwd = 1, lty = 1, col = "red")
lines(sim_acts.diag100K.ct, lwd = 1, lty = 1, col = "green")
lines(sim_acts_cond_tst.diag100K.ct, lwd = 1, lty = 1, col = "yellow")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.ct, lwd = 1, col = "gray", lty=4)
lines(observed.2019.ct, lwd = 1,  lty=2, col = "gray")
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019","2012 rate", "2019 rate"),
       col = c("black", "blue", "red", "green", "yellow", "gray", "gray"),lty=c(1,1,1,1,1,4,2), cex = .8)

dev.off()


#Diagnosis - Gonorrhea baseline
tiff(filename = "out/Baseline Gonorrhea Diagnosis.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,250,25)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 250), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual gonorrhea diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.diag100K.gc)), (length(sim_base.diag100K.gc)):1)
yy <- c(sim_base.diag100K.gc_low, rev(sim_base.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.gc, lwd = 1, col = "black")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.gc, lwd = 1, col = "gray", lty=4)
lines(observed.2019.gc, lwd = 1, col = "gray", lty=2)
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Mean diagnosed per 100K", "2012 rate", "2019 rate"),
       col = c("black", "gray", "gray"),lty=c(1,4,2), cex = .8)

dev.off()

#Diagnosis - Gonorrhea Interventions
tiff(filename = "out/Gonorrhea Diagnosis with changes.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,250,25)
plot(sim_base.diag100K.gc, type = "n", ylim = c(0, 250), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual gonorrhea diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.diag100K.gc, lwd = 1, lty = 1, col = "black")
lines(sim_cond.diag100K.gc, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.diag100K.gc, lwd = 1, lty = 2, col = "blue")
lines(sim_acts.diag100K.gc, lwd = 1, lty = 3, col = "blue")
lines(sim_acts_cond_tst.diag100K.gc, lwd = 1, lty = 1, col = "green")
lines(sim_nets.diag100K.gc, lwd = 1, lty = 1, col = "red")
lines(sim_acts_cond_tst_nets.diag100K.gc, lwd = 1, lty = 2, col = "red")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.gc, lwd = 1, col = "gray", lty = 4)
lines(observed.2019.gc, lwd = 1, col = "gray", lty=2)
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                       "Three above - 2019", "Partnership network - 2019", "All above"),
       col = c("black", "blue", "blue", "blue", "green", "red", "red"),lty=c(1,1,2,3,1,1,2), cex = .8)

dev.off()


#Diagnosis - Gonorrhea Interventions - no nets
tiff(filename = "out/Gonorrhea Diagnosis with behavior change.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,250,25)
plot(sim_base.diag100K.gc, type = "n", ylim = c(0, 250), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual gonorrhea diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.diag100K.gc, lwd = 1, lty = 1, col = "black")
lines(sim_cond.diag100K.gc, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.diag100K.gc, lwd = 1, lty = 1, col = "red")
lines(sim_acts.diag100K.gc, lwd = 1, lty = 1, col = "green")
lines(sim_acts_cond_tst.diag100K.gc, lwd = 1, lty = 1, col = "yellow")


#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.gc, lwd = 1, col = "gray", lty = 4)
lines(observed.2019.gc, lwd = 1, col = "gray", lty=2)
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019","2012 rate", "2019 rate"),
       col = c("black", "blue", "red", "green", "yellow", "gray", "gray"),lty=c(1,1,1,1,1,4,2), cex = .8)

dev.off()

#####################################################################################################################
#####################################################################################################################

#Incidence - Chlamydia baseline
tiff(filename = "out/Baseline Chlamydia Incidence.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,5,.5)
plot(sim_base.ir100.ct, type = "n", ylim = c(0, 5), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Chlamydia incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.ir100.ct)), (length(sim_base.ir100.ct)):1)
yy <- c(sim_base.ir100.ct_low, rev(sim_base.ir100.ct_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.ir100.ct, lwd = 1, col = "black")


legend("topleft", c("Mean incidence rate"),
       col = c("black"),lty=1, cex = .8)

dev.off()

#Incidence - Chlamydia Interventions
tiff(filename = "out/Chlamydia Incidence with changes.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,5,.5)
plot(sim_base.ir100.ct, type = "n", ylim = c(0, 5), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Chlamydia incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.ir100.ct, lwd = 1, lty = 1, col = "black")
lines(sim_cond.ir100.ct, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.ir100.ct, lwd = 1, lty = 2, col = "blue")
lines(sim_acts.ir100.ct, lwd = 1, lty = 3, col = "blue")
lines(sim_acts_cond_tst.ir100.ct, lwd = 1, lty = 1, col = "green")
lines(sim_nets.ir100.ct, lwd = 1, lty = 1, col = "red")
lines(sim_acts_cond_tst_nets.ir100.ct, lwd = 1, lty = 2, col = "red")




legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                       "Three above - 2019", "Partnership network - 2019", "All above"),
       col = c("black", "blue", "blue", "blue", "green", "red", "red"),lty=c(1,1,2,3,1,1,2), cex = .8)

dev.off()

#Incidence - Chlamydia Interventions - no nets
tiff(filename = "out/Chlamydia Incidence with behavior change.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(1,5,.5)
plot(sim_base.ir100.ct, type = "n", ylim = c(1, 5), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Chlamydia incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.ir100.ct, lwd = 1, lty = 1, col = "black")
lines(sim_cond.ir100.ct, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.ir100.ct, lwd = 1, lty = 1, col = "red")
lines(sim_acts.ir100.ct, lwd = 1, lty = 1, col = "green")
lines(sim_acts_cond_tst.ir100.ct, lwd = 1, lty = 1, col = "yellow")


legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019"),
       col = c("black", "blue", "red", "green", "yellow"),lty=c(1,1,1,1,1), cex = .8)

dev.off()


#Incidence - Gonorrhea baseline
tiff(filename = "out/Baseline Gonorrhea Incidence.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,2.5,.25)
plot(sim_base.ir100.gc, type = "n", ylim = c(0, 2.5), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Gonorrhea incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.prev.gc)), (length(sim_base.prev.gc)):1)
yy <- c(sim_base.ir100.gc_low, rev(sim_base.ir100.gc_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.ir100.gc, lwd = 1, col = "black")


legend("topleft", c("Mean incidence rate"),
       col = c("black"),lty=1, cex = .8)

dev.off()

#Incidence - Gonorrhea Interventions
tiff(filename = "out/Gonorrhea Incidence with changes.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,2,.25)
plot(sim_base.ir100.gc, type = "n", ylim = c(0, 2), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Gonorrhea incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.ir100.gc, lwd = 1, lty = 1, col = "red")
lines(sim_cond.ir100.gc, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.ir100.gc, lwd = 1, lty = 2, col = "blue")
lines(sim_acts.ir100.gc, lwd = 1, lty = 3, col = "blue")
lines(sim_acts_cond_tst.ir100.gc, lwd = 1, lty = 1, col = "green")
lines(sim_nets.ir100.gc, lwd = 1, lty = 1, col = "red")
lines(sim_acts_cond_tst_nets.ir100.gc, lwd = 1, lty = 2, col = "red")


legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                       "Three above - 2019", "Partnership network - 2019", "All above"),
       col = c("black", "blue", "blue", "blue", "green", "red", "red"),lty=c(1,1,2,3,1,1,2), cex = .8)

dev.off()


#Incidence - Gonorrhea Interventions - no nets
tiff(filename = "out/Gonorrhea Incidence with behavior change.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,2.5,.25)
plot(sim_base.ir100.gc, type = "n", ylim = c(0, 2.5), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Gonorrhea incidence per 100 person years at risk",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

lines(sim_base.ir100.gc, lwd = 1, lty = 1, col = "black")
lines(sim_cond.ir100.gc, lwd = 1, lty = 1, col = "blue")
lines(sim_tst.ir100.gc, lwd = 1, lty = 2, col = "red")
lines(sim_acts.ir100.gc, lwd = 1, lty = 3, col = "green")
lines(sim_acts_cond_tst.ir100.gc, lwd = 1, lty = 1, col = "yellow")


legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019"),
       col = c("black", "blue", "red", "green", "yellow"),lty=c(1,1,2,3,1), cex = .8)

dev.off()


#####  NETWORK STATS FOR NETS

#Main partnerships
tiff(filename = "out/Network statistics Main.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(9000,12000,250)

plot(sim_base.edges.main, type = "n", ylim = c(9000, 12000), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Main partnerships",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.edges.main)), (length(sim_base.edges.main)):1)
yy <- c(sim_base.edges.main_low, rev(sim_base.edges.main_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.edges.main, lwd = 1, col = "black")
lines(rep(11096.5,length(sim_base.edges.main)), lwd = 1, lty=3, col = "black")

xx1 <- c(1:(length(sim_nets.edges.main)), (length(sim_nets.edges.main)):1)
yy1 <- c(sim_nets.edges.main_low, rev(sim_base.edges.main_hi))
polygon(xx1, yy1, col = adjustcolor("pink", alpha = 0.2), border = NA)
lines(sim_nets.edges.main, lwd = 1, col = "red")
lines(rep(10415,length(sim_nets.edges.main)), lwd = 1, lty=3,col = "red")

dev.off()


##Node Match Black main partnerships
tiff(filename = "out/Network statistics Nodematch Black Main.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
yticks <- seq(500,1500,100)
plot(sim_base.NM.B.main, type = "n", ylim = c(500,1500 ), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Nodematch Race = Black Main partnerships",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.NM.B.main)), (length(sim_base.NM.B.main)):1)
yy <- c(sim_base.NM.B.main_low, rev(sim_base.NM.B.main_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.NM.B.main, lwd = 1, col = "black")
lines(rep(999.5,length(sim_base.NM.B.main)), lwd = 3, col = "black")

xx1 <- c(1:(length(sim_nets.NM.B.main)), (length(sim_nets.NM.B.main)):1)
yy1 <- c(sim_nets.NM.B.main_low, rev(sim_base.NM.B.main_hi))
polygon(xx1, yy1, col = adjustcolor("pink", alpha = 0.2), border = NA)
lines(sim_nets.edges.casl, lwd = 1, col = "red")
lines(rep(794,length(sim_nets.NM.B.main)), lwd = 1, col = "red")

dev.off()


##Casual partnerships
yticks <- seq(2000,8000,250)
plot(sim_base.edges.casl, type = "n", ylim = c(2000, 8000), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Casual partnerships",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.edges.casl)), (length(sim_base.edges.casl)):1)
yy <- c(sim_base.edges.casl_low, rev(sim_base.edges.casl_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.edges.casl, lwd = 1, col = "black")
lines(rep(5779,length(sim_base.edges.casl)), lwd = 3, col = "black")

xx1 <- c(1:(length(sim_nets.edges.casl)), (length(sim_nets.edges.casl)):1)
yy1 <- c(sim_nets.edges.casl_low, rev(sim_nets.edges.casl_hi))
polygon(xx1, yy1, col = adjustcolor("pink", alpha = 0.2), border = NA)
lines(sim_nets.edges.casl, lwd = 1, col = "red")
lines(rep(5955,length(sim_nets.edges.casl)), lwd = 1, col = "red")

##Node match Black Casual partnerships
yticks <- seq(0,3000,250)
plot(sim_base.NM.B.casl, type = "n", ylim = c(0, 3000), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Nodematch Race = Black Main partnerships",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.NM.B.casl)), (length(sim_base.NM.B.casl)):1)
yy <- c(sim_base.NM.B.casl_low, rev(sim_base.NM.B.casl_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.NM.B.casl, lwd = 1, col = "black")
lines(rep(1424.2204,length(sim_base.edges.casl)), lwd = 3, col = "black")

xx1 <- c(1:(length(sim_nets.NM.B.casl)), (length(sim_nets.NM.B.casl)):1)
yy1 <- c(sim_nets.NM.B.casl_low, rev(sim_base.NM.B.casl_hi))
polygon(xx1, yy1, col = adjustcolor("pink", alpha = 0.2), border = NA)
lines(sim_nets.NM.B.casl, lwd = 1, col = "red")
lines(rep(1192.5,length(sim_nets.edges.casl)), lwd = 1, col = "red")

dev.off()



##########  Chlamydia diagnosis with behavior change and 95% SI

#Diagnosis - Chlamydia Interventions - no nets
tiff(filename = "out/Chlamydia Diagnosis with behavior change and 95 SI.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(200,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(200, 800), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)
xx <- c(1:(length(sim_base.diag100K.ct)), (length(sim_base.diag100K.ct)):1)
yy <- c(sim_base.diag100K.ct_low, rev(sim_base.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.ct, lwd = 1, col = "black")



xx <- c(1:(length(sim_cond.diag100K.ct)), (length(sim_cond.diag100K.ct)):1)
yy <- c(sim_cond.diag100K.ct_low, rev(sim_cond.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("blue", alpha = 0.1), border = NA)
lines(sim_cond.diag100K.ct, lwd = 1, lty = 1, col = "blue")

xx <- c(1:(length(sim_tst.diag100K.ct)), (length(sim_tst.diag100K.ct)):1)
yy <- c(sim_tst.diag100K.ct_low, rev(sim_tst.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("red", alpha = 0.1), border = NA)
lines(sim_tst.diag100K.ct, lwd = 1, lty = 1, col = "red")

xx <- c(1:(length(sim_acts.diag100K.ct)), (length(sim_acts.diag100K.ct)):1)
yy <- c(sim_acts.diag100K.ct_low, rev(sim_acts.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("green", alpha = 0.1), border = NA)
lines(sim_acts.diag100K.ct, lwd = 1, lty = 1, col = "green")

xx <- c(1:(length(sim_acts_cond_tst.diag100K.ct)), (length(sim_acts_cond_tst.diag100K.ct)):1)
yy <- c(sim_acts_cond_tst.diag100K.ct_low, rev(sim_acts_cond_tst.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("yellow", alpha = 0.1), border = NA)
lines(sim_acts_cond_tst.diag100K.ct, lwd = 1, lty = 1, col = "yellow")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.ct, lwd = 1, col = "black", lty=4)
lines(observed.2019.ct, lwd = 1,  lty=2, col = "black")
abline(v=416, lwd = 1, col = "black")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019","2012 rate", "2019 rate"),
       col = c("black", "blue", "red", "green", "yellow", "gray", "gray"),lty=c(1,1,1,1,1,4,2), cex = .8)

dev.off()


##########  Gonorrhea  diagnosis with behavior change and 95% SI

#Diagnosis - Gonorrhea  Interventions - no nets
tiff(filename = "out/Gonorrhea  Diagnosis with behavior change and 95 SI.tiff", height = 7, width = 7, units = "in", res = 250)

par(mfrow = c(1,1), mar = c(5,6,3,3), mgp = c(2,1,0))
xticks <- seq(0, steps, 52)
yticks <- seq(0,250,25)
plot(sim_base.diag100K.gc, type = "n", ylim = c(0, 250), lwd = 3, col="black", axes=FALSE,
     xlab = "Year", ylab = "Annual gonorrhea  diagnosis per 100,000",  cex.main=.7)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=.7)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)
xx <- c(1:(length(sim_base.diag100K.gc)), (length(sim_base.diag100K.gc)):1)
yy <- c(sim_base.diag100K.gc_low, rev(sim_base.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.gc, lwd = 1, col = "black")



xx <- c(1:(length(sim_cond.diag100K.gc)), (length(sim_cond.diag100K.gc)):1)
yy <- c(sim_cond.diag100K.gc_low, rev(sim_cond.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("blue", alpha = 0.1), border = NA)
lines(sim_cond.diag100K.gc, lwd = 1, lty = 1, col = "blue")

xx <- c(1:(length(sim_tst.diag100K.gc)), (length(sim_tst.diag100K.gc)):1)
yy <- c(sim_tst.diag100K.gc_low, rev(sim_tst.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("red", alpha = 0.1), border = NA)
lines(sim_tst.diag100K.gc, lwd = 1, lty = 1, col = "red")

xx <- c(1:(length(sim_acts.diag100K.gc)), (length(sim_acts.diag100K.gc)):1)
yy <- c(sim_acts.diag100K.gc_low, rev(sim_acts.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("green", alpha = 0.1), border = NA)
lines(sim_acts.diag100K.gc, lwd = 1, lty = 1, col = "green")

xx <- c(1:(length(sim_acts_cond_tst.diag100K.gc)), (length(sim_acts_cond_tst.diag100K.gc)):1)
yy <- c(sim_acts_cond_tst.diag100K.gc_low, rev(sim_acts_cond_tst.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("yellow", alpha = 0.1), border = NA)
lines(sim_acts_cond_tst.diag100K.gc, lwd = 1, lty = 1, col = "yellow")

#############################################################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.gc, lwd = 1, col = "black", lty=4)
lines(observed.2019.gc, lwd = 1,  lty=2, col = "black")
abline(v=416, lwd = 1, col = "black")

legend("topleft", c("Baseline 2012", "Condoms - 2019", "Testing - 2019", "Coital frequency - 2019",
                    "Three above - 2019","2012 rate", "2019 rate"),
       col = c("black", "blue", "red", "green", "yellow", "gray", "gray"),lty=c(1,1,1,1,1,4,2), cex = .8)

dev.off()




##########################################################################################
##########################################################################################

###     Baseline four panel plot
tiff(filename = "out/Baseline four panel plot.tiff", height = 9, width = 7, units = "in", res = 350)
par(mfrow = c(2,2), mar = c(3,4,3,3), mgp = c(2,1,0))
#######################################
#Diagnosis - Chlamydia baseline
#######################################


xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE, main = "(A)",
     xlab = "Year", ylab = "Annual chlamydia diagnosis per 100,000",  cex.main=1)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=1)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.diag100K.ct)), (length(sim_base.diag100K.ct)):1)
yy <- c(sim_base.diag100K.ct_low, rev(sim_base.diag100K.ct_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.ct, lwd = 1, col = "black")

#####################
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.ct, lwd = 1, col = "gray", lty=4)
lines(observed.2019.ct, lwd = 1, lty=2, col = "gray")
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Mean diagnosed per 100K", "2012 rate", "2019 rate"),
       col = c("black", "gray", "gray"),lty=c(1,4,2), cex = 1)

#######################################
#Incidence - Chlamydia baseline
#######################################


xticks <- seq(0, steps, 52)
yticks <- seq(0,5,.5)
plot(sim_base.ir100.ct, type = "n", ylim = c(0, 5), lwd = 3, col="black", axes=FALSE, main = "(B)",
     xlab = "Year", ylab = "Chlamydia incidence per 100 person-years at risk",  cex.main=1)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=1)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.ir100.ct)), (length(sim_base.ir100.ct)):1)
yy <- c(sim_base.ir100.ct_low, rev(sim_base.ir100.ct_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.ir100.ct, lwd = 1, col = "black")


legend("topleft", c("Mean incidence rate"),
       col = c("black"),lty=1, cex = 1)




###################################
#Diagnosis - Gonorrhea
##################################
xticks <- seq(0, steps, 52)
yticks <- seq(0,800,100)
plot(sim_base.diag100K.ct, type = "n", ylim = c(0, 800), lwd = 3, col="black", axes=FALSE, main = "(C)",
     xlab = "Year", ylab = "Annual gonorrhea diagnosis per 100,000",  cex.main=1)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=1)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.diag100K.gc)), (length(sim_base.diag100K.gc)):1)
yy <- c(sim_base.diag100K.gc_low, rev(sim_base.diag100K.gc_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.diag100K.gc, lwd = 1, col = "black")

#############
# ADD LINES for 2012 observed and 2019 observed #############
lines(observed.2012.gc, lwd = 1, col = "gray", lty=4)
lines(observed.2019.gc, lwd = 1, col = "gray", lty=2)
abline(v=416, lwd = 1, col = "gray")

legend("topleft", c("Mean diagnosed per 100K", "2012 rate", "2019 rate"),
       col = c("black", "gray", "gray"),lty=c(1,4,2), cex = 1)

###################################
#Incidence - Gonorrhea
##################################

xticks <- seq(0, steps, 52)
yticks <- seq(0,5,.5)
plot(sim_base.ir100.gc, type = "n", ylim = c(0, 5), lwd = 3, col="black", axes=FALSE, main = "(D)",
     xlab = "Year", ylab = "Gonorrhea incidence per 100 person-years at risk",  cex.main=1)
axis(1, at = xticks, labels = c("2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022"), col.axis="black", las=1, cex.axis=1)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.7)

xx <- c(1:(length(sim_base.prev.gc)), (length(sim_base.prev.gc)):1)
yy <- c(sim_base.ir100.gc_low, rev(sim_base.ir100.gc_hi))
polygon(xx, yy, col = adjustcolor("gray", alpha = 0.2), border = NA)
lines(sim_base.ir100.gc, lwd = 1, col = "black")


legend("topleft", c("Mean incidence rate"),
       col = c("black"),lty=1, cex = 1)



dev.off()

##############################################################################################################

##Summary table at 2019

#GC
#incidence
base.ir.gc <- sim_base.ir100.gc[468]
base.ir.gc.low <- sim_base.ir100.gc_low[468]
base.ir.gc.hi <- sim_base.ir100.gc_hi[468]

cond.ir.gc <- sim_cond.ir100.gc[468]
cond.ir.gc.low <- sim_cond.ir100.gc_low[468]
cond.ir.gc.hi <- sim_cond.ir100.gc_hi[468]

tst.ir.gc <- sim_tst.ir100.gc[468]
tst.ir.gc.low <- sim_tst.ir100.gc_low[468]
tst.ir.gc.hi <- sim_tst.ir100.gc_hi[468]

acts.ir.gc <- sim_acts.ir100.gc[468]
acts.ir.gc.low <- sim_acts.ir100.gc_low[468]
acts.ir.gc.hi <- sim_acts.ir100.gc_hi[468]

behave.ir.gc <- sim_acts_cond_tst.ir100.gc[468]
behave.ir.gc.low <- sim_acts_cond_tst.ir100.gc_low[468]
behave.ir.gc.hi <- sim_acts_cond_tst.ir100.gc_hi[468]

nets.ir.gc <- sim_nets.ir100.gc[468]
nets.ir.gc.low <- sim_nets.ir100.gc_low[468]
nets.ir.gc.hi <- sim_nets.ir100.gc_hi[468]

all.ir.gc <- sim_acts_cond_tst_nets.ir100.gc[468]
all.ir.gc.low <- sim_acts_cond_tst_nets.ir100.gc_low[468]
all.ir.gc.hi <- sim_acts_cond_tst_nets.ir100.gc_hi[468]

#GC
#diagnosis
base.diag.gc <- sim_base.diag100K.gc[468]
base.diag.gc.low <- sim_base.diag100K.gc_low[468]
base.diag.gc.hi <- sim_base.diag100K.gc_hi[468]

cond.diag.gc <- sim_cond.diag100K.gc[468]
cond.diag.gc.low <- sim_cond.diag100K.gc_low[468]
cond.diag.gc.hi <- sim_cond.diag100K.gc_hi[468]

tst.diag.gc <- sim_tst.diag100K.gc[468]
tst.diag.gc.low <- sim_tst.diag100K.gc_low[468]
tst.diag.gc.hi <- sim_tst.diag100K.gc_hi[468]

acts.diag.gc <- sim_acts.diag100K.gc[468]
acts.diag.gc.low <- sim_acts.diag100K.gc_low[468]
acts.diag.gc.hi <- sim_acts.diag100K.gc_hi[468]

behave.diag.gc <- sim_acts_cond_tst.diag100K.gc[468]
behave.diag.gc.low <- sim_acts_cond_tst.diag100K.gc_low[468]
behave.diag.gc.hi <- sim_acts_cond_tst.diag100K.gc_hi[468]

nets.diag.gc <- sim_nets.diag100K.gc[468]
nets.diag.gc.low <- sim_nets.diag100K.gc_low[468]
nets.diag.gc.hi <- sim_nets.diag100K.gc_hi[468]

all.diag.gc <- sim_acts_cond_tst_nets.diag100K.gc[468]
all.diag.gc.low <- sim_acts_cond_tst_nets.diag100K.gc_low[468]
all.diag.gc.hi <- sim_acts_cond_tst_nets.diag100K.gc_hi[468]


#CT
#incidence
base.ir.ct <- sim_base.ir100.ct[468]
base.ir.ct.low <- sim_base.ir100.ct_low[468]
base.ir.ct.hi <- sim_base.ir100.ct_hi[468]

cond.ir.ct <- sim_cond.ir100.ct[468]
cond.ir.ct.low <- sim_cond.ir100.ct_low[468]
cond.ir.ct.hi <- sim_cond.ir100.ct_hi[468]

tst.ir.ct <- sim_tst.ir100.ct[468]
tst.ir.ct.low <- sim_tst.ir100.ct_low[468]
tst.ir.ct.hi <- sim_tst.ir100.ct_hi[468]

acts.ir.ct <- sim_acts.ir100.ct[468]
acts.ir.ct.low <- sim_acts.ir100.ct_low[468]
acts.ir.ct.hi <- sim_acts.ir100.ct_hi[468]

behave.ir.ct <- sim_acts_cond_tst.ir100.ct[468]
behave.ir.ct.low <- sim_acts_cond_tst.ir100.ct_low[468]
behave.ir.ct.hi <- sim_acts_cond_tst.ir100.ct_hi[468]

nets.ir.ct <- sim_nets.ir100.ct[468]
nets.ir.ct.low <- sim_nets.ir100.ct_low[468]
nets.ir.ct.hi <- sim_nets.ir100.ct_hi[468]

all.ir.ct <- sim_acts_cond_tst_nets.ir100.ct[468]
all.ir.ct.low <- sim_acts_cond_tst_nets.ir100.ct_low[468]
all.ir.ct.hi <- sim_acts_cond_tst_nets.ir100.ct_hi[468]

#CT
#diagnosis
base.diag.ct <- sim_base.diag100K.ct[468]
base.diag.ct.low <- sim_base.diag100K.ct_low[468]
base.diag.ct.hi <- sim_base.diag100K.ct_hi[468]

cond.diag.ct <- sim_cond.diag100K.ct[468]
cond.diag.ct.low <- sim_cond.diag100K.ct_low[468]
cond.diag.ct.hi <- sim_cond.diag100K.ct_hi[468]

tst.diag.ct <- sim_tst.diag100K.ct[468]
tst.diag.ct.low <- sim_tst.diag100K.ct_low[468]
tst.diag.ct.hi <- sim_tst.diag100K.ct_hi[468]

acts.diag.ct <- sim_acts.diag100K.ct[468]
acts.diag.ct.low <- sim_acts.diag100K.ct_low[468]
acts.diag.ct.hi <- sim_acts.diag100K.ct_hi[468]

behave.diag.ct <- sim_acts_cond_tst.diag100K.ct[468]
behave.diag.ct.low <- sim_acts_cond_tst.diag100K.ct_low[468]
behave.diag.ct.hi <- sim_acts_cond_tst.diag100K.ct_hi[468]

nets.diag.ct <- sim_nets.diag100K.ct[468]
nets.diag.ct.low <- sim_nets.diag100K.ct_low[468]
nets.diag.ct.hi <- sim_nets.diag100K.ct_hi[468]

all.diag.ct <- sim_acts_cond_tst_nets.diag100K.ct[468]
all.diag.ct.low <- sim_acts_cond_tst_nets.diag100K.ct_low[468]
all.diag.ct.hi <- sim_acts_cond_tst_nets.diag100K.ct_hi[468]

#Make table

names <- c("base.diag.gc", "base.diag.gc.low", "base.diag.gc.hi", "cond.diag.gc", "cond.diag.gc.low", "cond.diag.gc.hi",
           "tst.diag.gc", "tst.diag.gc.low", "tst.diag.gc.hi", "acts.diag.gc", "acts.diag.gc.low", "acts.diag.gc.hi",
           "behave.diag.gc", "behave.diag.gc.low", "behave.diag.gc.hi", "nets.diag.gc", "nets.diag.gc.low", "nets.diag.gc.hi",
           "all.diag.gc", "all.diag.gc.low", "all.diag.gc.hi",

           "base.ir.gc", "base.ir.gc.low", "base.ir.gc.hi", "cond.ir.gc", "cond.ir.gc.low", "cond.ir.gc.hi",
           "tst.ir.gc", "tst.ir.gc.low", "tst.ir.gc.hi", "acts.ir.gc", "acts.ir.gc.low", "acts.ir.gc.hi",
           "behave.ir.gc", "behave.ir.gc.low", "behave.ir.gc.hi", "nets.ir.gc", "nets.ir.gc.low", "nets.ir.gc.hi",
           "all.ir.gc", "all.ir.gc.low", "all.ir.gc.hi",

           "base.diag.ct", "base.diag.ct.low", "base.diag.ct.hi", "cond.diag.ct", "cond.diag.ct.low", "cond.diag.ct.hi",
           "tst.diag.ct", "tst.diag.ct.low", "tst.diag.ct.hi", "acts.diag.ct", "acts.diag.ct.low", "acts.diag.ct.hi",
           "behave.diag.ct", "behave.diag.ct.low", "behave.diag.ct.hi", "nets.diag.ct", "nets.diag.ct.low", "nets.diag.ct.hi",
           "all.diag.ct", "all.diag.ct.low", "all.diag.ct.hi",

           "base.ir.ct", "base.ir.ct.low", "base.ir.ct.hi", "cond.ir.ct", "cond.ir.ct.low", "cond.ir.ct.hi",
           "tst.ir.ct", "tst.ir.ct.low", "tst.ir.ct.hi", "acts.ir.ct", "acts.ir.ct.low", "acts.ir.ct.hi",
           "behave.ir.ct", "behave.ir.ct.low", "behave.ir.ct.hi", "nets.ir.ct", "nets.ir.ct.low", "nets.ir.ct.hi",
           "all.ir.ct", "all.ir.ct.low", "all.ir.ct.hi")


values <- c(base.diag.gc, base.diag.gc.low, base.diag.gc.hi, cond.diag.gc, cond.diag.gc.low, cond.diag.gc.hi,
            tst.diag.gc, tst.diag.gc.low, tst.diag.gc.hi, acts.diag.gc, acts.diag.gc.low, acts.diag.gc.hi,
            behave.diag.gc, behave.diag.gc.low, behave.diag.gc.hi, nets.diag.gc, nets.diag.gc.low, nets.diag.gc.hi,
            all.diag.gc, all.diag.gc.low, all.diag.gc.hi,

            base.ir.gc, base.ir.gc.low, base.ir.gc.hi, cond.ir.gc, cond.ir.gc.low, cond.ir.gc.hi,
            tst.ir.gc, tst.ir.gc.low, tst.ir.gc.hi, acts.ir.gc, acts.ir.gc.low, acts.ir.gc.hi,
            behave.ir.gc, behave.ir.gc.low, behave.ir.gc.hi, nets.ir.gc, nets.ir.gc.low, nets.ir.gc.hi,
            all.ir.gc, all.ir.gc.low, all.ir.gc.hi,

            base.diag.ct, base.diag.ct.low, base.diag.ct.hi, cond.diag.ct, cond.diag.ct.low, cond.diag.ct.hi,
            tst.diag.ct, tst.diag.ct.low, tst.diag.ct.hi, acts.diag.ct, acts.diag.ct.low, acts.diag.ct.hi,
            behave.diag.ct, behave.diag.ct.low, behave.diag.ct.hi, nets.diag.ct, nets.diag.ct.low, nets.diag.ct.hi,
            all.diag.ct, all.diag.ct.low, all.diag.ct.hi,

            base.ir.ct, base.ir.ct.low, base.ir.ct.hi, cond.ir.ct, cond.ir.ct.low, cond.ir.ct.hi,
            tst.ir.ct, tst.ir.ct.low, tst.ir.ct.hi, acts.ir.ct, acts.ir.ct.low, acts.ir.ct.hi,
            behave.ir.ct, behave.ir.ct.low, behave.ir.ct.hi, nets.ir.ct, nets.ir.ct.low, nets.ir.ct.hi,
            all.ir.ct, all.ir.ct.low, all.ir.ct.hi)

output<-as.data.frame(rbind(names,values))


write.xlsx(output, file = "out/Table of final incidence and diagnosis rates.xlsx")


##Summary table baseline by sex and race at 2012

#GC
#diagnosis
base.diag.gc <- sim_base.diag100K.gc[52]
base.diag.gc.low <- sim_base.diag100K.gc_low[52]
base.diag.gc.hi <- sim_base.diag100K.gc_hi[52]

base.diag.gc.BM <- sim_base.diag100K.gc.BM[52]
base.diag.gc.BM.low <- sim_base.diag100K.gc.BM_low[52]
base.diag.gc.BM.hi <- sim_base.diag100K.gc.BM_hi[52]

base.diag.gc.BF <- sim_base.diag100K.gc.BF[52]
base.diag.gc.BF.low <- sim_base.diag100K.gc.BF_low[52]
base.diag.gc.BF.hi <- sim_base.diag100K.gc.BF_hi[52]

base.diag.gc.HM <- sim_base.diag100K.gc.HM[52]
base.diag.gc.HM.low <- sim_base.diag100K.gc.HM_low[52]
base.diag.gc.HM.hi <- sim_base.diag100K.gc.HM_hi[52]

base.diag.gc.HF <- sim_base.diag100K.gc.HF[52]
base.diag.gc.HF.low <- sim_base.diag100K.gc.HF_low[52]
base.diag.gc.HF.hi <- sim_base.diag100K.gc.HF_hi[52]

base.diag.gc.WM <- sim_base.diag100K.gc.WM[52]
base.diag.gc.WM.low <- sim_base.diag100K.gc.WM_low[52]
base.diag.gc.WM.hi <- sim_base.diag100K.gc.WM_hi[52]

base.diag.gc.WF <- sim_base.diag100K.gc.WF[52]
base.diag.gc.WF.low <- sim_base.diag100K.gc.WF_low[52]
base.diag.gc.WF.hi <- sim_base.diag100K.gc.WF_hi[52]

#CT
#diagnosis
base.diag.ct <- sim_base.diag100K.ct[52]
base.diag.ct.low <- sim_base.diag100K.ct_low[52]
base.diag.ct.hi <- sim_base.diag100K.ct_hi[52]

base.diag.ct.BM <- sim_base.diag100K.ct.BM[52]
base.diag.ct.BM.low <- sim_base.diag100K.ct.BM_low[52]
base.diag.ct.BM.hi <- sim_base.diag100K.ct.BM_hi[52]

base.diag.ct.BF <- sim_base.diag100K.ct.BF[52]
base.diag.ct.BF.low <- sim_base.diag100K.ct.BF_low[52]
base.diag.ct.BF.hi <- sim_base.diag100K.ct.BF_hi[52]

base.diag.ct.HM <- sim_base.diag100K.ct.HM[52]
base.diag.ct.HM.low <- sim_base.diag100K.ct.HM_low[52]
base.diag.ct.HM.hi <- sim_base.diag100K.ct.HM_hi[52]

base.diag.ct.HF <- sim_base.diag100K.ct.HF[52]
base.diag.ct.HF.low <- sim_base.diag100K.ct.HF_low[52]
base.diag.ct.HF.hi <- sim_base.diag100K.ct.HF_hi[52]

base.diag.ct.WM <- sim_base.diag100K.ct.WM[52]
base.diag.ct.WM.low <- sim_base.diag100K.ct.WM_low[52]
base.diag.ct.WM.hi <- sim_base.diag100K.ct.WM_hi[52]

base.diag.ct.WF <- sim_base.diag100K.ct.WF[52]
base.diag.ct.WF.low <- sim_base.diag100K.ct.WF_low[52]
base.diag.ct.WF.hi <- sim_base.diag100K.ct.WF_hi[52]


#Make table

names <- c("base.diag.gc", "base.diag.gc.low", "base.diag.gc.hi",
           "base.diag.gc.BM", "base.diag.gc.BM.low", "base.diag.gc.BM.hi",
           "base.diag.gc.BF", "base.diag.gc.BF.low", "base.diag.gc.BF.hi",
           "base.diag.gc.HM", "base.diag.gc.HM.low", "base.diag.gc.HM.hi",
           "base.diag.gc.HF", "base.diag.gc.HF.low", "base.diag.gc.HF.hi",
           "base.diag.gc.WM", "base.diag.gc.WM.low", "base.diag.gc.WM.hi",
           "base.diag.gc.WF", "base.diag.gc.WF.low", "base.diag.gc.WF.hi",
           "base.diag.ct", "base.diag.ct.low", "base.diag.ct.hi",
           "base.diag.ct.BM", "base.diag.ct.BM.low", "base.diag.ct.BM.hi",
           "base.diag.ct.BF", "base.diag.ct.BF.low", "base.diag.ct.BF.hi",
           "base.diag.ct.HM", "base.diag.ct.HM.low", "base.diag.ct.HM.hi",
           "base.diag.ct.HF", "base.diag.ct.HF.low", "base.diag.ct.HF.hi",
           "base.diag.ct.WM", "base.diag.ct.WM.low", "base.diag.ct.WM.hi",
           "base.diag.ct.WF", "base.diag.ct.WF.low", "base.diag.ct.WF.hi")


values <- c(base.diag.gc, base.diag.gc.low, base.diag.gc.hi,
            base.diag.gc.BM, base.diag.gc.BM.low, base.diag.gc.BM.hi,
            base.diag.gc.BF, base.diag.gc.BF.low, base.diag.gc.BF.hi,
            base.diag.gc.HM, base.diag.gc.HM.low, base.diag.gc.HM.hi,
            base.diag.gc.HF, base.diag.gc.HF.low, base.diag.gc.HF.hi,
            base.diag.gc.WM, base.diag.gc.WM.low, base.diag.gc.WM.hi,
            base.diag.gc.WF, base.diag.gc.WF.low, base.diag.gc.WF.hi,
            base.diag.ct, base.diag.ct.low, base.diag.ct.hi,
            base.diag.ct.BM, base.diag.ct.BM.low, base.diag.ct.BM.hi,
            base.diag.ct.BF, base.diag.ct.BF.low, base.diag.ct.BF.hi,
            base.diag.ct.HM, base.diag.ct.HM.low, base.diag.ct.HM.hi,
            base.diag.ct.HF, base.diag.ct.HF.low, base.diag.ct.HF.hi,
            base.diag.ct.WM, base.diag.ct.WM.low, base.diag.ct.WM.hi,
            base.diag.ct.WF, base.diag.ct.WF.low, base.diag.ct.WF.hi)

output<-as.data.frame(rbind(names,values))


write.xlsx(output, file = "out/Table of 2012 diagnosis rates by race and sex.xlsx")


##Summary table baseline scenario and 2019 network diagnosis and incidence by sex and race at 2019

#GC
#baseline scenario
#diagnosis
base.diag.gc <- sim_base.diag100K.gc[468]
base.diag.gc.low <- sim_base.diag100K.gc_low[468]
base.diag.gc.hi <- sim_base.diag100K.gc_hi[468]

base.diag.gc.BM <- sim_base.diag100K.gc.BM[468]
base.diag.gc.BM.low <- sim_base.diag100K.gc.BM_low[468]
base.diag.gc.BM.hi <- sim_base.diag100K.gc.BM_hi[468]

base.diag.gc.BF <- sim_base.diag100K.gc.BF[468]
base.diag.gc.BF.low <- sim_base.diag100K.gc.BF_low[468]
base.diag.gc.BF.hi <- sim_base.diag100K.gc.BF_hi[468]

base.diag.gc.HM <- sim_base.diag100K.gc.HM[468]
base.diag.gc.HM.low <- sim_base.diag100K.gc.HM_low[468]
base.diag.gc.HM.hi <- sim_base.diag100K.gc.HM_hi[468]

base.diag.gc.HF <- sim_base.diag100K.gc.HF[468]
base.diag.gc.HF.low <- sim_base.diag100K.gc.HF_low[468]
base.diag.gc.HF.hi <- sim_base.diag100K.gc.HF_hi[468]

base.diag.gc.WM <- sim_base.diag100K.gc.WM[468]
base.diag.gc.WM.low <- sim_base.diag100K.gc.WM_low[468]
base.diag.gc.WM.hi <- sim_base.diag100K.gc.WM_hi[468]

base.diag.gc.WF <- sim_base.diag100K.gc.WF[468]
base.diag.gc.WF.low <- sim_base.diag100K.gc.WF_low[468]
base.diag.gc.WF.hi <- sim_base.diag100K.gc.WF_hi[468]

#2019 network
#diagnosis
nets.diag.gc <- sim_nets.diag100K.gc[468]
nets.diag.gc.low <- sim_nets.diag100K.gc_low[468]
nets.diag.gc.hi <- sim_nets.diag100K.gc_hi[468]

nets.diag.gc.BM <- sim_nets.diag100K.gc.BM[468]
nets.diag.gc.BM.low <- sim_nets.diag100K.gc.BM_low[468]
nets.diag.gc.BM.hi <- sim_nets.diag100K.gc.BM_hi[468]

nets.diag.gc.BF <- sim_nets.diag100K.gc.BF[468]
nets.diag.gc.BF.low <- sim_nets.diag100K.gc.BF_low[468]
nets.diag.gc.BF.hi <- sim_nets.diag100K.gc.BF_hi[468]

nets.diag.gc.HM <- sim_nets.diag100K.gc.HM[468]
nets.diag.gc.HM.low <- sim_nets.diag100K.gc.HM_low[468]
nets.diag.gc.HM.hi <- sim_nets.diag100K.gc.HM_hi[468]

nets.diag.gc.HF <- sim_nets.diag100K.gc.HF[468]
nets.diag.gc.HF.low <- sim_nets.diag100K.gc.HF_low[468]
nets.diag.gc.HF.hi <- sim_nets.diag100K.gc.HF_hi[468]

nets.diag.gc.WM <- sim_nets.diag100K.gc.WM[468]
nets.diag.gc.WM.low <- sim_nets.diag100K.gc.WM_low[468]
nets.diag.gc.WM.hi <- sim_nets.diag100K.gc.WM_hi[468]

nets.diag.gc.WF <- sim_nets.diag100K.gc.WF[468]
nets.diag.gc.WF.low <- sim_nets.diag100K.gc.WF_low[468]
nets.diag.gc.WF.hi <- sim_nets.diag100K.gc.WF_hi[468]

#GC
#baseline scenario
#incidence
base.ir100.gc <- sim_base.ir100.gc[468]
base.ir100.gc.low <- sim_base.ir100.gc_low[468]
base.ir100.gc.hi <- sim_base.ir100.gc_hi[468]

base.ir100.gc.BM <- sim_base.ir100.gc.BM[468]
base.ir100.gc.BM.low <- sim_base.ir100.gc.BM_low[468]
base.ir100.gc.BM.hi <- sim_base.ir100.gc.BM_hi[468]

base.ir100.gc.BF <- sim_base.ir100.gc.BF[468]
base.ir100.gc.BF.low <- sim_base.ir100.gc.BF_low[468]
base.ir100.gc.BF.hi <- sim_base.ir100.gc.BF_hi[468]

base.ir100.gc.HM <- sim_base.ir100.gc.HM[468]
base.ir100.gc.HM.low <- sim_base.ir100.gc.HM_low[468]
base.ir100.gc.HM.hi <- sim_base.ir100.gc.HM_hi[468]

base.ir100.gc.HF <- sim_base.ir100.gc.HF[468]
base.ir100.gc.HF.low <- sim_base.ir100.gc.HF_low[468]
base.ir100.gc.HF.hi <- sim_base.ir100.gc.HF_hi[468]

base.ir100.gc.WM <- sim_base.ir100.gc.WM[468]
base.ir100.gc.WM.low <- sim_base.ir100.gc.WM_low[468]
base.ir100.gc.WM.hi <- sim_base.ir100.gc.WM_hi[468]

base.ir100.gc.WF <- sim_base.ir100.gc.WF[468]
base.ir100.gc.WF.low <- sim_base.ir100.gc.WF_low[468]
base.ir100.gc.WF.hi <- sim_base.ir100.gc.WF_hi[468]


#2019 network
#incidence
nets.ir100.gc <- sim_nets.ir100.gc[468]
nets.ir100.gc.low <- sim_nets.ir100.gc_low[468]
nets.ir100.gc.hi <- sim_nets.ir100.gc_hi[468]

nets.ir100.gc.BM <- sim_nets.ir100.gc.BM[468]
nets.ir100.gc.BM.low <- sim_nets.ir100.gc.BM_low[468]
nets.ir100.gc.BM.hi <- sim_nets.ir100.gc.BM_hi[468]

nets.ir100.gc.BF <- sim_nets.ir100.gc.BF[468]
nets.ir100.gc.BF.low <- sim_nets.ir100.gc.BF_low[468]
nets.ir100.gc.BF.hi <- sim_nets.ir100.gc.BF_hi[468]

nets.ir100.gc.HM <- sim_nets.ir100.gc.HM[468]
nets.ir100.gc.HM.low <- sim_nets.ir100.gc.HM_low[468]
nets.ir100.gc.HM.hi <- sim_nets.ir100.gc.HM_hi[468]

nets.ir100.gc.HF <- sim_nets.ir100.gc.HF[468]
nets.ir100.gc.HF.low <- sim_nets.ir100.gc.HF_low[468]
nets.ir100.gc.HF.hi <- sim_nets.ir100.gc.HF_hi[468]

nets.ir100.gc.WM <- sim_nets.ir100.gc.WM[468]
nets.ir100.gc.WM.low <- sim_nets.ir100.gc.WM_low[468]
nets.ir100.gc.WM.hi <- sim_nets.ir100.gc.WM_hi[468]

nets.ir100.gc.WF <- sim_nets.ir100.gc.WF[468]
nets.ir100.gc.WF.low <- sim_nets.ir100.gc.WF_low[468]
nets.ir100.gc.WF.hi <- sim_nets.ir100.gc.WF_hi[468]

#########################################################################################################
#######################################################################################################
#CT
#baseline scenario
#diagnosis
base.diag.ct <- sim_base.diag100K.ct[468]
base.diag.ct.low <- sim_base.diag100K.ct_low[468]
base.diag.ct.hi <- sim_base.diag100K.ct_hi[468]

base.diag.ct.BM <- sim_base.diag100K.ct.BM[468]
base.diag.ct.BM.low <- sim_base.diag100K.ct.BM_low[468]
base.diag.ct.BM.hi <- sim_base.diag100K.ct.BM_hi[468]

base.diag.ct.BF <- sim_base.diag100K.ct.BF[468]
base.diag.ct.BF.low <- sim_base.diag100K.ct.BF_low[468]
base.diag.ct.BF.hi <- sim_base.diag100K.ct.BF_hi[468]

base.diag.ct.HM <- sim_base.diag100K.ct.HM[468]
base.diag.ct.HM.low <- sim_base.diag100K.ct.HM_low[468]
base.diag.ct.HM.hi <- sim_base.diag100K.ct.HM_hi[468]

base.diag.ct.HF <- sim_base.diag100K.ct.HF[468]
base.diag.ct.HF.low <- sim_base.diag100K.ct.HF_low[468]
base.diag.ct.HF.hi <- sim_base.diag100K.ct.HF_hi[468]

base.diag.ct.WM <- sim_base.diag100K.ct.WM[468]
base.diag.ct.WM.low <- sim_base.diag100K.ct.WM_low[468]
base.diag.ct.WM.hi <- sim_base.diag100K.ct.WM_hi[468]

base.diag.ct.WF <- sim_base.diag100K.ct.WF[468]
base.diag.ct.WF.low <- sim_base.diag100K.ct.WF_low[468]
base.diag.ct.WF.hi <- sim_base.diag100K.ct.WF_hi[468]

#2019 network
#diagnosis
nets.diag.ct <- sim_nets.diag100K.ct[468]
nets.diag.ct.low <- sim_nets.diag100K.ct_low[468]
nets.diag.ct.hi <- sim_nets.diag100K.ct_hi[468]

nets.diag.ct.BM <- sim_nets.diag100K.ct.BM[468]
nets.diag.ct.BM.low <- sim_nets.diag100K.ct.BM_low[468]
nets.diag.ct.BM.hi <- sim_nets.diag100K.ct.BM_hi[468]

nets.diag.ct.BF <- sim_nets.diag100K.ct.BF[468]
nets.diag.ct.BF.low <- sim_nets.diag100K.ct.BF_low[468]
nets.diag.ct.BF.hi <- sim_nets.diag100K.ct.BF_hi[468]

nets.diag.ct.HM <- sim_nets.diag100K.ct.HM[468]
nets.diag.ct.HM.low <- sim_nets.diag100K.ct.HM_low[468]
nets.diag.ct.HM.hi <- sim_nets.diag100K.ct.HM_hi[468]

nets.diag.ct.HF <- sim_nets.diag100K.ct.HF[468]
nets.diag.ct.HF.low <- sim_nets.diag100K.ct.HF_low[468]
nets.diag.ct.HF.hi <- sim_nets.diag100K.ct.HF_hi[468]

nets.diag.ct.WM <- sim_nets.diag100K.ct.WM[468]
nets.diag.ct.WM.low <- sim_nets.diag100K.ct.WM_low[468]
nets.diag.ct.WM.hi <- sim_nets.diag100K.ct.WM_hi[468]

nets.diag.ct.WF <- sim_nets.diag100K.ct.WF[468]
nets.diag.ct.WF.low <- sim_nets.diag100K.ct.WF_low[468]
nets.diag.ct.WF.hi <- sim_nets.diag100K.ct.WF_hi[468]

#CT
#baseline scenario
#incidence
base.ir100.ct <- sim_base.ir100.ct[468]
base.ir100.ct.low <- sim_base.ir100.ct_low[468]
base.ir100.ct.hi <- sim_base.ir100.ct_hi[468]

base.ir100.ct.BM <- sim_base.ir100.ct.BM[468]
base.ir100.ct.BM.low <- sim_base.ir100.ct.BM_low[468]
base.ir100.ct.BM.hi <- sim_base.ir100.ct.BM_hi[468]

base.ir100.ct.BF <- sim_base.ir100.ct.BF[468]
base.ir100.ct.BF.low <- sim_base.ir100.ct.BF_low[468]
base.ir100.ct.BF.hi <- sim_base.ir100.ct.BF_hi[468]

base.ir100.ct.HM <- sim_base.ir100.ct.HM[468]
base.ir100.ct.HM.low <- sim_base.ir100.ct.HM_low[468]
base.ir100.ct.HM.hi <- sim_base.ir100.ct.HM_hi[468]

base.ir100.ct.HF <- sim_base.ir100.ct.HF[468]
base.ir100.ct.HF.low <- sim_base.ir100.ct.HF_low[468]
base.ir100.ct.HF.hi <- sim_base.ir100.ct.HF_hi[468]

base.ir100.ct.WM <- sim_base.ir100.ct.WM[468]
base.ir100.ct.WM.low <- sim_base.ir100.ct.WM_low[468]
base.ir100.ct.WM.hi <- sim_base.ir100.ct.WM_hi[468]

base.ir100.ct.WF <- sim_base.ir100.ct.WF[468]
base.ir100.ct.WF.low <- sim_base.ir100.ct.WF_low[468]
base.ir100.ct.WF.hi <- sim_base.ir100.ct.WF_hi[468]


#2019 network
#incidence
nets.ir100.ct <- sim_nets.ir100.ct[468]
nets.ir100.ct.low <- sim_nets.ir100.ct_low[468]
nets.ir100.ct.hi <- sim_nets.ir100.ct_hi[468]

nets.ir100.ct.BM <- sim_nets.ir100.ct.BM[468]
nets.ir100.ct.BM.low <- sim_nets.ir100.ct.BM_low[468]
nets.ir100.ct.BM.hi <- sim_nets.ir100.ct.BM_hi[468]

nets.ir100.ct.BF <- sim_nets.ir100.ct.BF[468]
nets.ir100.ct.BF.low <- sim_nets.ir100.ct.BF_low[468]
nets.ir100.ct.BF.hi <- sim_nets.ir100.ct.BF_hi[468]

nets.ir100.ct.HM <- sim_nets.ir100.ct.HM[468]
nets.ir100.ct.HM.low <- sim_nets.ir100.ct.HM_low[468]
nets.ir100.ct.HM.hi <- sim_nets.ir100.ct.HM_hi[468]

nets.ir100.ct.HF <- sim_nets.ir100.ct.HF[468]
nets.ir100.ct.HF.low <- sim_nets.ir100.ct.HF_low[468]
nets.ir100.ct.HF.hi <- sim_nets.ir100.ct.HF_hi[468]

nets.ir100.ct.WM <- sim_nets.ir100.ct.WM[468]
nets.ir100.ct.WM.low <- sim_nets.ir100.ct.WM_low[468]
nets.ir100.ct.WM.hi <- sim_nets.ir100.ct.WM_hi[468]

nets.ir100.ct.WF <- sim_nets.ir100.ct.WF[468]
nets.ir100.ct.WF.low <- sim_nets.ir100.ct.WF_low[468]
nets.ir100.ct.WF.hi <- sim_nets.ir100.ct.WF_hi[468]



#Make table

base.diag.gc.BM
nets.diag.ct.HF

names1 <- c("base.diag.gc", "base.diag.gc.low", "base.diag.gc.hi",
           "base.diag.gc.BM", "base.diag.gc.BM.low", "base.diag.gc.BM.hi",
           "base.diag.gc.BF", "base.diag.gc.BF.low", "base.diag.gc.BF.hi",
           "base.diag.gc.HM", "base.diag.gc.HM.low", "base.diag.gc.HM.hi",
           "base.diag.gc.HF", "base.diag.gc.HF.low", "base.diag.gc.HF.hi",
           "base.diag.gc.WM", "base.diag.gc.WM.low", "base.diag.gc.WM.hi",
           "base.diag.gc.WF", "base.diag.gc.WF.low", "base.diag.gc.WF.hi",
           "base.diag.ct", "base.diag.ct.low", "base.diag.ct.hi",
           "base.diag.ct.BM", "base.diag.ct.BM.low", "base.diag.ct.BM.hi",
           "base.diag.ct.BF", "base.diag.ct.BF.low", "base.diag.ct.BF.hi",
           "base.diag.ct.HM", "base.diag.ct.HM.low", "base.diag.ct.HM.hi",
           "base.diag.ct.HF", "base.diag.ct.HF.low", "base.diag.ct.HF.hi",
           "base.diag.ct.WM", "base.diag.ct.WM.low", "base.diag.ct.WM.hi",
           "base.diag.ct.WF", "base.diag.ct.WF.low", "base.diag.ct.WF.hi")

names2 <- c("nets.diag.gc", "nets.diag.gc.low", "nets.diag.gc.hi",
           "nets.diag.gc.BM", "nets.diag.gc.BM.low", "nets.diag.gc.BM.hi",
           "nets.diag.gc.BF", "nets.diag.gc.BF.low", "nets.diag.gc.BF.hi",
           "nets.diag.gc.HM", "nets.diag.gc.HM.low", "nets.diag.gc.HM.hi",
           "nets.diag.gc.HF", "nets.diag.gc.HF.low", "nets.diag.gc.HF.hi",
           "nets.diag.gc.WM", "nets.diag.gc.WM.low", "nets.diag.gc.WM.hi",
           "nets.diag.gc.WF", "nets.diag.gc.WF.low", "nets.diag.gc.WF.hi",
           "nets.diag.ct", "nets.diag.ct.low", "nets.diag.ct.hi",
           "nets.diag.ct.BM", "nets.diag.ct.BM.low", "nets.diag.ct.BM.hi",
           "nets.diag.ct.BF", "nets.diag.ct.BF.low", "nets.diag.ct.BF.hi",
           "nets.diag.ct.HM", "nets.diag.ct.HM.low", "nets.diag.ct.HM.hi",
           "nets.diag.ct.HF", "nets.diag.ct.HF.low", "nets.diag.ct.HF.hi",
           "nets.diag.ct.WM", "nets.diag.ct.WM.low", "nets.diag.ct.WM.hi",
           "nets.diag.ct.WF", "nets.diag.ct.WF.low", "nets.diag.ct.WF.hi")



values1 <- c(base.diag.gc, base.diag.gc.low, base.diag.gc.hi,
            base.diag.gc.BM, base.diag.gc.BM.low, base.diag.gc.BM.hi,
            base.diag.gc.BF, base.diag.gc.BF.low, base.diag.gc.BF.hi,
            base.diag.gc.HM, base.diag.gc.HM.low, base.diag.gc.HM.hi,
            base.diag.gc.HF, base.diag.gc.HF.low, base.diag.gc.HF.hi,
            base.diag.gc.WM, base.diag.gc.WM.low, base.diag.gc.WM.hi,
            base.diag.gc.WF, base.diag.gc.WF.low, base.diag.gc.WF.hi,
            base.diag.ct, base.diag.ct.low, base.diag.ct.hi,
            base.diag.ct.BM, base.diag.ct.BM.low, base.diag.ct.BM.hi,
            base.diag.ct.BF, base.diag.ct.BF.low, base.diag.ct.BF.hi,
            base.diag.ct.HM, base.diag.ct.HM.low, base.diag.ct.HM.hi,
            base.diag.ct.HF, base.diag.ct.HF.low, base.diag.ct.HF.hi,
            base.diag.ct.WM, base.diag.ct.WM.low, base.diag.ct.WM.hi,
            base.diag.ct.WF, base.diag.ct.WF.low, base.diag.ct.WF.hi)

values2 <- c(nets.diag.gc, nets.diag.gc.low, nets.diag.gc.hi,
            nets.diag.gc.BM, nets.diag.gc.BM.low, nets.diag.gc.BM.hi,
            nets.diag.gc.BF, nets.diag.gc.BF.low, nets.diag.gc.BF.hi,
            nets.diag.gc.HM, nets.diag.gc.HM.low, nets.diag.gc.HM.hi,
            nets.diag.gc.HF, nets.diag.gc.HF.low, nets.diag.gc.HF.hi,
            nets.diag.gc.WM, nets.diag.gc.WM.low, nets.diag.gc.WM.hi,
            nets.diag.gc.WF, nets.diag.gc.WF.low, nets.diag.gc.WF.hi,
            nets.diag.ct, nets.diag.ct.low, nets.diag.ct.hi,
            nets.diag.ct.BM, nets.diag.ct.BM.low, nets.diag.ct.BM.hi,
            nets.diag.ct.BF, nets.diag.ct.BF.low, nets.diag.ct.BF.hi,
            nets.diag.ct.HM, nets.diag.ct.HM.low, nets.diag.ct.HM.hi,
            nets.diag.ct.HF, nets.diag.ct.HF.low, nets.diag.ct.HF.hi,
            nets.diag.ct.WM, nets.diag.ct.WM.low, nets.diag.ct.WM.hi,
            nets.diag.ct.WF, nets.diag.ct.WF.low, nets.diag.ct.WF.hi)

names3 <- c("base.ir100.gc", "base.ir100.gc.low", "base.ir100.gc.hi",
            "base.ir100.gc.BM", "base.ir100.gc.BM.low", "base.ir100.gc.BM.hi",
            "base.ir100.gc.BF", "base.ir100.gc.BF.low", "base.ir100.gc.BF.hi",
            "base.ir100.gc.HM", "base.ir100.gc.HM.low", "base.ir100.gc.HM.hi",
            "base.ir100.gc.HF", "base.ir100.gc.HF.low", "base.ir100.gc.HF.hi",
            "base.ir100.gc.WM", "base.ir100.gc.WM.low", "base.ir100.gc.WM.hi",
            "base.ir100.gc.WF", "base.ir100.gc.WF.low", "base.ir100.gc.WF.hi",
            "base.ir100.ct", "base.ir100.ct.low", "base.ir100.ct.hi",
            "base.ir100.ct.BM", "base.ir100.ct.BM.low", "base.ir100.ct.BM.hi",
            "base.ir100.ct.BF", "base.ir100.ct.BF.low", "base.ir100.ct.BF.hi",
            "base.ir100.ct.HM", "base.ir100.ct.HM.low", "base.ir100.ct.HM.hi",
            "base.ir100.ct.HF", "base.ir100.ct.HF.low", "base.ir100.ct.HF.hi",
            "base.ir100.ct.WM", "base.ir100.ct.WM.low", "base.ir100.ct.WM.hi",
            "base.ir100.ct.WF", "base.ir100.ct.WF.low", "base.ir100.ct.WF.hi")

names4 <- c("nets.ir100.gc", "nets.ir100.gc.low", "nets.ir100.gc.hi",
            "nets.ir100.gc.BM", "nets.ir100.gc.BM.low", "nets.ir100.gc.BM.hi",
            "nets.ir100.gc.BF", "nets.ir100.gc.BF.low", "nets.ir100.gc.BF.hi",
            "nets.ir100.gc.HM", "nets.ir100.gc.HM.low", "nets.ir100.gc.HM.hi",
            "nets.ir100.gc.HF", "nets.ir100.gc.HF.low", "nets.ir100.gc.HF.hi",
            "nets.ir100.gc.WM", "nets.ir100.gc.WM.low", "nets.ir100.gc.WM.hi",
            "nets.ir100.gc.WF", "nets.ir100.gc.WF.low", "nets.ir100.gc.WF.hi",
            "nets.ir100.ct", "nets.ir100.ct.low", "nets.ir100.ct.hi",
            "nets.ir100.ct.BM", "nets.ir100.ct.BM.low", "nets.ir100.ct.BM.hi",
            "nets.ir100.ct.BF", "nets.ir100.ct.BF.low", "nets.ir100.ct.BF.hi",
            "nets.ir100.ct.HM", "nets.ir100.ct.HM.low", "nets.ir100.ct.HM.hi",
            "nets.ir100.ct.HF", "nets.ir100.ct.HF.low", "nets.ir100.ct.HF.hi",
            "nets.ir100.ct.WM", "nets.ir100.ct.WM.low", "nets.ir100.ct.WM.hi",
            "nets.ir100.ct.WF", "nets.ir100.ct.WF.low", "nets.ir100.ct.WF.hi")



values3 <- c(base.ir100.gc, base.ir100.gc.low, base.ir100.gc.hi,
             base.ir100.gc.BM, base.ir100.gc.BM.low, base.ir100.gc.BM.hi,
             base.ir100.gc.BF, base.ir100.gc.BF.low, base.ir100.gc.BF.hi,
             base.ir100.gc.HM, base.ir100.gc.HM.low, base.ir100.gc.HM.hi,
             base.ir100.gc.HF, base.ir100.gc.HF.low, base.ir100.gc.HF.hi,
             base.ir100.gc.WM, base.ir100.gc.WM.low, base.ir100.gc.WM.hi,
             base.ir100.gc.WF, base.ir100.gc.WF.low, base.ir100.gc.WF.hi,
             base.ir100.ct, base.ir100.ct.low, base.ir100.ct.hi,
             base.ir100.ct.BM, base.ir100.ct.BM.low, base.ir100.ct.BM.hi,
             base.ir100.ct.BF, base.ir100.ct.BF.low, base.ir100.ct.BF.hi,
             base.ir100.ct.HM, base.ir100.ct.HM.low, base.ir100.ct.HM.hi,
             base.ir100.ct.HF, base.ir100.ct.HF.low, base.ir100.ct.HF.hi,
             base.ir100.ct.WM, base.ir100.ct.WM.low, base.ir100.ct.WM.hi,
             base.ir100.ct.WF, base.ir100.ct.WF.low, base.ir100.ct.WF.hi)

values4 <- c(nets.ir100.gc, nets.ir100.gc.low, nets.ir100.gc.hi,
            nets.ir100.gc.BM, nets.ir100.gc.BM.low, nets.ir100.gc.BM.hi,
            nets.ir100.gc.BF, nets.ir100.gc.BF.low, nets.ir100.gc.BF.hi,
            nets.ir100.gc.HM, nets.ir100.gc.HM.low, nets.ir100.gc.HM.hi,
            nets.ir100.gc.HF, nets.ir100.gc.HF.low, nets.ir100.gc.HF.hi,
            nets.ir100.gc.WM, nets.ir100.gc.WM.low, nets.ir100.gc.WM.hi,
            nets.ir100.gc.WF, nets.ir100.gc.WF.low, nets.ir100.gc.WF.hi,
            nets.ir100.ct, nets.ir100.ct.low, nets.ir100.ct.hi,
            nets.ir100.ct.BM, nets.ir100.ct.BM.low, nets.ir100.ct.BM.hi,
            nets.ir100.ct.BF, nets.ir100.ct.BF.low, nets.ir100.ct.BF.hi,
            nets.ir100.ct.HM, nets.ir100.ct.HM.low, nets.ir100.ct.HM.hi,
            nets.ir100.ct.HF, nets.ir100.ct.HF.low, nets.ir100.ct.HF.hi,
            nets.ir100.ct.WM, nets.ir100.ct.WM.low, nets.ir100.ct.WM.hi,
            nets.ir100.ct.WF, nets.ir100.ct.WF.low, nets.ir100.ct.WF.hi)


output<-as.data.frame(rbind(names1,values1,names2,values2,names3,values3,names4,values4))


write.xlsx(output, file = "out/Table of diagnosis and incidence rates by race and sex baseline verses nets.xlsx")



