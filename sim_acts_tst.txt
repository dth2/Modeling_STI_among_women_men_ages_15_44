
##Behavior Change to 2019
## Acts and testing

library(EpiModelHIV)
library(EpiModelHPC)

load("est/netparams_2011.rda")
load("est/netstats_2011.rda")
epistats <- readRDS("est/EpiStats_2011.rda")
epistats_2019 <- readRDS("est/EpiStats_2019.rda")

epistats$acts.mod.het <- epistats_2019$acts.mod.het
epistats$stdtst.mod.het <- epistats_2019$stdtst.mod.het

pull_env_vars()

nsteps <- (52*60*2) + 520

time.unit <- 7
method<-1


init <- init_msm()

param <- param_msm(netstats = netstats_2011,
                   epistats = epistats,
                   ugc.prob = c(0.8651850, 0.8651850, 0.8651850) * 1.0098,
                   vgc.prob = c( 0.8029740,  0.8029740,  0.8029740) * 1.0098,
                   uct.prob = c(0.7887059, 0.7887059, 0.7887059) * 0.985,
                   vct.prob = c(0.8598805, 0.8598805, 0.8598805) * 0.985,
                   ugc.ntx.int = 47.7227296 * 1.0098,
                   vgc.ntx.int = 47.7227296 * 1.0098,
                   uct.ntx.int = 62.1589747 * 0.985,
                   vct.ntx.int = 62.1589747 * 0.985)

control <- control_msm(simno = fsimno,
                       nsteps = nsteps,
                       nsims = ncores,
                       ncores = ncores,
                       start = (52*60*2) + 1,
                       initialize.FUN =  reinit_msm,
                       tergmLite = TRUE,
                       save.other = c(),
                       verbose = TRUE)




x<-sample(round((1:9),1),1)
if(x==1){netsim_hpc("est/sim.burnin.1.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==2){netsim_hpc("est/sim.burnin.2.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==3){netsim_hpc("est/sim.burnin.3.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==4){netsim_hpc("est/sim.burnin.4.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==5){netsim_hpc("est/sim.burnin.5.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==6){netsim_hpc("est/sim.burnin.6.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==7){netsim_hpc("est/sim.burnin.7.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==8){netsim_hpc("est/sim.burnin.8.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==9){netsim_hpc("est/sim.burnin.9.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==10){netsim_hpc("est/sim.burnin.10.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==11){netsim_hpc("est/sim.burnin.11.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==12){netsim_hpc("est/sim.burnin.12.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==13){netsim_hpc("est/sim.burnin.13.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==14){netsim_hpc("est/sim.burnin.14.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}
if(x==15){netsim_hpc("est/sim.burnin.15.rda",param, init, control, verbose = TRUE, save.min = FALSE, save.max = TRUE)}

#netsim_hpc("est/sim.burnin.rda", param, init, control, cp.save.int = 15, verbose = TRUE, save.min = TRUE, save.max = FALSE)


