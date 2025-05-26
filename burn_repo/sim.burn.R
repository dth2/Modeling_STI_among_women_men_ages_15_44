

library("methods")
library("EpiModelHIV")
library("EpiModelHPC")

pull_env_vars()


# Epidemic model

## Parameters
#est <- readRDS("est/small_object_est_2011_adj.rds")
load("est/netparams_2011.rda")
load("est/netstats_2011.rda")
load("est/epistats.rda")

time.unit <- 7
method<-1
nsteps <- 52*60*2

# Base model


param <- param_msm(netstats = netstats_2011,
                   epistats = epistats,
                   ugc.prob = c(0.8651850, 0.8651850, 0.8651850),
                   vgc.prob = c( 0.8029740,  0.8029740,  0.8029740),
                   uct.prob = c(0.7887059, 0.7887059, 0.7887059),
                   vct.prob = c(0.8598805, 0.8598805, 0.8598805),
                   ugc.ntx.int = 47.7227296,
                   vgc.ntx.int = 47.7227296,
                   uct.ntx.int = 62.1589747,
                   vct.ntx.int = 62.1589747)



init <- init_msm(prev.ugc = .025,
                 prev.rgc =  0,
                 prev.vgc = .025,
                 prev.uct = .025,
                 prev.rct =  0,
                 prev.vct = .025)


control <- control_msm(nsteps = nsteps,
                      simno = fsimno,
                      nsims = ncores,
                      ncores = ncores,
                      start = 1,
                      tergmLite = TRUE,
                      initialize.FUN = initialize_msm,
                      save.other = c("attr","el","temp"),
                      verbose = TRUE)



netsim_hpc("est/small_object_est_2011_adj.rda", param, init, control, verbose = TRUE, save.min = TRUE, save.max = TRUE)

#process_simfiles(simno = simno, min.n = njobs, nsims = nsims, delete.sub = TRUE)
