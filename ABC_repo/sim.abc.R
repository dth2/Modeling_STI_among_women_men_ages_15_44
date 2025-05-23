
rm(list = ls())
library("methods")
library("EpiModelHIV")
library("doParallel")
library("foreach")
library("EasyABC")


f <- function(x) {

  set.seed(x[1])

  suppressMessages(library("EpiModelHIV"))

  est <- readRDS("est/small_object_est_2011_adj.rds")
  load("est/netparams_2011.rda")
  load("est/netstats_2011.rda")
  load("est/epistats.rda")



  time.unit <- 7
  method<-1
  nsteps <- 52*60*2
  #nsteps <- 52


  param <- param_msm(netstats = netstats_2011,
                     epistats = epistats,
                     ugc.prob = c(x[2],x[3],x[4]),
                     vgc.prob = c(x[5],x[6],x[7]),
                     uct.prob = c(x[8],x[9],x[10]),
                     vct.prob = c(x[11],x[12],x[13]),
                     ugc.ntx.int = x[14],
                     vgc.ntx.int = x[14],
                     uct.ntx.int = x[15],
                     vct.ntx.int = x[15]
  )

  init <- init_msm(prev.ugc = .025,
                   prev.rgc =  0,
                   prev.vgc = .025,
                   prev.uct = .025,
                   prev.rct =  0,
                   prev.vct = .025)

  control <- control_msm(nsteps = nsteps,
                         nsims = 1,
                         tergmLite = TRUE,
                         save.other = c("attr", "temp", "el"), verbose = FALSE)



  sim <- netsim(est, param, init, control)
  df <- tail(as.data.frame(sim), 26)

  #CHLAMYDIA Diagnosis
  diag100K.ct <- mean(df$diag100K.ct)
  diag100K.ct.BF <- mean(df$diag100K.ct.BF)
  diag100K.ct.BM <- mean(df$diag100K.ct.BM)
  diag100K.ct.HF <- mean(df$diag100K.ct.HF)
  diag100K.ct.HM <- mean(df$diag100K.ct.HM)
  diag100K.ct.WF <- mean(df$diag100K.ct.WF)
  diag100K.ct.WM <- mean(df$diag100K.ct.WM)

  #GC Diagnosis
  diag100K.gc <- mean(df$diag100K.gc)
  diag100K.gc.BF <- mean(df$diag100K.gc.BF)
  diag100K.gc.BM <- mean(df$diag100K.gc.BM)
  diag100K.gc.HF <- mean(df$diag100K.gc.HF)
  diag100K.gc.HM <- mean(df$diag100K.gc.HM)
  diag100K.gc.WF <- mean(df$diag100K.gc.WF)
  diag100K.gc.WM <- mean(df$diag100K.gc.WM)



  out <- c(diag100K.ct, diag100K.ct.BF, diag100K.ct.BM, diag100K.ct.HF, diag100K.ct.HM, diag100K.ct.WF, diag100K.ct.WM,
           diag100K.gc, diag100K.gc.BF, diag100K.gc.BM, diag100K.gc.HF, diag100K.gc.HM, diag100K.gc.WF, diag100K.gc.WM)


  #out <- c(diag100K.ct, diag100K.gc)

  return(out)
}

#maybe more extreme
priors <- list(c("unif", 0.98, 1.0), #BM GC
               c("unif", 0.78, 0.85), #HM GC
               c("unif", 0.56, 0.64), #WM GC
               c("unif", 0.95, 1.0), #BF GC
               c("unif", 0.47, 0.54), #HF GC
               c("unif", 0.36, 0.45), #WF GC
               c("unif", 0.85, 0.91), #BM CT
               c("unif", 0.56, 0.63), #HM CT
               c("unif", 0.30, 0.37),    #WM CT
               c("unif", 0.98, 1.0),     #BF CT
               c("unif", 0.9, 0.97), #HF CT
               c("unif", 0.75, 0.85),    #WF CT
               c("unif", 76, 80),
               c("unif", 88.7, 88.95))


diag.targ <- c(453.4, 1542.4, 786, 555.7, 190.8, 254.7, 94.4, 106.7, 427.2, 447.4, 56.9, 62.3, 32.1, 28.5)
#diag.targ <- c(453.4, 106.7)



nsim <- 80
pacc <- .05



a <- ABC_sequential(method = "Lenormand",
                    model = f,
                    prior = priors,
                    nb_simul = nsim,
                    summary_stat_target = diag.targ,
                    p_acc_min = pacc,
                    progress_bar = TRUE,
                    n_cluster = 20,
                    use_seed = TRUE,
                    verbose = TRUE)

fn <- paste0("data/clR_new.", pacc*100, "pct.", nsim, "sim.rda")
save(a, file = fn)

