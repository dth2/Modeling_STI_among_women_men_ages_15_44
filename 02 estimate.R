# Estimate the C17 networks in 2011 and 2019

load("~/CAMP-HET-STI/NSFG/data/l_het_2011.rda")
l<-l_het

load("~/CAMP-HET-STI/est/netparams_2011.rda")
netparams <- netparams_2011
load("~/CAMP-HET-STI/est/netstats_2011.rda")
netstats <- netstats_2011



#Initialize network using `netstats` .
num <- netstats$demog$num
nw <- network::network.initialize(num, directed = FALSE)
attr.names <- names(netstats$attr)
attr.values <- netstats$attr
nw.2011 <- network::set.vertex.attribute(nw, attr.names, attr.values)
save(nw.2011,file = "~/CAMP-HET-STI/est/nw.2011.rda")


 lmain <- l[l$ptype == 1, ]
 lmain$ar.cat<-paste(lmain$age.grp,lmain$race3)
 lmain$p_ar.cat<-paste(lmain$p_age.grp,lmain$p_race3)
 table(lmain$ar.cat,lmain$p_ar.cat)
 dem.matrix.mh.11 <-table(lmain$ar.cat,lmain$p_ar.cat)
 dem.matrix.mh.11[dem.matrix.mh.11==0]<-.5
 dem.matrix.mh.11
 setwd("~/CAMP-HET-STI")
 save(dem.matrix.mh.11, file = "dem.matrix.mh.11.rda")



# Main HET Model
nw_main.het <- nw.2011

#Formula:
model_main.het <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = c(-3,-4)) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -2) +
  nodefactor("deg.casl.c.het", levels = -1) +
  concurrent +
  degrange(from = 3) +
  offset(nodematch("sex", diff = FALSE))



# Target Stats
netstats_main.het <- c(
  edges = netstats$main.het$edges,
  nodematch_age.grp = netstats$main.het$nodematch_age.grp,
  nodefactor_age.grp = netstats$main.het$nodefactor_age.grp[c(-3,-4)],
  absdiff_age = netstats$main.het$absdiff_age,
  nodematch_race = netstats$main.het$nodematch_race,
  nodefactor_race = netstats$main.het$nodefactor_race[-2],
  nodefactor_deg.casl.het = netstats$main.het$nodefactor_deg.casl.c[-1],
  concurrent = netstats$main.het$concurrent,
  degrange = 0
)
cbind(netstats_main.het)
netstats_main.het <- unname(netstats_main.het)

# Fit model

st <- Sys.time()


fit_main.het.2011 <- netest(nw_main.het,
                       formation = model_main.het,
                       coef.form = c(-Inf),
                       target.stats = netstats_main.het,
                       coef.diss = netstats$main.het$diss.byage,
                       #coef.diss = netstats$main.het$diss.homog,
                       constraints = ~bd(maxout = 2) + blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                       verbose = TRUE,
                       set.control.tergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.mh.11) + sparse,
                                               MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                      init.method = "MPLE",
                                      MCMLE.effectiveSize=NULL,
                                      MCMC.burnin=1e6,
                                      MCMC.interval=3e5,
                                      MCMC.samplesize=12000,
                                      init.MPLE.samplesize = 1.1e7,
                                      MPLE.constraints.ignore = TRUE,
                                      parallel = 10,
                                      SAN.maxit = 8,
                                      SAN.nsteps = 2e8))

et <- Sys.time()
print(et - st)

object.size(fit_main.het.2011)
fit_main.het.2011<-trim_netest(fit_main.het.2011)
object.size(fit_main.het.2011)

save(fit_main.het.2011,file = "~/CAMP-HET-STI/est/fit_main.het.2011.rda")



# Casual Model
load("~/CAMP-HET-STI/est/nw.2011.rda")
nw_casl.het <- nw.2011
lcasl <- l[l$ptype == 2, ]
lcasl$ar.cat<-paste(lcasl$age.grp,lcasl$race3)
lcasl$p_ar.cat<-paste(lcasl$p_age.grp,lcasl$p_race3)
table(lcasl$ar.cat,lcasl$p_ar.cat)
dem.matrix.ch.11<-table(lcasl$ar.cat,lcasl$p_ar.cat)
dem.matrix.ch.11[dem.matrix.ch.11==0]<-.5
dem.matrix.ch.11
setwd("~/CAMP-HET-STI")
save(dem.matrix.ch.11, file = "dem.matrix.ch.11.rda")


# Formula
model_casl <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -4) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodefactor("deg.main.c.het", levels = -1) +
  concurrent +
  degrange(from = 4) +
  #If race = TRUE:
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -1) +
  nodefactor("age15", levels = -1) +
  offset(nodematch("sex", diff = FALSE))

# Target Stats
netstats_casl <- c(
  edges = netstats$casl.het$edges,
  nodematch_age.grp = netstats$casl.het$nodematch_age.grp,
  nodefactor_age.grp = netstats$casl.het$nodefactor_age.grp[-4],
  absdiff_age = netstats$casl.het$absdiff_age,
  nodefactor_deg.main.c.het = netstats$casl.het$nodefactor_deg.main.c[-1],
  concurrent = netstats$casl.het$concurrent,
  degrange = 0,
  #If race = TRUE:
  nodematch_race = netstats$casl.het$nodematch_race,
  nodefactor_race = netstats$casl.het$nodefactor_race[-1],
  nodefactor_age15 = netstats$casl.het$nodefactor_age15[-1]
)
cbind(netstats_casl)
netstats_casl <- unname(netstats_casl)

st <- Sys.time()
# Fit model
fit_casl.het.2011 <- netest(nw_casl.het,
                   formation = model_casl,
                   target.stats = netstats_casl,
                   coef.form = c(-Inf),
                   coef.diss = netstats$casl.het$diss.byage,
                   constraints = ~ blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                   verbose = TRUE,
                   set.control.tergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.ch.11) + sparse,
                                           MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                           init.method = "MPLE",
                                           MCMLE.effectiveSize=NULL,
                                           MCMC.burnin=1e6,
                                           MCMC.interval=3e5,
                                           MCMC.samplesize=7500,
                                           init.MPLE.samplesize = 1.1e7,
                                           MPLE.constraints.ignore = TRUE,
                                           parallel = 10,
                                           SAN.maxit = 8,
                                           SAN.nsteps = 1e8))


et <- Sys.time()
print(et - st)

object.size(fit_casl.het.2011)
fit_casl.het.2011<-trim_netest(fit_casl.het.2011)
object.size(fit_casl.het.2011)

save(fit_casl.het.2011,file = "~/CAMP-HET-STI/est/fit_casl.het.2011.rda")




# One-Off HET Model

load("~/CAMP-HET-STI/est/nw.2011.rda")
nw_inst.het <- nw.2011
linst <- l[l$ptype == 3, ]
linst$ar.cat<-paste(linst$age.grp,linst$race3)
linst$p_ar.cat<-paste(linst$p_age.grp,linst$p_race3)
table(linst$ar.cat,linst$p_ar.cat)
dem.matrix.ih.11<-table(linst$ar.cat,linst$p_ar.cat)
dem.matrix.ih.11[dem.matrix.ih.11==0]<-.5
dem.matrix.ih.11
setwd("~/CAMP-HET-STI")
save(dem.matrix.ih.11, file = "dem.matrix.ih.11.rda")


# Formula
model_inst <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", levels = -4) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodefactor("risk.grp", levels = 5) +
  nodefactor("deg.tot.c.het", levels = -1) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -3) +
  offset(nodematch("sex", diff = FALSE))

  # Target Stats
netstats_inst <- c(
  edges = netstats$inst.het$edges,
  nodematch_age.grp = sum(netstats$inst.het$nodematch_age.grp),
  nodefactor_age.grp = netstats$inst.het$nodefactor_age.grp[-4],
  absdiff_age = netstats$inst.het$absdiff_age,
  nodefactor_risk.grp = netstats$inst.het$nodefactor_risk.grp[5],
  nodefactor_deg.tot.c.het = netstats$inst.het$nodefactor_deg.tot.c.het[-1],
  nodematch_race = netstats$inst.het$nodematch_race,
  nodefactor_race = netstats$inst.het$nodefactor_race[-3]
  )
cbind(netstats_inst)
netstats_inst <- unname(netstats_inst)

# Fit model
fit_inst.het.2011 <- netest(nw_inst.het,
                   formation = model_inst,
                   target.stats = netstats_inst,
                   coef.form = c(-Inf),
                   coef.diss = dissolution_coefs(~offset(edges), 1),
                   constraints = ~ blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                   verbose = TRUE,
                   set.control.ergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.ih.11) + sparse,
                                           MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                           init.method = "MPLE",
                                           MCMLE.effectiveSize=NULL,
                                           MCMC.burnin=1e6,
                                           MCMC.interval=2e5,
                                           MCMC.samplesize=8500,
                                           init.MPLE.samplesize = 2e7,
                                           MPLE.constraints.ignore = TRUE,
                                           parallel = 10,
                                           SAN.nsteps = 1e8,
                                           SAN.maxit = 8))



object.size(fit_inst.het.2011)
fit_inst.het.2011<-trim_netest(fit_inst.het.2011)
object.size(fit_inst.het.2011)

save(fit_inst.het.2011,file = "~/CAMP-HET-STI/est/fit_inst.het.2011.rda")



##############################################
# 8. Save Data
out <- list(fit_main.het = fit_main.het.2011, fit_casl.het = fit_casl.het.2011, fit_inst.het = fit_inst.het.2011)

saveRDS(out, file = "est/small_object_est_2011.rds")

object.size(out)


###########  REPEAT FOR 2019  #######################



load("~/CAMP-HET-STI/NSFG/data/l_het_2019.rda")
l<-l_het

load("~/CAMP-HET-STI/est/netparams_2019.rda")
netparams <- netparams_2019
load("~/CAMP-HET-STI/est/netstats_2019.rda")
netstats <- netstats_2019


#Initialize network using `netstats` .
num <- netstats$demog$num
nw <- network::network.initialize(num, directed = FALSE)
attr.names <- names(netstats$attr)
attr.values <- netstats$attr
nw.2019 <- network::set.vertex.attribute(nw, attr.names, attr.values)
save(nw.2019,file = "~/CAMP-HET-STI/est/nw.2019.rda")




lmain <- l[l$ptype == 1, ]
lmain$ar.cat<-paste(lmain$age.grp,lmain$race3)
lmain$p_ar.cat<-paste(lmain$p_age.grp,lmain$p_race3)
table(lmain$ar.cat,lmain$p_ar.cat)
dem.matrix.mh.19 <-table(lmain$ar.cat,lmain$p_ar.cat)
dem.matrix.mh.19[dem.matrix.mh.19==0]<-.5
dem.matrix.mh.19
setwd("~/CAMP-HET-STI")
save(dem.matrix.mh.19, file = "dem.matrix.mh.19.rda")



# Main HET Model
nw_main.het <- nw.2019

#Formula:
model_main.het <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = c(-3,-4)) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -2) +
  nodefactor("deg.casl.c.het", levels = -1) +
  concurrent +
  degrange(from = 3) +
  offset(nodematch("sex", diff = FALSE))



# Target Stats
netstats_main.het <- c(
  edges = netstats$main.het$edges,
  nodematch_age.grp = netstats$main.het$nodematch_age.grp,
  nodefactor_age.grp = netstats$main.het$nodefactor_age.grp[c(-3,-4)],
  absdiff_age = netstats$main.het$absdiff_age,
  nodematch_race = netstats$main.het$nodematch_race,
  nodefactor_race = netstats$main.het$nodefactor_race[-2],
  nodefactor_deg.casl.het = netstats$main.het$nodefactor_deg.casl.c[-1],
  concurrent = netstats$main.het$concurrent,
  degrange = 0
)
cbind(netstats_main.het)
netstats_main.het <- unname(netstats_main.het)


# Fit model

st <- Sys.time()


fit_main.het.2019 <- netest(nw_main.het,
                            formation = model_main.het,
                            coef.form = c(-Inf),
                            target.stats = netstats_main.het,
                            coef.diss = netstats$main.het$diss.byage,
                            constraints = ~bd(maxout = 2) + blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                            verbose = TRUE,
                            set.control.ergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.mh.19) + sparse,
                                                    MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                    init.method = "MPLE",
                                                    MCMLE.effectiveSize=NULL,
                                                    MCMC.burnin=1e6,
                                                    MCMC.interval=4e5,
                                                    MCMC.samplesize=13000,
                                                    init.MPLE.samplesize = 2e7,
                                                    MPLE.constraints.ignore = TRUE,
                                                    parallel = 10,
                                                    SAN.maxit = 8,
                                                    SAN.nsteps = 1e8))

et <- Sys.time()
print(et - st)

object.size(fit_main.het.2019)
fit_main.het.2019<-trim_netest(fit_main.het.2019)
object.size(fit_main.het.2019)

save(fit_main.het.2019,file = "~/CAMP-HET-STI/est/fit_main.het.2019.rda")



# Casual Model
load("~/CAMP-HET-STI/est/nw.2019.rda")
nw_casl.het <- nw.2019
lcasl <- l[l$ptype == 2, ]
lcasl$ar.cat<-paste(lcasl$age.grp,lcasl$race3)
lcasl$p_ar.cat<-paste(lcasl$p_age.grp,lcasl$p_race3)
table(lcasl$ar.cat,lcasl$p_ar.cat)
dem.matrix.ch.19<-table(lcasl$ar.cat,lcasl$p_ar.cat)
dem.matrix.ch.19[dem.matrix.ch.19==0]<-.5
dem.matrix.ch.19
setwd("~/CAMP-HET-STI")
save(dem.matrix.ch.19, file = "dem.matrix.19.ch.rda")


# Formula
model_casl <- ~edges +
  nodematch("age.grp", diff = TRUE) +
  nodefactor("age.grp", levels = -4) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodefactor("deg.main.c.het", levels = -1) +
  concurrent +
  degrange(from = 4) +
  #If race = TRUE:
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -1) +
  nodefactor("age15", levels = -1) +
  offset(nodematch("sex", diff = FALSE))

# Target Stats
netstats_casl <- c(
  edges = netstats$casl.het$edges,
  nodematch_age.grp = netstats$casl.het$nodematch_age.grp,
  nodefactor_age.grp = netstats$casl.het$nodefactor_age.grp[-4],
  absdiff_age = netstats$casl.het$absdiff_age,
  nodefactor_deg.main.c.het = netstats$casl.het$nodefactor_deg.main.c[-1],
  concurrent = netstats$casl.het$concurrent,
  degrange = 0,
  #If race = TRUE:
  nodematch_race = netstats$casl.het$nodematch_race,
  nodefactor_race = netstats$casl.het$nodefactor_race[-1],
  nodefactor_age15 = netstats$casl.het$nodefactor_age15[-1]
)
cbind(netstats_casl)
netstats_casl <- unname(netstats_casl)

st <- Sys.time()
# Fit model
fit_casl.het.2019 <- netest(nw_casl.het,
                            formation = model_casl,
                            target.stats = netstats_casl,
                            coef.form = c(-Inf),
                            coef.diss = netstats$casl.het$diss.byage,
                            constraints = ~ blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                            verbose = TRUE,
                            set.control.ergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.ch.19) + sparse,
                                                    MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                    init.method = "MPLE",
                                                    MCMLE.effectiveSize=NULL,
                                                    MCMC.burnin=1e6,
                                                    MCMC.interval=3e5,
                                                    MCMC.samplesize=7500,
                                                    init.MPLE.samplesize = 1.1e7,
                                                    MPLE.constraints.ignore = TRUE,
                                                    parallel = 10,
                                                    SAN.nsteps = 1e8,
                                                    SAN.maxit = 8))




et <- Sys.time()
print(et - st)

object.size(fit_casl.het.2019)
fit_casl.het.2019<-trim_netest(fit_casl.het.2019)
object.size(fit_casl.het.2019)

save(fit_casl.het.2019,file = "~/CAMP-HET-STI/est/fit_casl.het.2019.rda")




# One-Off HET Model

load("~/CAMP-HET-STI/est/nw.2019.rda")
nw_inst.het <- nw.2019
linst <- l[l$ptype == 3, ]
linst$ar.cat<-paste(linst$age.grp,linst$race3)
linst$p_ar.cat<-paste(linst$p_age.grp,linst$p_race3)
table(linst$ar.cat,linst$p_ar.cat)
dem.matrix.ih.19<-table(linst$ar.cat,linst$p_ar.cat)
dem.matrix.ih.19[dem.matrix.ih.19==0]<-.5
dem.matrix.ih.19
setwd("~/CAMP-HET-STI")
save(dem.matrix.ih.19, file = "dem.matrix.ih.19.rda")


# Formula
model_inst <- ~edges +
  nodematch("age.grp", diff = FALSE) +
  nodefactor("age.grp", levels = -4) +
  absdiff(~age + 2.0*(sex == 2)) +
  nodefactor("risk.grp", levels = 5) +
  nodefactor("deg.tot.c.het", levels = -1) +
  nodematch("race", diff = TRUE) +
  nodefactor("race", levels = -3) +
  offset(nodematch("sex", diff = FALSE))

# Target Stats
netstats_inst <- c(
  edges = netstats$inst.het$edges,
  nodematch_age.grp = sum(netstats$inst.het$nodematch_age.grp),
  nodefactor_age.grp = netstats$inst.het$nodefactor_age.grp[-4],
  absdiff_age = netstats$inst.het$absdiff_age,
  nodefactor_risk.grp = netstats$inst.het$nodefactor_risk.grp[5],
  nodefactor_deg.tot.c.het = netstats$inst.het$nodefactor_deg.tot.c.het[-1],
  nodematch_race = netstats$inst.het$nodematch_race,
  nodefactor_race = netstats$inst.het$nodefactor_race[-3]
)
cbind(netstats_inst)
netstats_inst <- unname(netstats_inst)

# Fit model
fit_inst.het.2019 <- netest(nw_inst.het,
                            formation = model_inst,
                            target.stats = netstats_inst,
                            coef.form = c(-Inf),
                            coef.diss = dissolution_coefs(~offset(edges), 1),
                            constraints = ~ blocks(attr = ~sex, levels2 = diag(TRUE, 2)),
                            verbose = TRUE,
                            set.control.ergm = list(SAN.prop = ~strat(attr = ~paste(age.grp, race), pmat=dem.matrix.ih.19) + sparse,
                                                    MCMC.prop = ~strat(attr = ~paste(age.grp, race), empirical = TRUE) + sparse,
                                                    init.method = "MPLE",
                                                    MCMLE.effectiveSize=NULL,
                                                    MCMC.burnin=1e6,
                                                    MCMC.interval=2e5,
                                                    MCMC.samplesize=8500,
                                                    init.MPLE.samplesize = 1.1e7,
                                                    MPLE.constraints.ignore = TRUE,
                                                    parallel = 10,
                                                    SAN.nsteps = 1e8,
                                                    SAN.maxit = 8))



object.size(fit_inst.het.2019)
fit_inst.het.2019<-trim_netest(fit_inst.het.2019)
object.size(fit_inst.het.2019)

save(fit_inst.het.2019,file = "~/CAMP-HET-STI/est/fit_inst.het.2019.rda")



##############################################
# 8. Save Data
out <- list(fit_main.het = fit_main.het.2019, fit_casl.het = fit_casl.het.2019, fit_inst.het = fit_inst.het.2019)

saveRDS(out, file = "est/small_object_est_2019.rds")

object.size(out)

