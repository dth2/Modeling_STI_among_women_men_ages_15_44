#' Calculate Network Target Statistics
#'
#' @description Calculates the final target statistics for the network models by
#'              applying individual-level network statistics against the population
#'              size and structure, for use in the EpiModelHIV workflow.
#'
#' @param epistats Output from \code{\link{build_epistats}}.
#' @param netparams Output from \code{\link{build_netparams}}.
#' @param network.size Size of the starting network.
#' @param expect.mort Expected average mortality level to pass into
#'        \code{\link{dissolution_coefs}} function.
#' @param edges.avg Whether degree differences exist along race. TRUE
#'        or FALSE; default of FALSE.
#'
#' @details
#' \code{build_netstats} takes output from \code{\link{build_epistats}} and
#' \code{\link{build_netparams}} to build the relevant network statistics
#' that will be used in network estimation using package \link{EpiModel}.
#'
#' The parameter \code{edge.avg} allows a user set the network stated edges
#' to that estimated in \code{\link{build_netparams}} (divided by 2),
#' with \code{edges.avg = FALSE}, or, if sample proportions do not match
#' ARTnet population proportions, set to a weighted racial average
#' with \code{edges.avg = TRUE}.
#'
#' @export
#'
#' @examples
#' epistats <- build_epistats(geog.lvl = "city", geog.cat = "Atlanta")
#' netparams <- build_netparams(epistats = epistats, smooth.main.dur = TRUE)
#' netstats <- build_netstats(epistats, netparams)
#'


build_netstats <- function(netparams, d,l,
                           network.size = 50000,
                           expect.mort = 0.0001,
                           edges.avg = FALSE) {


  # Demographic Initialization ----------------------------------------------

  out <- list()
  out$demog <- list()

  ##race flag
  out$race <- TRUE

  # Overall network size
  num <- out$demog$num <- network.size

  # Population size by race group
  # race.dist.3cat


  num.B <- out$demog$num.B <- round(sum(d$race3==1))
  num.H <- out$demog$num.H <- round(sum(d$race3==2))
  num.W <- out$demog$num.W <- round(sum(d$race3==3))

  num.m <- out$demog$num.m <- round(sum(d$sex==1))
  num.f <- out$demog$num.f <- round(sum(d$sex==2))



  #Age range
  out$demog$ages <- sort(unique(d$age))
  out$demog$age.breaks <- c(14,19,25,35,45)

  ## Age-sex-specific mortality rates (B, H, W)

  asmr.B.m <- c(0.000653, 0.000829, 0.001024, 0.001235, 0.001448, 0.001671, 0.001871, 0.002010,
                0.002072, 0.002076, 0.002057, 0.002048, 0.002053, 0.002093, 0.002158, 0.002226,
                0.002286, 0.002349, 0.002415, 0.002490, 0.002587, 0.002704, 0.002826, 0.002943,
                0.003063, 0.003200, 0.003372, 0.003584, 0.003846, 0.004164)

  asmr.H.m <- c(0.000342, 0.000434, 0.000527, 0.000616, 0.000699, 0.000782, 0.000860, 0.000918,
                0.000952, 0.000967, 0.000975, 0.000985, 0.000995, 0.001009, 0.001026, 0.001045,
                0.001064, 0.001082, 0.001100, 0.001121, 0.001145, 0.001180, 0.001235, 0.001314,
                0.001415, 0.001530, 0.001655, 0.001795, 0.001951, 0.002125)

  asmr.W.m <- c(0.000387, 0.000485,0.000599, 0.000731,0.000869, 0.001011,0.001141, 0.001241,
                0.001304, 0.001339,0.001367, 0.001399,0.001427, 0.001453,0.001477, 0.001503,
                0.001530, 0.001561,0.001596, 0.001640,0.001703, 0.001783,0.001872, 0.001960,
                0.002053, 0.002159,0.002291, 0.002457,0.002668, 0.002924)

  asmr.B.f <- c(0.000238, 0.000279, 0.000328, 0.000386, 0.000447, 0.000511, 0.000572, 0.000621,
                0.000658, 0.000688, 0.000721, 0.000762, 0.000807, 0.000856, 0.000906, 0.000959,
                0.001017, 0.001086, 0.001172, 0.001275, 0.001400, 0.001537, 0.001674, 0.001798,
                0.001914, 0.002033, 0.002176, 0.002350, 0.002569, 0.002827)

  asmr.H.f <- c(0.000166, 0.000191, 0.000217, 0.000242, 0.000266, 0.000292, 0.000316, 0.000334,
                0.000344, 0.000347, 0.000347, 0.000350, 0.000358, 0.000372, 0.000393, 0.000416,
                0.000439, 0.000459, 0.000474, 0.000488, 0.000505, 0.000529, 0.000563, 0.000610,
                0.000669, 0.000734, 0.000807, 0.000901, 0.001020, 0.001161)

  asmr.W.f <- c(0.000204, 0.000251, 0.000295, 0.000331, 0.000361, 0.000391, 0.000421, 0.000451,
                0.000480, 0.000511, 0.000542, 0.000575, 0.000610, 0.000645, 0.000681, 0.000722,
                0.000765, 0.000807, 0.000845, 0.000886, 0.000936, 0.001002, 0.001076, 0.001156,
                0.001240, 0.001329, 0.001429, 0.001551, 0.001701, 0.001878)


    # transformed to weekly rates

    trans.asmr.B.m <- 1 - (1 - asmr.B.m)^(1/52)
    trans.asmr.H.m <- 1 - (1 - asmr.H.m)^(1/52)
    trans.asmr.W.m <- 1 - (1 - asmr.W.m)^(1/52)
    trans.asmr.B.f <- 1 - (1 - asmr.B.f)^(1/52)
    trans.asmr.H.f <- 1 - (1 - asmr.H.f)^(1/52)
    trans.asmr.W.f <- 1 - (1 - asmr.W.f)^(1/52)

    # Null rate for 0-14, transformed rates, total rate for 45

    vec.asmr.B.m <- c(rep(0, 14), trans.asmr.B.m, 1)
    vec.asmr.H.m <- c(rep(0, 14), trans.asmr.H.m, 1)
    vec.asmr.W.m <- c(rep(0, 14), trans.asmr.W.m, 1)
    vec.asmr.B.f <- c(rep(0, 14), trans.asmr.B.f, 1)
    vec.asmr.H.f <- c(rep(0, 14), trans.asmr.H.f, 1)
    vec.asmr.W.f <- c(rep(0, 14), trans.asmr.W.f, 1)


    asmr <- data.frame(age = 1:45, vec.asmr.B.m, vec.asmr.H.m, vec.asmr.W.m, vec.asmr.B.f, vec.asmr.H.f, vec.asmr.W.f)

    out$demog$asmr <- asmr

  # Nodal Attribute Initialization ------------------------------------------

  out$attr <- list()

  # age attributes
  partial<-sample(x=0:51,size=length(d$age),replace=TRUE)
  partial<-partial*(1/52)
  attr_age <- d$age + partial
  out$attr$age <- attr_age

  attr_sqrt.age <- sqrt(attr_age)
  out$attr$sqrt.age <- attr_sqrt.age

  attr_age.grp <- d$age.grp
  out$attr$age.grp <- attr_age.grp

  # race attribute
  attr_race <- as.numeric(d$race3)
  out$attr$race <- attr_race

  # sex attributes
  attr_sex <- as.numeric(d$sex)
  out$attr$sex <- attr_sex

  # MSM attribute
  attr_msm <- as.numeric(d$msm)
  out$attr$msm <- attr_msm

  # HET attribute
  attr_het <- as.numeric(d$het)
  out$attr$het <- attr_het



  # deg.casl attribute
  attr_deg.casl.het <- d$deg.casl.het
  out$attr$deg.casl.het <- attr_deg.casl.het

  attr_deg.casl.msm <- d$deg.casl.msm
  out$attr$deg.casl.msm <- attr_deg.casl.msm

  # deg.casl.c attribute
  attr_deg.casl.c.het <- d$deg.casl.het
  attr_deg.casl.c.het[attr_deg.casl.c.het >= 1] <-1
  out$attr$deg.casl.c.het <- attr_deg.casl.c.het

  attr_deg.casl.c.msm <- d$deg.casl.msm
  attr_deg.casl.c.msm[attr_deg.casl.c.msm >=1 ] <-1
  out$attr$deg.casl.c.msm <- attr_deg.casl.c.msm


  # deg main attribute
  attr_deg.main.het <- d$deg.main.het
  out$attr$deg.main.het <- attr_deg.main.het

  attr_deg.main.msm <- d$deg.main.msm
  out$attr$deg.main.msm <- attr_deg.main.msm

  # deg main.c attribute
  attr_deg.main.c.het <- d$deg.main.het
  attr_deg.main.c.het[attr_deg.main.c.het >= 1] <- 1
  out$attr$deg.main.c.het <- attr_deg.main.c.het

  attr_deg.main.c.msm <- d$deg.main.msm
  attr_deg.main.c.msm[attr_deg.main.c.msm >= 1] <-1
  out$attr$deg.main.c.msm <- attr_deg.main.c.msm


  # deg tot.c  attribute
  attr_deg.tot.c.het <- d$deg.tot.c.het
  out$attr$deg.tot.c.het <- attr_deg.tot.c.het

  attr_deg.tot.c.msm <- d$deg.tot.c.msm
  out$attr$deg.tot.c.msm <- attr_deg.tot.c.msm

  # risk group
  attr_risk.grp <- d$risk.grp
  out$attr$risk.grp <- attr_risk.grp

  # role class
  attr_role.class <- d$role.class
  out$attr$role.class <- attr_role.class

  # age 15
  attr_age15 <- d$age15
  out$attr$age15 <- attr_age15

  out$attr$dem.cat <- rep(NA,length(out$attr$race))

  out$attr$dem.cat <- ifelse(out$attr$msm==0 & out$attr$sex==1 & out$attr$race==1, 1,
                                                  ifelse(out$attr$msm==0 & out$attr$sex==1 & out$attr$race==2, 2,
                                                         ifelse(out$attr$msm==0 & out$attr$sex==1 & out$attr$race==3, 3,
                                                                ifelse(out$attr$msm==0 & out$attr$sex==2 & out$attr$race==1, 4,
                                                                       ifelse(out$attr$msm==0 & out$attr$sex==2 & out$attr$race==2, 5,
                                                                              ifelse(out$attr$msm==0 & out$attr$sex==2 & out$attr$race==3, 6,out$attr$dem.cat))))))

  # diag status
  # if (is.null(epistats$init.hiv.prev)) {
  #   if (race == TRUE) {
  #     xs <- data.frame(age = attr_age, race.cat3 = attr_race, geogYN = 1)
  #     preds <- predict(epistats$hiv.mod, newdata = xs, type = "response")
  #     attr_diag.status <- rbinom(num, 1, preds)
  #     out$attr$diag.status <- attr_diag.status
  #   }  else {
  #     xs <- data.frame(age = attr_age, geogYN = 1)
  #     preds <- predict(epistats$hiv.mod, newdata = xs, type = "response")
  #     attr_diag.status <- rbinom(num, 1, preds)
  #     out$attr$diag.status <- attr_diag.status
  #   }
  # } else {
  #   if (race == TRUE) {
  #     init.hiv.prev <- epistats$init.hiv.prev
  #     samp.B <- which(attr_race == 1)
  #     exp.B <- ceiling(length(samp.B)*init.hiv.prev[1])
  #     samp.H <- which(attr_race == 2)
  #     exp.H <- ceiling(length(samp.H)*init.hiv.prev[2])
  #     samp.W <- which(attr_race == 3)
  #     exp.W <- ceiling(length(samp.W)*init.hiv.prev[3])
  #
  #     attr_diag.status <- rep(0, network.size)
  #
  #     attr_diag.status[sample(samp.B, exp.B)] <- 1
  #     attr_diag.status[sample(samp.H, exp.H)] <- 1
  #     attr_diag.status[sample(samp.W, exp.W)] <- 1
  #
  #     out$attr$diag.status <- attr_diag.status
  #
  #   } else {
  #     init.hiv.prev <- epistats$init.hiv.prev[1]
  #     samp.size <- ceiling(network.size*init.hiv.prev)
  #     attr_diag.status <- sample(1:network.size, samp.size)
  #     out$attr$diag.status <- rep(0, network.size)
  #     out$attr$diag.status[attr_diag.status] <- 1
  #   }
  # }
  #

  attr_diag.status <- rep(0, network.size)

  # Main HET Model -----------------------------------------------------------

  out$main.het <- list()

  ## edges

  out$main.het$edges <- (netparams$main.het$md.main * num) / 2

  ## nodefactor("race")
  nodefactor_race <- table(out$attr$race) * netparams$main.het$nf.race
  out$main.het$nodefactor_race <- unname(nodefactor_race)

  ## nodematch("race")
  nodematch_race <- nodefactor_race/2 * netparams$main.het$nm.race
  out$main.het$nodematch_race <- unname(nodematch_race)


  ## nodefactor("age.grp")
  nodefactor_age.grp <- table(out$attr$age.grp) * netparams$main.het$nf.age.grp
  out$main.het$nodefactor_age.grp <- unname(nodefactor_age.grp)

  ## nodematch("age.grp")
  nodematch_age.grp <- nodefactor_age.grp/2 * netparams$main.het$nm.age.grp
  out$main.het$nodematch_age.grp <- unname(nodematch_age.grp)

  ## absdiff("age")
  absdiff_age <- out$main.het$edges * netparams$main.het$absdiff.age
  out$main.het$absdiff_age <- absdiff_age

  ## absdiff("sqrt.age")
  absdiff_sqrt.age <- out$main.het$edges * netparams$main.het$absdiff.sqrt.age
  out$main.het$absdiff_sqrt.age <- absdiff_sqrt.age

  ## nodefactor("age15")
  nodefactor_age15 <- table(out$attr$age15) * netparams$main.het$nf.age15
  out$main.het$nodefactor_age15 <- unname(nodefactor_age15)

  ## nodefactor("deg.casl")
  out$main.het$nodefactor_deg.casl <-
    num * netparams$main.het$deg.casl.dist * netparams$main.het$nf.deg.casl

  ## nodefactor("deg.casl.c")
  nodefactor_deg.casl.c <- table(out$attr$deg.casl.c.het) * netparams$main.het$nf.deg.casl.c.het
  out$main.het$nodefactor_deg.casl.c <- unname(nodefactor_deg.casl.c)

  ## concurrent
  out$main.het$concurrent <- num * netparams$main.het$concurrent

  ## nodefactor("diag.status")
  #  nodefactor_diag.status <-
  #    table(out$attr$diag.status) * netparams$main$nf.diag.status
  #  out$main$nodefactor_diag.status <- unname(nodefactor_diag.status)

  ## Dissolution
  out$main.het$diss.homog <- dissolution_coefs(dissolution = ~offset(edges),
                                           duration = netparams$main.het$durs.main.homog$mean.dur.adj,
                                           d.rate = expect.mort)
  out$main.het$diss.byage <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
                                           duration = netparams$main.het$durs.main.byage$mean.dur.adj,
                                           d.rate = expect.mort)


  # Casual HET Model ------------------------------------------------------------

  out$casl.het <- list()

  ## edges

  out$casl.het$edges <- (netparams$casl.het$md.casl * num) / 2

  ## nodefactor("race")
  nodefactor_race <- table(out$attr$race) * netparams$casl.het$nf.race
  out$casl.het$nodefactor_race <- unname(nodefactor_race)

  ## nodematch("race")
  nodematch_race <- nodefactor_race/2 * netparams$casl.het$nm.race
  out$casl.het$nodematch_race <- unname(nodematch_race)


  ## nodefactor("age.grp")
  nodefactor_age.grp <- table(out$attr$age.grp) * netparams$casl.het$nf.age.grp
  out$casl.het$nodefactor_age.grp <- unname(nodefactor_age.grp)

  ## nodematch("age.grp")
  nodematch_age.grp <- nodefactor_age.grp/2 * netparams$casl.het$nm.age.grp
  out$casl.het$nodematch_age.grp <- unname(nodematch_age.grp)

  ## absdiff("age")
  absdiff_age <- out$casl.het$edges * netparams$casl.het$absdiff.age
  out$casl.het$absdiff_age <- absdiff_age

  ## absdiff("sqrt.age")
  absdiff_sqrt.age <- out$casl.het$edges * netparams$casl.het$absdiff.sqrt.age
  out$casl.het$absdiff_sqrt.age <- absdiff_sqrt.age

  ## nodefactor("age15")
  nodefactor_age15 <- table(out$attr$age15) * netparams$casl.het$nf.age15
  out$casl.het$nodefactor_age15 <- unname(nodefactor_age15)


  ## nodefactor("deg.main")
  out$casl.het$nodefactor_deg.main <- num * netparams$casl.het$deg.main.dist * netparams$casl.het$nf.deg.main.het

  ## nodefactor("deg.main.c")
  nodefactor_deg.main.c <- table(out$attr$deg.main.c.het) * netparams$casl.het$nf.deg.main.c.het
  out$casl.het$nodefactor_deg.main.c <- unname(nodefactor_deg.main.c)

  ## concurrent
  out$casl.het$concurrent <- num * netparams$casl.het$concurrent

  ## nodefactor("diag.status")
  #  nodefactor_diag.status <- table(out$attr$diag.status) * netparams$casl$nf.diag.status
  #  out$casl$nodefactor_diag.status <- unname(nodefactor_diag.status)

  ## Dissolution
  out$casl.het$diss.homog <- dissolution_coefs(dissolution = ~offset(edges),
                                               duration = netparams$casl.het$durs.casl.homog$mean.dur.adj,
                                               d.rate = expect.mort)
  out$casl.het$diss.byage <- dissolution_coefs(dissolution = ~offset(edges) + offset(nodematch("age.grp", diff = TRUE)),
                                               duration = netparams$casl.het$durs.casl.byage$mean.dur.adj,
                                               d.rate = expect.mort)



  # One-Time Model ----------------------------------------------------------

  out$inst.het <- list()

  ## edges
  out$inst.het$edges <- (netparams$inst.het$md.inst * num) / 2

  ## nodefactor("race")
  nodefactor_race <- table(out$attr$race) * netparams$inst.het$nf.race
  out$inst.het$nodefactor_race <- unname(nodefactor_race)

  ## nodematch("race")
  nodematch_race <- nodefactor_race/2 * netparams$inst.het$nm.race
  out$inst.het$nodematch_race <- unname(nodematch_race)


  ## nodefactor("age.grp")
  nodefactor_age.grp <- table(out$attr$age.grp) * netparams$inst.het$nf.age.grp
  out$inst.het$nodefactor_age.grp <- unname(nodefactor_age.grp)

  ## nodematch("age.grp")
  nodematch_age.grp <- nodefactor_age.grp/2 * netparams$inst.het$nm.age.grp
  out$inst.het$nodematch_age.grp <- unname(nodematch_age.grp)

  ## absdiff("age")
  absdiff_age <- out$inst.het$edges * netparams$inst.het$absdiff.age
  out$inst.het$absdiff_age <- absdiff_age

  ## absdiff("sqrt.age")
  absdiff_sqrt.age <- out$inst.het$edges * netparams$inst.het$absdiff.sqrt.age
  out$inst.het$absdiff_sqrt.age <- absdiff_sqrt.age

  ## nodefactor("risk.grp")
  nodefactor_risk.grp <- table(out$attr$risk.grp) * netparams$inst.het$nf.risk.grp
  out$inst.het$nodefactor_risk.grp <- unname(nodefactor_risk.grp)

  ## nodefactor("deg.tot.c.het")
  nodefactor_deg.tot.c.het <- table(out$attr$deg.tot.c.het) * netparams$inst.het$nf.deg.tot.c.het
  out$inst.het$nodefactor_deg.tot.c.het <- unname(nodefactor_deg.tot.c.het)

  ## nodefactor("diag.status")
  #  nodefactor_diag.status <- table(out$attr$diag.status) * netparams$inst$nf.diag.status
  #  out$inst$nodefactor_diag.status <- unname(nodefactor_diag.status)



  return(out)
}
