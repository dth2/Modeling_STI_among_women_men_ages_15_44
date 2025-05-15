
#' @title STI Transmission Module
#'
#' @description Stochastically simulates GC/CT transmission given the current
#'              state of the edgelist.
#'
#' @inheritParams aging_msm
#'
#' @keywords module msm
#'
#' @export
#'
stitrans_msm <- function(dat, at) {

  ## Inputs
  # Attributes
  race <- get_attr(dat, "race")
  sex <- get_attr(dat, "sex")
  age <- get_attr(dat, "age")
  dem.cat <- get_attr(dat, "dem.cat")
  deg.main <- get_attr(dat, "deg.main")
  deg.casl <- get_attr(dat, "deg.casl")

  # Current infection state
  rGC <- get_attr(dat, "rGC")
  uGC <- get_attr(dat, "uGC")
  vGC <- get_attr(dat, "vGC")
  rCT <- get_attr(dat, "rCT")
  uCT <- get_attr(dat, "uCT")
  vCT <- get_attr(dat, "vCT")

  # n Times infected
  rGC.timesInf <- get_attr(dat, "rGC.timesInf")
  uGC.timesInf <- get_attr(dat, "uGC.timesInf")
  vGC.timesInf <- get_attr(dat, "vGC.timesInf")
  rCT.timesInf <- get_attr(dat, "rCT.timesInf")
  uCT.timesInf <- get_attr(dat, "uCT.timesInf")
  vCT.timesInf <- get_attr(dat, "vCT.timesInf")

  # Infection time
  rGC.infTime <- get_attr(dat, "rGC.infTime")
  uGC.infTime <- get_attr(dat, "uGC.infTime")
  vGC.infTime <- get_attr(dat, "vGC.infTime")
  rCT.infTime <- get_attr(dat, "rCT.infTime")
  uCT.infTime <- get_attr(dat, "uCT.infTime")
  vCT.infTime <- get_attr(dat, "vCT.infTime")

  # Infection symptoms (non-varying)
  rGC.sympt <- get_attr(dat, "rGC.sympt")
  uGC.sympt <- get_attr(dat, "uGC.sympt")
  vGC.sympt <- get_attr(dat, "vGC.sympt")
  rCT.sympt <- get_attr(dat, "rCT.sympt")
  uCT.sympt <- get_attr(dat, "uCT.sympt")
  vCT.sympt <- get_attr(dat, "vCT.sympt")

  # Parameters
  # Acquisition probabilities given contact with infected person
  rgc.prob <- get_param(dat, "rgc.prob")
  ugc.prob <- get_param(dat, "ugc.prob")
  vgc.prob <- get_param(dat, "vgc.prob")
  rct.prob <- get_param(dat, "rct.prob")
  uct.prob <- get_param(dat, "uct.prob")
  vct.prob <- get_param(dat, "vct.prob")

  # Probability of symptoms given infection
  rgc.sympt.prob <- get_param(dat, "rgc.sympt.prob")
  ugc.sympt.prob <- get_param(dat, "ugc.sympt.prob")
  vgc.sympt.prob <- get_param(dat, "vgc.sympt.prob")
  rct.sympt.prob <- get_param(dat, "rct.sympt.prob")
  uct.sympt.prob <- get_param(dat, "uct.sympt.prob")
  vct.sympt.prob <- get_param(dat, "vct.sympt.prob")

  # Relative risk of infection given condom use during act
  sti.cond.eff.rr    <- get_param(dat, "sti.cond.eff.rr")
  sti.cond.fail.rr   <- get_param(dat, "sti.cond.fail.rr")

  # placeholders

  idsInf_rgc <- idsInf_ugc <- idsInf_vgc <- idsInf_rct <- idsInf_uct <- idsInf_vct <- NULL

  # Pull act list
  al <- dat[["temp"]][["al"]]

  ## ins variable coding
  # ins = 0 : p2 is insertive
  # ins = 1 : p1 is insertive
  # ins = 2 : both p1 and p2 are insertive


  # # Rectal GC -----------------------------------------------------------
  #
  # # Requires: uGC in insertive man, and no rGC in receptive man
  # p1Inf_rgc <- which(
  #   uGC[al[, "p1"]] == 1 &
  #   uGC.infTime[al[, "p1"]] < at &
  #   rGC[al[, "p2"]] == 0 &
  #   al[, "ins"] %in% c(1, 2)
  # )
  # p2Inf_rgc <- which(
  #   uGC[al[, "p2"]] == 1 &
  #   uGC.infTime[al[, "p2"]] < at &
  #   rGC[al[, "p1"]] == 0 &
  #   al[, "ins"] %in% c(0, 2)
  # )
  # allActs_rgc <- c(p1Inf_rgc, p2Inf_rgc)
  #
  # # UAI modifier
  # uai_rgc <- al[allActs_rgc, "uai"]
  # tprob_rgc <- rep(rgc.prob, length(allActs_rgc))
  #
  # # Transform to log odds
  # tlo_rgc <- log(tprob_rgc / (1 - tprob_rgc))
  #
  # # Modify log odds by race-specific condom effectiveness
  # races <- c(race[al[p1Inf_rgc, "p1"]], race[al[p2Inf_rgc, "p2"]])
  # condom.rr <- rep(NA, length(races))
  # for (i in sort(unique(races))) {
  #   ids.race <- which(races == i)
  #   condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  # }
  #
  # tlo_rgc[uai_rgc == 0] <- tlo_rgc[uai_rgc == 0] + log(condom.rr[uai_rgc == 0])
  #
  # # Back-transform to probability
  # tprob_rgc <- plogis(tlo_rgc)
  #
  # # Stochastic transmission
  # trans_rgc <- runif(length(allActs_rgc)) < tprob_rgc
  #
  # # Determine the infected partner
  # idsInf_rgc <- numeric()
  # if (sum(trans_rgc) > 0) {
  #   transAL_rgc <- al[allActs_rgc[trans_rgc == 1], , drop = FALSE]
  #   idsInf_rgc <- c(
  #     intersect(al[p1Inf_rgc, "p2"], transAL_rgc[, "p2"]),
  #     intersect(al[p2Inf_rgc, "p1"], transAL_rgc[, "p1"])
  #   )
  #   stopifnot(all(rGC[idsInf_rgc] == 0))
  # }
  #
  # # Update attributes
  # rGC[idsInf_rgc] <- 1
  # rGC.infTime[idsInf_rgc] <- at
  # rGC.sympt[idsInf_rgc] <- runif(length(idsInf_rgc)) < rgc.sympt.prob
  # rGC.timesInf[idsInf_rgc] <- rGC.timesInf[idsInf_rgc] + 1
  #

  # Vaginal GC -----------------------------------------------------------

  # Requires: uGC in insertive man, and no vGC in receptive female
  p1Inf_vgc <- which(
    uGC[al[, "p2"]] == 1 &
      uGC.infTime[al[, "p2"]] < at &
      vGC[al[, "p1"]] == 0
      & al[, "ins"] %in% c(0, 2)
  )
#  p2Inf_vgc <- which(
#    uGC[al[, "p2"]] == 1 &
#      uGC.infTime[al[, "p2"]] < at &
#      vGC[al[, "p1"]] == 0
#      & al[, "ins"] %in% c(0, 2)
#  )
  allActs_vgc <- c(p1Inf_vgc)

  # UAI modifier
  uai_vgc <- al[allActs_vgc, "uai"]


  # Subset by demographic category and apply race specific probability of acquisition
  races <- c(race[al[p1Inf_vgc, "p1"]])
  tprob_vgc <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    tprob_vgc[ids.race] <- vgc.prob[i]
  }


 # tprob_vgc <- rep(vgc.prob, length(allActs_vgc))

  # Transform to log odds
  tlo_vgc <- log(tprob_vgc / (1 - tprob_vgc))

  # Modify log odds by race-specific condom effectiveness
  races <- c(race[al[p1Inf_vgc, "p1"]])
  condom.rr <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  }

  tlo_vgc[uai_vgc == 0] <- tlo_vgc[uai_vgc == 0] + log(condom.rr[uai_vgc == 0])

  # Back-transform to probability
  tprob_vgc <- plogis(tlo_vgc)

  # Stochastic transmission
  trans_vgc <- runif(length(allActs_vgc)) < tprob_vgc

  # Determine the infected partner
  idsInf_vgc <- numeric()
  if (sum(trans_vgc) > 0) {
    transAL_vgc <- al[allActs_vgc[trans_vgc == 1], , drop = FALSE]
    idsInf_vgc <- c(transAL_vgc[, "p1"])

    stopifnot(all(vGC[idsInf_vgc] == 0))
  }

  # Update attributes
  vGC[idsInf_vgc] <- 1
  vGC.infTime[idsInf_vgc] <- at
  vGC.sympt[idsInf_vgc] <- runif(length(idsInf_vgc)) < vgc.sympt.prob
  vGC.timesInf[idsInf_vgc] <- vGC.timesInf[idsInf_vgc] + 1

  # # Urethral GC to be coded later for AI ---------------------------------------------------------
  #
  # # Requires: rGC in receptive man, and no uGC in insertive man
  # p1Inf_ugc <- which(
  #   rGC[al[, "p1"]] == 1 &
  #   rGC.infTime[al[, "p1"]] < at &
  #   uGC[al[, "p2"]] == 0 &
  #   al[, "ins"] %in% c(0, 2)
  # )
  # p2Inf_ugc <- which(
  #   rGC[al[, "p2"]] == 1 &
  #   rGC.infTime[al[, "p2"]] < at &
  #   uGC[al[, "p1"]] == 0 &
  #   al[, "ins"] %in% c(1, 2)
  # )
  # allActs_ugc <- c(p1Inf_ugc, p2Inf_ugc)
  #
  # # UAI modifier
  # uai_ugc <- al[allActs_ugc, "uai"]
  # tprob_ugc <- rep(ugc.prob, length(allActs_ugc))
  #
  # # Transform to log odds
  # tlo_ugc <- log(tprob_ugc / (1 - tprob_ugc))
  #
  # # Modify log odds by race-specific condom effectiveness
  # races <- c(race[al[p1Inf_ugc, "p2"]], race[al[p2Inf_ugc, "p1"]])
  # condom.rr <- rep(NA, length(races))
  # for (i in sort(unique(races))) {
  #   ids.race <- which(races == i)
  #   condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  # }
  #
  # tlo_ugc[uai_ugc == 0] <- tlo_ugc[uai_ugc == 0] + log(condom.rr[uai_ugc == 0])
  #
  # # Back-transform to probability
  # tprob_ugc <- plogis(tlo_ugc)
  #
  # # Stochastic transmission
  # trans_ugc <- runif(length(allActs_ugc)) < tprob_ugc
  #
  # # Determine the newly infected partner
  # idsInf_ugc <- numeric()
  # if (sum(trans_ugc) > 0) {
  #   transAL_ugc <- al[allActs_ugc[trans_ugc == 1],  , drop = FALSE]
  #   idsInf_ugc <- c(
  #     intersect(al[p1Inf_ugc, "p2"], transAL_ugc[, "p2"]),
  #     intersect(al[p2Inf_ugc, "p1"], transAL_ugc[, "p1"])
  #   )
  #   stopifnot(all(uGC[idsInf_ugc] == 0))
  # }
  #
  # # Update attributes
  # uGC[idsInf_ugc] <- 1
  # uGC.infTime[idsInf_ugc] <- at
  # uGC.sympt[idsInf_ugc] <- runif(length(idsInf_ugc)) < ugc.sympt.prob
  # uGC.timesInf[idsInf_ugc] <- uGC.timesInf[idsInf_ugc] + 1
  #


   # Urethral GC from female partner (for now all sex is Vaginal but the vector is ai )---------------------------------------------------------

  # Requires: vGC in receptive woman, and no uGC in insertive man
  p2Inf_ugc <- which(
    vGC[al[, "p1"]] == 1 &
      vGC.infTime[al[, "p1"]] < at &
      uGC[al[, "p2"]] == 0
      # & al[, "ins"] %in% c(0, 2)
  )

  allActs_ugc <- c(p2Inf_ugc)

  # UAI modifier
  uai_ugc <- al[allActs_ugc, "uai"]

  # Subset by demographic category and apply race specific probability of acquisition
  races <- c(race[al[p2Inf_ugc, "p2"]])
  tprob_ugc <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    tprob_ugc[ids.race] <- ugc.prob[i]
  }


  #tprob_ugc <- rep(ugc.prob, length(allActs_ugc))

  # Transform to log odds
  tlo_ugc <- log(tprob_ugc / (1 - tprob_ugc))

  # Modify log odds by race-specific condom effectiveness
  races <- c(race[al[p2Inf_ugc, "p2"]])
  condom.rr <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  }

  tlo_ugc[uai_ugc == 0] <- tlo_ugc[uai_ugc == 0] + log(condom.rr[uai_ugc == 0])

  # Back-transform to probability
  tprob_ugc <- plogis(tlo_ugc)

  # Stochastic transmission
  trans_ugc <- runif(length(allActs_ugc)) < tprob_ugc

  # Determine the newly infected partner
  idsInf_ugc <- numeric()
  if (sum(trans_ugc) > 0) {
    transAL_ugc <- al[allActs_ugc[trans_ugc == 1],  , drop = FALSE]
    idsInf_ugc <- c(transAL_ugc[, "p2"])

    stopifnot(all(uGC[idsInf_ugc] == 0))
  }

  # Update attributes
  uGC[idsInf_ugc] <- 1
  uGC.infTime[idsInf_ugc] <- at
  uGC.sympt[idsInf_ugc] <- runif(length(idsInf_ugc)) < ugc.sympt.prob
  uGC.timesInf[idsInf_ugc] <- uGC.timesInf[idsInf_ugc] + 1


  # # Rectal CT to be updated later when vi and ai are split-----------------------------------------------------------
  #
  # # Requires: uCT in insertive man, and no rCT in receptive man
  # p1Inf_rct <- which(
  #   uCT[al[, "p1"]] == 1 &
  #     uCT.infTime[al[, "p1"]] < at &
  #     rCT[al[, "p2"]] == 0 &
  #     al[, "ins"] %in% c(1, 2)
  # )
  # p2Inf_rct <- which(
  #   uCT[al[, "p2"]] == 1 &
  #   uCT.infTime[al[, "p2"]] < at &
  #   rCT[al[, "p1"]] == 0 &
  #   al[, "ins"] %in% c(0, 2)
  # )
  # allActs_rct <- c(p1Inf_rct, p2Inf_rct)
  #
  # # UAI modifier
  # uai_rct <- al[allActs_rct, "uai"]
  # tprob_rct <- rep(rct.prob, length(allActs_rct))
  #
  # # Transform to log odds
  # tlo_rct <- log(tprob_rct / (1 - tprob_rct))
  #
  # # Modify log odds by race-specific condom effectiveness
  # races <- c(race[al[p1Inf_rct, "p1"]], race[al[p2Inf_rct, "p2"]])
  # condom.rr <- rep(NA, length(races))
  # for (i in sort(unique(races))) {
  #   ids.race <- which(races == i)
  #   condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  # }
  #
  # tlo_rct[uai_rct == 0] <- tlo_rct[uai_rct == 0] + log(condom.rr[uai_rct == 0])
  #
  # # Back-transform to probability
  # tprob_rct <- plogis(tlo_rct)
  #
  # # Stochastic transmission
  # trans_rct <- runif(length(allActs_rct)) < tprob_rct
  #
  # # Determine the newly infected partner
  # idsInf_rct <- numeric()
  # if (sum(trans_rct) > 0) {
  #   transAL_rct <- al[allActs_rct[trans_rct == 1],  , drop = FALSE]
  #   idsInf_rct <- c(
  #     intersect(al[p1Inf_rct, "p2"], transAL_rct[, "p2"]),
  #     intersect(al[p2Inf_rct, "p1"], transAL_rct[, "p1"])
  #   )
  #   stopifnot(all(rCT[idsInf_rct] == 0))
  # }
  #
  # # Update attributes
  # rCT[idsInf_rct] <- 1
  # rCT.infTime[idsInf_rct] <- at
  # rCT.sympt[idsInf_rct] <- runif(length(idsInf_rct)) < rct.sympt.prob
  # rCT.timesInf[idsInf_rct] <- rCT.timesInf[idsInf_rct] + 1
  #


  # Vaginal CT currently all sex is vaginal but is on the ai vector-----------------------------------------------------------

  # Requires: uCT in insertive man, and no vCT in receptive woman

  p1Inf_vct <- which(
    uCT[al[, "p2"]] == 1 &
      uCT.infTime[al[, "p2"]] < at &
      vCT[al[, "p1"]] == 0
      # & al[, "ins"] %in% c(0, 2)
  )

    allActs_vct <- c(p1Inf_vct)

  # UAI modifier
  uai_vct <- al[allActs_vct, "uai"]

  # Subset by demographic category and apply race specific probability of acquisition
  races <- c(race[al[p1Inf_vct, "p1"]])
  tprob_vct <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    tprob_vct[ids.race] <- vct.prob[i]
  }


  #tprob_vct <- rep(vct.prob, length(allActs_vct))

  # Transform to log odds
  tlo_vct <- log(tprob_vct / (1 - tprob_vct))

  # Modify log odds by race-specific condom effectiveness
  races <- c(race[al[p1Inf_vct, "p1"]])
  condom.rr <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  }

  tlo_vct[uai_vct == 0] <- tlo_vct[uai_vct == 0] + log(condom.rr[uai_vct == 0])

  # Back-transform to probability
  tprob_vct <- plogis(tlo_vct)

  # Stochastic transmission
  trans_vct <- runif(length(allActs_vct)) < tprob_vct

  # Determine the newly infected partner
  idsInf_vct <- numeric()
  if (sum(trans_vct) > 0) {
    transAL_vct <- al[allActs_vct[trans_vct == 1],  , drop = FALSE]
    idsInf_vct <- transAL_vct[, "p1"]

    stopifnot(all(vCT[idsInf_vct] == 0))
  }




  # Update attributes
  vCT[idsInf_vct] <- 1
  vCT.infTime[idsInf_vct] <- at
  vCT.sympt[idsInf_vct] <- runif(length(idsInf_vct)) < vct.sympt.prob
  vCT.timesInf[idsInf_vct] <- vCT.timesInf[idsInf_vct] + 1


  # # Urethral CT from AI needs updating for male and female---------------------------------------------------------
  #
  # # Requires: rCT in receptive man, and no uCT in insertive man
  # p1Inf_uct <- which(
  #   rCT[al[, "p1"]] == 1 &
  #   rCT.infTime[al[, "p1"]] < at &
  #   uCT[al[, "p2"]] == 0 &
  #   al[, "ins"] %in% c(0, 2)
  # )
  # p2Inf_uct <- which(
  #   rCT[al[, "p2"]] == 1 &
  #   rCT.infTime[al[, "p2"]] < at &
  #   uCT[al[, "p1"]] == 0 &
  #   al[, "ins"] %in% c(1, 2)
  # )
  # allActs_uct <- c(p1Inf_uct, p2Inf_uct)
  #
  # # UAI modifier
  # uai_uct <- al[allActs_uct, "uai"]
  # tprob_uct <- rep(uct.prob, length(allActs_uct))
  #
  # # Transform to log odds
  # tlo_uct <- log(tprob_uct / (1 - tprob_uct))
  #
  # # Modify log odds by race-specific condom effectiveness
  # races <- c(race[al[p1Inf_uct, "p2"]], race[al[p2Inf_uct, "p1"]])
  # condom.rr <- rep(NA, length(races))
  # for (i in sort(unique(races))) {
  #   ids.race <- which(races == i)
  #   condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  # }
  #
  # tlo_uct[uai_uct == 0] <- tlo_uct[uai_uct == 0] + log(condom.rr[uai_uct == 0])
  #
  # # Back-transform to probability
  # tprob_uct <- plogis(tlo_uct)
  #
  # # Stochastic transmission
  # trans_uct <- runif(length(allActs_uct)) < tprob_uct
  #
  # # Determine the newly infected partner
  # idsInf_uct <- numeric()
  # if (sum(trans_uct) > 0) {
  #   transAL_uct <- al[allActs_uct[trans_uct == 1],  , drop = FALSE]
  #   idsInf_uct <- c(
  #     intersect(al[p1Inf_uct, "p2"], transAL_uct[, "p2"]),
  #     intersect(al[p2Inf_uct, "p1"], transAL_uct[, "p1"])
  #   )
  #   stopifnot(all(uCT[idsInf_uct] == 0))
  # }
  #
  # # Update attributes
  # uCT[idsInf_uct] <- 1
  # uCT.infTime[idsInf_uct] <- at
  # uCT.sympt[idsInf_uct] <- runif(length(idsInf_uct)) < uct.sympt.prob
  # uCT.timesInf[idsInf_uct] <- uCT.timesInf[idsInf_uct] + 1
  #

  # Urethral CT from VI needs updating (currently all sex is vaginal and is carried on ai vector)------------------------------------------

  # Requires: vCT in receptive woman, and no uCT in insertive man
  p2Inf_uct <- which(
    vCT[al[, "p1"]] == 1 &
      vCT.infTime[al[, "p1"]] < at &
      uCT[al[, "p2"]] == 0
      # & al[, "ins"] %in% c(0, 2)
  )

  allActs_uct <- c(p2Inf_uct)

  # UAI modifier
  uai_uct <- al[allActs_uct, "uai"]

  # Subset by demographic category and apply race specific probability of acquisition
  races <- c(race[al[p2Inf_uct, "p2"]])
  tprob_uct <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    tprob_uct[ids.race] <- uct.prob[i]
  }


  #tprob_uct <- rep(uct.prob, length(allActs_uct))

  # Transform to log odds
  tlo_uct <- log(tprob_uct / (1 - tprob_uct))

  # Modify log odds by race-specific condom effectiveness
  races <- c(race[al[p2Inf_uct, "p2"]])
  condom.rr <- rep(NA, length(races))
  for (i in sort(unique(races))) {
    ids.race <- which(races == i)
    condom.rr[ids.race] <- 1 - (sti.cond.eff.rr - sti.cond.fail.rr[i])
  }

  tlo_uct[uai_uct == 0] <- tlo_uct[uai_uct == 0] + log(condom.rr[uai_uct == 0])

  # Back-transform to probability
  tprob_uct <- plogis(tlo_uct)

  # Stochastic transmission
  trans_uct <- runif(length(allActs_uct)) < tprob_uct

  # Determine the newly infected partner
  idsInf_uct <- numeric()
  if (sum(trans_uct) > 0) {
    transAL_uct <- al[allActs_uct[trans_uct == 1],  , drop = FALSE]
    idsInf_uct <- transAL_uct[, "p2"]

    stopifnot(all(uCT[idsInf_uct] == 0))
  }

  # Update attributes
  uCT[idsInf_uct] <- 1
  uCT.infTime[idsInf_uct] <- at
  uCT.sympt[idsInf_uct] <- runif(length(idsInf_uct)) < uct.sympt.prob
  uCT.timesInf[idsInf_uct] <- uCT.timesInf[idsInf_uct] + 1


  # Output --------------------------------------------------------------

  # attributes
  dat <- set_attr(dat, "rGC", rGC)
  dat <- set_attr(dat, "uGC", uGC)
  dat <- set_attr(dat, "vGC", vGC)
  dat <- set_attr(dat, "rCT", rCT)
  dat <- set_attr(dat, "uCT", uCT)
  dat <- set_attr(dat, "vCT", vCT)

  dat <- set_attr(dat, "rGC.infTime", rGC.infTime)
  dat <- set_attr(dat, "uGC.infTime", uGC.infTime)
  dat <- set_attr(dat, "vGC.infTime", vGC.infTime)
  dat <- set_attr(dat, "rCT.infTime", rCT.infTime)
  dat <- set_attr(dat, "uCT.infTime", uCT.infTime)
  dat <- set_attr(dat, "vCT.infTime", vCT.infTime)

  dat <- set_attr(dat, "rGC.timesInf", rGC.timesInf)
  dat <- set_attr(dat, "uGC.timesInf", uGC.timesInf)
  dat <- set_attr(dat, "vGC.timesInf", vGC.timesInf)
  dat <- set_attr(dat, "rCT.timesInf", rCT.timesInf)
  dat <- set_attr(dat, "uCT.timesInf", uCT.timesInf)
  dat <- set_attr(dat, "vCT.timesInf", vCT.timesInf)

  dat <- set_attr(dat, "rGC.sympt", rGC.sympt)
  dat <- set_attr(dat, "uGC.sympt", uGC.sympt)
  dat <- set_attr(dat, "vGC.sympt", vGC.sympt)
  dat <- set_attr(dat, "rCT.sympt", rCT.sympt)
  dat <- set_attr(dat, "uCT.sympt", uCT.sympt)
  dat <- set_attr(dat, "vCT.sympt", vCT.sympt)


  # Summary stats
  dat <- set_epi(dat, "incid.gc", at, length(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc))))
  dat <- set_epi(dat, "incid.rgc", at, length(idsInf_rgc))
  dat <- set_epi(dat, "incid.ugc", at, length(idsInf_ugc))
  dat <- set_epi(dat, "incid.vgc", at, length(idsInf_vgc))

  dat <- set_epi(dat, "incid.gc.M", at,
                 length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(sex == 1))))
  dat <- set_epi(dat, "incid.gc.F", at,
                 length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(sex == 2))))

   dat <- set_epi(dat, "incid.gc.B", at,
     length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(race == 1))))
   dat <- set_epi(dat, "incid.gc.H", at,
     length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(race == 2))))
   dat <- set_epi(dat, "incid.gc.W", at,
     length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(race == 3))))

   dat <- set_epi(dat, "incid.gc.BM", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 1))))
   dat <- set_epi(dat, "incid.gc.HM", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 2))))
   dat <- set_epi(dat, "incid.gc.WM", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 3))))
   dat <- set_epi(dat, "incid.gc.BF", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 4))))
   dat <- set_epi(dat, "incid.gc.HF", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 5))))
   dat <- set_epi(dat, "incid.gc.WF", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(dem.cat == 6))))

   dat <- set_epi(dat, "incid.gc.adol", at,
                  length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(age < 18))))

  dat <- set_epi(dat, "incid.ct", at, length(union(idsInf_rct, union(idsInf_uct, idsInf_vct))))
  dat <- set_epi(dat, "incid.rct", at, length(idsInf_rct))
  dat <- set_epi(dat, "incid.uct", at, length(idsInf_uct))
  dat <- set_epi(dat, "incid.vct", at, length(idsInf_vct))

  dat <- set_epi(dat, "incid.ct.M", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(sex == 1))))
  dat <- set_epi(dat, "incid.ct.F", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(sex == 2))))

  dat <- set_epi(dat, "incid.ct.B", at,
   length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(race == 1))))
  dat <- set_epi(dat, "incid.ct.H", at,
   length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(race == 2))))
  dat <- set_epi(dat, "incid.ct.W", at,
   length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(race == 3))))


  dat <- set_epi(dat, "incid.ct.BM", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 1))))
  dat <- set_epi(dat, "incid.ct.HM", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 2))))
  dat <- set_epi(dat, "incid.ct.WM", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 3))))
  dat <- set_epi(dat, "incid.ct.BF", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 4))))
  dat <- set_epi(dat, "incid.ct.HF", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 5))))
  dat <- set_epi(dat, "incid.ct.WF", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(dem.cat == 6))))

  dat <- set_epi(dat, "incid.ct.adol", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(age < 18))))


  ##Count the number of transmission are people in main and casual parterships


  dat <- set_epi(dat, "incid.ct.main", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(deg.main >= 1))))

  dat <- set_epi(dat, "incid.ct.casl", at,
                 length(intersect(union(idsInf_rct, union(idsInf_uct, idsInf_vct)), which(deg.casl >= 1))))


  dat <- set_epi(dat, "incid.gc.main", at,
                 length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(deg.main >= 1))))

  dat <- set_epi(dat, "incid.gc.casl", at,
                 length(intersect(union(idsInf_rgc, union(idsInf_ugc, idsInf_vgc)), which(deg.casl >= 1))))


    # Check all infected have all STI attributes
  stopifnot(
    all(!is.na(rGC.infTime[rGC == 1])),
    all(!is.na(rGC.sympt[rGC == 1])),
    all(!is.na(uGC.infTime[uGC == 1])),
    all(!is.na(uGC.sympt[uGC == 1])),
    all(!is.na(vGC.infTime[vGC == 1])),
    all(!is.na(vGC.sympt[vGC == 1])),
    all(!is.na(rCT.infTime[rCT == 1])),
    all(!is.na(rCT.sympt[rCT == 1])),
    all(!is.na(uCT.infTime[uCT == 1])),
    all(!is.na(uCT.sympt[uCT == 1])),
    all(!is.na(vCT.infTime[vCT == 1])),
    all(!is.na(vCT.sympt[vCT == 1]))
  )

  return(dat)
}
