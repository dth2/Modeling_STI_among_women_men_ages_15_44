
#' @title STI Treatment Module
#'
#' @description Stochastically simulates GC/CT diagnosis and treatment.
#'
#' @inheritParams aging_msm
#'
#' @keywords module msm
#'
#' @export
#'
stitx_msm <- function(dat, at) {

  ## Input
  # Attributes
  sex <- get_attr(dat, "sex")
  age.grp <- get_attr(dat, "age.grp")
  race <- get_attr(dat, "race")
  deg.main.c.het <- get_attr(dat, "deg.main.c.het")
  deg.casl.c.het <- get_attr(dat, "deg.casl.c.het")
  dem.cat <- get_attr(dat, "dem.cat")
  prepStat <- get_attr(dat, "prepStat")
  prepStartTime <- get_attr(dat, "prepStartTime")
  prepLastStiScreen <- get_attr(dat, "prepLastStiScreen")

  rGC <- get_attr(dat, "rGC")
  rGC.infTime <- get_attr(dat, "rGC.infTime")
  rGC.sympt <- get_attr(dat, "rGC.sympt")
  rGC.tx <- get_attr(dat, "rGC.tx")
  rGC.tx.prep <- get_attr(dat, "rGC.tx.prep")

  uGC <- get_attr(dat, "uGC")
  uGC.infTime <- get_attr(dat, "uGC.infTime")
  uGC.sympt <- get_attr(dat, "uGC.sympt")
  uGC.tx <- get_attr(dat, "uGC.tx")
  uGC.tx.prep <- get_attr(dat, "uGC.tx.prep")

  vGC <- get_attr(dat, "vGC")
  vGC.infTime <- get_attr(dat, "vGC.infTime")
  vGC.sympt <- get_attr(dat, "vGC.sympt")
  vGC.tx <- get_attr(dat, "vGC.tx")
  vGC.tx.prep <- get_attr(dat, "vGC.tx.prep")

  rCT <- get_attr(dat, "rCT")
  rCT.infTime <- get_attr(dat, "rCT.infTime")
  rCT.sympt <- get_attr(dat, "rCT.sympt")
  rCT.tx <- get_attr(dat, "rCT.tx")
  rCT.tx.prep <- get_attr(dat, "rCT.tx.prep")

  uCT <- get_attr(dat, "uCT")
  uCT.infTime <- get_attr(dat, "uCT.infTime")
  uCT.sympt <- get_attr(dat, "uCT.sympt")
  uCT.tx <- get_attr(dat, "uCT.tx")
  uCT.tx.prep <- get_attr(dat, "uCT.tx.prep")

  vCT <- get_attr(dat, "vCT")
  vCT.infTime <- get_attr(dat, "vCT.infTime")
  vCT.sympt <- get_attr(dat, "vCT.sympt")
  vCT.tx <- get_attr(dat, "vCT.tx")
  vCT.tx.prep <- get_attr(dat, "vCT.tx.prep")

  # Parameters
  gc.sympt.tx.prob  <- get_param(dat, "gc.sympt.tx.prob")
  ct.sympt.tx.prob  <- get_param(dat, "ct.sympt.tx.prob")
  gc.asympt.tx.prob <- get_param(dat, "gc.asympt.tx.prob")
  ct.asympt.tx.prob <- get_param(dat, "ct.asympt.tx.prob")

  prep.sti.screen.int <- get_param(dat, "prep.sti.screen.int")
  prep.sti.tx.prob    <- get_param(dat, "prep.sti.tx.prob")
  netstats   <- get_param(dat, "netstats")
  epistats   <- get_param(dat, "epistats")

  # STI Testing Models
  stdtst.mod <- epistats[["stdtst.mod.het"]]

  #select data with demographics in screening model
  x <- data.frame(
    sex = sex,
    age.grp = age.grp,
    race = race,
    deg.main.c.het = deg.main.c.het,
    deg.casl.c.het = deg.casl.c.het
  )

  ## Predictions is based on test in the last year
  sti.tst.prob <- unname(predict(stdtst.mod, newdata = x, type = "response"))

  ## Convert to a weekly probability

  sti.tst.prob <- (-(1-sti.tst.prob)^(1/52)) +1

  ## STI testing draw
  sti.tst <- rbinom(length(sti.tst.prob),1, prob = sti.tst.prob)

  RGC_tx <- which(sti.tst == 1 & rGC == 1 & rGC.infTime < at & is.na(rGC.tx))
  UGC_tx <- which(sti.tst == 1 & uGC == 1 & uGC.infTime < at & is.na(uGC.tx))
  VGC_tx <- which(sti.tst == 1 & vGC == 1 & vGC.infTime < at & is.na(vGC.tx))

  RCT_tx <- which(sti.tst == 1 & rCT == 1 & rCT.infTime < at & is.na(rCT.tx))
  UCT_tx <- which(sti.tst == 1 & uCT == 1 & uCT.infTime < at & is.na(uCT.tx))
  VCT_tx <- which(sti.tst == 1 & vCT == 1 & vCT.infTime < at & is.na(vCT.tx))

  # ## Symptomatic GC Treatment ##
  # idsRGC_tx_sympt <- which(
  #   rGC == 1 &
  #   rGC.infTime < at &
  #   rGC.sympt == 1 &
  #   is.na(rGC.tx)
  # )
  # idsUGC_tx_sympt <- which(
  #   uGC == 1 &
  #   uGC.infTime < at &
  #   uGC.sympt == 1 &
  #   is.na(uGC.tx)
  # )
  # idsVGC_tx_sympt <- which(
  #   vGC == 1 &
  #     vGC.infTime < at &
  #     vGC.sympt == 1 &
  #     is.na(vGC.tx)
  # )
  # # Subset by demographic category
  # idsGC_tx_sympt <- union(idsRGC_tx_sympt, union(idsUGC_tx_sympt,idsVGC_tx_sympt))
  # dem.cats <- sort(unique(dem.cat[idsGC_tx_sympt]))
  # txGC_sympt <- rep(NA, length(idsGC_tx_sympt))
  # for (i in dem.cats) {
  #   ids.dem.cat <- which(dem.cat[idsGC_tx_sympt] == i)
  #   txGC_sympt[ids.dem.cat] <- runif(length(ids.dem.cat)) < gc.sympt.tx.prob[i]
  # }
  # ids_txGC_sympt <- idsGC_tx_sympt[which(txGC_sympt == 1)]
  #
  # # Subset by site
  # txRGC_sympt <- intersect(idsRGC_tx_sympt, ids_txGC_sympt)
  # txUGC_sympt <- intersect(idsUGC_tx_sympt, ids_txGC_sympt)
  # txVGC_sympt <- intersect(idsVGC_tx_sympt, ids_txGC_sympt)
  #
  # ## Asymptomatic GC Treatment ##
  # idsRGC_tx_asympt <- which(
  #   rGC == 1 &
  #   rGC.infTime < at &
  #   rGC.sympt == 0 &
  #   is.na(rGC.tx) &
  #   prepStat == 0
  # )
  # idsUGC_tx_asympt <- which(
  #   uGC == 1 &
  #   uGC.infTime < at &
  #   uGC.sympt == 0 &
  #   is.na(uGC.tx) &
  #   prepStat == 0
  # )
  # idsVGC_tx_asympt <- which(
  #   vGC == 1 &
  #     vGC.infTime < at &
  #     vGC.sympt == 0 &
  #     is.na(vGC.tx) &
  #     prepStat == 0
  # )
  #
  # # Subset by demographic category
  # idsGC_tx_asympt <- union(idsRGC_tx_asympt, union(idsUGC_tx_asympt, idsVGC_tx_asympt))
  # dem.cats <- sort(unique(dem.cat[idsGC_tx_asympt]))
  # txGC_asympt <- rep(NA, length(idsGC_tx_asympt))
  # for (i in dem.cats) {
  #   ids.dem.cat <- which(dem.cat[idsGC_tx_asympt] == i)
  #   txGC_asympt[ids.dem.cat] <- runif(length(ids.dem.cat)) < gc.asympt.tx.prob[i]
  # }
  # ids_txGC_asympt <- idsGC_tx_asympt[which(txGC_asympt == 1)]
  #
  # # Subset by site
  # txRGC_asympt <- intersect(idsRGC_tx_asympt, ids_txGC_asympt)
  # txUGC_asympt <- intersect(idsUGC_tx_asympt, ids_txGC_asympt)
  # txVGC_asympt <- intersect(idsVGC_tx_asympt, ids_txGC_asympt)
  #
  # ## All Treated GC ##
  #
  # # IDs of persons sucessfully treated
  # txRGC <- union(txRGC_sympt, txRGC_asympt)
  # txUGC <- union(txUGC_sympt, txUGC_asympt)
  # txVGC <- union(txVGC_sympt, txVGC_asympt)
  #
  # # IDs of persons eligible for treatment
  # idsRGC_tx <- union(idsRGC_tx_sympt, idsRGC_tx_asympt)
  # idsUGC_tx <- union(idsUGC_tx_sympt, idsUGC_tx_asympt)
  # idsVGC_tx <- union(idsVGC_tx_sympt, idsVGC_tx_asympt)
  #
  # ## Symptomatic CT Treatment ##
  # idsRCT_tx_sympt <- which(
  #   rCT == 1 &
  #   rCT.infTime < at &
  #   rCT.sympt == 1 &
  #   is.na(rCT.tx)
  # )
  # idsUCT_tx_sympt <- which(
  #   uCT == 1 &
  #   uCT.infTime < at &
  #   uCT.sympt == 1 &
  #   is.na(uCT.tx)
  # )
  # idsVCT_tx_sympt <- which(
  #   vCT == 1 &
  #     vCT.infTime < at &
  #     vCT.sympt == 1 &
  #     is.na(vCT.tx)
  # )
  #
  # # Subset by demographic category
  # idsCT_tx_sympt <- union(idsRCT_tx_sympt, union(idsUCT_tx_sympt, idsVCT_tx_sympt))
  # dem.cats <- sort(unique(dem.cat[idsCT_tx_sympt]))
  # txCT_sympt <- rep(NA, length(idsCT_tx_sympt))
  # for (i in dem.cats) {
  #   ids.dem.cat <- which(dem.cat[idsCT_tx_sympt] == i)
  #   txCT_sympt[ids.dem.cat] <- runif(length(ids.dem.cat)) < ct.sympt.tx.prob[i]
  # }
  # ids_txCT_sympt <- idsCT_tx_sympt[which(txCT_sympt == 1)]
  #
  # # Subset by site
  # txRCT_sympt <- intersect(idsRCT_tx_sympt, ids_txCT_sympt)
  # txUCT_sympt <- intersect(idsUCT_tx_sympt, ids_txCT_sympt)
  # txVCT_sympt <- intersect(idsVCT_tx_sympt, ids_txCT_sympt)
  #
  # ## Asymptomatic CT Treatment ##
  # idsRCT_tx_asympt <- which(
  #   rCT == 1 &
  #   rCT.infTime < at &
  #   rCT.sympt == 0 &
  #   is.na(rCT.tx) &
  #   prepStat == 0
  # )
  # idsUCT_tx_asympt <- which(
  #   uCT == 1 &
  #   uCT.infTime < at &
  #   uCT.sympt == 0 &
  #   is.na(uCT.tx) &
  #   prepStat == 0
  # )
  # idsVCT_tx_asympt <- which(
  #   vCT == 1 &
  #     vCT.infTime < at &
  #     vCT.sympt == 0 &
  #     is.na(vCT.tx) &
  #     prepStat == 0
  # )
  #
  # # Subset by demographic category
  # idsCT_tx_asympt <- union(idsRCT_tx_asympt, union(idsUCT_tx_asympt, idsVCT_tx_asympt))
  # dem.cats <- sort(unique(dem.cat[idsCT_tx_asympt]))
  # txCT_asympt <- rep(NA, length(idsCT_tx_asympt))
  # for (i in dem.cats) {
  #   ids.dem.cat <- which(dem.cat[idsCT_tx_asympt] == i)
  #   txCT_asympt[ids.dem.cat] <- runif(length(ids.dem.cat)) < ct.asympt.tx.prob[i]
  # }
  # ids_txCT_asympt <- idsCT_tx_asympt[which(txCT_asympt == 1)]
  #
  # # Subset by site
  # txRCT_asympt <- intersect(idsRCT_tx_asympt, ids_txCT_asympt)
  # txUCT_asympt <- intersect(idsUCT_tx_asympt, ids_txCT_asympt)
  # txVCT_asympt <- intersect(idsVCT_tx_asympt, ids_txCT_asympt)
  #
  # ## All Treated CT ##
  # txRCT <- union(txRCT_sympt, txRCT_asympt)
  # txUCT <- union(txUCT_sympt, txUCT_asympt)
  # txVCT <- union(txVCT_sympt, txVCT_asympt)
  #
  # idsRCT_tx <- union(idsRCT_tx_sympt, idsRCT_tx_asympt)
  # idsUCT_tx <- union(idsUCT_tx_sympt, idsUCT_tx_asympt)
  # idsVCT_tx <- union(idsVCT_tx_sympt, idsVCT_tx_asympt)
  #
  #
  #
  # ## Interval-based treatment for MSM on PrEP ##
  #
  # idsSTI_screen <- which(prepStartTime == at |
  #                        (at - prepLastStiScreen >= prep.sti.screen.int))
  #
  # dat <- set_attr(dat, "prepLastStiScreen", at, posit_ids = idsSTI_screen)
  #
  #
  # idsRGC_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     rGC == 1 &
  #     rGC.infTime < at &
  #     is.na(rGC.tx.prep)
  #   )
  # )
  # idsUGC_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     uGC == 1 &
  #     uGC.infTime < at &
  #     is.na(uGC.tx.prep)
  #   )
  # )
  # idsVGC_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     vGC == 1 &
  #       vGC.infTime < at &
  #       is.na(vGC.tx.prep)
  #   )
  # )
  # idsRCT_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     rCT == 1 &
  #     rCT.infTime < at &
  #     is.na(rCT.tx.prep)
  #   )
  # )
  # idsUCT_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     uCT == 1 &
  #     uCT.infTime < at &
  #     is.na(uCT.tx.prep)
  #   )
  # )
  # idsVCT_prep_tx <- intersect(
  #   idsSTI_screen,
  #   which(
  #     vCT == 1 &
  #       vCT.infTime < at &
  #       is.na(vCT.tx.prep)
  #   )
  # )
  # txRGC_prep <- idsRGC_prep_tx[runif(length(idsRGC_prep_tx)) < prep.sti.tx.prob]
  # txUGC_prep <- idsUGC_prep_tx[runif(length(idsUGC_prep_tx)) < prep.sti.tx.prob]
  # txVGC_prep <- idsVGC_prep_tx[runif(length(idsVGC_prep_tx)) < prep.sti.tx.prob]
  # txRCT_prep <- idsRCT_prep_tx[runif(length(idsRCT_prep_tx)) < prep.sti.tx.prob]
  # txUCT_prep <- idsUCT_prep_tx[runif(length(idsUCT_prep_tx)) < prep.sti.tx.prob]
  # txVCT_prep <- idsVCT_prep_tx[runif(length(idsVCT_prep_tx)) < prep.sti.tx.prob]

  ## Update Attributes ##
#  rGC.tx[idsRGC_tx] <- 0
  rGC.tx[RGC_tx] <- 1

#  uGC.tx[idsUGC_tx] <- 0
  uGC.tx[UGC_tx] <- 1

#  vGC.tx[idsVGC_tx] <- 0
  vGC.tx[VGC_tx] <- 1

#  rCT.tx[idsRCT_tx] <- 0
  rCT.tx[RCT_tx] <- 1

#  uCT.tx[idsUCT_tx] <- 0
  uCT.tx[UCT_tx] <- 1

#  vCT.tx[idsVCT_tx] <- 0
  vCT.tx[VCT_tx] <- 1

  # rGC.tx.prep[idsRGC_prep_tx] <- 0
  # rGC.tx.prep[txRGC_prep] <- 1
  #
  # uGC.tx.prep[idsUGC_prep_tx] <- 0
  # uGC.tx.prep[txUGC_prep] <- 1
  #
  # vGC.tx.prep[idsVGC_prep_tx] <- 0
  # vGC.tx.prep[txVGC_prep] <- 1
  #
  # rCT.tx.prep[idsRCT_prep_tx] <- 0
  # rCT.tx.prep[txRCT_prep] <- 1
  #
  # uCT.tx.prep[idsUCT_prep_tx] <- 0
  # uCT.tx.prep[txUCT_prep] <- 1
  #
  # vCT.tx.prep[idsVCT_prep_tx] <- 0
  # vCT.tx.prep[txVCT_prep] <- 1

  ## Add tx at other anatomical site ##
  rGC.tx[(uGC.tx == 1 | uGC.tx.prep == 1) & sex == 1 & rGC == 1] <- 1
  rGC.tx[(vGC.tx == 1 | vGC.tx.prep == 1) & sex == 2 & rGC == 1] <- 1
  uGC.tx[(rGC.tx == 1 | rGC.tx.prep == 1) & sex == 1 & uGC == 1] <- 1
  vGC.tx[(rGC.tx == 1 | rGC.tx.prep == 1) & sex == 2 & vGC == 1] <- 1

  rCT.tx[(uCT.tx == 1 | uCT.tx.prep == 1) & sex == 1 & rCT == 1] <- 1
  rCT.tx[(vCT.tx == 1 | vCT.tx.prep == 1) & sex == 2 & rCT == 1] <- 1
  uCT.tx[(rCT.tx == 1 | rCT.tx.prep == 1) & sex == 1 & uCT == 1] <- 1
  vCT.tx[(rCT.tx == 1 | rCT.tx.prep == 1) & sex == 2 & vCT == 1] <- 1

  dat <- set_attr(dat, "rGC.tx", rGC.tx)
  dat <- set_attr(dat, "uGC.tx", uGC.tx)
  dat <- set_attr(dat, "vGC.tx", vGC.tx)
  dat <- set_attr(dat, "rCT.tx", rCT.tx)
  dat <- set_attr(dat, "uCT.tx", uCT.tx)
  dat <- set_attr(dat, "vCT.tx", vCT.tx)
  # dat <- set_attr(dat, "rGC.tx.prep", rGC.tx.prep)
  # dat <- set_attr(dat, "uGC.tx.prep", uGC.tx.prep)
  # dat <- set_attr(dat, "vGC.tx.prep", vGC.tx.prep)
  # dat <- set_attr(dat, "rCT.tx.prep", rCT.tx.prep)
  # dat <- set_attr(dat, "uCT.tx.prep", uCT.tx.prep)
  # dat <- set_attr(dat, "vCT.tx.prep", vCT.tx.prep)


  ####  Track diagnosis / treatment counts as rates of demographic pop and rates of infected

  dat <- set_epi(dat, "tst.tr.gc", at,
                 length(union(RGC_tx, union(UGC_tx, VGC_tx))))
  dat <- set_epi(dat, "tst.tr.gc.M", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(sex == 1))))
  dat <- set_epi(dat, "tst.tr.gc.F", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(sex == 2))))
  dat <- set_epi(dat, "tst.tr.gc.B", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(race == 1))))
  dat <- set_epi(dat, "tst.tr.gc.H", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(race == 2))))
  dat <- set_epi(dat, "tst.tr.gc.W", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(race == 3))))

  dat <- set_epi(dat, "tst.tr.gc.BM", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 1))))
  dat <- set_epi(dat, "tst.tr.gc.HM", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 2))))
  dat <- set_epi(dat, "tst.tr.gc.WM", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 3))))
  dat <- set_epi(dat, "tst.tr.gc.BF", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 4))))
  dat <- set_epi(dat, "tst.tr.gc.HF", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 5))))
  dat <- set_epi(dat, "tst.tr.gc.WF", at,
                 length(intersect(union(RGC_tx, union(UGC_tx, VGC_tx)), which(dem.cat == 6))))

  dat <- set_epi(dat, "tst.tr.ct", at,
                 length(union(RCT_tx, union(UCT_tx, VCT_tx))))
  dat <- set_epi(dat, "tst.tr.ct.M", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(sex == 1))))
  dat <- set_epi(dat, "tst.tr.ct.F", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(sex == 2))))
  dat <- set_epi(dat, "tst.tr.ct.B", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(race == 1))))
  dat <- set_epi(dat, "tst.tr.ct.H", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(race == 2))))
  dat <- set_epi(dat, "tst.tr.ct.W", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(race == 3))))

  dat <- set_epi(dat, "tst.tr.ct.BM", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 1))))
  dat <- set_epi(dat, "tst.tr.ct.HM", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 2))))
  dat <- set_epi(dat, "tst.tr.ct.WM", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 3))))
  dat <- set_epi(dat, "tst.tr.ct.BF", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 4))))
  dat <- set_epi(dat, "tst.tr.ct.HF", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 5))))
  dat <- set_epi(dat, "tst.tr.ct.WF", at,
                 length(intersect(union(RCT_tx, union(UCT_tx, VCT_tx)), which(dem.cat == 6))))

  return(dat)
}
