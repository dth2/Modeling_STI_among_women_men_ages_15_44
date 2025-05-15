
#' @title STI Recovery Module
#'
#' @description Stochastically simulates GC/CT recovery.
#'
#' @inheritParams aging_msm
#'
#' @keywords module msm
#'
#' @export
#'
stirecov_msm <- function(dat, at) {

  ## Input
  # Attributes
  race <- get_attr(dat, "race")
  sex <- get_attr(dat, "sex")
  dem.cat <- get_attr(dat, "dem.cat")

  rGC <- get_attr(dat, "rGC")
  rGC.infTime <- get_attr(dat, "rGC.infTime")
  rGC.tx <- get_attr(dat, "rGC.tx")
  rGC.tx.prep <- get_attr(dat, "rGC.tx.prep")

  uGC <- get_attr(dat, "uGC")
  uGC.infTime <- get_attr(dat, "uGC.infTime")
  uGC.tx <- get_attr(dat, "uGC.tx")
  uGC.tx.prep <- get_attr(dat, "uGC.tx.prep")

  vGC <- get_attr(dat, "vGC")
  vGC.infTime <- get_attr(dat, "vGC.infTime")
  vGC.tx <- get_attr(dat, "vGC.tx")
  vGC.tx.prep <- get_attr(dat, "vGC.tx.prep")

  rCT <- get_attr(dat, "rCT")
  rCT.infTime <- get_attr(dat, "rCT.infTime")
  rCT.tx <- get_attr(dat, "rCT.tx")
  rCT.tx.prep <- get_attr(dat, "rCT.tx.prep")

  uCT <- get_attr(dat, "uCT")
  uCT.infTime <- get_attr(dat, "uCT.infTime")
  uCT.tx <- get_attr(dat, "uCT.tx")
  uCT.tx.prep <- get_attr(dat, "uCT.tx.prep")

  vCT <- get_attr(dat, "vCT")
  vCT.infTime <- get_attr(dat, "vCT.infTime")
  vCT.tx <- get_attr(dat, "vCT.tx")
  vCT.tx.prep <- get_attr(dat, "vCT.tx.prep")

  # Parameters
  rgc.ntx.int <- get_param(dat, "rgc.ntx.int")
  ugc.ntx.int <- get_param(dat, "ugc.ntx.int")
  vgc.ntx.int <- get_param(dat, "vgc.ntx.int")
  gc.tx.int   <- get_param(dat, "gc.tx.int")

  rct.ntx.int <- get_param(dat, "rct.ntx.int")
  uct.ntx.int <- get_param(dat, "uct.ntx.int")
  vct.ntx.int <- get_param(dat, "vct.ntx.int")
  ct.tx.int   <- get_param(dat, "ct.tx.int")

  # GC Recovery ---------------------------------------------------------

  # Untreated (asymptomatic and symptomatic)
  idsRGC_ntx <- which(
    rGC == 1 &
    rGC.infTime < at &
    (is.na(rGC.tx) | rGC.tx == 0) &
    (is.na(rGC.tx.prep) | rGC.tx.prep == 0)
  )
  idsUGC_ntx <- which(
    uGC == 1 &
    uGC.infTime < at &
    (is.na(uGC.tx) | uGC.tx == 0) &
    (is.na(uGC.tx.prep) | uGC.tx.prep == 0)
  )
  idsVGC_ntx <- which(
    vGC == 1 &
      vGC.infTime < at &
      (is.na(vGC.tx) | vGC.tx == 0) &
      (is.na(vGC.tx.prep) | vGC.tx.prep == 0)
  )

  recovRGC_ntx <- idsRGC_ntx[at - rGC.infTime[idsRGC_ntx] >= rgc.ntx.int]
  recovUGC_ntx <- idsUGC_ntx[at - uGC.infTime[idsUGC_ntx] >= ugc.ntx.int]
  recovVGC_ntx <- idsVGC_ntx[at - vGC.infTime[idsVGC_ntx] >= vgc.ntx.int]

  # Treated (asymptomatic and symptomatic)
  idsRGC_tx <- which(rGC == 1 &
                       rGC.infTime < at &
                       (rGC.tx == 1 | rGC.tx.prep == 1))
  idsUGC_tx <- which(uGC == 1 &
                       uGC.infTime < at &
                       (uGC.tx == 1 | uGC.tx.prep == 1))
  idsVGC_tx <- which(vGC == 1 &
                       vGC.infTime < at &
                       (vGC.tx == 1 | vGC.tx.prep == 1))

  recovRGC_tx <- idsRGC_tx[at - rGC.infTime[idsRGC_tx] >= gc.tx.int]
  recovUGC_tx <- idsUGC_tx[at - uGC.infTime[idsUGC_tx] >= gc.tx.int]
  recovVGC_tx <- idsVGC_tx[at - vGC.infTime[idsVGC_tx] >= gc.tx.int]

  recovRGC <- c(recovRGC_ntx, recovRGC_tx)
  recovUGC <- c(recovUGC_ntx, recovUGC_tx)
  recovVGC <- c(recovVGC_ntx, recovVGC_tx)

  dat <- set_attr(dat, "rGC", 0, posit_ids = recovRGC)
  dat <- set_attr(dat, "rGC.sympt", NA, posit_ids = recovRGC)
  dat <- set_attr(dat, "rGC.infTime", NA, posit_ids = recovRGC)
  dat <- set_attr(dat, "rGC.tx", NA, posit_ids = recovRGC)
  dat <- set_attr(dat, "rGC.tx.prep", NA, posit_ids = recovRGC)

  dat <- set_attr(dat, "uGC", 0, posit_ids = recovUGC)
  dat <- set_attr(dat, "uGC.sympt", NA, posit_ids = recovUGC)
  dat <- set_attr(dat, "uGC.infTime", NA, posit_ids = recovUGC)
  dat <- set_attr(dat, "uGC.tx", NA, posit_ids = recovUGC)
  dat <- set_attr(dat, "uGC.tx.prep", NA, posit_ids = recovUGC)

  dat <- set_attr(dat, "vGC", 0, posit_ids = recovVGC)
  dat <- set_attr(dat, "vGC.sympt", NA, posit_ids = recovVGC)
  dat <- set_attr(dat, "vGC.infTime", NA, posit_ids = recovVGC)
  dat <- set_attr(dat, "vGC.tx", NA, posit_ids = recovVGC)
  dat <- set_attr(dat, "vGC.tx.prep", NA, posit_ids = recovVGC)

  # CT Recovery ---------------------------------------------------------

  # Untreated (asymptomatic and symptomatic)
  idsRCT_ntx <- which(
    rCT == 1 &
    rCT.infTime < at &
    (is.na(rCT.tx) | rCT.tx == 0) &
    (is.na(rCT.tx.prep) | rCT.tx.prep == 0)
  )
  idsUCT_ntx <- which(
    uCT == 1 &
    uCT.infTime < at &
    (is.na(uCT.tx) | uCT.tx == 0) &
    (is.na(uCT.tx.prep) | uCT.tx.prep == 0)
  )
  idsVCT_ntx <- which(
    vCT == 1 &
      vCT.infTime < at &
      (is.na(vCT.tx) | vCT.tx == 0) &
      (is.na(vCT.tx.prep) | vCT.tx.prep == 0)
  )

  recovRCT_ntx <- idsRCT_ntx[at - rCT.infTime[idsRCT_ntx] >= rct.ntx.int]
  recovUCT_ntx <- idsUCT_ntx[at - uCT.infTime[idsUCT_ntx] >= uct.ntx.int]
  recovVCT_ntx <- idsVCT_ntx[at - vCT.infTime[idsVCT_ntx] >= vct.ntx.int]

  # Treated (asymptomatic and symptomatic)
  idsRCT_tx <- which(
    rCT == 1 &
    rCT.infTime < at &
   (rCT.tx == 1 | rCT.tx.prep == 1)
  )
  idsUCT_tx <- which(
    uCT == 1 &
    uCT.infTime < at &
    (uCT.tx == 1 | uCT.tx.prep == 1)
  )
  idsVCT_tx <- which(
    vCT == 1 &
      vCT.infTime < at &
      (vCT.tx == 1 | vCT.tx.prep == 1)
  )

  recovRCT_tx <- idsRCT_tx[at - rCT.infTime[idsRCT_tx] >= ct.tx.int]
  recovUCT_tx <- idsUCT_tx[at - uCT.infTime[idsUCT_tx] >= ct.tx.int]
  recovVCT_tx <- idsVCT_tx[at - vCT.infTime[idsVCT_tx] >= ct.tx.int]

  recovRCT <- c(recovRCT_ntx, recovRCT_tx)
  recovUCT <- c(recovUCT_ntx, recovUCT_tx)
  recovVCT <- c(recovVCT_ntx, recovVCT_tx)

  # Output ------------------------------------------------------------------

  dat <- set_attr(dat, "rCT", 0, posit_ids = recovRCT)
  dat <- set_attr(dat, "rCT.sympt", NA, posit_ids = recovRCT)
  dat <- set_attr(dat, "rCT.infTime", NA, posit_ids = recovRCT)
  dat <- set_attr(dat, "rCT.tx", NA, posit_ids = recovRCT)
  dat <- set_attr(dat, "rCT.tx.prep", NA, posit_ids = recovRCT)

  dat <- set_attr(dat, "uCT", 0, posit_ids = recovUCT)
  dat <- set_attr(dat, "uCT.sympt", NA, posit_ids = recovUCT)
  dat <- set_attr(dat, "uCT.infTime", NA, posit_ids = recovUCT)
  dat <- set_attr(dat, "uCT.tx", NA, posit_ids = recovUCT)
  dat <- set_attr(dat, "uCT.tx.prep", NA, posit_ids = recovUCT)

  dat <- set_attr(dat, "vCT", 0, posit_ids = recovVCT)
  dat <- set_attr(dat, "vCT.sympt", NA, posit_ids = recovVCT)
  dat <- set_attr(dat, "vCT.infTime", NA, posit_ids = recovVCT)
  dat <- set_attr(dat, "vCT.tx", NA, posit_ids = recovVCT)
  dat <- set_attr(dat, "vCT.tx.prep", NA, posit_ids = recovVCT)


  return(dat)
}
