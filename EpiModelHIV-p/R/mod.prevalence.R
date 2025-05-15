
#' @title Prevalence Calculations within Time Steps
#'
#' @description This module calculates demographic, transmission, and clinical
#'              statistics at each time step within the simulation.
#'
#' @inheritParams aging_msm
#'
#' @details
#' Summary statistic calculations are of two broad forms: prevalence and
#' incidence. This function establishes the summary statistic vectors for both
#' prevalence and incidence at time 1, and then calculates the prevalence
#' statistics for times 2 onward. Incidence statistics (e.g., number of new
#' infections or deaths) are calculated within the modules as they depend on
#' vectors that are not stored external to the module.
#'
#' @return
#' This function returns the \code{dat} object with an updated summary of current
#' attributes stored in \code{dat$epi}.
#'
#' @keywords module msm
#'
#' @export
#'
prevalence_msm <- function(dat, at) {

  # Attributes
  active        <- get_attr(dat, "active")
  status        <- get_attr(dat, "status")
  diag.status   <- get_attr(dat, "diag.status")
  race          <- get_attr(dat, "race")
  age           <- get_attr(dat, "age")
  sex           <- get_attr(dat, "sex")
  dem.cat           <- get_attr(dat, "dem.cat")

  prepElig  <- get_attr(dat, "prepElig")
  prepStat  <- get_attr(dat, "prepStat")

  rGC <- get_attr(dat, "rGC")
  uGC <- get_attr(dat, "uGC")
  vGC <- get_attr(dat, "vGC")
  rCT <- get_attr(dat, "rCT")
  uCT <- get_attr(dat, "uCT")
  vCT <- get_attr(dat, "vCT")

  # Parameter
  time.unit <- get_param(dat, "time.unit")

  # Pop Size / Demog
  num <- sum(active == 1, na.rm = TRUE)
  dat <- set_epi(dat, "num", at,  num)
  num.B <- sum(race == 1, na.rm = TRUE)
  dat <- set_epi(dat, "num.B", at,  num.B)
  num.H <- sum(race == 2, na.rm = TRUE)
  dat <- set_epi(dat, "num.H", at,  num.H)
  num.W <- sum(race == 3, na.rm = TRUE)
  dat <- set_epi(dat, "num.W", at,  num.W)
  num.M <- sum(sex == 1, na.rm = TRUE)
  dat <- set_epi(dat, "num.M", at,  num.M)
  num.F <- sum(sex == 2, na.rm = TRUE)
  dat <- set_epi(dat, "num.F", at,  num.F)
  num.BM <- sum(dem.cat == 1, na.rm = TRUE)
  dat <- set_epi(dat, "num.BM", at,  num.BM)
  num.HM <- sum(dem.cat == 2, na.rm = TRUE)
  dat <- set_epi(dat, "num.HM", at,  num.HM)
  num.WM <- sum(dem.cat == 3, na.rm = TRUE)
  dat <- set_epi(dat, "num.WM", at,  num.WM)
  num.BF <- sum(dem.cat == 4, na.rm = TRUE)
  dat <- set_epi(dat, "num.BF", at,  num.BF)
  num.HF <- sum(dem.cat == 5, na.rm = TRUE)
  dat <- set_epi(dat, "num.HF", at,  num.HF)
  num.WF <- sum(dem.cat == 6, na.rm = TRUE)
  dat <- set_epi(dat, "num.WF", at,  num.WF)
  num.adol <- sum(age < 18)
  dat <- set_epi(dat, "num.adol", at,  num.adol)


  dat$epi$age.mean[at] <- mean(age, na.rm = TRUE)
  dat$epi$s.num[at] <- sum(status == 0, na.rm = TRUE)
  dat$epi$i.num[at] <- sum(status == 1, na.rm = TRUE)
  dat$epi$i.num.B[at] <- sum(status == 1 & race == 1, na.rm = TRUE)
  dat$epi$i.num.H[at] <- sum(status == 1 & race == 2, na.rm = TRUE)
  dat$epi$i.num.W[at] <- sum(status == 1 & race == 3, na.rm = TRUE)

  # HIV Prev and Incid
  # dat$epi$i.prev[at] <- dat$epi$i.num[at] / dat$epi$num[at]
  # dat$epi$i.prev.B[at] <- sum(race == 1 & status == 1, na.rm = TRUE) /
  #   sum(race == 1, na.rm = TRUE)
  # dat$epi$i.prev.H[at] <- sum(race == 2 & status == 1, na.rm = TRUE) /
  #   sum(race == 2, na.rm = TRUE)
  # dat$epi$i.prev.W[at] <- sum(race == 3 & status == 1, na.rm = TRUE) /
  #   sum(race == 3, na.rm = TRUE)
  #
  # dat$epi$i.prev.dx[at] <- sum(diag.status == 1, na.rm = TRUE) / dat$epi$num[at]
  # dat$epi$i.prev.dx.B[at] <- sum(race == 1 & diag.status == 1, na.rm = TRUE) /
  #   sum(race == 1, na.rm = TRUE)
  # dat$epi$i.prev.dx.H[at] <- sum(race == 2 & diag.status == 1, na.rm = TRUE) /
  #   sum(race == 2, na.rm = TRUE)
  # dat$epi$i.prev.dx.W[at] <- sum(race == 3 & diag.status == 1, na.rm = TRUE) /
  #   sum(race == 3, na.rm = TRUE)
  #
  # dat$epi$ir100[at] <- (dat$epi$incid[at] / sum(status == 0, dat$epi$incid[at], na.rm = TRUE)) * 100 * (364 / time.unit)
  # dat$epi$ir100.B[at] <- (dat$epi$incid.B[at] / sum(status == 0 & race == 1,
  #                                                   dat$epi$incid.B[at], na.rm = TRUE)) * 100 * (364 / time.unit)
  # dat$epi$ir100.H[at] <- (dat$epi$incid.H[at] / sum(status == 0 & race == 2,
  #                                                   dat$epi$incid.H[at], na.rm = TRUE)) * 100 * (364 / time.unit)
  # dat$epi$ir100.W[at] <- (dat$epi$incid.W[at] / sum(status == 0 & race == 3,
  #                                                   dat$epi$incid.W[at], na.rm = TRUE)) * 100 * (364 / time.unit)
  #
  # dat$epi$prepElig[at] <- sum(prepElig == 1, na.rm = TRUE)
  # dat$epi$prepElig.B[at] <- sum(prepElig == 1 & race == 1, na.rm = TRUE)
  # dat$epi$prepElig.H[at] <- sum(prepElig == 1 & race == 2, na.rm = TRUE)
  # dat$epi$prepElig.W[at] <- sum(prepElig == 1 & race == 3, na.rm = TRUE)
  #
  # dat$epi$prepCurr[at] <- sum(prepStat == 1, na.rm = TRUE)
  # dat$epi$prepCurr.B[at] <- sum(prepStat == 1 & race == 1, na.rm = TRUE)
  # dat$epi$prepCurr.H[at] <- sum(prepStat == 1 & race == 2, na.rm = TRUE)
  # dat$epi$prepCurr.W[at] <- sum(prepStat == 1 & race == 3, na.rm = TRUE)

  # STIs
  incid.rgc <- get_epi(dat, "incid.rgc", at)
  incid.ugc <- get_epi(dat, "incid.ugc", at)
  incid.vgc <- get_epi(dat, "incid.vgc", at)
  incid.rct <- get_epi(dat, "incid.rct", at)
  incid.uct <- get_epi(dat, "incid.uct", at)
  incid.vct <- get_epi(dat, "incid.vct", at)

  incid.gc.B <- get_epi(dat, "incid.gc.B", at)
  incid.gc.H <- get_epi(dat, "incid.gc.H", at)
  incid.gc.W <- get_epi(dat, "incid.gc.W", at)
  incid.gc.M <- get_epi(dat, "incid.gc.M", at)
  incid.gc.F <- get_epi(dat, "incid.gc.F", at)
  incid.gc.BM <- get_epi(dat, "incid.gc.BM", at)
  incid.gc.HM <- get_epi(dat, "incid.gc.HM", at)
  incid.gc.WM <- get_epi(dat, "incid.gc.WM", at)
  incid.gc.BF <- get_epi(dat, "incid.gc.BF", at)
  incid.gc.HF <- get_epi(dat, "incid.gc.HF", at)
  incid.gc.WF <- get_epi(dat, "incid.gc.WF", at)
  incid.gc.adol <- get_epi(dat, "incid.gc.adol", at)

  incid.ct.B <- get_epi(dat, "incid.ct.B", at)
  incid.ct.H <- get_epi(dat, "incid.ct.H", at)
  incid.ct.W <- get_epi(dat, "incid.ct.W", at)
  incid.ct.M <- get_epi(dat, "incid.ct.M", at)
  incid.ct.F <- get_epi(dat, "incid.ct.F", at)
  incid.ct.BM <- get_epi(dat, "incid.ct.BM", at)
  incid.ct.HM <- get_epi(dat, "incid.ct.HM", at)
  incid.ct.WM <- get_epi(dat, "incid.ct.WM", at)
  incid.ct.BF <- get_epi(dat, "incid.ct.BF", at)
  incid.ct.HF <- get_epi(dat, "incid.ct.HF", at)
  incid.ct.WF <- get_epi(dat, "incid.ct.WF", at)
  incid.ct.adol <- get_epi(dat, "incid.ct.adol", at)

  prev.gc <- sum((rGC == 1 | uGC == 1 | vGC), na.rm = TRUE) / num
  prev.ct <- sum((rCT == 1 | uCT == 1 | vCT), na.rm = TRUE) / num
  prev.gc.M <- sum((rGC == 1 | uGC == 1 | vGC) & sex == 1, na.rm = TRUE) / num.M
  prev.ct.M <- sum((rCT == 1 | uCT == 1 | vCT) & sex == 1, na.rm = TRUE) / num.M
  prev.gc.F <- sum((rGC == 1 | uGC == 1 | vGC) & sex == 2, na.rm = TRUE) / num.F
  prev.ct.F <- sum((rCT == 1 | uCT == 1 | vCT) & sex == 2, na.rm = TRUE) / num.F
  prev.gc.B <- sum((rGC == 1 | uGC == 1 | vGC) & race == 1, na.rm = TRUE) / num.B
  prev.ct.B <- sum((rCT == 1 | uCT == 1 | vCT) & race == 1, na.rm = TRUE) / num.B
  prev.gc.H <- sum((rGC == 1 | uGC == 1 | vGC) & race == 2, na.rm = TRUE) / num.H
  prev.ct.H <- sum((rCT == 1 | uCT == 1 | vCT) & race == 2, na.rm = TRUE) / num.H
  prev.gc.W <- sum((rGC == 1 | uGC == 1 | vGC) & race == 3, na.rm = TRUE) / num.W
  prev.ct.W <- sum((rCT == 1 | uCT == 1 | vCT) & race == 3, na.rm = TRUE) / num.W
  prev.gc.BM <- sum((rGC == 1 | uGC == 1 | vGC) & race == 1 & sex == 1, na.rm = TRUE) / num.BM
  prev.ct.BM <- sum((rCT == 1 | uCT == 1 | vCT) & race == 1 & sex == 1, na.rm = TRUE) / num.BM
  prev.gc.HM <- sum((rGC == 1 | uGC == 1 | vGC) & race == 2 & sex == 1, na.rm = TRUE) / num.HM
  prev.ct.HM <- sum((rCT == 1 | uCT == 1 | vCT) & race == 2 & sex == 1, na.rm = TRUE) / num.HM
  prev.gc.WM <- sum((rGC == 1 | uGC == 1 | vGC) & race == 3 & sex == 1, na.rm = TRUE) / num.WM
  prev.ct.WM <- sum((rCT == 1 | uCT == 1 | vCT) & race == 3 & sex == 1, na.rm = TRUE) / num.WM
  prev.gc.BF <- sum((rGC == 1 | uGC == 1 | vGC) & race == 1 & sex == 2, na.rm = TRUE) / num.BF
  prev.ct.BF <- sum((rCT == 1 | uCT == 1 | vCT) & race == 1 & sex == 2, na.rm = TRUE) / num.BF
  prev.gc.HF <- sum((rGC == 1 | uGC == 1 | vGC) & race == 2 & sex == 2, na.rm = TRUE) / num.HF
  prev.ct.HF <- sum((rCT == 1 | uCT == 1 | vCT) & race == 2 & sex == 2, na.rm = TRUE) / num.HF
  prev.gc.WF <- sum((rGC == 1 | uGC == 1 | vGC) & race == 3 & sex == 2, na.rm = TRUE) / num.WM
  prev.ct.WF <- sum((rCT == 1 | uCT == 1 | vCT) & race == 3 & sex == 2, na.rm = TRUE) / num.WM
  prev.gc.adol <- sum((rGC == 1 | uGC == 1 | vGC) & age < 18, na.rm = TRUE) / num.adol
  prev.ct.adol <- sum((rCT == 1 | uCT == 1 | vCT) & age < 18, na.rm = TRUE) / num.adol


  ##Incidence per 100 person years at risk
  ir100.rgc <- (incid.rgc / sum(rGC == 0, incid.rgc, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ugc <- (incid.ugc / sum(uGC == 0, incid.ugc, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.vgc <- (incid.vgc / sum(vGC == 0, incid.vgc, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc <- ir100.rgc + ir100.ugc + ir100.vgc

  ir100.gc.B <- (incid.gc.B / sum(sum(uGC == 0 & race == 1), incid.gc.B, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.H <- (incid.gc.H / sum(sum(uGC == 0 & race == 2), incid.gc.H, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.W <- (incid.gc.W / sum(sum(uGC == 0 & race == 3), incid.gc.W, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.gc.M <- (incid.gc.M / sum(sum(uGC == 0 & sex == 1), incid.gc.M, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.F <- (incid.gc.F / sum(sum(uGC == 0 & sex == 2), incid.gc.F, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.gc.BM <- (incid.gc.BM / sum(sum(uGC == 0 & dem.cat == 1), incid.gc.BM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.HM <- (incid.gc.HM / sum(sum(uGC == 0 & dem.cat == 2), incid.gc.HM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.WM <- (incid.gc.WM / sum(sum(uGC == 0 & dem.cat == 3), incid.gc.WM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.BF <- (incid.gc.BF / sum(sum(uGC == 0 & dem.cat == 4), incid.gc.BF, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.HF <- (incid.gc.HF / sum(sum(uGC == 0 & dem.cat == 5), incid.gc.HF, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.gc.WF <- (incid.gc.WF / sum(sum(uGC == 0 & dem.cat == 6), incid.gc.WF, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.gc.adol <- (incid.gc.adol / sum(sum(uGC == 0 & age < 18), incid.gc.adol, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.rct <- (incid.rct / sum(rCT == 0, incid.rct, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.uct <- (incid.uct / sum(uCT == 0, incid.uct, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.vct <- (incid.vct / sum(vCT == 0, incid.vct, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct <- ir100.rct + ir100.uct + ir100.vct

  ir100.sti <- ir100.gc + ir100.ct

  ir100.ct.B <- (incid.ct.B / sum(sum(uGC == 0 & race == 1), incid.ct.B, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.H <- (incid.ct.H / sum(sum(uGC == 0 & race == 2), incid.ct.H, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.W <- (incid.ct.W / sum(sum(uGC == 0 & race == 3), incid.ct.W, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.ct.M <- (incid.ct.M / sum(sum(uGC == 0 & sex == 1), incid.ct.M, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.F <- (incid.ct.F / sum(sum(uGC == 0 & sex == 2), incid.ct.F, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.ct.BM <- (incid.ct.BM / sum(sum(uGC == 0 & dem.cat == 1), incid.ct.BM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.HM <- (incid.ct.HM / sum(sum(uGC == 0 & dem.cat == 2), incid.ct.HM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.WM <- (incid.ct.WM / sum(sum(uGC == 0 & dem.cat == 3), incid.ct.WM, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.BF <- (incid.ct.BF / sum(sum(uGC == 0 & dem.cat == 4), incid.ct.BF, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.HF <- (incid.ct.HF / sum(sum(uGC == 0 & dem.cat == 5), incid.ct.HF, na.rm = TRUE)) * 100 * (364 / time.unit)
  ir100.ct.WF <- (incid.ct.WF / sum(sum(uGC == 0 & dem.cat == 6), incid.ct.WF, na.rm = TRUE)) * 100 * (364 / time.unit)

  ir100.ct.adol <- (incid.ct.adol / sum(sum(uGC == 0 & age < 18), incid.ct.adol, na.rm = TRUE)) * 100 * (364 / time.unit)

  dat <- set_epi(dat, "prev.gc", at, prev.gc)
  dat <- set_epi(dat, "prev.gc.B", at, prev.gc.B)
  dat <- set_epi(dat, "prev.gc.H", at, prev.gc.H)
  dat <- set_epi(dat, "prev.gc.W", at, prev.gc.W)
  dat <- set_epi(dat, "prev.gc.M", at, prev.gc.M)
  dat <- set_epi(dat, "prev.gc.F", at, prev.gc.F)
  dat <- set_epi(dat, "prev.gc.BM", at, prev.gc.BM)
  dat <- set_epi(dat, "prev.gc.HM", at, prev.gc.HM)
  dat <- set_epi(dat, "prev.gc.WM", at, prev.gc.WM)
  dat <- set_epi(dat, "prev.gc.BF", at, prev.gc.BF)
  dat <- set_epi(dat, "prev.gc.HF", at, prev.gc.HM)
  dat <- set_epi(dat, "prev.gc.WF", at, prev.gc.WM)
  dat <- set_epi(dat, "prev.gc.adol", at, prev.gc.adol)

  dat <- set_epi(dat, "prev.ct", at, prev.ct)
  dat <- set_epi(dat, "prev.ct.B", at, prev.ct.B)
  dat <- set_epi(dat, "prev.ct.H", at, prev.ct.H)
  dat <- set_epi(dat, "prev.ct.W", at, prev.ct.W)
  dat <- set_epi(dat, "prev.ct.M", at, prev.ct.M)
  dat <- set_epi(dat, "prev.ct.F", at, prev.ct.F)
  dat <- set_epi(dat, "prev.ct.BM", at, prev.ct.BM)
  dat <- set_epi(dat, "prev.ct.HM", at, prev.ct.HM)
  dat <- set_epi(dat, "prev.ct.WM", at, prev.ct.WM)
  dat <- set_epi(dat, "prev.ct.BF", at, prev.ct.BF)
  dat <- set_epi(dat, "prev.ct.HF", at, prev.ct.HM)
  dat <- set_epi(dat, "prev.ct.WF", at, prev.ct.WM)
  dat <- set_epi(dat, "prev.ct.adol", at, prev.ct.adol)

  dat <- set_epi(dat, "ir100.gc", at, ir100.gc)
  dat <- set_epi(dat, "ir100.gc.B", at, ir100.gc.B)
  dat <- set_epi(dat, "ir100.gc.H", at, ir100.gc.H)
  dat <- set_epi(dat, "ir100.gc.W", at, ir100.gc.W)
  dat <- set_epi(dat, "ir100.gc.M", at, ir100.gc.M)
  dat <- set_epi(dat, "ir100.gc.F", at, ir100.gc.F)
  dat <- set_epi(dat, "ir100.gc.BM", at, ir100.gc.BM)
  dat <- set_epi(dat, "ir100.gc.HM", at, ir100.gc.HM)
  dat <- set_epi(dat, "ir100.gc.WM", at, ir100.gc.WM)
  dat <- set_epi(dat, "ir100.gc.BF", at, ir100.gc.BF)
  dat <- set_epi(dat, "ir100.gc.HF", at, ir100.gc.HM)
  dat <- set_epi(dat, "ir100.gc.WF", at, ir100.gc.WM)
  dat <- set_epi(dat, "ir100.gc.adol", at, ir100.gc.adol)

  dat <- set_epi(dat, "ir100.ct", at, ir100.ct)
  dat <- set_epi(dat, "ir100.ct.B", at, ir100.ct.B)
  dat <- set_epi(dat, "ir100.ct.H", at, ir100.ct.H)
  dat <- set_epi(dat, "ir100.ct.W", at, ir100.ct.W)
  dat <- set_epi(dat, "ir100.ct.M", at, ir100.ct.M)
  dat <- set_epi(dat, "ir100.ct.F", at, ir100.ct.F)
  dat <- set_epi(dat, "ir100.ct.BM", at, ir100.ct.BM)
  dat <- set_epi(dat, "ir100.ct.HM", at, ir100.ct.HM)
  dat <- set_epi(dat, "ir100.ct.WM", at, ir100.ct.WM)
  dat <- set_epi(dat, "ir100.ct.BF", at, ir100.ct.BF)
  dat <- set_epi(dat, "ir100.ct.HF", at, ir100.ct.HM)
  dat <- set_epi(dat, "ir100.ct.WF", at, ir100.ct.WM)
  dat <- set_epi(dat, "ir100.ct.adol", at, ir100.ct.adol)






  dat$epi$gc.num[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1), na.rm = TRUE)
  dat$epi$gc.num.B[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & race == 1, na.rm = TRUE)
  dat$epi$gc.num.H[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & race == 2, na.rm = TRUE)
  dat$epi$gc.num.W[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & race == 3, na.rm = TRUE)
  dat$epi$gc.num.M[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & sex == 1, na.rm = TRUE)
  dat$epi$gc.num.F[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & sex == 2, na.rm = TRUE)
  dat$epi$gc.num.BM[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 1, na.rm = TRUE)
  dat$epi$gc.num.HM[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 2, na.rm = TRUE)
  dat$epi$gc.num.WM[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 3, na.rm = TRUE)
  dat$epi$gc.num.BF[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 4, na.rm = TRUE)
  dat$epi$gc.num.HF[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 5, na.rm = TRUE)
  dat$epi$gc.num.WF[at] <- sum((rGC == 1 | uGC == 1 | vGC == 1) & dem.cat == 6, na.rm = TRUE)

  dat$epi$ct.num[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1), na.rm = TRUE)
  dat$epi$ct.num.B[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & race == 1, na.rm = TRUE)
  dat$epi$ct.num.H[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & race == 2, na.rm = TRUE)
  dat$epi$ct.num.W[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & race == 3, na.rm = TRUE)
  dat$epi$ct.num.M[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & sex == 1, na.rm = TRUE)
  dat$epi$ct.num.F[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & sex == 2, na.rm = TRUE)
  dat$epi$ct.num.BM[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 1, na.rm = TRUE)
  dat$epi$ct.num.HM[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 2, na.rm = TRUE)
  dat$epi$ct.num.WM[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 3, na.rm = TRUE)
  dat$epi$ct.num.BF[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 4, na.rm = TRUE)
  dat$epi$ct.num.HF[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 5, na.rm = TRUE)
  dat$epi$ct.num.WF[at] <- sum((rCT == 1 | uCT == 1 | vCT == 1) & dem.cat == 6, na.rm = TRUE)

  dat$epi$sti.num[at] <-  max(0,(dat$epi$gc.num[at] + dat$epi$ct.num[at]))
  dat$epi$sti.num.B[at] <- max(0,(dat$epi$gc.num.B[at] + dat$epi$ct.num.B[at]))
  dat$epi$sti.num.H[at] <- max(0,(dat$epi$gc.num.H[at] + dat$epi$ct.num.H[at]))
  dat$epi$sti.num.W[at] <- max(0,(dat$epi$gc.num.W[at] + dat$epi$ct.num.W[at]))
  dat$epi$sti.num.M[at] <- max(0,(dat$epi$gc.num.M[at] + dat$epi$ct.num.M[at]))
  dat$epi$sti.num.F[at] <- max(0,(dat$epi$gc.num.F[at] + dat$epi$ct.num.M[at]))
  dat$epi$sti.num.BM[at] <- max(0,(dat$epi$gc.num.BM[at] + dat$epi$ct.num.BM[at]))
  dat$epi$sti.num.HM[at] <- max(0,(dat$epi$gc.num.HM[at] + dat$epi$ct.num.HM[at]))
  dat$epi$sti.num.WM[at] <- max(0,(dat$epi$gc.num.WM[at] + dat$epi$ct.num.WM[at]))
  dat$epi$sti.num.BF[at] <- max(0,(dat$epi$gc.num.BF[at] + dat$epi$ct.num.BF[at]))
  dat$epi$sti.num.HF[at] <- max(0,(dat$epi$gc.num.HF[at] + dat$epi$ct.num.HF[at]))
  dat$epi$sti.num.WF[at] <- max(0,(dat$epi$gc.num.WF[at] + dat$epi$ct.num.WF[at]))


  ##Annual Incidence per 100K persons
  ir100K.gc <- max(0,(sum(dat$epi$incid.gc[max(1,(at-52)):at]) / (mean(dat$epi$num[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  ir100K.gc.M <- max(0,(sum(dat$epi$incid.gc.M[max(1,(at-52)):at]) / (mean(dat$epi$num.M[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.M[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.F <- max(0,(sum(dat$epi$incid.gc.F[max(1,(at-52)):at]) / (mean(dat$epi$num.F[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.F[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  ir100K.gc.BM <- max(0,(sum(dat$epi$incid.gc.BM[max(1,(at-52)):at]) / (mean(dat$epi$num.BM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.BM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.HM <- max(0,(sum(dat$epi$incid.gc.HM[max(1,(at-52)):at]) / (mean(dat$epi$num.HM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.HM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.WM <- max(0,(sum(dat$epi$incid.gc.WM[max(1,(at-52)):at]) / (mean(dat$epi$num.WM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.WM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.BF <- max(0,(sum(dat$epi$incid.gc.BF[max(1,(at-52)):at]) / (mean(dat$epi$num.BF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.BF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.HF <- max(0,(sum(dat$epi$incid.gc.HF[max(1,(at-52)):at]) / (mean(dat$epi$num.HF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.HF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.gc.WF <- max(0,(sum(dat$epi$incid.gc.WF[max(1,(at-52)):at]) / (mean(dat$epi$num.WF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$gc.num.WF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  ir100K.ct <- max(0,(sum(dat$epi$incid.ct[max(1,(at-52)):at]) / (mean(dat$epi$num[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  ir100K.ct.M <- max(0,(sum(dat$epi$incid.ct.M[max(1,(at-52)):at]) / (mean(dat$epi$num.M[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.M[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.F <- max(0,(sum(dat$epi$incid.ct.F[max(1,(at-52)):at]) / (mean(dat$epi$num.F[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.F[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  ir100K.ct.BM <- max(0,(sum(dat$epi$incid.ct.BM[max(1,(at-52)):at]) / (mean(dat$epi$num.BM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.BM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.HM <- max(0,(sum(dat$epi$incid.ct.HM[max(1,(at-52)):at]) / (mean(dat$epi$num.HM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.HM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.WM <- max(0,(sum(dat$epi$incid.ct.WM[max(1,(at-52)):at]) / (mean(dat$epi$num.WM[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.WM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.BF <- max(0,(sum(dat$epi$incid.ct.BF[max(1,(at-52)):at]) / (mean(dat$epi$num.BF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.BF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.HF <- max(0,(sum(dat$epi$incid.ct.HF[max(1,(at-52)):at]) / (mean(dat$epi$num.HF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.HF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  ir100K.ct.WF <- max(0,(sum(dat$epi$incid.ct.WF[max(1,(at-52)):at]) / (mean(dat$epi$num.WF[max(1,(at-52)):at],na.rm = TRUE)- mean(dat$epi$ct.num.WF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)


  ##Annual diagnosis per 100K persons
  diag100K.gc <- max(0,(sum(dat$epi$tst.tr.gc[max(1,(at-52)):at],na.rm=TRUE) / (mean(dat$epi$num[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  diag100K.gc.M <- max(0,(sum(dat$epi$tst.tr.gc.M[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.M[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.F <- max(0,(sum(dat$epi$tst.tr.gc.F[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.F[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  diag100K.gc.BM <- max(0,(sum(dat$epi$tst.tr.gc.BM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.BM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.HM <- max(0,(sum(dat$epi$tst.tr.gc.HM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.HM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.WM <- max(0,(sum(dat$epi$tst.tr.gc.WM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.WM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.BF <- max(0,(sum(dat$epi$tst.tr.gc.BF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.BF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.HF <- max(0,(sum(dat$epi$tst.tr.gc.HF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.HF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.gc.WF <- max(0,(sum(dat$epi$tst.tr.gc.WF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.WF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  diag100K.ct <- max(0,(sum(dat$epi$tst.tr.ct[max(1,(at-52)):at],na.rm=TRUE) / (mean(dat$epi$num[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  diag100K.ct.M <- max(0,(sum(dat$epi$tst.tr.ct.M[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.M[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.F <- max(0,(sum(dat$epi$tst.tr.ct.F[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.F[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)

  diag100K.ct.BM <- max(0,(sum(dat$epi$tst.tr.ct.BM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.BM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.HM <- max(0,(sum(dat$epi$tst.tr.ct.HM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.HM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.WM <- max(0,(sum(dat$epi$tst.tr.ct.WM[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.WM[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.BF <- max(0,(sum(dat$epi$tst.tr.ct.BF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.BF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.HF <- max(0,(sum(dat$epi$tst.tr.ct.HF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.HF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)
  diag100K.ct.WF <- max(0,(sum(dat$epi$tst.tr.ct.WF[max(1,(at-52)):at], na.rm=TRUE) / (mean(dat$epi$num.WF[max(1,(at-52)):at],na.rm = TRUE))) * 100000, na.rm = TRUE)


  dat <- set_epi(dat, "ir100K.gc", at, ir100K.gc)

  dat <- set_epi(dat, "ir100K.gc.M", at, ir100K.gc.M)
  dat <- set_epi(dat, "ir100K.gc.F", at, ir100K.gc.F)

  dat <- set_epi(dat, "ir100K.gc.BM", at, ir100K.gc.BM)
  dat <- set_epi(dat, "ir100K.gc.HM", at, ir100K.gc.HM)
  dat <- set_epi(dat, "ir100K.gc.WM", at, ir100K.gc.WM)
  dat <- set_epi(dat, "ir100K.gc.BF", at, ir100K.gc.BF)
  dat <- set_epi(dat, "ir100K.gc.HF", at, ir100K.gc.HF)
  dat <- set_epi(dat, "ir100K.gc.WF", at, ir100K.gc.WF)

  dat <- set_epi(dat, "ir100K.ct", at, ir100K.ct)

  dat <- set_epi(dat, "ir100K.ct.M", at, ir100K.ct.M)
  dat <- set_epi(dat, "ir100K.ct.F", at, ir100K.ct.F)

  dat <- set_epi(dat, "ir100K.ct.BM", at, ir100K.ct.BM)
  dat <- set_epi(dat, "ir100K.ct.HM", at, ir100K.ct.HM)
  dat <- set_epi(dat, "ir100K.ct.WM", at, ir100K.ct.WM)
  dat <- set_epi(dat, "ir100K.ct.BF", at, ir100K.ct.BF)
  dat <- set_epi(dat, "ir100K.ct.HF", at, ir100K.ct.HF)
  dat <- set_epi(dat, "ir100K.ct.WF", at, ir100K.ct.WF)

  dat <- set_epi(dat, "diag100K.gc", at, diag100K.gc)

  dat <- set_epi(dat, "diag100K.gc.M", at, diag100K.gc.M)
  dat <- set_epi(dat, "diag100K.gc.F", at, diag100K.gc.F)

  dat <- set_epi(dat, "diag100K.gc.BM", at, diag100K.gc.BM)
  dat <- set_epi(dat, "diag100K.gc.HM", at, diag100K.gc.HM)
  dat <- set_epi(dat, "diag100K.gc.WM", at, diag100K.gc.WM)
  dat <- set_epi(dat, "diag100K.gc.BF", at, diag100K.gc.BF)
  dat <- set_epi(dat, "diag100K.gc.HF", at, diag100K.gc.HF)
  dat <- set_epi(dat, "diag100K.gc.WF", at, diag100K.gc.WF)

  dat <- set_epi(dat, "diag100K.ct", at, diag100K.ct)

  dat <- set_epi(dat, "diag100K.ct.M", at, diag100K.ct.M)
  dat <- set_epi(dat, "diag100K.ct.F", at, diag100K.ct.F)

  dat <- set_epi(dat, "diag100K.ct.BM", at, diag100K.ct.BM)
  dat <- set_epi(dat, "diag100K.ct.HM", at, diag100K.ct.HM)
  dat <- set_epi(dat, "diag100K.ct.WM", at, diag100K.ct.WM)
  dat <- set_epi(dat, "diag100K.ct.BF", at, diag100K.ct.BF)
  dat <- set_epi(dat, "diag100K.ct.HF", at, diag100K.ct.HF)
  dat <- set_epi(dat, "diag100K.ct.WF", at, diag100K.ct.WF)

  return(dat)
}
