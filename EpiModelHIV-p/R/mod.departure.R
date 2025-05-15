
#' @title Depature Module
#'
#' @description Module function for simulting both general and disease-related
#'              departures, including deaths, among population members.
#'
#' @inheritParams aging_msm
#'
#' @details
#' Deaths are divided into two categories: general deaths, for which demographic
#' data on age-specific mortality rates applies; and disease-related diseases,
#' for which the rate of death is a function of progression to end-stage AIDS.
#'
#' @return
#' This function returns the updated \code{dat} object accounting for deaths.
#' The deaths are deactivated from the main and casual networks, as those are in
#' \code{networkDynamic} class objects; dead nodes are not deleted from the
#' instant network until the \code{\link{simnet_msm}} module for bookkeeping
#' purposes.
#'
#' @keywords module msm
#' @export
#'
departure_msm <- function(dat, at) {

  ## Input
  # Attributes
  active <- get_attr(dat, "active")
  age    <- get_attr(dat, "age")
  race   <- get_attr(dat, "race")
  status <- get_attr(dat, "status")
  stage  <- get_attr(dat, "stage")
  dem.cat  <- get_attr(dat, "dem.cat")
  uid    <- get_unique_ids(dat)

  age <- floor(age)

  # Parameters
  aids.mort.rate  <- get_param(dat, "aids.mort.rate")
  netstats        <- get_param(dat, "netstats")

  asmr <- netstats[["demog"]][["asmr"]]

  ## HIV-related deaths
  idsEligAIDS <- which(stage == 4)
  idsDepAIDS <- idsEligAIDS[runif(length(idsEligAIDS)) < aids.mort.rate]

  ## General deaths
  idsElig <- which(as.logical(active))
  idsElig <- setdiff(idsElig, idsDepAIDS)
  rates <- rep(NA, length(idsElig))

  dem.cats <- sort(unique(dem.cat))
  for (i in seq_along(dem.cats)) {
    ids.dem.cats <- which(dem.cat == dem.cats[i])
    rates[ids.dem.cats] <- asmr[age[ids.dem.cats], i + 1]
  }
  idsDep <- idsElig[runif(length(rates)) < rates]



  idsDepAll <- unique(c(idsDep, idsDepAIDS))
  depHIV <- intersect(idsDepAll, which(as.logical(status)))
  depHIV.old <- intersect(depHIV, which(age >= 65))

  if (length(idsDepAll) > 0) {
    dat <- set_attr(dat, "active", 0, posit_ids = idsDepAll)
    tergmLite.track.duration <- get_control(dat, "tergmLite.track.duration")
    for (i in 1:3) {
      dat[["el"]][[i]] <- delete_vertices(
        dat[["el"]][[i]],
        idsDepAll
      )

      if (i < 3 && tergmLite.track.duration == TRUE) {
        dat$nw[[i]] %n% "lasttoggle" <-
          delete_vertices(dat$nw[[i]] %n% "lasttoggle", idsDepAll)
      }
    }
    dat[["attr"]] <- deleteAttr(dat[["attr"]], idsDepAll)
    attr.length <- unique(vapply(dat[["attr"]], length, numeric(1)))
    if (attr.length != attributes(dat[["el"]][[1]])[["n"]]) {
      stop("mismatch between el and attr length in departures mod")
    }
  }


  ## Summary Output
  dat <- set_epi(dat, "dep.gen", at, length(idsDep))
  dat <- set_epi(dat, "dep.AIDS", at, length(idsDepAIDS))
  dat <- set_epi(dat, "dep.HIV", at, length(depHIV))
  dat <- set_epi(dat, "dep.HIV.old", at, length(depHIV.old))
  dat <- set_epi(dat, "dep.ALL", at, length(idsDepAll))

  return(dat)
}
