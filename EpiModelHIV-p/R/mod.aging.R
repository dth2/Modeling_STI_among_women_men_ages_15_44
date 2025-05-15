
#' @title Aging Module
#'
#' @description Module for aging over time for active nodes in the population.
#'
#' @param dat Master data list object of class \code{dat} containing networks,
#'        individual-level attributes, and summary statistics.
#' @param at Current time step.
#'
#' @return
#' This function returns \code{dat} after updating the nodal attribute
#' \code{age} and \code{sqrt.age}. The \code{sqrt.age} vertex attribute is also
#' updated on the three networks.
#'
#' @keywords module msm
#' @export
#'
aging_msm <- function(dat, at) {

  ## Input
  # Attributes
  age     <- get_attr(dat, "age")
  active  <- get_attr(dat, "active")
  age.grp <- get_attr(dat, "age.grp")

  # Parameters
  time.unit <- get_param(dat, "time.unit")

  ## Process

  age[active == 1] <- age[active == 1] + time.unit / 364

  netstats <- get_param(dat, "netstats")
  age.breaks <- netstats[["demog"]][["age.breaks"]]
  age.grp[active == 1] <- cut(
    age[active == 1],
    age.breaks,
    labels = FALSE,
    right = FALSE
  )

  ## Output
  # Set Attributes
  dat <- set_attr(dat, "age.grp", age.grp)
  dat <- set_attr(dat, "age", age)
  dat <- set_attr(dat, "age15", 0, posit_ids = which(age >= 16))
  dat <- set_attr(dat, "adol", 0, posit_ids = which(age >= 18))

  return(dat)
}
