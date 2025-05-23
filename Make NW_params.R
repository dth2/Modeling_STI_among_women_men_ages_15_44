# #' Calculate Individual-Level Network Parameters
#'
#' @description Builds statistical models predicting mean degree, mixing, and
#'              duration of sexual partnerships, for use in the EpiModelHIV
#'              workflow.
#'
#' @param epistats Output from \code{\link{build_epistats}}.
#' @param smooth.main.dur If \code{TRUE}, average main durations for oldest
#'        and second oldest age groups.
#'
#' @details
#' \code{build_netparams} is a helper function that constructs the necessary
#' network parameters for use in building network models with \code{\link{build_netstats}},
#' building on models estimated using \code{\link{build_epistats}}.
#' The parameter \code{smooth.main.dur} is used when partnership duration and
#' mortality compete in the eldest age group; in such a case mean duration is
#' averaged over the oldest and second oldest age groups (as specified by \code{age.breaks}
#' in \code{\link{build_epistats}}). Subsequently, this smoothing is only done
#' if there are three or more age categories specified.
#'
#' @export
#' @examples
#' epistats <- build_epistats(geog.lvl = "state", geog.cat = "GA", race = TRUE,
#'                            age.limits = c(20, 50),
#'                            age.breaks = c(20, 30, 40))
#' netparams <- build_netparams(epistats = epistats, smooth.main.dur = TRUE)


build_netparams <- function(d,l) {

  age.grps <- 4
  out <- list()


  # 1. Main HET Model -----------------------------------------------------------

  out$main.het <- list()
  lmain <- l[l$ptype == 1, ]


  ## edges ----
  mod <- glm(deg.main.het ~ 1,
             data = d, family = poisson())
  # summary(mod)

  pred <- exp(coef(mod)[[1]])

  out$main.het$md.main <- as.numeric(pred)


  ## nodematch("age.grp") ----

  table(lmain$age.grp)
  table(lmain$p_age.grp)

  lmain$same.age.grp <- ifelse(lmain$age.grp == lmain$p_age.grp, 1, 0)

  mod <- glm(same.age.grp ~ age.grp,
             data = lmain, family = binomial())
  # summary(mod)

  dat <- data.frame(age.grp = 1:4)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nm.age.grp <- as.numeric(pred)

  ## absdiff("age") ----

  lmain$ad <- abs(lmain$age - lmain$p_age)
  lmain$ad.sr <- abs(sqrt(lmain$age) - sqrt(lmain$p_age))

  mod <- lm(ad ~ 1, data = lmain)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$main.het$absdiff.age <- as.numeric(pred)



  ## absdiff("sqrt.age") ----

  mod <- lm(ad.sr ~ 1, data = lmain)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$main.het$absdiff.sqrt.age <- as.numeric(pred)


  ## nodefactor("age.grp") ----

  mod <- glm(deg.main.het ~ age.grp + sqrt(age.grp),
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(age.grp = 1:age.grps)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nf.age.grp <- as.numeric(pred)

  ## nodefactor("age15") ----

  mod <- glm(deg.main.het ~ as.factor(age15),
             data = d, family = poisson())
   summary(mod)

  dat <- data.frame(age15 = 0:1)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nf.age15 <- as.numeric(pred)


  ## nodematch("race", diff = TRUE) ----

  lmain$same.race <- ifelse(lmain$race3 == lmain$p_race3, 1, 0)

  mod <- glm(same.race ~ as.factor(race3),
             data = lmain, family = binomial())
  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nm.race <- as.numeric(pred)


  ## nodefactor("race") ----

  mod <- glm(deg.main.het ~ as.factor(race3),
             data = d, family = poisson())

  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nf.race <- as.numeric(pred)


  ## nodefactor("deg.casl") ----

  mod <- glm(deg.main.het ~ deg.casl.het,
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(deg.casl.het = sort(unique(d$deg.casl.het)))
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nf.deg.casl.het <- as.numeric(pred)

  deg.casl.dist <- prop.table(table(d$deg.casl.het))
  out$main.het$deg.casl.dist <- as.numeric(deg.casl.dist)

  ## nodefactor("deg.casl.c") ----

  mod <- glm(deg.main.het ~ as.factor(deg.casl.c.het),
             data = d, family = poisson())
  summary(mod)

  dat <- data.frame(deg.casl.c.het = 0:1)
  pred <- predict(mod, newdata = dat, type = "response")

  out$main.het$nf.deg.casl.c.het <- as.numeric(pred)




  ## concurrent ----

  mod <- glm(deg.main.conc.het ~ 1,
             data = d, family = binomial())
  # summary(mod)

  pred <- exp(coef(mod)[[1]])/(1+exp(coef(mod)[[1]]))

  out$main.het$concurrent <- as.numeric(pred)



  ## nodefactor("diag.status") ----

  # mod <- glm(deg.main ~ hiv2,
  #              data = d, family = poisson())
  #   # summary(mod)
  #
  #   dat <- data.frame(hiv2 = 0:1)
  #   pred <- predict(mod, newdata = dat, type = "response")
  #
  #   out$main$nf.diag.status <- as.numeric(pred)


  ## Durations ----

  # overall
  durs.main <- lmain %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()

  # create city weights
  wt <- 1

  # The weekly dissolution rate is function of the mean of the geometric distribution
  # which relates to the median as:
  durs.main$rates.main.adj <- 1 - (2^(-1/(wt*durs.main$median.dur)))

  # Mean duration associated with a geometric distribution that median:
  durs.main$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.main$median.dur))))
  out$main.het$durs.main.homog <- durs.main

  # stratified by age

  # first, non-matched by age group
  durs.main.nonmatch <- lmain %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    filter(same.age.grp == 0) %>%
    # group_by(index.age.grp) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()
  durs.main.nonmatch$age.grp <- 0

  # then, matched within age-groups
  durs.main.matched <- lmain %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    filter(same.age.grp == 1) %>%
    group_by(age.grp) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()
  durs.main.matched

  durs.main.all <- rbind(durs.main.nonmatch, durs.main.matched)

  durs.main.all$rates.main.adj <- 1 - (2^(-1/(wt*durs.main.all$median.dur)))
  durs.main.all$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.main.all$median.dur))))

  durs.main.all <- durs.main.all[, c(3, 1, 2, 4, 5)]
  out$main.het$durs.main.byage <- durs.main.all



  # 2. Casual HET Model ---------------------------------------------------------

  out$casl.het <- list()
  lcasl <- l[l$ptype == 2, ]


  ## edges ----

  mod <- glm(deg.casl.het ~ 1,
             data = d, family = poisson())
  # summary(mod)

  pred <- exp(coef(mod)[[1]])

  out$casl.het$md.casl <- as.numeric(pred)


  ## nodematch("age.grp") ----

  lcasl$same.age.grp <- ifelse(lcasl$age.grp == lcasl$p_age.grp, 1, 0)

  mod <- glm(same.age.grp ~ age.grp,
             data = lcasl, family = binomial())
  # summary(mod)

  dat <- data.frame(age.grp = 1:age.grps)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nm.age.grp <- as.numeric(pred)



  ## absdiff("age") ----

  lcasl$ad <- abs(lcasl$age - lcasl$p_age)
  lcasl$ad.sr <- abs(sqrt(lcasl$age) - sqrt(lcasl$p_age))

  mod <- lm(ad ~ 1, data = lcasl)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$casl.het$absdiff.age <- as.numeric(pred)


  ## absdiff("sqrt.age") ----

  mod <- lm(ad.sr ~ 1, data = lcasl)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$casl.het$absdiff.sqrt.age <- as.numeric(pred)


  ## nodefactor("age.grp") ----

  mod <- glm(deg.casl.het ~ age.grp + sqrt(age.grp),
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(age.grp = 1:age.grps)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nf.age.grp <- as.numeric(pred)


  ## nodefactor("age15") ----

  mod <- glm(deg.casl.het ~ as.factor(age15),
             data = d, family = poisson())
  summary(mod)

  dat <- data.frame(age15 = 0:1)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nf.age15 <- as.numeric(pred)

  ## nodematch("race") ----


  lcasl$same.race <- ifelse(lcasl$race3 == lcasl$p_race3, 1, 0)

  mod <- glm(same.race ~ as.factor(race3),
             data = lcasl, family = binomial())
  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nm.race <- as.numeric(pred)


  ## nodefactor("race") ----

  mod <- glm(deg.casl.het ~ as.factor(race3),
             data = d, family = poisson())

  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nf.race <- as.numeric(pred)


  ## nodefactor("deg.main") ----

  mod <- glm(deg.casl.het ~ deg.main.het,
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(deg.main.het = 0:2)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nf.deg.main.het <- as.numeric(pred)

  deg.main.dist <- prop.table(table(d$deg.main.het))
  out$casl.het$deg.main.dist <- as.numeric(deg.main.dist)


  ## nodefactor("deg.main.c") ----

  mod <- glm(deg.casl.het ~ as.factor(deg.main.c.het),
             data = d, family = poisson())
  summary(mod)

  dat <- data.frame(deg.main.c.het = 0:1)
  pred <- predict(mod, newdata = dat, type = "response")

  out$casl.het$nf.deg.main.c.het <- as.numeric(pred)


  ## concurrent ----

  mod <- glm(deg.casl.conc.het ~ 1,
             data = d, family = binomial())
  # summary(mod)

  pred <- exp(coef(mod)[[1]])/(1+exp(coef(mod)[[1]]))

  out$casl.het$concurrent <- as.numeric(pred)

  ## nodefactor("diag.status") ----

  # mod <- glm(deg.casl.het ~ hiv2,
  #            data = d, family = poisson())
  # # summary(mod)
  #
  # dat <- data.frame(hiv2 = 0:1)
  # pred <- predict(mod, newdata = dat, type = "response")
  #
  # out$casl.het$nf.diag.status <- as.numeric(pred)


  ## Durations ----

  # overall
  durs.casl <- lcasl %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()

  # create city weights
  wt <- 1

  # The weekly dissolution rate is function of the mean of the geometric distribution
  # which relates to the median as:
  durs.casl$rates.casl.adj <- 1 - (2^(-1/(wt*durs.casl$median.dur)))

  # Mean duration associated with a geometric distribution that median:
  durs.casl$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.casl$median.dur))))
  out$casl.het$durs.casl.homog <- durs.casl

  # stratified by age

  # first, non-matched by age group
  durs.casl.nonmatch <- lcasl %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    filter(same.age.grp == 0) %>%
    # group_by(index.age.grp) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()
  durs.casl.nonmatch$age.grp <- 0

  # then, matched within age-groups
  durs.casl.matched <- lcasl %>%
    filter(RVI == 1 | IVI == 1) %>%
    filter(age.grp <= 4) %>%
    filter(same.age.grp == 1) %>%
    group_by(age.grp) %>%
    summarise(mean.dur = mean(duration, na.rm = TRUE),
              median.dur = median(duration, na.rm = TRUE)) %>%
    as.data.frame()
  # durs.casl.matched

  durs.casl.all <- rbind(durs.casl.nonmatch, durs.casl.matched)
  # durs.casl.all

  durs.casl.all$rates.casl.adj <- 1 - (2^(-1/(wt*durs.casl.all$median.dur)))
  durs.casl.all$mean.dur.adj <- 1/(1 - (2^(-1/(wt*durs.casl.all$median.dur))))

  durs.casl.all <- durs.casl.all[, c(3, 1, 2, 4, 5)]
  out$casl.het$durs.casl.byage <- durs.casl.all



  # 3. One-off HET Model --------------------------------------------------------

  out$inst.het <- list()
  linst <- l[l$ptype == 3, ]

  ## edges ----

  head(d$count.oo.part.trunc.het, 25)
  table(d$count.oo.part.trunc.het)
  # summary(d$count.oo.part)

  # yearly rate (adjust coefs after estimation)
  d$count.oo.part.trunc.het <- d$count.oo.part.trunc.het
  # summary(d$rate.oo.part)


  mod <- glm(count.oo.part.trunc.het ~ 1,
             data = d, family = poisson())
  # summary(mod)

  pred <- exp(coef(mod)[[1]])

  out$inst.het$md.inst <- as.numeric(pred)



  ## nodematch("age.grp") ----

  linst$same.age.grp <- ifelse(linst$age.grp == linst$p_age.grp, 1, 0)

  mod <- glm(same.age.grp ~ age.grp,
             data = linst, family = binomial())
  # summary(mod)

  dat <- data.frame(age.grp = 1:age.grps)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nm.age.grp <- as.numeric(pred)


  ## absdiff("age") ----

  linst$ad <- abs(linst$age - linst$p_age)
  linst$ad.sr <- abs(sqrt(linst$age) - sqrt(linst$p_age))

  mod <- lm(ad ~ 1, data = linst)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$inst.het$absdiff.age <- as.numeric(pred)

  ## absdiff("sqrt.age") ----

  mod <- lm(ad.sr ~ 1, data = linst)
  # summary(mod)

  pred <- coef(mod)[[1]]

  out$inst.het$absdiff.sqrt.age <- as.numeric(pred)


  ## nodefactor("age.grp") ----

  mod <- glm(count.oo.part.trunc.het ~ age.grp + sqrt(age.grp),
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(age.grp = 1:age.grps)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nf.age.grp <- as.numeric(pred)



  ## nodematch("race", diff = TRUE) ----

  linst$same.race <- ifelse(linst$race3 == linst$p_race3, 1, 0)

  mod <- glm(same.race ~ as.factor(race3),
             data = linst, family = binomial())
  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nm.race <- as.numeric(pred)


  ## nodefactor("race") ----

  mod <- glm(count.oo.part.trunc.het ~ as.factor(race3),
             data = d, family = poisson())

  # summary(mod)

  dat <- data.frame(race3 = 1:3)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nf.race <- as.numeric(pred)

  ## nodefactor("risk.grp") ----

  mod <- glm(count.oo.part.trunc.het ~ as.factor(risk.grp),
             data = d, family = poisson())

  # summary(mod)

  dat <- data.frame(risk.grp = 1:5)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nf.risk.grp <- as.numeric(pred)


  ## nodefactor("deg.tot.c.het") ----

  mod <- glm(count.oo.part.trunc.het ~ as.factor(deg.tot.c.het),
             data = d, family = poisson())
  # summary(mod)

  dat <- data.frame(deg.tot.c.het = 0:2)
  pred <- predict(mod, newdata = dat, type = "response")

  out$inst.het$nf.deg.tot.c.het <- as.numeric(pred)


  ## nodefactor("diag.status") ----

  #
  # mod <- glm(count.oo.part.trunc.het ~ hiv2,
  #            data = d, family = poisson())
  # # summary(mod)
  #
  # dat <- data.frame(hiv2 = 0:1)
  # pred <- predict(mod, newdata = dat, type = "response")/52
  #
  # out$inst.het$nf.diag.status <- as.numeric(pred)





  # 4. Other Parameters -----------------------------------------------------

  ## Sexual Role ----

  out$all$role.type <- prop.table(table(d$role.class[d$msm==1]))



  # SAVE --------------------------------------------------------------------

  return(out)
}

