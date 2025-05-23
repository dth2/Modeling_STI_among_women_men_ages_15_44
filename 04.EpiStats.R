
##
## Epidemic parameters analysis for CAMP-HET-STI NSFG Data
## Network data from Deven's analysis
## Behavior data from David's analysis

#Pull in the network data from 2011
setwd("~/CAMP-HET-STI")
load("~/CAMP-HET-STI/NSFG/data/d_het_2011.rda")
load("~/CAMP-HET-STI/NSFG/data/l_het_2011.rda")
d<-d_het
l<-l_het

## Packages ##
library("tidyverse")


names(l)
names(d)


# Data Processing ---------------------------------------------------------

# Age
table(l$age, useNA = "always")
table(l$p_age, useNA = "always")

l$comb.age <- l$age + l$p_age
l$diff.age <- abs(l$age - l$p_age)



# redistribute NAs in proportion to non-missing partner races
#make race combination for partnerships
l$race.combo <- rep(NA, nrow(l))
l$race.combo[l$race3 == 1 & l$p_race3 == 1] <- 1
l$race.combo[l$race3 == 1 & l$p_race3 %in% 2:3] <- 2
l$race.combo[l$race3 == 2 & l$p_race3 %in% c(1, 3)] <- 3
l$race.combo[l$race3 == 2 & l$p_race3 == 2] <- 4
l$race.combo[l$race3 == 3 & l$p_race3 %in% 1:2] <- 5
l$race.combo[l$race3 == 3 & l$p_race3 == 3] <- 6

table(l$race.combo)

#rename race3.
l$race<-l$race3
d$race<-d$race3


# Act Rates ---------------------------------------------------------------

# acts/per week/per partnership for main and casual partnerships MSM and HET


# Pull Data
la.het <- select(l, ptype, duration, comb.age,
             race.combo, RVI, IVI, adol,
             acts = vag.acts.week, cp.acts = vag.acts.week.cp) %>%
  filter(ptype %in% 1:2) %>%
  filter(RVI == 1 | IVI == 1)
la.het <- select(la.het, -c(RVI, IVI))
head(la.het, 25)
nrow(la.het)


# Poisson Model
acts.mod.het <- glm(floor(acts*52) ~ duration + I(duration^2) + as.factor(race.combo) +
                      as.factor(ptype) + duration*as.factor(ptype) + comb.age + I(comb.age^2) + as.factor(adol),
                    family = poisson(), data = la.het)
summary(acts.mod.het)

x.het <- expand.grid(duration = 16,
                     ptype = 2,
                     race.combo = 1:6,
                     comb.age = 36,
                     adol = 0)
pred.het <- predict(acts.mod.het, newdata = x.het, type = "response", se.fit = FALSE)
pred.acts.het <- cbind(x.het, pred.het = pred.het/52)
pred.acts.het
mean(floor(la.het$acts*52),na.rm = TRUE)
summary(la.het$acts[la.het$ptype==1])
summary(la.het$acts[la.het$ptype==2])

# Condom Use // Main Casual -----------------------------------------------

#HET
par(mar = c(3,3,1,1), mgp = c(2,1,0))
ggplot(la.het, aes(acts,cp.acts)) +
  geom_point()

summary(la.het$cp.acts)

la.het$prob.cond <- la.het$cp.acts / la.het$acts
head(la.het, 25)

table(la.het$acts, useNA = "always")

hist(la.het$prob.cond)
table(la.het$prob.cond)
summary(la.het$prob.cond)


summary(la.het$prob.cond[la.het$ptype == 1])
summary(la.het$prob.cond[la.het$ptype == 2])

la.het$any.cond <- ifelse(la.het$prob.cond > 0, 1, 0)
#GET THE CONDOM RATES BY PTYPE.
summary(la.het$any.cond[la.het$ptype == 1])
summary(la.het$any.cond[la.het$ptype == 2])
summary(la.het$any.cond)

summary(la.het$any.cond[la.het$race.combo == 1])
summary(la.het$any.cond[la.het$race.combo == 2])
summary(la.het$any.cond[la.het$race.combo == 3])
summary(la.het$any.cond[la.het$race.combo == 4])
summary(la.het$any.cond[la.het$race.combo == 5])
summary(la.het$any.cond[la.het$race.combo == 6])

la.het$never.cond <- ifelse(la.het$prob.cond == 0, 1, 0)
table(la.het$never.cond)

cond.mc.mod.het <- glm(any.cond ~ duration + I(duration^2) + as.factor(race.combo) +
                         as.factor(ptype) + duration*as.factor(ptype) + comb.age + I(comb.age^2) + as.factor(adol),
                       family = binomial(), data = la.het)
summary(cond.mc.mod.het)

x.het <- expand.grid(duration = 26,
                     ptype = 1,
                     race.combo = 1:6,
                     comb.age = 44,
                     adol = 0)
pred.het <- predict(cond.mc.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het

#GET PREDICTION FOR APPENDIX OF CONDOM USE IN CASUAL PARTNERSHIPS

x.het <- expand.grid(duration = 26,
                     ptype = 2,
                     race.combo = 1:6,
                     comb.age = 44,
                     adol = 0)
pred.het <- predict(cond.mc.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het


tiff(file = "out/condom.probs_2011.tiff", height = 8, width = 4, units = "in", res = 250,
    title = "Condom Use Probability Distribution_2011"
)
par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(1,1))

hist(la.het$prob.cond, main="Hetersexual Condom Use")
#hist(la.msm$prob.cond, main="MSM Condom Use")

dev.off()


# Condom Use // Inst ------------------------------------------------------


#HET
lb.het <- select(l, ptype, comb.age, race.combo, adol,
                 RVI, IVI, condls) %>%
  filter(ptype == 3) %>%
  filter(RVI == 1 | IVI == 1)
head(lb.het, 40)
table(lb.het$ptype)
table(lb.het$condls)

lb.het$prob.cond <- rep(NA, nrow(lb.het))

cond.oo.mod.het <- glm(condls ~ as.factor(race.combo) +
                         comb.age + I(comb.age^2) + as.factor(adol),
                       family = binomial(), data = lb.het)
summary(cond.oo.mod.het)

x.het <- expand.grid(race.combo = 1:6,
                     comb.age = 38,
                     adol= 1)
pred.het <- predict(cond.oo.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het

# Init HIV Status ---------------------------------------------------------

# d1 <- select(d, race.cat3, cityYN, age, hiv2)
#
# hiv.mod <- glm(hiv2 ~ age + cityYN + as.factor(race.cat3) + cityYN*as.factor(race.cat3),
#                data = d1, family = binomial())
# summary(hiv.mod)
# x <- expand.grid(age = 15:65, race.cat3 = 1:3, cityYN = 0:1)
# pred <- predict(hiv.mod, newdata = x)
# pred <- cbind(x, est = plogis(pred))
# pred
#
# ggplot(pred, aes(age, est, color = as.factor(race.cat3), lty = as.factor(cityYN))) +
#   geom_line() +
#   scale_color_viridis_d() +
#   theme_minimal()

###########################################################
##STI testing probs
dst.het <- select(d, sex, age.grp, race, deg.main.c.het, deg.casl.c.het, STDTST12) %>%
  filter(STDTST12 %in% 0:1)
head(dst.het, 25)
nrow(dst.het)


# binomial Model
stdtst.mod.het <- glm(STDTST12 ~ as.factor(sex) + as.factor(age.grp) + as.factor(race) +
                        as.factor(deg.main.c.het) + as.factor(deg.casl.c.het),
                    family = binomial(), data = dst.het)
summary(stdtst.mod.het)

x.het <- expand.grid(sex = 2,
                     age.grp = 1:4,
                     race = 1:3,
                     deg.main.c.het = 0,
                     deg.casl.c.het = 1)

pred.het <- predict(stdtst.mod.het, newdata = x.het, type = "response", se.fit = FALSE)
pred.stdtst.het <- cbind(x.het, pred.het)
pred.stdtst.het

pred.stdtst.het <- cbind(x.het, (-(1-pred.het)^(1/52)) +1)
pred.stdtst.het

summary(dst.het$STDTST12[dst.het$race == 1 & dst.het$sex == 1])
summary(dst.het$STDTST12[dst.het$race == 2 & dst.het$sex == 1])
summary(dst.het$STDTST12[dst.het$race == 3 & dst.het$sex == 1])
summary(dst.het$STDTST12[dst.het$race == 1 & dst.het$sex == 2])
summary(dst.het$STDTST12[dst.het$race == 2 & dst.het$sex == 2])
summary(dst.het$STDTST12[dst.het$race == 3 & dst.het$sex == 2])

summary(dst.het$STDTST12[dst.het$sex == 2])
summary(dst.het$STDTST12[dst.het$age.grp == 1 & dst.het$sex == 2])
summary(dst.het$STDTST12[dst.het$race == 2 & dst.het$sex == 2])

summary(dst.het$STDTST12[dst.het$sex == 1])

summary(dst.het$STDTST12)

# Save Out File -----------------------------------------------------------

out <- list()

out$time.unit <- 7
out$acts.mod.het <- acts.mod.het
out$cond.mc.mod.het <- cond.mc.mod.het
out$cond.oo.mod.het <- cond.oo.mod.het
out$stdtst.mod.het <- stdtst.mod.het
fn <- paste("data/EpiStats_2011", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("est/EpiStats_2011", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("~/CAMP-HET-STI/scenarios/calib/est/EpiStats_2011", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("~/CAMP-HET-STI/scenarios/Model testing/est/EpiStats_2011", "rda", sep = ".")
saveRDS(out, file = fn)


rm(list=ls())

########### EpiStats for 2019  ######
#####################################

#Pull in the network data from 2019
setwd("~/CAMP-HET-STI")
load("~/CAMP-HET-STI/NSFG/data/d_het_2019.rda")
load("~/CAMP-HET-STI/NSFG/data/l_het_2019.rda")
d<-d_het
l<-l_het

## Packages ##
library("tidyverse")


names(l)
names(d)


# Data Processing ---------------------------------------------------------

# Age
table(l$age, useNA = "always")
table(l$p_age, useNA = "always")

l$comb.age <- l$age + l$p_age
l$diff.age <- abs(l$age - l$p_age)



# redistribute NAs in proportion to non-missing partner races
#make race combination for partnerships
l$race.combo <- rep(NA, nrow(l))
l$race.combo[l$race3 == 1 & l$p_race3 == 1] <- 1
l$race.combo[l$race3 == 1 & l$p_race3 %in% 2:3] <- 2
l$race.combo[l$race3 == 2 & l$p_race3 %in% c(1, 3)] <- 3
l$race.combo[l$race3 == 2 & l$p_race3 == 2] <- 4
l$race.combo[l$race3 == 3 & l$p_race3 %in% 1:2] <- 5
l$race.combo[l$race3 == 3 & l$p_race3 == 3] <- 6

table(l$race.combo)

#rename race3.
l$race<-l$race3
d$race<-d$race3

# Act Rates ---------------------------------------------------------------

# acts/per week/per partnership for main and casual partnerships MSM and HET


# Pull Data
la.het <- select(l, ptype, duration, comb.age,
                 race.combo, RVI, IVI, adol,
                 acts = vag.acts.week, cp.acts = vag.acts.week.cp) %>%
  filter(ptype %in% 1:2) %>%
  filter(RVI == 1 | IVI == 1)
la.het <- select(la.het, -c(RVI, IVI))
head(la.het, 25)
nrow(la.het)


# Poisson Model
acts.mod.het <- glm(floor(acts*52) ~ duration + I(duration^2) + as.factor(race.combo) +
                      as.factor(ptype) + duration*as.factor(ptype) + comb.age + I(comb.age^2) + as.factor(adol),
                    family = poisson(), data = la.het)
summary(acts.mod.het)

x.het <- expand.grid(duration = 16,
                     ptype = 2,
                     race.combo = 1:6,
                     comb.age = 36,
                     adol = 0)
pred.het <- predict(acts.mod.het, newdata = x.het, type = "response", se.fit = FALSE)
pred.acts.het <- cbind(x.het, pred.het = pred.het/52)
pred.acts.het

summary(la.het$acts[la.het$ptype==1])
summary(la.het$acts[la.het$ptype==2])

# Condom Use // Main Casual -----------------------------------------------

#HET
par(mar = c(3,3,1,1), mgp = c(2,1,0))
ggplot(la.het, aes(acts,cp.acts)) +
  geom_point()

summary(la.het$cp.acts)

la.het$prob.cond <- la.het$cp.acts / la.het$acts
head(la.het, 25)

table(la.het$acts, useNA = "always")

hist(la.het$prob.cond)
table(la.het$prob.cond)
summary(la.het$prob.cond)

summary(la.het$prob.cond[la.het$ptype == 1])
summary(la.het$prob.cond[la.het$ptype == 2])

la.het$any.cond <- ifelse(la.het$prob.cond > 0, 1, 0)
#GET THE CONDOM USE RATES BY PTYPE
summary(la.het$any.cond[la.het$ptype == 1])
summary(la.het$any.cond[la.het$ptype == 2])
summary(la.het$any.cond)

summary(la.het$any.cond[la.het$race.combo == 1])
summary(la.het$any.cond[la.het$race.combo == 2])
summary(la.het$any.cond[la.het$race.combo == 3])
summary(la.het$any.cond[la.het$race.combo == 4])
summary(la.het$any.cond[la.het$race.combo == 5])
summary(la.het$any.cond[la.het$race.combo == 6])

la.het$never.cond <- ifelse(la.het$prob.cond == 0, 1, 0)
table(la.het$never.cond)

cond.mc.mod.het <- glm(any.cond ~ duration + I(duration^2) + as.factor(race.combo) +
                         as.factor(ptype) + duration*as.factor(ptype) + comb.age + I(comb.age^2) + as.factor(adol),
                       family = binomial(), data = la.het)
summary(cond.mc.mod.het)

x.het <- expand.grid(duration = 26,
                     ptype = 1,
                     race.combo = 1:6,
                     comb.age = 44,
                     adol = 0)
pred.het <- predict(cond.mc.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het

#GET PREDITIONS FOR CASUAL PARTNERSHIPS
x.het <- expand.grid(duration = 26,
                     ptype = 2,
                     race.combo = 1:6,
                     comb.age = 44,
                     adol = 0)
pred.het <- predict(cond.mc.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het


tiff(file = "out/condom.probs_2019.tiff", height = 8, width = 4, units = "in", res = 250,
    title = "Condom Use Probability Distribution_2019"
)
par(mar = c(4,4,1,1), mgp = c(2,1,0), mfrow = c(1,1))

hist(la.het$prob.cond, main="Hetersexual Condom Use")
#hist(la.msm$prob.cond, main="MSM Condom Use")

dev.off()


# Condom Use // Inst ------------------------------------------------------


#HET
lb.het <- select(l, ptype, comb.age, race.combo, adol,
                 RVI, IVI, condls) %>%
  filter(ptype == 3) %>%
  filter(RVI == 1 | IVI == 1)
head(lb.het, 40)
table(lb.het$ptype)
table(lb.het$condls)

lb.het$prob.cond <- rep(NA, nrow(lb.het))

cond.oo.mod.het <- glm(condls ~ as.factor(race.combo) +
                         comb.age + I(comb.age^2) + as.factor(adol),
                       family = binomial(), data = lb.het)
summary(cond.oo.mod.het)

x.het <- expand.grid(race.combo = 1:6,
                     comb.age = 44,
                     adol= 0)
pred.het <- predict(cond.oo.mod.het, newdata = x.het, type = "response")
pred.cond.het <- cbind(x.het, pred.het)
pred.cond.het

# Init HIV Status ---------------------------------------------------------

# d1 <- select(d, race.cat3, cityYN, age, hiv2)
#
# hiv.mod <- glm(hiv2 ~ age + cityYN + as.factor(race.cat3) + cityYN*as.factor(race.cat3),
#                data = d1, family = binomial())
# summary(hiv.mod)
# x <- expand.grid(age = 15:65, race.cat3 = 1:3, cityYN = 0:1)
# pred <- predict(hiv.mod, newdata = x)
# pred <- cbind(x, est = plogis(pred))
# pred
#
# ggplot(pred, aes(age, est, color = as.factor(race.cat3), lty = as.factor(cityYN))) +
#   geom_line() +
#   scale_color_viridis_d() +
#   theme_minimal()

###########################################################
##STI testing probs
dst.het <- select(d, sex, age.grp, race, deg.main.c.het, deg.casl.c.het, STDTST12) %>%
  filter(STDTST12 %in% 0:1)
head(dst.het, 25)
nrow(dst.het)


# binomial Model
stdtst.mod.het <- glm(STDTST12 ~ as.factor(sex) + as.factor(age.grp) + as.factor(race) +
                        as.factor(deg.main.c.het) + as.factor(deg.casl.c.het),
                      family = binomial(), data = dst.het)
summary(stdtst.mod.het)

x.het <- expand.grid(sex = 2,
                     age.grp = 1:4,
                     race = 1:3,
                     deg.main.c.het = 0,
                     deg.casl.c.het = 1)

pred.het <- predict(stdtst.mod.het, newdata = x.het, type = "response", se.fit = FALSE)
pred.stdtst.het <- cbind(x.het, pred.het)
pred.stdtst.het

pred.stdtst.het <- cbind(x.het, (-(1-pred.het)^(1/52)) +1)
pred.stdtst.het

summary(dst.het$STDTST12[race == 1 & sex == 1])
summary(dst.het$STDTST12[race == 2 & sex == 1])
summary(dst.het$STDTST12[race == 3 & sex == 1])
summary(dst.het$STDTST12[race == 1 & sex == 2])
summary(dst.het$STDTST12[race == 2 & sex == 2])
summary(dst.het$STDTST12[race == 3 & sex == 2])

summary(dst.het$STDTST12[dst.het$sex == 2])
summary(dst.het$STDTST12[dst.het$sex == 1])

summary(dst.het$STDTST12)

# Save Out File -----------------------------------------------------------

out <- list()

out$time.unit <- 7
out$acts.mod.het <- acts.mod.het
out$cond.mc.mod.het <- cond.mc.mod.het
out$cond.oo.mod.het <- cond.oo.mod.het
out$stdtst.mod.het <- stdtst.mod.het
fn <- paste("data/EpiStats_2019", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("est/EpiStats_2019", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("~/CAMP-HET-STI/scenarios/calib/est/EpiStats_2019", "rda", sep = ".")
saveRDS(out, file = fn)

fn <- paste("~/CAMP-HET-STI/scenarios/Model testing/est/EpiStats_2019", "rda", sep = ".")
saveRDS(out, file = fn)


rm(list=ls())

