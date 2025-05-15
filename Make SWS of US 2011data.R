# This program will sample data from the wide data files from the NSFG in order to
# create a self-weighted Sample of the US.
library(egonet)
library(EpiModel)
library(tidyverse)


load("/homes/dth2/CAMP 17/NSFG/NSFGsexnet2011_reduced_national.rdata")

#Reset age for 45 year olds to 44 which is the upper limit of the survey
old<-which(NSFGsexnet2011_reduced_national$age==45)
NSFGsexnet2011_reduced_national$age[old]<-44


#Split out the data to males and females
m <- which(NSFGsexnet2011_reduced_national$sex == 1)
f <- which(NSFGsexnet2011_reduced_national$sex == 2)

males <- NSFGsexnet2011_reduced_national[m,]
females <- NSFGsexnet2011_reduced_national[f,]

##ADD role class
males$role.class <- rep(1,length(males$age))
females$role.class <- rep(2,length(females$age))

## we want the pop to go to 45 we are not expanding to older adults in this project.



## Adjust the weights from population weights to proportions
males$weight3 <- males$weight2/(sum(males$weight2))
females$weight3 <- females$weight2/(sum(females$weight2))

##Create the self weighted sample
maledat <- sample_n(males,25294, replace = TRUE, prob = males$weight3)
femaledat <- sample_n(females,24706, replace = TRUE,females$weight3)


femaledat$ego<-1:24706
maledat$ego<-24707:50000

het_national <- rbind(maledat, femaledat)
het_national$ego <-het_national$ego

##make new age.grp for full age range
het_national$age.grp <- ifelse(het_national$age < 19, 1,
                            ifelse(het_national$age >= 19 & het_national$age < 25, 2,
                            ifelse(het_national$age >= 25 & het_national$age < 35, 3,
                                   ifelse(het_national$age >= 35 & het_national$age < 45, 4,NA))))




het_national$race3 <- het_national$RACE3
het_national = subset(het_national, select = -c(RACE3, Agecat) )
het_national$sqrt.age <- sqrt(het_national$age)


##Limit STD testing indicators to those with a no or yes response
het_national$STDTST12 <- ifelse(het_national$STDTST12 > 1,NA,het_national$STDTST12)


save(het_national,file = "het_national_2011.rda")

### define the ptype as Spouse/cohab casual or OT
het_national$ptype1 <- rep(NA,length(het_national$ego))
het_national$ptype2 <- rep(NA,length(het_national$ego))
het_national$ptype3 <- rep(NA,length(het_national$ego))

#spouse cohab
spc <- which(het_national$optype1 <= 2 & het_national$active1 ==1)
het_national$ptype1[spc] <- 1

spc <- which(het_national$optype2 <= 2 & het_national$active2 ==1)
het_national$ptype2[spc] <- 1

spc <- which(het_national$optype3 <= 2 & het_national$active3 ==1)
het_national$ptype3[spc] <- 1

#casual
cp <- which(het_national$optype1 <= 5 & het_national$optype1 > 2 & het_national$active1 ==1)
het_national$ptype1[cp] <- 2

cp <- which(het_national$optype2 <= 5 & het_national$optype2 > 2 & het_national$active2 ==1)
het_national$ptype2[cp] <- 2

cp <- which(het_national$optype3 <= 5 & het_national$optype3 > 2 & het_national$active3 ==1)
het_national$ptype3[cp] <- 2


#One-time
#Select partners with length 0 and happened in the last month
ot <- which(het_national$len1 == 0 & het_national$dls1 <=1 & het_national$active1 == 0)
het_national$ptype1[ot] <- 3

ot <- which(het_national$len2 == 0 & het_national$dls2 <=1 & het_national$active2 == 0)
het_national$ptype2[ot] <- 3

ot <- which(het_national$len3 == 0 & het_national$dls3 <=1  & het_national$active3 == 0)
het_national$ptype3[ot] <- 3

#Add variable for specifying het Vs MSM network participation
het_national$het<-rep(1,length(het_national$ego))
het_national$msm<-rep(0,length(het_national$ego))

het_national <- transform(het_national,
                   sex=as.character(sex),
                   race3=as.character(race3),
                   prace1=as.character(prace1),
                   prace2=as.character(prace2),
                   prace3=as.character(prace3),
                   het=as.character(het),
                   msm=as.character(msm),
                   role.class=as.character(role.class),
                   stringsAsFactors=FALSE
)

#get a count of main and casl.

het_national$deg.main.het <- rep(0,length(het_national$ego))
het_national$deg.casl.het <- rep(0,length(het_national$ego))
het_national$deg.tot.het <- rep(0,length(het_national$ego))
het_national$deg.tot.c.het <- rep(0,length(het_national$ego))
het_national$deg.main.msm <- rep(0,length(het_national$ego))
het_national$deg.casl.msm <- rep(0,length(het_national$ego))
het_national$deg.tot.msm <- rep(0,length(het_national$ego))
het_national$deg.tot.c.msm <- rep(0,length(het_national$ego))

c1 <- which(het_national$ptype1 == 1 & het_national$active1 == 1)
c2 <- which(het_national$ptype2 == 1 & het_national$active2 == 1)
c3 <- which(het_national$ptype3 == 1 & het_national$active3 == 1)

het_national$deg.main.het[c1] <- het_national$deg.main.het[c1]+1
het_national$deg.main.het[c2] <- het_national$deg.main.het[c2]+1
het_national$deg.main.het[c3] <- het_national$deg.main.het[c3]+1
het_national$deg.main.het <- pmin(2,het_national$deg.main.het)

p1 <- which(het_national$ptype1 == 2 & het_national$active1 == 1)
p2 <- which(het_national$ptype2 == 2 & het_national$active2 == 1)
p3 <- which(het_national$ptype3 == 2 & het_national$active3 == 1)

het_national$deg.casl.het[p1] <- het_national$deg.casl.het[p1]+1
het_national$deg.casl.het[p2] <- het_national$deg.casl.het[p2]+1
het_national$deg.casl.het[p3] <- het_national$deg.casl.het[p3]+1

het_national$deg.tot.het <-het_national$deg.main.het  + het_national$deg.casl.het

het_national$deg.main.c.het <- pmin(1,het_national$deg.main.het)
het_national$deg.casl.c.het <- pmin(1,het_national$deg.casl.het)
het_national$deg.tot.c.het <- pmin(2,het_national$deg.tot.het)
het_national$deg.main.c.msm <- rep(0,length(het_national$age))
het_national$deg.casl.c.msm <- rep(0,length(het_national$age))
het_national$deg.tot.c.msm <- rep(0,length(het_national$age))

# Concurrency
het_national$deg.main.conc.het <- ifelse(het_national$deg.main.het > 1, 1, 0)
het_national$deg.casl.conc.het <- ifelse(het_national$deg.casl.het > 1, 1, 0)

het_national$deg.main.conc.msm <- rep(0,length(het_national$ego))
het_national$deg.casl.conc.msm <- rep(0,length(het_national$ego))


het_national$count.oo.part.trunc.het <- pmax(0,het_national$deg.ot)
het_national$count.oo.part.trunc.msm <- rep(0,length(het_national$deg.ot))

het_national <- het_national[order(het_national$count.oo.part.trunc.het),]

het_national$risk.grp <- apportion_lr(length(het_national$age),1:5,c(.2,.2,.2,.2,.2))
het_national <- het_national[order(het_national$ego),]


##Calculate the act rate in main and casual for just those with a singe partnership of one type lasting at least len >=1.
#Act rates will come from David's longitudinal study!!!!
m<-which(het_national$ptype1 == 1 & het_national$len1 >= 1 & het_national$deg.tot.het == 1)
c<-which(het_national$ptype1 == 2 & het_national$len1 >= 1 & het_national$deg.tot.het == 1)

het_national$vag.acts.week<-rep(NA,length(het_national$sex))
het_national$vag.acts.week.cp<-rep(NA,length(het_national$sex))

het_national$vag.acts.week[m] <- het_national$sex4wk[m]
het_national$vag.acts.week.cp[m] <- het_national$cond4wk[m]

het_national$vag.acts.week[c] <- het_national$sex4wk[c]
het_national$vag.acts.week.cp[c] <- het_national$cond4wk[c]

het_national$vag.acts.week.cp <- ifelse(het_national$vag.acts.week.cp > het_national$vag.acts.week, het_national$vag.acts.week,
                                     het_national$vag.acts.week.cp)

het_national$vag.acts.week <- het_national$vag.acts.week/4
het_national$vag.acts.week.cp <- het_national$vag.acts.week.cp/4


##Compute time since last HIV test
het_national$cmhivtst <- ifelse(het_national$cmhivtst==9998 | het_national$cmhivtst==9999 ,NA,het_national$cmhivtst)
het_national$tlnt <- (het_national$cmintvw - het_national$cmhivtst)
het_national$tlnt <- het_national$tlnt*(52/12)


##Make the long pship file

dfl <- reshape_edgelist(het_national, delete_empty = c("active", "ptype", "len"), all = FALSE)

act1 <- exclude_and_report(dfl, list("active==1",
                                     "once==0"))

act0 <- exclude_and_report(dfl, list("once==1"
))

alters <- rbind(act1, act0)
alters <- transform(alters, onceLastMonth=ifelse(once==1 & dls==0,1,0))

##Relationship length is in months and we need weeks
alters$len <- (alters$len/12)*52
alters$duration <- alters$len

# These are all heterosexual ties
alters <- within(alters, {
  todelete_sex <- sex
  p_sex <- ifelse(todelete_sex==1, 2, 1)
  role.class <- ifelse(sex==1,1,2)
  p_race3 <- prace
  p_age <- page
  p_condls <- condls
})

with(alters, table(todelete_sex, sex))

table(alters$ptype, alters$once, useNA='ifany')
rows<-which(alters$ptype==1 & alters$once==1)
alters$once[rows]<-0

##Sex type indicators

alters$RAI <-rep(0,length(alters$ego))
alters$IAI <-rep(0,length(alters$ego))
alters$RVI <-rep(0,length(alters$ego))
alters$RVI[alters$sex==2] <-1
alters$IVI <-rep(0,length(alters$ego))
alters$IVI[alters$sex==1] <-1
alters$anal.acts.week.cp <-rep(0,length(alters$ego))
#alters$vag.acts.week <-rep(0,length(alters$ego))


#There are missing values for 467 partner race variables and 286 partner age variable
#Impute the missing variables


race.mix.prop <- prop.table(table(alters$race3,alters$p_race3))
b <- which(alters$race3 == 1 & is.na(alters$p_race3) == TRUE)
h <- which(alters$race3 == 2 & is.na(alters$p_race3) == TRUE)
o <- which(alters$race3 == 3 & is.na(alters$p_race3) == TRUE)

alters$p_race3[b] <- sample(c(1:3),length(b), replace = TRUE, prob= race.mix.prop[1,])
alters$p_race3[h] <- sample(c(1:3),length(h), replace = TRUE, prob= race.mix.prop[2,])
alters$p_race3[o] <- sample(c(1:3),length(o), replace = TRUE, prob= race.mix.prop[3,])

for(i in 15:44){

  bm.m <- which(alters$race3 == 1 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == TRUE)
  bm.k <- which(alters$race3 == 1 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[bm.k],length(bm.m),replace=TRUE)
  alters$p_age[bm.m]<-j

  hm.m <- which(alters$race3 == 2 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == TRUE)
  hm.k <- which(alters$race3 == 2 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[hm.k],length(hm.m),replace=TRUE)
  alters$p_age[hm.m]<-j

  om.m <- which(alters$race3 == 3 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == TRUE)
  om.k <- which(alters$race3 == 3 & alters$sex == 1 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[om.k],length(om.m),replace=TRUE)
  alters$p_age[om.m]<-j

  bf.m <- which(alters$race3 == 1 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == TRUE)
  bf.k <- which(alters$race3 == 1 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[bf.k],length(bf.m),replace=TRUE)
  alters$p_age[bf.m]<-j

  hf.m <- which(alters$race3 == 2 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == TRUE)
  hf.k <- which(alters$race3 == 2 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[hf.k],length(hf.m),replace=TRUE)
  alters$p_age[hf.m]<-j

  of.m <- which(alters$race3 == 3 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == TRUE)
  of.k <- which(alters$race3 == 3 & alters$sex == 2 & alters$age == i & is.na(alters$p_age) == FALSE)
  j <- sample(alters$p_age[of.k],length(of.m),replace=TRUE)
  alters$p_age[of.m]<-j


}

alters$p_sqrt.age <- sqrt(alters$p_age)
alters$p_age.grp <- ifelse(alters$p_age < 19, 1,
                            ifelse(alters$p_age >= 19 & alters$p_age < 25, 2,
                                   ifelse(alters$p_age >= 25 & alters$p_age < 35, 3,
                                          ifelse(alters$p_age >= 35, 4,NA))))


alters$ONCE <- ifelse(alters$ptype==3,1,0)
alters$ONGOING <- ifelse((alters$ptype==1 | alters$ptype==2),1,0)

alters$anal.acts.week <- rep(0,length(alters$p_age.grp))
alters$RECUAI <- rep(NA,length(alters$p_age.grp))
alters$INSUAI <- rep(NA,length(alters$p_age.grp))

alters$adol<- rep(0,length(alters$p_age.grp))
alters$adol <- ifelse(alters$age < 18 | alters$p_age < 18 ,1,0)

# alters4 %>%
#   rename(
#     p_age = age,
#     sepal_width = Sepal.Width
#   )

# Spouses/Cohabs
#alters1 <- subset(alters,
#                  active==1 & ptype==1,
#                  select=c('ego', 'sex','age', 'sqrt.age', 'race3',
#                           'het', 'msm', 'duration'))

# Others
#alters2 <- subset(alters,
#                  active==1 & ptype==2,
#                  select=c('ego', 'sex','age', 'sqrt.age', 'race3',
#                           'het', 'msm', 'duration'))

# One-Times
#alters3 <- subset(alters,
#                  once==1 & active!=1,
#                  select=c('ego', 'sex','age', 'sqrt.age', 'race3',
#                           'het', 'msm', 'duration'))


# Check to see is the alters are all captured
#(nsfg_alt <- nrow(alters))
#nsfg_alt_split <- c(nrow(alters1), nrow(alters2), nrow(alters3))

#sum(nsfg_alt_split)

#Add age 15 to use as a factor
het_national$age15<-ifelse(het_national$age==15,1,0)

save(het_national,file = "het_national_2011_clean.rda")

# Make a list object
#het_national_egodata <- list(het_egos=subset(het_national, select=c('weight3', 'ego', 'sex', 'age', 'sqrt.age',
#                                                 'race3', 'het', 'msm', 'role.class', 'deg.main',
#                                                 'deg.casl', 'risk.grp', 'age.grp')),
#                     het_altersCohab=alters1, het_alterscasl=alters2,
#                     het_altersOT=alters3)

#Store wide and long as list

d_het<-subset(het_national,select=c('ego', 'age', 'sqrt.age', 'age.grp', 'race3', 'sex', 'het', 'msm', 'role.class',
                                 'risk.grp', 'deg.main.het', 'deg.casl.het', 'deg.tot.het', 'deg.main.msm', 'deg.casl.msm', 'deg.tot.msm',
                                 'deg.main.conc.het', 'deg.casl.conc.het', 'deg.main.conc.msm', 'deg.casl.conc.msm',
                                 'count.oo.part.trunc.het', 'count.oo.part.trunc.msm','age15',
                                 'deg.main.c.het', 'deg.casl.c.het', 'deg.tot.c.het', 'deg.main.c.msm', 'deg.casl.c.msm',
                                 'deg.tot.c.msm', 'evhivtst', 'tlnt', 'STDTST12'))

l_het<-subset(alters,select=c('ego', 'age', 'sqrt.age', 'age.grp', 'race3', 'sex', 'het', 'msm',
                               'RAI', 'IAI', 'RVI', 'IVI', 'p_age', 'p_sqrt.age', 'p_age.grp', 'p_race3',
                               'p_sex', 'ptype', 'duration',
                               'anal.acts.week', 'anal.acts.week.cp', 'vag.acts.week', 'vag.acts.week.cp',
                               'ONCE', 'ONGOING', 'RECUAI','INSUAI','condls', 'adol'))

save(d_het,file = "/homes/dth2/CAMP 17/NSFG/data/d_het_2011.rda")
save(l_het,file = "/homes/dth2/CAMP 17/NSFG/data/l_het_2011.rda")



#####################################################################################################
#####################################################################################################
