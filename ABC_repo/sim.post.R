
##############################################################################################################

load("~/CAMP-HET-STI/scenarios/ABC/data/clR_new.2pct.80sim.rda")

p <- as.data.frame(a$param)
s <- as.data.frame(a$stats)
w <- a$weights


names(p) <- c("ugc.prob", "vgc.prob",
              "uct.prob", "vct.prob",
              "gc.ntx.int", "ct.ntx.int")

#names(s) <- c("diag100K.ct", "diag100K.ct.BF", "diag100K.ct.BM", "diag100K.ct.HF", "diag100K.ct.HM", "diag100K.ct.WF", "diag100K.ct.WM",
#              "diag100K.gc", "diag100K.gc.BF", "diag100K.gc.BM", "diag100K.gc.HF", "diag100K.gc.HM", "diag100K.gc.WF", "diag100K.gc.WM")

names(s) <- c("diag100K.ct", "diag100K.ct.ratio",
         "diag100K.gc", "diag100K.gc.ratio")


#names(s) <- c("diag100K.ct", "diag100K.gc")


mean.s <- apply(s, 2, function(x) sum(x * w))
mean.p <- apply(p, 2, function(x) sum(x * w))


#tar.avg <- c(453.4, 1542.4, 786, 555.7, 190.8, 254.7, 94.4, 106.7, 427.2, 447.4, 56.9, 62.3, 32.1, 28.5)
tar.avg  <- c(453.4, 0.4084, 106.7, .9741)


data.frame(mean.s, tar.avg)

mean.p


par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(2,2))
for (i in 1:ncol(s)) {
  hist(s[, i], col = "bisque2", border = "white", main = names(s)[i])
  abline(v = tar.avg[i], lwd = 2, col = "red")
}

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(3,2))
for (i in 1:ncol(p)) {
  hist(p[, i], col = "bisque2", border = "white", main = names(p)[i])
}

save(mean.p, file = "~/CAMP-HET-STI/scenarios/ABC/data/abc.avg.parms.rda")
save(mean.p, file = "~/CAMP-HET-STI/scenarios/ABC/est/meta.parms.rda")

for (i in seq_along(mean.p)) {
  assign(names(mean.p)[i], unname(mean.p[i]))
}



##################################################################################


##############################################################################################################
## Not matching black estimates
load("~/CAMP-HET-STI/scenarios/ABC/data/clR_nb.1.5pct.80sim.rda")

p <- as.data.frame(a$param)
s <- as.data.frame(a$stats)
w <- a$weights


names(p) <- c("ugc.prob_B", "ugc.prob_H", "ugc.prob_W",
              "vgc.prob_B", "vgc.prob_H", "vgc.prob_W",
              "uct.prob_B", "uct.prob_H", "uct.prob_W",
              "vct.prob_B", "vct.prob_H", "vct.prob_W",
              "gc.ntx.int", "ct.ntx.int")

names(s) <- c("diag100K.ct", "diag100K.ct.HF", "diag100K.ct.HM", "diag100K.ct.WF", "diag100K.ct.WM",
              "diag100K.gc", "diag100K.gc.HF", "diag100K.gc.HM", "diag100K.gc.WF", "diag100K.gc.WM")

mean.s <- apply(s, 2, function(x) sum(x * w))
mean.p <- apply(p, 2, function(x) sum(x * w))


tar.avg <- c(453.4, 555.7, 190.8, 254.7, 94.4, 106.7, 56.9, 62.3, 32.1, 28.5)

data.frame(mean.s, tar.avg)

mean.p


par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(7,2))
for (i in 1:ncol(s)) {
  hist(s[, i], col = "bisque2", border = "white", main = names(s)[i])
  abline(v = tar.avg[i], lwd = 2, col = "red")
}

par(mar = c(3,3,1,1), mgp = c(2,1,0), mfrow = c(7,2))
for (i in 1:ncol(p)) {
  hist(p[, i], col = "bisque2", border = "white", main = names(p)[i])
}

#save(mean.p, file = "~/CAMP-HET-STI/scenarios/ABC/data/abc.avg.parms.rda")
#save(mean.p, file = "~/CAMP-HET-STI/scenarios/ABC/est/meta.parms.rda")

for (i in seq_along(mean.p)) {
  assign(names(mean.p)[i], unname(mean.p[i]))
}



