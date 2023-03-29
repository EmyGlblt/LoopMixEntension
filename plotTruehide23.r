library(spatstat)
library(lattice)
library(sp)
library(maptools)
library(raster)
library(geostatsp)
library(rgdal)
library(caret)
library(gridExtra)
library(grid)
library(latticeExtra)


# saved data and environmental info
load('TrueM.RDATA')


# simulation result (Hidding from the truth)
#load('LMtrue22.RDATA')  # summarized 17 times 50 summary : 850 simulation total
load('LMtrue22All.RDATA')  # summarized 32 times with 50 or less to reach 1000 simulations

#load('LMtrue22b.RDATA')

### quick plot of the species
par(mfrow=c(2,2))
plot(Mcarb.pp$x, Mcarb.pp$y)
plot(Mcog.pp$x, Mcog.pp$y)
plot(Msch.pp$x, Msch.pp$y)
plot(test.ppp$x, test.ppp$y)
par(mfrow=c(1,1))

yrange= c(min(Mcarb.pp$y, Msch.pp$y, Mcog.pp$y, test.ppp$y), max(Mcarb.pp$y, Msch.pp$y, Mcog.pp$y, test.ppp$y))
xrange= c(min(Mcarb.pp$x, Msch.pp$x, Mcog.pp$x, test.ppp$x), max(Mcarb.pp$x, Msch.pp$x, Mcog.pp$x, test.ppp$x))

plot(Mcarb.pp, pch=16, cex = 1.1)
plot(Mcarb.pp, add=TRUE, col="orange", pch=16, cex = 1.1)
plot(Mcog.pp, add = TRUE, col = "purple", pch=17, cex = 1.1)
plot(Msch.pp, add = TRUE, col = "turquoise3", pch=15, cex = 1.1)
plot(test.ppp, add = TRUE, chars="?", col = "black", cex = 0.8)



# Weights - classification
#...................................................................................................
#knn
A.knn20 = array((unlist(knn20)), dim = c(dim(knn20[[1]]), 20))

knn20.mean = apply(A.knn20, c(1,2), mean)
colnames(knn20.mean) = colnames(knn20[[1]])

knn20.qlow = apply(A.knn20, c(1,2), function(x) quantile(x, prob=0.025))
colnames(knn20.qlow) = colnames(knn20[[1]])

knn20.qup = apply(A.knn20, c(1,2), function(x) quantile(x, prob=0.975))
colnames(knn20.qup) = colnames(knn20[[1]])



A.knn50 = array((unlist(knn50)), dim = c(dim(knn50[[1]]), 20))

knn50.mean = apply(A.knn50, c(1,2), mean)
colnames(knn50.mean) = colnames(knn50[[1]])

knn50.qlow = apply(A.knn50, c(1,2), function(x) quantile(x, prob=0.025))
colnames(knn50.qlow) = colnames(knn50[[1]])

knn50.qup = apply(A.knn50, c(1,2), function(x) quantile(x, prob=0.975))
colnames(knn50.qup) = colnames(knn50[[1]])


A.knn80 = array((unlist(knn80)), dim = c(dim(knn80[[1]]), 20))

knn80.mean = apply(A.knn80, c(1,2), mean)
colnames(knn80.mean) = colnames(knn80[[1]])

knn80.qlow = apply(A.knn80, c(1,2), function(x) quantile(x, prob=0.025))
colnames(knn80.qlow) = colnames(knn80[[1]])

knn80.qup = apply(A.knn80, c(1,2), function(x) quantile(x, prob=0.975))
colnames(knn80.qup) = colnames(knn80[[1]])


# LT
A.LT20 = array((unlist(LT20)), dim = c(dim(LT20[[1]]), 20))

LT20.mean = apply(A.LT20, c(1,2), mean)
colnames(LT20.mean) = colnames(LT20[[1]])

LT20.qlow = apply(A.LT20, c(1,2), function(x) quantile(x, prob=0.025))
colnames(LT20.qlow) = colnames(LT20[[1]])

LT20.qup = apply(A.LT20, c(1,2), function(x) quantile(x, prob=0.975))
colnames(LT20.qup) = colnames(LT20[[1]])



A.LT50 = array((unlist(LT50)), dim = c(dim(LT50[[1]]), 20))

LT50.mean = apply(A.LT50, c(1,2), mean)
colnames(LT50.mean) = colnames(LT50[[1]])

LT50.qlow = apply(A.LT50, c(1,2), function(x) quantile(x, prob=0.025))
colnames(LT50.qlow) = colnames(LT50[[1]])

LT50.qup = apply(A.LT50, c(1,2), function(x) quantile(x, prob=0.975))
colnames(LT50.qup) = colnames(LT50[[1]])


A.LT80 = array((unlist(LT80)), dim = c(dim(LT80[[1]]), 20))

LT80.mean = apply(A.LT80, c(1,2), mean)
colnames(LT80.mean) = colnames(LT80[[1]])

LT80.qlow = apply(A.LT80, c(1,2), function(x) quantile(x, prob=0.025))
colnames(LT80.qlow) = colnames(LT80[[1]])

LT80.qup = apply(A.LT80, c(1,2), function(x) quantile(x, prob=0.975))
colnames(LT80.qup) = colnames(LT80[[1]])


# Plot of the weigths for each observation
#...................................................................................................
par(mfrow=c(2,3),     # 2x4 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 2, 3, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)

plot(x=seq_along(knn20.mean[,1]), y=knn20.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="", ylab="weight", main="knn method - 20%")
points(x=seq_along(knn20.mean[,2]), y=knn20.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(knn20.mean[,3]), y=knn20.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))


plot(x=seq_along(knn50.mean[,1]), y=knn50.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="", ylab="weight", main="knn method - 50%")
points(x=seq_along(knn50.mean[,2]), y=knn50.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(knn50.mean[,3]), y=knn50.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))

plot(x=seq_along(knn80.mean[,1]), y=knn80.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="", ylab="weight", main="knn method - 80%")
points(x=seq_along(knn80.mean[,2]), y=knn80.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(knn80.mean[,3]), y=knn80.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))
legend("topright", c("Mcarb", "Mcog", "Msch"), col = c("orange", "purple", "Turquoise3"),
       pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)


plot(x=seq_along(LT20.mean[,1]), y=LT20.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="observations", ylab="weight", main="LoopT method - 20%")
points(x=seq_along(LT20.mean[,2]), y=LT20.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(LT20.mean[,3]), y=LT20.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))

plot(x=seq_along(LT50.mean[,1]), y=LT50.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="observations", ylab="weight", main="LoopT method - 50%")
points(x=seq_along(LT50.mean[,2]), y=LT50.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(LT50.mean[,3]), y=LT50.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))

plot(x=seq_along(LT80.mean[,1]), y=LT80.mean[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="observations", ylab="weight", main="LoopT method - 80%")
points(x=seq_along(LT80.mean[,2]), y=LT80.mean[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(LT80.mean[,3]), y=LT80.mean[,3], col = "Turquoise3", pch=17, ylim=c(0,1))



#...................................................................................................


# plot all test of hiding for LB
# with color based on membership classification

par(mfrow=c(3,3),     # 2x4 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 2), # space for one row of text at ticks and to separate plots
    mgp = c(1.5, 0.5, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)

# Known points
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 0.9, 
     main="True species", ylim=yrange, xlim=xrange, xlab="", ylab="Y")
points(Mcarb.pp$x, Mcarb.pp$y, col="orange", pch=16, cex = 0.9)
points(Mcog.pp$x, Mcog.pp$y, col = "purple", pch=17, cex = 0.9)
points(Msch.pp$x, Msch.pp$y, col = "turquoise3", pch=15, cex = 0.9)
legend("topright" ,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=1.2,
       pch=c(16,17,15), bty = "n")






# for 20% hidden
#...........................................
# knn
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="knn 20", xlim=xrange, xlab="", ylab="")

max_Wk = apply(knn20.mean, 1, which.max)
max_Wk.vec = (c(sum(max_Wk== 1), sum(max_Wk== 2), sum(max_Wk== 3))) 


kcarb20.x = true.ppp$x[max_Wk== 1]
kcarb20.y = true.ppp$y[max_Wk== 1]

kcog20.x = true.ppp$x[max_Wk== 2]
kcog20.y = true.ppp$y[max_Wk== 2]

ksch20.x = true.ppp$x[max_Wk== 3]
ksch20.y = true.ppp$y[max_Wk== 3]

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

Wk.carb20 = knn20.mean[,1][which(max_Wk==1)]
#W.carb = W.carb[-which(W.carb==1)]
Wk.carb20$Col <- oranpal(5)[as.numeric(cut(Wk.carb20, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

Wk.cog20 = knn20.mean[,2][c(which(max_Wk==2))]
#W.cog = W.cog[-which(W.cog==1)]
Wk.cog20$Col <- medpurp(5)[as.numeric(cut(Wk.cog20, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

Wk.sch20 = knn20.mean[,3][which(max_Wk==3)]
#W.sch = W.sch[-which(W.sch==1)]
Wk.sch20$Col <- tblue(5)[as.numeric(cut(Wk.sch20, breaks = 5))]


points(kcarb20.x, kcarb20.y, pch=16, col = alpha(Wk.carb20$Col, 0.4), cex = 2)
points(kcog20.x, kcog20.y, pch=17, col = alpha(Wk.cog20$Col, 0.4), cex = 2)
points(ksch20.x, ksch20.y, pch=15, col = alpha(Wk.sch20$Col, 0.4), cex = 2)

# LoopT
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="LoopT 20", xlim=xrange, xlab="", ylab="")

max_W = apply(LT20.mean, 1, which.max)
max_W.vec = (c(sum(max_W== 1), sum(max_W== 2), sum(max_W== 3))) 

#max_W[true.ppp$marks=="Unknown"]
#max_Wunk.vec = (c(sum(max_W[Northmix.pppdf$marks=="Unknown"]== 1),
#                  sum(max_W[Northmix.pppdf$marks=="Unknown"]== 2),
#                  sum(max_W[Northmix.pppdf$marks=="Unknown"]== 3)))

tcarb20.x = true.ppp$x[max_W== 1]
tcarb20.y = true.ppp$y[max_W== 1]
New.carb20 = cbind(tcarb20.x, tcarb20.y)

tcog20.x = true.ppp$x[max_W== 2]
tcog20.y = true.ppp$y[max_W== 2]
New.cog20 = cbind(tcog20.x, tcog20.y)

tsch20.x = true.ppp$x[max_W== 3]
tsch20.y = true.ppp$y[max_W== 3]
New.sch20 = cbind(tsch20.x, tsch20.y)

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

W.carb20 = LT20.mean[,1][which(max_W==1)]
#W.carb = W.carb[-which(W.carb==1)]
W.carb20$Col <- oranpal(5)[as.numeric(cut(W.carb20, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

W.cog20 = LT20.mean[,2][c(which(max_W==2))]
#W.cog = W.cog[-which(W.cog==1)]
W.cog20$Col <- medpurp(5)[as.numeric(cut(W.cog20, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

W.sch20 = LT20.mean[,3][which(max_W==3)]
#W.sch = W.sch[-which(W.sch==1)]
W.sch20$Col <- tblue(5)[as.numeric(cut(W.sch20, breaks = 5))]


points(tcarb20.x, tcarb20.y, pch=16, col = alpha(W.carb20$Col, 0.4), cex = 2)
points(tcog20.x, tcog20.y, pch=17, col = alpha(W.cog20$Col, 0.4), cex = 2)
points(tsch20.x, tsch20.y, pch=15, col = alpha(W.sch20$Col, 0.4), cex = 2)




# 50% hidden
#...........................................
# Known points
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, col='white',
     main="True species", ylim=yrange, cex.main=2, xlim=xrange, xlab="", ylab="Y")
points(Mcarb.pp$x, Mcarb.pp$y, col=alpha("orange", 0.4), pch=16, cex = 2)
points(Mcog.pp$x, Mcog.pp$y, col = alpha("purple", 0.4), pch=17, cex = 2)
points(Msch.pp$x, Msch.pp$y, col = alpha("turquoise3", 0.4), pch=15, cex = 2)
legend("topright" ,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=2,
       pch=c(16,17,15), bty = "n")




# for 50
# knn
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="knn 50", xlim=xrange, xlab="", ylab="")

max_Wk = apply(knn50.mean, 1, which.max)
max_Wk.vec = (c(sum(max_Wk== 1), sum(max_Wk== 2), sum(max_Wk== 3))) 


kcarb50.x = true.ppp$x[max_Wk== 1]
kcarb50.y = true.ppp$y[max_Wk== 1]

kcog50.x = true.ppp$x[max_Wk== 2]
kcog50.y = true.ppp$y[max_Wk== 2]

ksch50.x = true.ppp$x[max_Wk== 3]
ksch50.y = true.ppp$y[max_Wk== 3]

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

Wk.carb50 = knn50.mean[,1][which(max_Wk==1)]
Wk.carb50$Col <- oranpal(5)[as.numeric(cut(Wk.carb50, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

Wk.cog50 = knn50.mean[,2][c(which(max_Wk==2))]
Wk.cog50$Col <- medpurp(5)[as.numeric(cut(Wk.cog50, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

Wk.sch50 = knn50.mean[,3][which(max_Wk==3)]
Wk.sch50$Col <- tblue(5)[as.numeric(cut(Wk.sch50, breaks = 5))]


points(kcarb50.x, kcarb50.y, pch=16, col = alpha(Wk.carb50$Col, 0.4), cex = 2)
points(kcog50.x, kcog50.y, pch=17, col = alpha(Wk.cog50$Col, 0.4), cex = 2)
points(ksch50.x, ksch50.y, pch=15, col = alpha(Wk.sch50$Col, 0.4), cex = 2)

# LoopT
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="LoopT 50", xlim=xrange, xlab="", ylab="")

max_W = apply(LT50.mean, 1, which.max)
max_W.vec = (c(sum(max_W== 1), sum(max_W== 2), sum(max_W== 3))) 


tcarb50.x = true.ppp$x[max_W== 1]
tcarb50.y = true.ppp$y[max_W== 1]
New.carb50 = cbind(tcarb50.x, tcarb50.y)

tcog50.x = true.ppp$x[max_W== 2]
tcog50.y = true.ppp$y[max_W== 2]
New.cog50 = cbind(tcog50.x, tcog50.y)

tsch50.x = true.ppp$x[max_W== 3]
tsch50.y = true.ppp$y[max_W== 3]
New.sch50 = cbind(tsch50.x, tsch50.y)

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

W.carb50 = LT50.mean[,1][which(max_W==1)]
#W.carb = W.carb[-which(W.carb==1)]
W.carb50$Col <- oranpal(5)[as.numeric(cut(W.carb50, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

W.cog50 = LT50.mean[,2][c(which(max_W==2))]
#W.cog = W.cog[-which(W.cog==1)]
W.cog50$Col <- medpurp(5)[as.numeric(cut(W.cog50, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

W.sch50 = LT50.mean[,3][which(max_W==3)]
#W.sch = W.sch[-which(W.sch==1)]
W.sch50$Col <- tblue(5)[as.numeric(cut(W.sch50, breaks = 5))]


points(tcarb50.x, tcarb50.y, pch=16, col = alpha(W.carb50$Col, 0.4), cex = 2)
points(tcog50.x, tcog50.y, pch=17, col = alpha(W.cog50$Col, 0.4), cex = 2)
points(tsch50.x, tsch50.y, pch=15, col = alpha(W.sch50$Col, 0.4), cex = 2)



# 80 % hidden
#...........................................
# Known points
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, col='white',
     main="True species", ylim=yrange, cex.main=2, xlim=xrange, xlab="X", ylab="Y")
points(Mcarb.pp$x, Mcarb.pp$y, col=alpha("orange", 0.4), pch=16, cex = 2)
points(Mcog.pp$x, Mcog.pp$y, col = alpha("purple", 0.4), pch=17, cex = 2)
points(Msch.pp$x, Msch.pp$y, col = alpha("turquoise3", 0.4), pch=15, cex = 2)
legend("topright" ,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=2,
       pch=c(16,17,15), bty = "n")




# for 80
# knn
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="knn 80", xlim=xrange, xlab="X", ylab="")

max_Wk = apply(knn80.mean, 1, which.max)
max_Wk.vec = (c(sum(max_Wk== 1), sum(max_Wk== 2), sum(max_Wk== 3))) 


kcarb80.x = true.ppp$x[max_Wk== 1]
kcarb80.y = true.ppp$y[max_Wk== 1]

kcog80.x = true.ppp$x[max_Wk== 2]
kcog80.y = true.ppp$y[max_Wk== 2]

ksch80.x = true.ppp$x[max_Wk== 3]
ksch80.y = true.ppp$y[max_Wk== 3]

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

Wk.carb80 = knn80.mean[,1][which(max_Wk==1)]
Wk.carb80$Col <- oranpal(5)[as.numeric(cut(Wk.carb80, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

Wk.cog80 = knn80.mean[,2][c(which(max_Wk==2))]
Wk.cog80$Col <- medpurp(5)[as.numeric(cut(Wk.cog80, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

Wk.sch80 = knn80.mean[,3][which(max_Wk==3)]
Wk.sch80$Col <- tblue(5)[as.numeric(cut(Wk.sch80, breaks = 5))]


points(kcarb80.x, kcarb80.y, pch=16, col = alpha(Wk.carb80$Col, 0.4), cex = 2)
points(kcog80.x, kcog80.y, pch=17, col = alpha(Wk.cog80$Col, 0.4), cex = 2)
points(ksch80.x, ksch80.y, pch=15, col = alpha(Wk.sch80$Col, 0.4), cex = 2)

# LoopT
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, cex.main=2, 
     col="white", main="LoopT 80", xlim=xrange, xlab="X", ylab="")

max_W = apply(LT80.mean, 1, which.max)
max_W.vec = (c(sum(max_W== 1), sum(max_W== 2), sum(max_W== 3))) 


tcarb80.x = true.ppp$x[max_W== 1]
tcarb80.y = true.ppp$y[max_W== 1]
New.carb80 = cbind(tcarb80.x, tcarb80.y)

tcog80.x = true.ppp$x[max_W== 2]
tcog80.y = true.ppp$y[max_W== 2]
New.cog80 = cbind(tcog80.x, tcog80.y)

tsch80.x = true.ppp$x[max_W== 3]
tsch80.y = true.ppp$y[max_W== 3]
New.sch80 = cbind(tsch80.x, tsch80.y)

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

W.carb80 = LT80.mean[,1][which(max_W==1)]
#W.carb = W.carb[-which(W.carb==1)]
W.carb80$Col <- oranpal(5)[as.numeric(cut(W.carb80, breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

W.cog80 = LT80.mean[,2][c(which(max_W==2))]
#W.cog = W.cog[-which(W.cog==1)]
W.cog80$Col <- medpurp(5)[as.numeric(cut(W.cog80, breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

W.sch80 = LT80.mean[,3][which(max_W==3)]
#W.sch = W.sch[-which(W.sch==1)]
W.sch80$Col <- tblue(5)[as.numeric(cut(W.sch80, breaks = 5))]


points(tcarb80.x, tcarb80.y, pch=16, col = alpha(W.carb80$Col, 0.4), cex = 2)
points(tcog80.x, tcog80.y, pch=17, col = alpha(W.cog80$Col, 0.4), cex = 2)
points(tsch80.x, tsch80.y, pch=15, col = alpha(W.sch80$Col, 0.4), cex = 2)






# Performances measures
#...................................................................................................

# Accuracy Measures
par(mfrow=c(1,2),     # 2x4 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)

###----------------- RSS


boxplot(cbind(meanRSSknn[,1], meanRSSLoopT[,1],
              meanRSSknn[,2], meanRSSLoopT[,2],
              meanRSSknn[,3], meanRSSLoopT[,3]),
        col = c("yellow","yellow","green","green", "blue", "blue"),
        names = c("knn", "LoopT",
                  "knn", "LoopT",
                  "knn", "LoopT"),
        at = c(1,2, 4,5, 7,8), 
        ylab = "", main="meanRSS")


###----------------- Accuracy

boxplot(cbind(accmatknn[,1], accmatLoopT[,1],
              accmatknn[,2], accmatLoopT[,2],
              accmatknn[,3], accmatLoopT[,3]),
        col = c("yellow","yellow","green","green", "blue", "blue"),
        names = c("knn", "LoopT",
                  "knn", "LoopT",
                  "knn", "LoopT"),
        at = c(1,2, 4,5, 7,8), 
        ylab = "", main="Accuracy")



