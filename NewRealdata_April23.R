library(spatstat)
library(lattice)
library(sp)
library(maptools)
library(raster)
library(rgdal)
library(caret)
library(gridExtra)
library(grid)
library(latticeExtra)
library(ppmlasso)

#------------------------------------------------------------------------------
# Real data test
#------------------------------------------------------------------------------

load('RealDataMenv.RDATA')

#------------------------------------------------------------------------------------
#  function ppmlassoMixEngine
#------------------------------------------------------------------------------------

#source("funct_ppmlassoMixEngine.R")
source("LassoTestsimpackage290323.r")


sp_all.list = do.call(list, list(sp_sub, test.ppp))
# function to test
Testmixfunction = ppmlassoMixEngine(Known.ppp = true.ppp, Unknown.ppp = Unk.ppp, 
                                    quadsenv = env.xy,
                                    classif = "hard", initweights = "knn", k=1, 
                                    ppmform=formS, sp.scale=5,
                                    alpha=1, cov.bias=10, kVal = 0, n.fits=50)

# Get the final weights
New.weights = Testmixfunction$New_weights[(true.ppp$n+1):Northmix.pppdf$n,]

# Get the predictions
Mcarb.pred = as.data.frame(Testmixfunction$pred.loc[[1]])
Mcog.pred = as.data.frame(Testmixfunction$pred.loc[[2]])
Msch.pred = as.data.frame(Testmixfunction$pred.loc[[3]])


# species coordinates (known labels)
Mcarb.coord = cbind(env.xy$X, env.xy$Y)
colnames(Mcarb.coord) = c("x", "y")
Mcog.coord = cbind(env.xy$X, env.xy$Y)
colnames(Mcog.coord) = c("x", "y")
Msch.coord = cbind(env.xy$X, env.xy$Y)
colnames(Msch.coord) = c("x", "y")

#levelplot
all.val = c(Mcarb.pred$V1, Mcog.pred$V1, Msch.pred$V1)
col.l <- colorRampPalette(c("gold", "violetred","midnightblue"))
ckey <- list(labels=list(cex=1.2))
minVal=min(Mcarb.pred, Mcog.pred, Msch.pred)
maxVal=max(Mcarb.pred, Mcog.pred, Msch.pred)
Q1Val = mean(quantile(Mcarb.pred$V1)[2], quantile(Mcog.pred$V1)[2], quantile(Msch.pred$V1)[2])

Lcarb=levelplot(Mcarb.pred$V1~Mcarb.coord[,1] + Mcarb.coord[,2],
                at = unique(c(seq(minVal, 5e-3, length=10),
                              seq(5e-3, maxVal, length=30))),
                col.regions = col.l,
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "M. Carb",
                            cex = 2.5))

Lcog=levelplot(Mcog.pred$V1~Mcog.coord[,1] + Mcog.coord[,2],
               col.regions = col.l,
               at = unique(c(seq(minVal, 5e-3, length=10),
                             seq(5e-3, maxVal, length=30))),
               xlab = "", ylab = "",              # remove axis titles
               colorkey=F,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               main = list(label = "M. Cog",
                           cex = 2.5))

Lsch=levelplot(Msch.pred$V1~Msch.coord[,1] + Msch.coord[,2],
               col.regions = col.l,
               at = unique(c(seq(minVal, 5e-3, length=10),
                             seq(5e-3,maxVal, length=30))),
               xlab = "", ylab = "",              # remove axis titles
               colorkey=ckey,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               main = list(label = "M. sch",
                           cex = 2.5))


comb_levObj <- c(Lcarb, Lcog, Lsch, 
                 layout = c(3, 1), merge.legends = T)
update(comb_levObj,
       xlab = c("Mcarb", "Mcog", "Msch"),
       main="Mixture models - knn")


# keep the range of values for comparison later..
minval1=minVal
maxval1=maxVal


#-------------------------------------------------------------------------
# Intensity at the quad. scheme vs covariates values


#intensity response Mcarb - cov
quad.Mcarb = data.frame(x = quads.df$x, y = quads.df$y, marks = "Mcarb")

par(mfrow=c(2,4))
plot(Gsub.new$AUS_alt, Testmixfunction$pred.loc[[1]], col="orange", main="M. carbinensis")
plot(Gsub.new$CLIM06_1k, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$CLIM18_1k, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$CLIM05_1k1, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$CLIM11_1k, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$CLIM13_1k, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$Dist_stream, Testmixfunction$pred.loc[[1]], col="orange")
plot(Gsub.new$Dist_road, Testmixfunction$pred.loc[[1]], col="orange")

#intensity response Mcog - cov
quad.Mcog = data.frame(x = quads.df$x, y = quads.df$y, marks = "Mcog")

plot(Gsub.new$AUS_alt, Testmixfunction$pred.loc[[2]], col="purple", main="M. coggeri")
plot(Gsub.new$CLIM06_1k, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$CLIM18_1k, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$CLIM05_1k1, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$CLIM11_1k, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$CLIM13_1k, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$Dist_stream, Testmixfunction$pred.loc[[2]], col="purple")
plot(Gsub.new$Dist_road, Testmixfunction$pred.loc[[2]], col="purple")

#intensity response Msch - cov
quad.Msch = data.frame(x = quads.df$x, y = quads.df$y, marks = "Msch")

plot(Gsub.new$AUS_alt, Testmixfunction$pred.loc[[3]], col="turquoise3", main="M. coggeri")
plot(Gsub.new$CLIM06_1k, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$CLIM18_1k, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$CLIM05_1k1, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$CLIM11_1k, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$CLIM13_1k, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$Dist_stream, Testmixfunction$pred.loc[[3]], col="turquoise3")
plot(Gsub.new$Dist_road, Testmixfunction$pred.loc[[3]], col="turquoise3")

par(mfrow=c(1,1))
#-----------------------------------------------------------------------------------------------




# Run individual models for teh true species only
#-----------------------------------------------------------------------------------------------

sp1.xy = as.data.frame(cbind(Mcarb.pp$x, Mcarb.pp$y))
colnames(sp1.xy) = c("X", "Y")

Gsubdf = as.data.frame(Gsub.c[,1:8])
env.xy = as.data.frame(cbind(quads.df$x, quads.df$y, Gsubdf))   #quads only needed in the context of ppp pbject implemented
colnames(env.xy) = c("X", "Y", paste0(colnames(Gsubdf)))


#"+", paste(names(Gsubdf)[3], collapse="+"), 
#"+", paste(names(Gsubdf)[6], collapse="+")))
cov.bias=10
kVal=0
if(is.null(cov.bias)){
  env.xy = env.xy
}else{
  pred.env.xy = env.xy
  set.Val = cov.bias #Variables to set to a certain value
  for (b in set.Val){
    pred.env.xy[,b] = kVal*pred.env.xy[,b]
  }
}
Test.sp1 = ppmlasso(formS, sp1.xy, env.xy, n.fits = 100, sp.scale = 5,
                    alpha=0.7, criterion="bic")

sp2.xy = as.data.frame(cbind(Mcog.pp$x, Mcog.pp$y))
colnames(sp2.xy) = c("X", "Y")
Test.sp2 = ppmlasso(formS, sp2.xy, env.xy, n.fits = 100, sp.scale = 5,
                    alpha=0.7, criterion="bic")

sp3.xy = as.data.frame(cbind(Msch.pp$x, Msch.pp$y))
colnames(sp3.xy) = c("X", "Y")
Test.sp3 = ppmlasso(formS, sp3.xy, n.fits = 100, env.xy, sp.scale = 5,
                    alpha=0.7, criterion="bic")

sp1.pred = predict.ppmlasso(Test.sp1, newdata = pred.env.xy)
sp2.pred = predict.ppmlasso(Test.sp2, newdata = pred.env.xy)
sp3.pred = predict.ppmlasso(Test.sp3, newdata = pred.env.xy)

cor12=cor(sp1.pred, sp2.pred)
cor13=cor(sp1.pred, sp3.pred)
cor23=cor(sp2.pred, sp3.pred)

cor12
cor13
cor23


col.l <- colorRampPalette(c("gold", "violetred","midnightblue"))
ckey <- list(labels=list(cex=1.2))
minVal2= min(sp1.pred, sp2.pred, sp3.pred)
maxVal2= max(sp1.pred, sp2.pred, sp3.pred)
all.val = rbind(sp1.pred, sp2.pred, sp3.pred)


M.coord = cbind(env.xy$X, env.xy$Y)
colnames(M.coord) = c("x", "y")

L1ind=levelplot(sp1.pred~M.coord[,1] + M.coord[,2],
                col.regions = col.l,
                at = unique(c(seq(minVal2, 1e-3, length=10),
                              seq(1e-3, maxVal2, length=30))), 
                xlab = "", ylab = "",# remove axis titles
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                main = list(label = "M. Carb",
                            cex = 2.5))

L2ind= levelplot(sp2.pred~M.coord[,1] + M.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal2, 1e-3, length=10),
                               seq(1e-3, maxVal2, length=30))), 
                 xlab = "", ylab = "",# remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Cog",
                             cex = 2.5))

L3ind=levelplot(sp3.pred~M.coord[,1] + M.coord[,2],
                col.regions = col.l,
                at = unique(c(seq(minVal2, 1e-3, length=10),
                              seq(1e-3, maxVal2, length=30))), 
                xlab = "", ylab = "",              # remove axis titles
                colorkey=ckey,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                main = list(label = "M. sch",
                            cex = 2.5))

comb_levObj <- c(L1ind, L2ind, L3ind, 
                 layout = c(3, 1), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       xlab = c("Mcarb", "Mcog", "Msch"),
       main="Individual models")

#-----------------------------------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------

########  ppmlassoLoopEngine

test.ppp=split(Northmix.pppdf)$Unknown
marks(test.ppp)="Unknown"

true.ppp=superimpose(Mcarb=Mcarb.pp, Mcog=Mcog.pp, Msch=Msch.pp)

sp_sub = list(Mcarb.pp, Mcog.pp, Msch.pp)

#------------------------------------------------------------------------------------
#  function ppmlassoLoopEngine
#------------------------------------------------------------------------------------


Testrealadd = ppmlassoLoopEngine(true.ppp, Unk.ppp, quadsenv = Gsubnew.c, ppmform=formS,
                                 sp.scale=5, n.fits=50, addpt = c("LoopT"),
                                 alpha=1, delta_max=0.5, delta_min=0.1, delta_step =0.1, num.add = NULL,
                                 cov.bias=10, kVal =0, kAreaInt=NULL, maxit = 50, r=NULL,
                                 verbose = TRUE, plots = FALSE, tol = 1e-6, max.it = 100, criterion="bic")

# fit
Testrealadd$ppm_sp

# Get the weights (membership probabilities)
par(mfrow=c(1,1))
pl.weights = Testrealadd$New_weights
plot(x=seq_along(pl.weights[,1]), y=pl.weights[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="observations", ylab="weight")
points(x=seq_along(pl.weights[,2]), y=pl.weights[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(pl.weights[,3]), y=pl.weights[,3], col = "Turquoise3", pch=17, ylim=c(0,1))
legend(5,0, c("Mcarb", "Mcog", "Msch"), col = c("orange", "purple", "Turquoise3"),
       pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)


#-------------------------------------------------------------------------
# in case not done before, summarize the coordinates for the true species
Mcarb.coord = cbind(env.xy$X, env.xy$Y)
colnames(Mcarb.coord) = c("x", "y")
Mcog.coord = cbind(env.xy$X, env.xy$Y)
colnames(Mcog.coord) = c("x", "y")
Msch.coord = cbind(env.xy$X, env.xy$Y)
colnames(Msch.coord) = c("x", "y")


### Get the predictions
all.val = rbind(Testrealadd$fitsp[[1]], Testrealadd$fitsp[[2]],
                Testrealadd$fitsp[[3]])
minVal=min(all.val)
maxVal=max(all.val)
Q1Val=quantile(all.val)[2]
col.l <- colorRampPalette(c("gold", "violetred","midnightblue"))
ckey <- list(labels=list(cex=1.2))


#other scale
L1loop=levelplot(Testrealadd$fitsp[[1]]~Mcarb.coord[,1] + Mcarb.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=30))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Carb",
                             cex = 2.5))

L2loop=levelplot(Testrealadd$fitsp[[2]]~Mcog.coord[,1] + Mcog.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=30))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Cog",
                             cex = 2.5))

L3loop=levelplot(Testrealadd$fitsp[[3]]~Msch.coord[,1] + Msch.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=30))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=ckey,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. sch",
                             cex = 2.5))


comb_levObj <- c(L1loop, L2loop, L3loop, 
                 layout = c(3, 1), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       xlab = c("Mcarb", "Mcog", "Msch"),
       main="Loopgr models")


#intensity response Mcarb - cov
par(mfrow=c(2,4),     # 2x4 layout
    oma = c(2, 2, 1, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)
plot(Gsub.new$AUS_alt, Testrealadd$fitsp[[1]], col="orange", main="M. carbinensis", ylab="predicted intensity", xlab="Altitude")
plot(Gsub.new$CLIM05_1k1, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Clim05")
plot(Gsub.new$CLIM06_1k, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Clim06")
plot(Gsub.new$CLIM11_1k, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Clim11")
plot(Gsub.new$CLIM13_1k, Testrealadd$fitsp[[1]], col="orange", ylab="predicted intensity", xlab="Clim13")
plot(Gsub.new$CLIM18_1k, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Clim18")
plot(Gsub.new$Dist_stream, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Dist stream")
plot(Gsub.new$Dist_road, Testrealadd$fitsp[[1]], col="orange", ylab="", xlab="Dist road")


#intensity response Mcog - cov
plot(Gsub.new$AUS_alt, Testrealadd$fitsp[[2]], col="purple", main="M. coggeri", ylab="predicted intensity", xlab="Altitude")
plot(Gsub.new$CLIM05_1k1, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Clim05")
plot(Gsub.new$CLIM06_1k, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Clim06")
plot(Gsub.new$CLIM11_1k, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Clim11")
plot(Gsub.new$CLIM13_1k, Testrealadd$fitsp[[2]], col="purple", ylab="predicted intensity", xlab="Clim13")
plot(Gsub.new$CLIM18_1k, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Clim18")
plot(Gsub.new$Dist_stream, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Dist stream")
plot(Gsub.new$Dist_road, Testrealadd$fitsp[[2]], col="purple", ylab="", xlab="Dist road")


#intensity response Msch - cov
plot(Gsub.new$AUS_alt, Testrealadd$fitsp[[3]], col="turquoise3", main="M. schevilli", ylab="predicted intensity", xlab="Altitude")
plot(Gsub.new$CLIM05_1k1, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Clim05")
plot(Gsub.new$CLIM06_1k, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Clim06")
plot(Gsub.new$CLIM11_1k, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Clim11")
plot(Gsub.new$CLIM13_1k, Testrealadd$fitsp[[3]], col="turquoise3", ylab="predicted intensity", xlab="Clim13")
plot(Gsub.new$CLIM18_1k, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Clim18")
plot(Gsub.new$Dist_stream, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Dist stream")
plot(Gsub.new$Dist_road, Testrealadd$fitsp[[3]], col="turquoise3", ylab="", xlab="Dist road")

par(mfrow=c(1,1))





#-----------------------------------------------------------------------------------------------
## all plots - predictions comparisons
#-----------------------------------------------------------------------------------------------


minVal=min(minval1, minVal2, minVal)
maxVal=max(maxval1, maxVal2, maxVal)

Lcarb=levelplot(Mcarb.pred$V1~Mcarb.coord[,1] + Mcarb.coord[,2],
                at = unique(c(seq(minVal, 1e-3, length=10),
                              seq(1e-3, maxVal, length=15))),
                col.regions = col.l,
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "M. Carb",
                            cex = 2.5))

Lcog=levelplot(Mcog.pred$V1~Mcog.coord[,1] + Mcog.coord[,2],
               col.regions = col.l,
               at = unique(c(seq(minVal, 1e-3, length=10),
                             seq(1e-3, maxVal, length=15))),
               xlab = "", ylab = "",              # remove axis titles
               colorkey=F,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               main = list(label = "M. Cog",
                           cex = 2.5))

Lsch=levelplot(Msch.pred$V1~Msch.coord[,1] + Msch.coord[,2],
               col.regions = col.l,
               at = unique(c(seq(minVal, 1e-3, length=10),
                             seq(1e-3,maxVal, length=15))),
               xlab = "", ylab = "",              # remove axis titles
               colorkey=F,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               main = list(label = "M. sch",
                           cex = 2.5))


L1ind=levelplot(sp1.pred~M.coord[,1] + M.coord[,2],
                col.regions = col.l,
                at = unique(c(seq(minVal, 1e-3, length=10),
                              seq(1e-3, maxVal, length=15))), 
                xlab = "", ylab = "",# remove axis titles
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                main = list(label = "M. Carb",
                            cex = 2.5))

L2ind= levelplot(sp2.pred~M.coord[,1] + M.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=15))), 
                 xlab = "", ylab = "",# remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Cog",
                             cex = 2.5))

L3ind=levelplot(sp3.pred~M.coord[,1] + M.coord[,2],
                col.regions = col.l,
                at = unique(c(seq(minVal, 1e-3, length=10),
                              seq(1e-3, maxVal, length=15))), 
                xlab = "", ylab = "",              # remove axis titles
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                main = list(label = "M. sch",
                            cex = 2.5))

L1loop=levelplot(Testrealadd$fitsp[[1]]~Mcarb.coord[,1] + Mcarb.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=15))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Carb",
                             cex = 2.5))

L2loop=levelplot(Testrealadd$fitsp[[2]]~Mcog.coord[,1] + Mcog.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=15))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=F,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. Cog",
                             cex = 2.5))

L3loop=levelplot(Testrealadd$fitsp[[3]]~Msch.coord[,1] + Msch.coord[,2],
                 col.regions = col.l,
                 at = unique(c(seq(minVal, 1e-3, length=10),
                               seq(1e-3, maxVal, length=15))),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=ckey,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 main = list(label = "M. sch",
                             cex = 2.5))


comb_levObj <- c(L1loop, L2loop, L3loop, 
                 L1ind, L2ind, L3ind,
                 Lcarb, Lcog, Lsch, 
                 layout = c(3, 3), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       xlab = c("Mcarb", "Mcog", "Msch"),
       ylab = c("LoopT", "indiv", "Mixture knn"),
       main="Myxophies species predicted distribution")



# Final membership probabilities
par(mfrow=c(1,1),     # 2x4 layout
    oma = c(2, 2, 3, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)
boxplot(cbind(New.weights, pl.weights),
        col=c("orange", "purple", "turquoise3",
              "orange", "purple", "turquoise3"),
        at = c(1,2,3, 5,6,7), cex=1,cex.lab=1.3, cex.axis=1.1,
        main="Mixture method                                       LoopT method")
mtext(c("Final membership probabilities"),
      side=3, line=2, cex = 2.5)


# Final membership probabilities comparisons
par(mfrow=c(2,1),     # 2x4 layout
    oma = c(2, 2, 1, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 3, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)
plot(x=seq_along(New.weights[,1]), y=New.weights[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="", ylab="weight", main="knn method")
points(x=seq_along(New.weights[,2]), y=New.weights[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(New.weights[,3]), y=New.weights[,3], col = "Turquoise3", pch=17, ylim=c(0,1))
legend(1,1, c("Mcarb", "Mcog", "Msch"), col = c("orange", "purple", "Turquoise3"),
       pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)


plot(x=seq_along(pl.weights[,1]), y=pl.weights[,1], col = "orange", pch=16, ylim=c(0,1),
     xlab="observations", ylab="weight", main="LoopT method")
points(x=seq_along(pl.weights[,2]), y=pl.weights[,2], col = "purple", pch=18, ylim=c(0,1))
points(x=seq_along(pl.weights[,3]), y=pl.weights[,3], col = "Turquoise3", pch=17, ylim=c(0,1))
#legend(1,1, c("Mcarb", "Mcog", "Msch"), col = c("orange", "purple", "Turquoise3"),
#       pch = c(16, 18, 17), xjust = 1, yjust = 0, merge = FALSE)




## -------------------------------------------------------------------------------------------------------------------------
# New plots
# PLots observations with membership probabilities (weights) for color intensity
par(mfrow=c(1,3),     # 2x4 layout
    oma = c(2, 2, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(1, 1, 2, 2), # space for one row of text at ticks and to separate plots
    mgp = c(2, 1, 0),    # axis label at 2 rows distance, tick labels at 1 row
    xpd = NA)

yrange= c(min(Mcarb.pp$y, Msch.pp$y, Mcog.pp$y, test.ppp$y), max(Mcarb.pp$y, Msch.pp$y, Mcog.pp$y, test.ppp$y))
xrange= c(min(Mcarb.pp$x, Msch.pp$x, Mcog.pp$x, test.ppp$x), max(Mcarb.pp$x, Msch.pp$x, Mcog.pp$x, test.ppp$x))


###   the truth/the known points
plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2.2, ylim=yrange, cex.main=2, 
     cex.axis = 1.5, cex.lab = 1.5,
     xlim=xrange, ylab="Y", xlab="X", col="white", main="Before")
points(Mcarb.pp$x, Mcarb.pp$y, col="orange", pch=16, cex = 2.2)
points(Mcog.pp$x, Mcog.pp$y, col = "purple", pch=17, cex = 2.2)
points(Msch.pp$x, Msch.pp$y, col = "turquoise3", pch=15, cex = 2.2)

## the points with unknown labels
points(test.ppp$x, test.ppp$y, pch="?", col = "black", cex = 2)
legend(-240, 8265,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=2,
       pch=c(16,17,15), bty = "n")
legend(-240, 8225,legend=c("Unknown"),
       col=c("black"), cex=2,
       pch="?", bty = "n")




###

# Plot the distribution of the observations based on their weights (Color) 
###

# to have the australian map (north east) with points location
library(png)
Picture <- readPNG("E:/Newcastle/RealData-project1/mappts.png")
grid.raster(Picture, x=0.1, y=0.26, width=.14)
#....................................................................


plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2, ylim=yrange, xlim=xrange,cex.main=2,
     cex.axis = 1.5, cex.lab = 1.5,
     ylab="", xlab="", main=" knn")
points(Mcarb.pp$x, Mcarb.pp$y, col="orange", pch=16, cex = 2.2)
points(Mcog.pp$x, Mcog.pp$y, col = "purple", pch=17, cex = 2.2)
points(Msch.pp$x, Msch.pp$y, col = "turquoise3", pch=15, cex = 2.2)

max_W = apply(Testmixfunction$New_weights, 1, which.max)
max_W.vec = (c(sum(max_W== 1), sum(max_W== 2), sum(max_W== 3))) 

max_W[Northmix.pppdf$marks=="Unknown"]
max_Wunk.vec = (c(sum(max_W[Northmix.pppdf$marks=="Unknown"]== 1),
                  sum(max_W[Northmix.pppdf$marks=="Unknown"]== 2),
                  sum(max_W[Northmix.pppdf$marks=="Unknown"]== 3)))

tcarb.x = test.ppp$x[max_W[Northmix.pppdf$marks=="Unknown"]== 1]
tcarb.y = test.ppp$y[max_W[Northmix.pppdf$marks=="Unknown"]== 1]
New.carb = cbind(tcarb.x, tcarb.y)

tcog.x = test.ppp$x[max_W[Northmix.pppdf$marks=="Unknown"]== 2]
tcog.y = test.ppp$y[max_W[Northmix.pppdf$marks=="Unknown"]== 2]
New.cog = cbind(tcog.x, tcog.y)

tsch.x = test.ppp$x[max_W[Northmix.pppdf$marks=="Unknown"]== 3]
tsch.y = test.ppp$y[max_W[Northmix.pppdf$marks=="Unknown"]== 3]
New.sch = cbind(tsch.x, tsch.y)

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))

W.carb = Testmixfunction$New_weights[,1][which(max_W==1)]
W.carb = W.carb[-which(W.carb==1)]
W.carb$Col <- oranpal(5)[as.numeric(cut(W.carb,breaks = 5))] ## produce error if no points assign to Mcarb..

medpurp <- colorRampPalette(c('plum2','purple4'))

W.cog = Testmixfunction$New_weights[,2][which(max_W==2)]
W.cog = W.cog[-which(W.cog==1)]
W.cog$Col <- medpurp(5)[as.numeric(cut(W.cog,breaks = 5))]## produce error if no points assign to Mcog..

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

W.sch = Testmixfunction$New_weights[,3][which(max_W==3)]
W.sch = W.sch[-which(W.sch==1)]
W.sch$Col <- tblue(5)[as.numeric(cut(W.sch,breaks = 5))]## produce error if no points assign to Msch..




points(tcarb.x, tcarb.y, pch=16, col = W.carb$Col, cex = 2.2)
points(tcog.x, tcog.y, pch=17, col = W.cog$Col, cex = 2.2)
points(tsch.x, tsch.y, pch=15, col = W.sch$Col, cex = 2.2)

legend(-240, 8265 ,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=2,
       pch=c(16,17,15), bty = "n")



plot(Mcarb.pp$x, Mcarb.pp$y, pch=16, cex = 2.2, ylim=yrange, cex.main=2,
     cex.axis = 1.5, cex.lab = 1.5,
     xlim=xrange, ylab="", xlab="", col="white", main=" LoopT")
points(Mcarb.pp$x, Mcarb.pp$y, col="orange", pch=16, cex = 2.2)
points(Mcog.pp$x, Mcog.pp$y, col = "purple", pch=17, cex = 2.2)
points(Msch.pp$x, Msch.pp$y, col = "turquoise3", pch=15, cex = 2.2)


True.W = Testmixfunction$New_weights[1:true.ppp$n,]
colnames(pl.weights)=c("Mcarb", "Mcog", "Msch")
all.weights = rbind(True.W, pl.weights)
max_WL = apply(all.weights, 1, which.max)
max_WL.vec = (c(sum(max_WL== 1), sum(max_WL== 2), sum(max_WL== 3))) 

max_WL[Northmix.pppdf$marks=="Unknown"]
max_Wunk.vec = (c(sum(max_WL[Northmix.pppdf$marks=="Unknown"]== 1),
                  sum(max_WL[Northmix.pppdf$marks=="Unknown"]== 2),
                  sum(max_WL[Northmix.pppdf$marks=="Unknown"]== 3)))

lcarb.x = test.ppp$x[max_WL[Northmix.pppdf$marks=="Unknown"]== 1]
lcarb.y = test.ppp$y[max_WL[Northmix.pppdf$marks=="Unknown"]== 1]
New.carb = cbind(lcarb.x, lcarb.y)

lcog.x = test.ppp$x[max_WL[Northmix.pppdf$marks=="Unknown"]== 2]
lcog.y = test.ppp$y[max_WL[Northmix.pppdf$marks=="Unknown"]== 2]
New.cog = cbind(lcog.x, lcog.y)

lsch.x = test.ppp$x[max_WL[Northmix.pppdf$marks=="Unknown"]== 3]
lsch.y = test.ppp$y[max_WL[Northmix.pppdf$marks=="Unknown"]== 3]
New.sch = cbind(lsch.x, lsch.y)

# Color scale
oranpal <- colorRampPalette(c('orange1','darkorange4'))


Wlt.CA = pl.weights[,1][which(max_WL[264:444]==1)]
Wlt.carb = Wlt.CA[Wlt.CA>=0.1]
Wlt.carb$Col <- oranpal(5)[as.numeric(cut(Wlt.carb,breaks = 5))]

medpurp <- colorRampPalette(c('plum2','purple4'))

Wlt.CO = pl.weights[,2][which(max_WL[264:444]==2)]
Wlt.cog = Wlt.CO[Wlt.CO>=0.1]
Wlt.cog$Col <- medpurp(5)[as.numeric(cut(Wlt.cog,breaks = 5))]

tblue <- colorRampPalette(c('lightblue3','dodgerblue4'))

Wlt.SC = pl.weights[,3][which(max_WL[264:444]==3)]
Wlt.sch = Wlt.SC[Wlt.SC>=0.1]
Wlt.sch$Col <- tblue(5)[as.numeric(cut(Wlt.sch,breaks = 5))]

Wlt.noca = which(Wlt.CA<0.1) # 
Wlt.noco = which(Wlt.CO<0.1) #
Wlt.nosch = which(Wlt.SC<0.1)

points(lcarb.x, lcarb.y, pch=16, col = Wlt.carb$Col, cex = 2.2)
points(lcog.x, lcog.y, pch=17, col = Wlt.cog$Col, cex = 2.2)
points(lsch.x, lsch.y, pch=15, col = Wlt.sch$Col, cex = 2.2)
#points(c(lcarb.x[Wlt.nosch], lcog.x[Wlt.noco],lsch.x[Wlt.nosch]), 
#       c(lcarb.y[Wlt.nosch], lcog.y[Wlt.noco], lsch.x[Wlt.nosch]), 
#       pch="?", col = "black", cex = 2.2)
legend(-240, 8265,legend=c("Mcarb", "Mcog","Msch"),
       col=c("orange", "purple", "turquoise3"), cex=2,
       pch=c(16,17,15), bty = "n")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

