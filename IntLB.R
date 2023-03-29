library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(latticeExtra)
library(viridis)
library(spatstat)


load("DifAb_nocor_Oct20.RDATA")
#load("EqAb_nocor_Oct20.RDATA")
#load("EqAb_cor_Oct20.RDATA")
#load("DifAb_cor_Oct20.RDATA")

load("ResInttest1LB.RDATA")
#load("ResInttest2LB.RDATA")
#load("ResInttest3LB.RDATA")
#load("ResInttest4LB.RDATA")


quads.df = expand.grid(seq(0, 100, 1), seq(0, 100, 1))
names(quads.df) = c("X", "Y")


### For test 80%
###
knn1=levelplot(Mean.knn1_80~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               colorkey=F,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species1",
                           cex = 1.5)
)

CF1=levelplot(Mean.CF1_80~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
              xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species1",
                          cex = 1.5)
)

indiv1=levelplot(Mean.indiv1_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopT1=levelplot(Mean.LoopT1_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopA1=levelplot(Mean.LoopA1_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


sp1 = levelplot(sp1_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),              # remove axis titles
                colorkey=F,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species1",
                            cex = 1.5))


## Sp2

knn2=levelplot(Mean.knn2_80~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species2",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species2",
                           cex = 1.5)
)

CF2=levelplot(Mean.CF2_80~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species2",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species2",
                          cex = 1.5)
)

indiv2=levelplot(Mean.indiv2_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopT2=levelplot(Mean.LoopT2_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


LoopA2=levelplot(Mean.LoopA2_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


sp2=levelplot(sp2_int~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species2",
                          cex = 1.5))


###  sp3

knn3=levelplot(Mean.knn3_80~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species3",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species3",
                           cex = 1.5))

CF3=levelplot(Mean.CF3_80~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species3",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species3",
                          cex = 1.5))

indiv3=levelplot(Mean.indiv3_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopT3=levelplot(Mean.LoopT3_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopA3=levelplot(Mean.LoopA3_80~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))


sp3 = levelplot(sp3_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),
                colorkey=F,
                scales = list(y = list(draw = FALSE),
                              x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species3",
                            cex = 1.5))

margin = theme(plot.margin = unit(c(0,0,0,2), "cm"))



comb_levObj <- c(knn3, CF3, indiv3, LoopT3, LoopA3, sp3,
                 knn2, CF2, indiv2, LoopT2, LoopA2, sp2,
                 knn1, CF1, indiv1, LoopT1, LoopA1, sp1,
                 layout = c(6, 3), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       ylab = c("species3", "species2", "species1"),
       xlab = c("knn", "CF", "indiv", "LoopT","LoopA", "initial process"),
       main="Lasso Bias correction - 80% of hidden observation")






### For test 50%

###
knn1=levelplot(Mean.knn1_50~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species1",
                           cex = 1.5)
)

CF1=levelplot(Mean.CF1_50~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
              xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species1",
                          cex = 1.5)
)

indiv1=levelplot(Mean.indiv1_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopT1=levelplot(Mean.LoopT1_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopA1=levelplot(Mean.LoopA1_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


sp1 = levelplot(sp1_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),              # remove axis titles
                colorkey=FALSE,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species1",
                            cex = 1.5))


## Sp2

knn2=levelplot(Mean.knn2_50~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species2",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species1",
                           cex = 1.5)
)

CF2=levelplot(Mean.CF2_50~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species2",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species2",
                          cex = 1.5)
)

indiv2=levelplot(Mean.indiv2_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


LoopT2=levelplot(Mean.LoopT2_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


LoopA2=levelplot(Mean.LoopA2_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


sp2=levelplot(sp2_int~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species2",
                          cex = 1.5))


###  sp3

knn3=levelplot(Mean.knn3_50~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species3",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species3",
                           cex = 1.5))

CF3=levelplot(Mean.CF3_50~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species3",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species3",
                          cex = 1.5))

indiv3=levelplot(Mean.indiv3_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopT3=levelplot(Mean.LoopT3_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopA3=levelplot(Mean.LoopA3_50~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))


sp3 = levelplot(sp3_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),
                colorkey=F,
                scales = list(y = list(draw = FALSE),
                              x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species3",
                            cex = 1.5))

margin = theme(plot.margin = unit(c(0,0,0,2), "cm"))


comb_levObj <- c(knn3, CF3, indiv3, LoopT3, LoopA3, sp3,
                 knn2, CF2, indiv2, LoopT2, LoopA2, sp2,
                 knn1, CF1, indiv1, LoopT1, LoopA1, sp1,
                 layout = c(6, 3), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       ylab = c("species3", "species2", "species1"),
       xlab = c("knn", "CF", "indiv", "LoopT","LoopA", "initial process"),
       main="Lasso bias correction - 50% of hidden observation")





### For test 20%

###
knn1=levelplot(Mean.knn1_20~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
               xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species1",
                           cex = 1.5)
)

CF1=levelplot(Mean.CF1_20~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
              xlab = "", ylab = "species1",              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species1",
                          cex = 1.5)
)

indiv1=levelplot(Mean.indiv1_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopT1=levelplot(Mean.LoopT1_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


LoopA1=levelplot(Mean.LoopA1_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                 xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species1",
                             cex = 1.5))


sp1 = levelplot(sp1_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),              # remove axis titles
                colorkey=FALSE,
                scales = list(y = list(draw = FALSE), x = list(draw = FALSE)),
                xlab = "", ylab = "",              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species1",
                            cex = 1.5))


## Sp2

knn2=levelplot(Mean.knn2_20~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species2",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species2",
                           cex = 1.5)
)

CF2=levelplot(Mean.CF2_20~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species2",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species2",
                          cex = 1.5)
)

indiv2=levelplot(Mean.indiv2_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


LoopT2=levelplot(Mean.LoopT2_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


LoopA2=levelplot(Mean.LoopA2_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species2",
                             cex = 1.5))


sp2=levelplot(sp2_int~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species1",
                          cex = 1.5))


###  sp3

knn3=levelplot(Mean.knn3_20~quads.df$X + quads.df$Y,
               #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
               #cuts = 40,
               col.regions = viridis(20),
               xlab = "", ylab = "species3",              # remove axis titles
               colorkey=FALSE,
               scales = list(y = list(draw = FALSE),
                             x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
               main = list(label = "species3",
                           cex = 1.5))

CF3=levelplot(Mean.CF3_20~quads.df$X + quads.df$Y,
              #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
              #cuts = 40,
              col.regions = viridis(20),
              xlab = "", ylab = "species3",              # remove axis titles
              colorkey=FALSE,
              scales = list(y = list(draw = FALSE),
                            x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
              main = list(label = "species3",
                          cex = 1.5))

indiv3=levelplot(Mean.indiv3_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopT3=levelplot(Mean.LoopT3_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 col.regions = viridis(20),
                 xlab = "", ylab = "",              # remove axis titles
                 colorkey=FALSE,
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))

LoopA3=levelplot(Mean.LoopA3_20~quads.df$X + quads.df$Y,
                 #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                 #cuts = 40,
                 colorkey=FALSE,
                 col.regions = viridis(20),
                 scales = list(y = list(draw = FALSE),
                               x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                 main = list(label = "species3",
                             cex = 1.5))


sp3 = levelplot(sp3_int~quads.df$X + quads.df$Y,
                #at= unique(c(seq(minVal, Q1Val/2, length=10), seq(Q1Val/2, maxVal, length=30))),
                #cuts = 40,
                col.regions = viridis(20),
                colorkey=F,
                scales = list(y = list(draw = FALSE),
                              x = list(draw = FALSE)),              # remove axis title         # change font size for x- & y-axis text
                main = list(label = "species3",
                            cex = 1.5))

margin = theme(plot.margin = unit(c(0,0,0,2), "cm"))



comb_levObj <- c(knn3, CF3, indiv3, LoopT3, LoopA3, sp3,
                 knn2, CF2, indiv2, LoopT2, LoopA2, sp2,
                 knn1, CF1, indiv1, LoopT1, LoopA1, sp1,
                 layout = c(6, 3), merge.legends = T)
#print(comb_levObj)
update(comb_levObj,
       ylab = c("species3", "species2", "species1"),
       xlab = c("knn", "CF", "indiv", "LoopT","LoopA", "initial process"),
       main="Lasso bias correction - 20% of hidden observation")



