setwd("//rcgdata.newcastle.edu.au/c3286500/Sim/LassoBias")


load("ResnoLBtest1.RDATA")

# re order to have indiv first!
NoLBacc = accmatmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLBIMSE = IMSEmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLBsumcor = sumcormatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLBmeanRSS = meanRSSmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]

load("Restest1noLnoB.RDATA")
NoLnoBacc = accmatmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLnoBIMSE = IMSEmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLnoBsumcor = sumcormatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
NoLnoBmeanRSS = meanRSSmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]

load("ReslassoBtest1.RDATA")
LBacc = accmatmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LBIMSE = IMSEmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LBsumcor = sumcormatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LBmeanRSS = meanRSSmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]

load("ReslassonoBtest1.RDATA")
LnoBacc = accmatmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LnoBIMSE = IMSEmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LnoBsumcor = sumcormatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]
LnoBmeanRSS = meanRSSmatT1[, c(5, 1:4, 6:8, 13, 9:12, 14:16, 21, 17:20, 22:24)]

par(mfrow=c(2,2))
par(mar=c(5,4,2,1))


####################################################
## meanRSS #------------------------------------------------------------------------------
par(mfrow=c(2,2),     # 2x4 layout
    oma = c(3, 0, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(6, 6, 5, 1), # space for one row of text at ticks and to separate plots
    mgp = c(3.8, 0.8, 0)    # axis label at 2 rows distance, tick labels at 1 row
    #xpd = TRUE
)
layout(rbind(cbind(1,2),cbind(3,4),5), heights=c(5.5,5.5,0.8))



b1 = boxplot(NoLnoBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                   "green","green","green","green","green","green","green","green",
                                   "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                   "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "meanRSS", main="no lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)

abline(h=c(b1$stats[3,1], b1$stats[3,9],b1$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)



boxplot(NoLnoBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                               "green","green","green","green","green","green","green","green",
                               "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                               "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26), add=T, 
        ylab = "meanRSS", main="no lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)




b2 = boxplot(NoLBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                 "green","green","green","green","green","green","green","green",
                                 "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                 "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "meanRSS", main="no lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)

abline(h=c(b2$stats[3,1], b2$stats[3,9], b2$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)

boxplot(NoLBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                             "green","green","green","green","green","green","green","green",
                             "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                             "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26), add=T, 
        ylab = "meanRSS", main="no lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)


b3 = boxplot(LnoBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                 "green","green","green","green","green","green","green","green",
                                 "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                 "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "meanRSS", main="lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)

abline(h=c(b3$stats[3,1], b3$stats[3,9], b3$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)

boxplot(LnoBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                             "green","green","green","green","green","green","green","green",
                             "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                             "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26), add = T, 
        ylab = "meanRSS", main="lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)



b4 = boxplot(LBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                               "green","green","green","green","green","green","green","green",
                               "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                               "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "meanRSS", main="lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)


abline(h=c(b4$stats[3,1], b4$stats[3,9],b4$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)


boxplot(LBmeanRSS, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                           "green","green","green","green","green","green","green","green",
                           "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                           "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26), add=T, 
        ylab = "meanRSS", main="lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)


reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

reset()

legend("bottom", title="hidden observations", 
       bty = "n",
       c("20%","50%","80%"), fill=c("yellow", "green", "blue"), 
       horiz=TRUE, cex = 1.6)

legend(0.6, 0.005, lty = 2,
       bty = "n", c("median performance individual method"), col=c("black"), 
       horiz=TRUE, cex=1.6)



## IMSE #------------------------------------------------------------------------------
par(mfrow=c(2,2),     # 2x4 layout
    oma = c(3, 0, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(6, 6, 5, 1), # space for one row of text at ticks and to separate plots
    mgp = c(4, 0.8, 0)    # axis label at 2 rows distance, tick labels at 1 row
    #xpd = TRUE
)
layout(rbind(cbind(1,2),cbind(3,4),5), heights=c(5.5,5.5,0.8))



b1 = boxplot(NoLnoBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                "green","green","green","green","green","green","green","green",
                                "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                "blue"),
        names = c(expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", ylim=c(4e3, 3e5), yaxt="n", 
        ylab = "sumNIMSE", main="no lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)
axis(2, at = c(1e4, 5e4, 1e5, 3e5), cex.axis = 2,
     labels = c("1e4", "5e4","1e5", "3e5"),
     las=1)

abline(h=c(b1$stats[3,1], b1$stats[3,9],b1$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)

boxplot(NoLnoBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                            "green","green","green","green","green","green","green","green",
                            "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                            "blue"),
        names = c(expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                  expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                  expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", ylim=c(4e3, 3e5), yaxt="n", add=T,
        ylab = "sumNIMSE", main="no lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)



b2 = boxplot(NoLBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                              "green","green","green","green","green","green","green","green",
                              "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                              "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim = c(4e3, 3e5),
        ylab = "sumNIMSE", main="no lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)
axis(2, at = c(1e4, 5e4, 1e5, 3e5),  cex.axis = 2,
     labels = c("1e4", "5e4","1e5", "3e5"),
     las=1)

abline(h=c(b2$stats[3,1], b2$stats[3,9], b2$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)


boxplot(NoLBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                          "green","green","green","green","green","green","green","green",
                          "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                          "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim = c(4e3, 3e5), add=T,
        ylab = "sumNIMSE", main="no lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)




b3 = boxplot(LnoBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                              "green","green","green","green","green","green","green","green",
                              "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                              "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim=c(4e3, 3e5),
        ylab = "sumNIMSE", main="lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)
axis(2, at = c(1e4, 5e4, 1e5, 3e5),  cex.axis = 2,
     labels = c("1e4", "5e4","1e5", "3e5"),
     las=1)

abline(h=c(b3$stats[3,1], b3$stats[3,9], b3$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)


boxplot(LnoBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                          "green","green","green","green","green","green","green","green",
                          "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                          "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim=c(4e3, 3e5), add=T,
        ylab = "sumNIMSE", main="lasso no bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)




b4 = boxplot(LBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                            "green","green","green","green","green","green","green","green",
                            "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                            "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13 ,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim=c(4e3, 3e5),
        ylab = "sumNIMSE", main="lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)
axis(2, at = c(1e4, 5e4, 1e5, 3e5),  cex.axis = 2,
     labels = c("1e4", "5e4","1e5", "3e5"),
     las=1)

abline(h=c(b4$stats[3,1], b4$stats[3,9], b4$stats[3,17]),
       lwd=2, lty= 2, col=c('yellow2', 'green2','blue2'), untf=TRUE)


boxplot(LBIMSE, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                        "green","green","green","green","green","green","green","green",
                        "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                        "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                   "LoopA","LoopT","LoopE"),#log="y",
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13 ,14,15,16,17, 19,20,21,22,23,24,25,26),
        log="y", yaxt="n", ylim=c(4e3, 3e5), add=T,
        ylab = "sumNIMSE", main="lasso bias correction", las = 2, cex=3, cex.lab=2, cex.axis=2, cex.main = 3)


reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

reset()

legend("bottom", title="hidden observations", 
       bty = "n",
       c("20%","50%","80%"), fill=c("yellow", "green", "blue"), 
       horiz=TRUE, cex = 1.6)

legend(0.6, 0.005, lty = 2,
       bty = "n", c("median performance individual method"), col=c("black"), 
       horiz=TRUE, cex=1.6)


## acc #------------------------------------------------------------------------------
par(mfrow=c(2,2),     # 2x4 layout
    oma = c(3, 0, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(4, 4, 2.9, 1), # space for one row of text at ticks and to separate plots
    mgp = c(3, 0.8, 0)    # axis label at 2 rows distance, tick labels at 1 row
    #xpd = TRUE
)
layout(rbind(cbind(1,2),cbind(3,4),5), heights=c(5.5,5.5,0.4))



boxplot(NoLnoBacc, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                  "green","green","green","green","green","green","green","green",
                                  "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                  "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "accmat", main="no lasso no bias correction", ylim=c(0, 1), las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)



boxplot(NoLBacc, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                "green","green","green","green","green","green","green","green",
                                "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "accmat", main="no lasso bias correction", ylim=c(0, 1), las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)


boxplot(LnoBacc, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                "green","green","green","green","green","green","green","green",
                                "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "accmat", main="lasso no bias correction", ylim=c(0, 1), las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)



boxplot(LBacc, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                              "green","green","green","green","green","green","green","green",
                              "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                              "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "accmat", main="lasso bias correction", ylim=c(0, 1), las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)

reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

reset()

legend("bottom", title="hidden observations", 
       bty = "n",
       c("20%","50%","80%"), fill=c("yellow", "green", "blue"), 
       horiz=TRUE, cex=0.9)


## sumcor #------------------------------------------------------------------------------
par(mfrow=c(2,2),     # 2x4 layout
    oma = c(3, 0, 0, 0), # two rows of text at the outer left and bottom margin
    mar = c(4, 4, 2.9, 1), # space for one row of text at ticks and to separate plots
    mgp = c(3, 0.8, 0)    # axis label at 2 rows distance, tick labels at 1 row
    #xpd = TRUE
)
layout(rbind(cbind(1,2),cbind(3,4),5), heights=c(5.5,5.5,0.4))


boxplot(NoLnoBsumcor, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                  "green","green","green","green","green","green","green","green",
                                  "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                  "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "sumcor", main="no lasso no bias correction", las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)


boxplot(NoLBsumcor, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                "green","green","green","green","green","green","green","green",
                                "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "sumcor", main="no lasso bias correction", las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)


boxplot(LnoBsumcor, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                                "green","green","green","green","green","green","green","green",
                                "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                                "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "sumcor", main="lasso no bias correction", las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)



boxplot(LBsumcor, col = c("yellow","yellow","yellow","yellow","yellow","yellow","yellow","yellow",
                              "green","green","green","green","green","green","green","green",
                              "blue", "blue", "blue", "blue", "blue", "blue", "blue",
                              "blue"),
        names = c( expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE",
                   expression(bold("indiv")),"knn", "kmeans", "random", "CF",
                  "LoopA","LoopT","LoopE"),
        at = c(1,2,3,4,5,6,7,8, 10,11,12,13,14,15,16,17, 19,20,21,22,23,24,25,26),
        ylab = "sumcor", main="lasso bias correction", las = 2, cex=2, cex.lab=1.5, cex.axis=1.5)

reset <- function() {
  par(mfrow=c(1, 1), oma=rep(0, 4), mar=rep(0, 4), new=TRUE)
  plot(0:1, 0:1, type="n", xlab="", ylab="", axes=FALSE)
}

reset()

legend("bottom", title="hidden observations", 
       bty = "n",
       c("20%","50%","80%"), fill=c("yellow", "green", "blue"), 
       horiz=TRUE, cex=0.9)

