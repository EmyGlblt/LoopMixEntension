library(spatstat)

### Plot of the coefficients
#..............................................................

#load("RescoefLnoBtest3.RData")

#load("RescoefLnoBtest1.RData")
#load("RescoefnoLBtest1.RData")
#load("RescoefnoLnoBtest1.RData")
load("RescoefLBtest1.RData")

load("DifAb_nocor_Oct20.RDATA") #test1
#load("EqAb_nocor_Oct20.RDATA") #test2
#load("DifAb_cor_Oct20.RDATA") #test3
#load("EqAb_cor_Oct20.RDATA") #test4

Ini.coef.sp1 = c(po1_coef[-1], rep(0,4))
Ini.coef.sp2 = c(po2_coef[-1], rep(0,4))
Ini.coef.sp3 = c(po3_coef[-1], rep(0,4))

vec.names =  c("v1", "v1.2", "v2", "v2.2", "d_rd", 
               "d1", "d1.2", "d2", "d2.2")

#n.sims=50
#hidepct=c(0.2,0.5,0.8)


par(mfrow=c(5,3),     # 2x4 layout
    oma = c(4, 0.5, 2, 0.5), # two rows of text at the outer LAft and bottom margin
    mar = c(0, 2.7, 1.2, 0.5), # space for one row of text at ticks and to separate plots
    mgp = c(1.8, 0.6, 0),
    xpd = FALSE
)

# 
# #20%
#..............................................................
# boxplot(cbind(coef.knn.mat[2,1,,1],
#               coef.knn.mat[3,1,,1],coef.knn.mat[4,1,,1],
#               coef.knn.mat[5,1,,1],coef.knn.mat[6,1,,1],
#               coef.knn.mat[7,1,,1],coef.knn.mat[8,1,,1],
#               coef.knn.mat[9,1,,1],coef.knn.mat[10,1,,1]),
#               border=c(rep("skyblue3",9)), col = "lightgray",
#               names=c("", "", "","",
#                       "","", "", "",""),ylim=c(-10,14),
#               ylab="", main="Sp1")
# mtext("knn", 2,line=1.7,col="skyblue3", cex=0.8)
# points(Ini.coef.sp1, pch = 3, col="blue3",cex=1.5) 
# # we get very high value not sure it is worth comparing??
# 
# boxplot(cbind(coef.knn.mat[2,2,,1],
#               coef.knn.mat[3,2,,1],coef.knn.mat[4,2,,1],
#               coef.knn.mat[5,2,,1],coef.knn.mat[6,2,,1],
#               coef.knn.mat[7,2,,1],coef.knn.mat[8,2,,1],
#               coef.knn.mat[9,2,,1],coef.knn.mat[10,2,,1]),
#         border=c(rep("skyblue3",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-5,5),
#         ylab="", main="Sp2")
# points(Ini.coef.sp2, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.knn.mat[2,3,,1],
#               coef.knn.mat[3,3,,1],coef.knn.mat[4,3,,1],
#               coef.knn.mat[5,3,,1],coef.knn.mat[6,3,,1],
#               coef.knn.mat[7,3,,1],coef.knn.mat[8,3,,1],
#               coef.knn.mat[9,3,,1],coef.knn.mat[10,3,,1]),
#         border=c(rep("skyblue3",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-4,4),
#         ylab="", main="Sp3")
# points(Ini.coef.sp3, pch = 3, col="blue3",cex=1.5) 
# 
# 
# boxplot(cbind(coef.CF.mat[2,1,,1],
#               coef.CF.mat[3,1,,1],coef.CF.mat[4,1,,1],
#               coef.CF.mat[5,1,,1],coef.CF.mat[6,1,,1],
#               coef.CF.mat[7,1,,1],coef.CF.mat[8,1,,1],
#               coef.CF.mat[9,1,,1],coef.CF.mat[10,1,,1]),
#         border=c(rep("hotpink3",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-8,8),
#         ylab="")
# mtext("CF", 2,line=1.7,col="hotpink3", cex=0.8)
# points(Ini.coef.sp1, pch = 3, col="blue3",cex=1.5)
# 
# boxplot(cbind(coef.CF.mat[2,2,,1],
#               coef.CF.mat[3,2,,1],coef.CF.mat[4,2,,1],
#               coef.CF.mat[5,2,,1],coef.CF.mat[6,2,,1],
#               coef.CF.mat[7,2,,1],coef.CF.mat[8,2,,1],
#               coef.CF.mat[9,2,,1],coef.CF.mat[10,2,,1]),
#         border=c(rep("hotpink3",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-5,5),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.CF.mat[2,3,,1],
#               coef.CF.mat[3,3,,1],coef.CF.mat[4,3,,1],
#               coef.CF.mat[5,3,,1],coef.CF.mat[6,3,,1],
#               coef.CF.mat[7,3,,1],coef.CF.mat[8,3,,1],
#               coef.CF.mat[9,3,,1],coef.CF.mat[10,3,,1]),
#         border=c(rep("hotpink3",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.ind.mat[2,1,,1],
#               coef.ind.mat[3,1,,1],coef.ind.mat[4,1,,1],
#               coef.ind.mat[5,1,,1],coef.ind.mat[6,1,,1],
#               coef.ind.mat[7,1,,1],coef.ind.mat[8,1,,1],
#               coef.ind.mat[9,1,,1],coef.ind.mat[10,1,,1]),
#         border=c(rep("darkgoldenrod",9)), col = "lightgray",
#         names=c("", "", "","",
#                 "","", "", "",""),ylim=c(-4,8),
#         ylab="")
# mtext("Indiv", 2,line=1.7,col="darkgoldenrod", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3",cex=1.5)
# 
# 
# boxplot(cbind(coef.ind.mat[2,2,,1],
#               coef.ind.mat[3,2,,1],coef.ind.mat[4,2,,1],
#               coef.ind.mat[5,2,,1],coef.ind.mat[6,2,,1],
#               coef.ind.mat[7,2,,1],coef.ind.mat[8,2,,1],
#               coef.ind.mat[9,2,,1],coef.ind.mat[10,2,,1]),
#         border=c(rep("darkgoldenrod",9)), col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-5,5),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3",cex=1.5) 
# 
# 
# boxplot(cbind(coef.ind.mat[2,3,,1],
#               coef.ind.mat[3,3,,1],coef.ind.mat[4,3,,1],
#               coef.ind.mat[5,3,,1],coef.ind.mat[6,3,,1],
#               coef.ind.mat[7,3,,1],coef.ind.mat[8,3,,1],
#               coef.ind.mat[9,3,,1],coef.ind.mat[10,3,,1]),
#         border=c(rep("darkgoldenrod",9)),col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.LT.mat[2,1,,1],
#               coef.LT.mat[3,1,,1],coef.LT.mat[4,1,,1],
#               coef.LT.mat[5,1,,1],coef.LT.mat[6,1,,1],
#               coef.LT.mat[7,1,,1],coef.LT.mat[8,1,,1],
#               coef.LT.mat[9,1,,1],coef.LT.mat[10,1,,1]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-4,8),
#         ylab="")
# mtext("LoopT", 2,line=1.7,col="chartreuse3", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3",cex=1.5)
# 
# boxplot(cbind(coef.LT.mat[2,2,,1],
#               coef.LT.mat[3,2,,1],coef.LT.mat[4,2,,1],
#               coef.LT.mat[5,2,,1],coef.LT.mat[6,2,,1],
#               coef.LT.mat[7,2,,1],coef.LT.mat[8,2,,1],
#               coef.LT.mat[9,2,,1],coef.LT.mat[10,2,,1]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("","", "", "","","", "",
#                 "",""),ylim=c(-5,5),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.LT.mat[2,3,,1],
#               coef.LT.mat[3,3,,1],coef.LT.mat[4,3,,1],
#               coef.LT.mat[5,3,,1],coef.LT.mat[6,3,,1],
#               coef.LT.mat[7,3,,1],coef.LT.mat[8,3,,1],
#               coef.LT.mat[9,3,,1],coef.LT.mat[10,3,,1]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3",cex=1.5) 
# 
# 
# boxplot(cbind(coef.LA.mat[2,1,,1],
#               coef.LA.mat[3,1,,1],coef.LA.mat[4,1,,1],
#               coef.LA.mat[5,1,,1],coef.LA.mat[6,1,,1],
#               coef.LA.mat[7,1,,1],coef.LA.mat[8,1,,1],
#               coef.LA.mat[9,1,,1],coef.LA.mat[10,1,,1]),
#         border=c(rep("coral2",9)),col = "lightgray",
#         names=vec.names, las=2,ylim=c(-4,8),
#         ylab="")
# mtext("LoopA", 2,line=1.7,col="coral2", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3",cex=1.5)
# 
# 
# boxplot(cbind(coef.LA.mat[2,2,,1],
#               coef.LA.mat[3,2,,1],coef.LA.mat[4,2,,1],
#               coef.LA.mat[5,2,,1],coef.LA.mat[6,2,,1],
#               coef.LA.mat[7,2,,1],coef.LA.mat[8,2,,1],
#               coef.LA.mat[9,2,,1],coef.LA.mat[10,2,,1]),
#         border=c(rep("coral2",9)),col = "lightgray",
#         names=vec.names, las=2,ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3",cex=1.5) 
# 
# boxplot(cbind(coef.LA.mat[2,3,,1],
#               coef.LA.mat[3,3,,1],coef.LA.mat[4,3,,1],
#               coef.LA.mat[5,3,,1],coef.LA.mat[6,3,,1],
#               coef.LA.mat[7,3,,1],coef.LA.mat[8,3,,1],
#               coef.LA.mat[9,3,,1],coef.LA.mat[10,3,,1]),
#         border=c(rep("coral2",9)), col = "lightgray",
#         las=2, names=vec.names, ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3",cex=1.5) 
# 
# mtext("Coefficients 20% hidden observations - L noB - Test3", 
#       side = 3, line = 0, outer = TRUE, cex = 1)
# 
# 
# 
# #50%
#..............................................................
# boxplot(cbind(coef.knn.mat[2,1,,2],
#               coef.knn.mat[3,1,,2],coef.knn.mat[4,1,,2],
#               coef.knn.mat[5,1,,2],coef.knn.mat[6,1,,2],
#               coef.knn.mat[7,1,,2],coef.knn.mat[8,1,,2],
#               coef.knn.mat[9,1,,2],coef.knn.mat[10,1,,2]),
#         border=c(rep("skyblue3",9)), col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-4,15),
#         ylab="", main="Sp1")
# mtext("knn", 2,line=1.7,col="skyblue3", cex=0.8)
# points(Ini.coef.sp1, pch = 3, col="blue3") 
# # we get very high value not sure it is worth comparing??
# 
# boxplot(cbind(coef.knn.mat[2,2,,2],
#               coef.knn.mat[3,2,,2],coef.knn.mat[4,2,,2],
#               coef.knn.mat[5,2,,2],coef.knn.mat[6,2,,2],
#               coef.knn.mat[7,2,,2],coef.knn.mat[8,2,,2],
#               coef.knn.mat[9,2,,2],coef.knn.mat[10,2,,2]),
#         border=c(rep("skyblue3",9)),col = "lightgray",
#         names=c("", "", "","","", "", "",
#                 "",""),ylim=c(-5,5),
#         ylab="", main="Sp2")
# points(Ini.coef.sp2, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.knn.mat[2,3,,2],
#               coef.knn.mat[3,3,,2],coef.knn.mat[4,3,,2],
#               coef.knn.mat[5,3,,2],coef.knn.mat[6,3,,2],
#               coef.knn.mat[7,3,,2],coef.knn.mat[8,3,,2],
#               coef.knn.mat[9,3,,2],coef.knn.mat[10,3,,2]),
#         border=c(rep("skyblue3",9)),col = "lightgray",
#         names=c("","", "", "","","", "",
#                 "",""),ylim=c(-4,4),
#         ylab="", main="Sp3")
# points(Ini.coef.sp3, pch = 3, col="blue3") 
# 
# 
# boxplot(cbind(coef.CF.mat[2,1,,2],
#               coef.CF.mat[3,1,,2],coef.CF.mat[4,1,,2],
#               coef.CF.mat[5,1,,2],coef.CF.mat[6,1,,2],
#               coef.CF.mat[7,1,,2],coef.CF.mat[8,1,,2],
#               coef.CF.mat[9,1,,2],coef.CF.mat[10,1,,2]),
#         border=c(rep("hotpink3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-10,5),
#         ylab="")
# mtext("CF", 2,line=1.7,col="hotpink3", cex=0.8)
# points(Ini.coef.sp1, pch = 3, col="blue3")
# 
# boxplot(cbind(coef.CF.mat[2,2,,2],
#               coef.CF.mat[3,2,,2],coef.CF.mat[4,2,,2],
#               coef.CF.mat[5,2,,2],coef.CF.mat[6,2,,2],
#               coef.CF.mat[7,2,,2],coef.CF.mat[8,2,,2],
#               coef.CF.mat[9,2,,2],coef.CF.mat[10,2,,2]),
#         border=c(rep("hotpink3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-5,4),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.CF.mat[2,3,,2],
#               coef.CF.mat[3,3,,2],coef.CF.mat[4,3,,2],
#               coef.CF.mat[5,3,,2],coef.CF.mat[6,3,,2],
#               coef.CF.mat[7,3,,2],coef.CF.mat[8,3,,2],
#               coef.CF.mat[9,3,,2],coef.CF.mat[10,3,,2]),
#         border=c(rep("hotpink3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-6,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.ind.mat[2,1,,2],
#               coef.ind.mat[3,1,,2],coef.ind.mat[4,1,,2],
#               coef.ind.mat[5,1,,2],coef.ind.mat[6,1,,2],
#               coef.ind.mat[7,1,,2],coef.ind.mat[8,1,,2],
#               coef.ind.mat[9,1,,2],coef.ind.mat[10,1,,2]),
#         border=c(rep("darkgoldenrod",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-8,12),
#         ylab="")
# mtext("Indiv", 2,line=1.7,col="darkgoldenrod", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3")
# 
# 
# boxplot(cbind(coef.ind.mat[2,2,,2],
#               coef.ind.mat[3,2,,2],coef.ind.mat[4,2,,2],
#               coef.ind.mat[5,2,,2],coef.ind.mat[6,2,,2],
#               coef.ind.mat[7,2,,2],coef.ind.mat[8,2,,2],
#               coef.ind.mat[9,2,,2],coef.ind.mat[10,2,,2]),
#         border=c(rep("darkgoldenrod",9)), col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-5,5),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3") 
# 
# 
# boxplot(cbind(coef.ind.mat[2,3,,2],
#               coef.ind.mat[3,3,,2],coef.ind.mat[4,3,,2],
#               coef.ind.mat[5,3,,2],coef.ind.mat[6,3,,2],
#               coef.ind.mat[7,3,,2],coef.ind.mat[8,3,,2],
#               coef.ind.mat[9,3,,2],coef.ind.mat[10,3,,2]),
#         border=c(rep("darkgoldenrod",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-6,6),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.LT.mat[2,1,,2],
#               coef.LT.mat[3,1,,2],coef.LT.mat[4,1,,2],
#               coef.LT.mat[5,1,,2],coef.LT.mat[6,1,,2],
#               coef.LT.mat[7,1,,2],coef.LT.mat[8,1,,2],
#               coef.LT.mat[9,1,,2],coef.LT.mat[10,1,,2]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-4,10),
#         ylab="")
# mtext("LoopT", 2,line=1.7,col="chartreuse3", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3")
# 
# boxplot(cbind(coef.LT.mat[2,2,,2],
#               coef.LT.mat[3,2,,2],coef.LT.mat[4,2,,2],
#               coef.LT.mat[5,2,,2],coef.LT.mat[6,2,,2],
#               coef.LT.mat[7,2,,2],coef.LT.mat[8,2,,2],
#               coef.LT.mat[9,2,,2],coef.LT.mat[10,2,,2]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-5,5),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.LT.mat[2,3,,2],
#               coef.LT.mat[3,3,,2],coef.LT.mat[4,3,,2],
#               coef.LT.mat[5,3,,2],coef.LT.mat[6,3,,2],
#               coef.LT.mat[7,3,,2],coef.LT.mat[8,3,,2],
#               coef.LT.mat[9,3,,2],coef.LT.mat[10,3,,2]),
#         border=c(rep("chartreuse3",9)),col = "lightgray",
#         names=c("","", "", "","","", "", 
#                 "",""),ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3") 
# 
# 
# boxplot(cbind(coef.LA.mat[2,1,,2],
#               coef.LA.mat[3,1,,2],coef.LA.mat[4,1,,2],
#               coef.LA.mat[5,1,,2],coef.LA.mat[6,1,,2],
#               coef.LA.mat[7,1,,2],coef.LA.mat[8,1,,2],
#               coef.LA.mat[9,1,,2],coef.LA.mat[10,1,,2]),
#         border=c(rep("coral2",9)),col = "lightgray",
#         names=vec.names, las=2,ylim=c(-4,8),
#         ylab="")
# mtext("LoopA", 2,line=1.7,col="coral2", cex=0.8)
# 
# points(Ini.coef.sp1, pch = 3, col="blue3")
# 
# 
# boxplot(cbind(coef.LA.mat[2,2,,2],
#               coef.LA.mat[3,2,,2],coef.LA.mat[4,2,,2],
#               coef.LA.mat[5,2,,2],coef.LA.mat[6,2,,2],
#               coef.LA.mat[7,2,,2],coef.LA.mat[8,2,,2],
#               coef.LA.mat[9,2,,2],coef.LA.mat[10,2,,2]),
#         border=c(rep("coral2",9)),col = "lightgray",
#         names=vec.names, las=2,ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp2, pch = 3, col="blue3") 
# 
# boxplot(cbind(coef.LA.mat[2,3,,2],
#               coef.LA.mat[3,3,,2],coef.LA.mat[4,3,,2],
#               coef.LA.mat[5,3,,2],coef.LA.mat[6,3,,2],
#               coef.LA.mat[7,3,,2],coef.LA.mat[8,3,,2],
#               coef.LA.mat[9,3,,2],coef.LA.mat[10,3,,2]),
#         border=c(rep("coral2",9)), col = "lightgray",
#         las=2, names=vec.names, ylim=c(-4,4),
#         ylab="")
# points(Ini.coef.sp3, pch = 3, col="blue3") 
# 
# mtext("Coefficients 50% hidden observations - L noB - Test3", 
#       side = 3, line = 0, outer = TRUE, cex = 1)


#80%
#..............................................................
boxplot(cbind(coef.knn.mat[2,1,,3],
              coef.knn.mat[3,1,,3],coef.knn.mat[4,1,,3],
              coef.knn.mat[5,1,,3],coef.knn.mat[6,1,,3],
              coef.knn.mat[7,1,,3],coef.knn.mat[8,1,,3],
              coef.knn.mat[9,1,,3],coef.knn.mat[10,1,,3]),
        border=c(rep("skyblue3",9)), col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""), ylim=c(-10,10),
        ylab="", main="Sp1")
mtext("knn", 2,line=1.7,col="skyblue3", cex=0.8)
points(Ini.coef.sp1, pch = 3, col="blue3") 
# we get very high value not sure it is worth comparing??

boxplot(cbind(coef.knn.mat[2,2,,3],
              coef.knn.mat[3,2,,3],coef.knn.mat[4,2,,3],
              coef.knn.mat[5,2,,3],coef.knn.mat[6,2,,3],
              coef.knn.mat[7,2,,3],coef.knn.mat[8,2,,3],
              coef.knn.mat[9,2,,3],coef.knn.mat[10,2,,3]),
        border=c(rep("skyblue3",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-8,6),
        ylab="", main="Sp2")
points(Ini.coef.sp2, pch = 3, col="blue3") 

boxplot(cbind(coef.knn.mat[2,3,,3],
              coef.knn.mat[3,3,,3],coef.knn.mat[4,3,,3],
              coef.knn.mat[5,3,,3],coef.knn.mat[6,3,,3],
              coef.knn.mat[7,3,,3],coef.knn.mat[8,3,,3],
              coef.knn.mat[9,3,,3],coef.knn.mat[10,3,,3]),
        border=c(rep("skyblue3",9)),col = "lightgray",
        names=c("","", "", "","","", "",
                "",""), ylim=c(-10,40),
        ylab="", main="Sp3")
points(Ini.coef.sp3, pch = 3, col="blue3") 


boxplot(cbind(coef.CF.mat[2,1,,3],
              coef.CF.mat[3,1,,3],coef.CF.mat[4,1,,3],
              coef.CF.mat[5,1,,3],coef.CF.mat[6,1,,3],
              coef.CF.mat[7,1,,3],coef.CF.mat[8,1,,3],
              coef.CF.mat[9,1,,3],coef.CF.mat[10,1,,3]),
        border=c(rep("hotpink3",9)),col = "lightgray",
        names=c("","", "", "","","", "",
                "",""),ylim=c(-4,4),
        ylab="")
mtext("CF", 2,line=1.7,col="hotpink3", cex=0.8)
points(Ini.coef.sp1, pch = 3, col="blue3")

boxplot(cbind(coef.CF.mat[2,2,,3],
              coef.CF.mat[3,2,,3],coef.CF.mat[4,2,,3],
              coef.CF.mat[5,2,,3],coef.CF.mat[6,2,,3],
              coef.CF.mat[7,2,,3],coef.CF.mat[8,2,,3],
              coef.CF.mat[9,2,,3],coef.CF.mat[10,2,,3]),
        border=c(rep("hotpink3",9)),col = "lightgray",
        names=c("","", "", "","","", "",
                "",""),ylim=c(-4,4),
        ylab="")
points(Ini.coef.sp2, pch = 3, col="blue3") 

boxplot(cbind(coef.CF.mat[2,3,,3],
              coef.CF.mat[3,3,,3],coef.CF.mat[4,3,,3],
              coef.CF.mat[5,3,,3],coef.CF.mat[6,3,,3],
              coef.CF.mat[7,3,,3],coef.CF.mat[8,3,,3],
              coef.CF.mat[9,3,,3],coef.CF.mat[10,3,,3]),
        border=c(rep("hotpink3",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-4,4),
        ylab="")
points(Ini.coef.sp3, pch = 3, col="blue3") 

boxplot(cbind(coef.ind.mat[2,1,,3],
              coef.ind.mat[3,1,,3],coef.ind.mat[4,1,,3],
              coef.ind.mat[5,1,,3],coef.ind.mat[6,1,,3],
              coef.ind.mat[7,1,,3],coef.ind.mat[8,1,,3],
              coef.ind.mat[9,1,,3],coef.ind.mat[10,1,,3]),
        border=c(rep("darkgoldenrod",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-10,15),
        ylab="")
mtext("Indiv", 2,line=1.7,col="darkgoldenrod", cex=0.8)

points(Ini.coef.sp1, pch = 3, col="blue3")


boxplot(cbind(coef.ind.mat[2,2,,3],
              coef.ind.mat[3,2,,3],coef.ind.mat[4,2,,3],
              coef.ind.mat[5,2,,3],coef.ind.mat[6,2,,3],
              coef.ind.mat[7,2,,3],coef.ind.mat[8,2,,3],
              coef.ind.mat[9,2,,3],coef.ind.mat[10,2,,3]),
        border=c(rep("darkgoldenrod",9)), col = "lightgray",
        names=c("","", "", "","","", "",
                "",""),ylim=c(-6,6),
        ylab="")
points(Ini.coef.sp2, pch = 3, col="blue3") 


boxplot(cbind(coef.ind.mat[2,3,,3],
              coef.ind.mat[3,3,,3],coef.ind.mat[4,3,,3],
              coef.ind.mat[5,3,,3],coef.ind.mat[6,3,,3],
              coef.ind.mat[7,3,,3],coef.ind.mat[8,3,,3],
              coef.ind.mat[9,3,,3],coef.ind.mat[10,3,,3]),
        border=c(rep("darkgoldenrod",9)),col = "lightgray",
        names=c("","", "", "","","", "",
                "",""),ylim=c(-40,40),
        ylab="")
points(Ini.coef.sp3, pch = 3, col="blue3") 

boxplot(cbind(coef.LT.mat[2,1,,3],
              coef.LT.mat[3,1,,3],coef.LT.mat[4,1,,3],
              coef.LT.mat[5,1,,3],coef.LT.mat[6,1,,3],
              coef.LT.mat[7,1,,3],coef.LT.mat[8,1,,3],
              coef.LT.mat[9,1,,3],coef.LT.mat[10,1,,3]),
        border=c(rep("chartreuse3",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-4,4),
        ylab="")
mtext("LoopT", 2,line=1.7,col="chartreuse3", cex=0.8)

points(Ini.coef.sp1, pch = 3, col="blue3")

boxplot(cbind(coef.LT.mat[2,2,,3],
              coef.LT.mat[3,2,,3],coef.LT.mat[4,2,,3],
              coef.LT.mat[5,2,,3],coef.LT.mat[6,2,,3],
              coef.LT.mat[7,2,,3],coef.LT.mat[8,2,,3],
              coef.LT.mat[9,2,,3],coef.LT.mat[10,2,,3]),
        border=c(rep("chartreuse3",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-6,4),
        ylab="")
points(Ini.coef.sp2, pch = 3, col="blue3") 

boxplot(cbind(coef.LT.mat[2,3,,3],
              coef.LT.mat[3,3,,3],coef.LT.mat[4,3,,3],
              coef.LT.mat[5,3,,3],coef.LT.mat[6,3,,3],
              coef.LT.mat[7,3,,3],coef.LT.mat[8,3,,3],
              coef.LT.mat[9,3,,3],coef.LT.mat[10,3,,3]),
        border=c(rep("chartreuse3",9)),col = "lightgray",
        names=c("","", "", "","","", "", 
                "",""),ylim=c(-40,60),
        ylab="")
points(Ini.coef.sp3, pch = 3, col="blue3") 


boxplot(cbind(coef.LA.mat[2,1,,3],
              coef.LA.mat[3,1,,3],coef.LA.mat[4,1,,3],
              coef.LA.mat[5,1,,3],coef.LA.mat[6,1,,3],
              coef.LA.mat[7,1,,3],coef.LA.mat[8,1,,3],
              coef.LA.mat[9,1,,3],coef.LA.mat[10,1,,3]),
        border=c(rep("coral2",9)),col = "lightgray",
        names=vec.names, las=2,ylim=c(-4,4),
        ylab="")
mtext("LoopA", 2,line=1.7,col="coral2", cex=0.8)

points(Ini.coef.sp1, pch = 3, col="blue3")


boxplot(cbind(coef.LA.mat[2,2,,3],
              coef.LA.mat[3,2,,3],coef.LA.mat[4,2,,3],
              coef.LA.mat[5,2,,3],coef.LA.mat[6,2,,3],
              coef.LA.mat[7,2,,3],coef.LA.mat[8,2,,3],
              coef.LA.mat[9,2,,3],coef.LA.mat[10,2,,3]),
        border=c(rep("coral2",9)),col = "lightgray",
        names=vec.names, las=2,ylim=c(-4,4),
        ylab="")
points(Ini.coef.sp2, pch = 3, col="blue3") 

boxplot(cbind(coef.LA.mat[2,3,,3],
              coef.LA.mat[3,3,,3],coef.LA.mat[4,3,,3],
              coef.LA.mat[5,3,,3],coef.LA.mat[6,3,,3],
              coef.LA.mat[7,3,,3],coef.LA.mat[8,3,,3],
              coef.LA.mat[9,3,,3],coef.LA.mat[10,3,,3]),
        border=c(rep("coral2",9)), col = "lightgray",
        las=2, names=vec.names,ylim=c(-4,4),
        ylab="")
points(Ini.coef.sp3, pch = 3, col="blue3") 

mtext("Coefficients 80% hidden observations - Lasso bias - Test 1", 
      side = 3, line = 0, outer = TRUE, cex = 1)
