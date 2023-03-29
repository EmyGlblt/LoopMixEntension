setwd("//rcgdata.newcastle.edu.au/c3286500/Sim/LassoBias")

load("Sim_2030_lassoBAugtest1Oct20.RDATA")
LB1 = QuickTest

load("Sim_41_lassoBAugtest1Oct20.RDATA")
LB2 = QuickTest

load("Sim_451_lassoBAugtest1Oct20.RDATA")
LB3 = QuickTest

load("Sim_733_lassoBAugtest1Oct20.RDATA")
LB4 = QuickTest

load("Sim_88_lassoBAugtest1Oct20.RDATA")
LB5 = QuickTest

load("Sim_619_lassoBAugtest1Oct20.RDATA")
LB6 = QuickTest

load("Sim_910_lassoBAugtest1Oct20.RDATA")
LB7 = QuickTest

load("Sim_21_lassoBAugtest1Oct20.RDATA")
LB8 = QuickTest

load("Sim_2100_lassoBAugtest1Oct20.RDATA")
LB9 = QuickTest

load("Sim_333_lassoBAugtest1Oct20.RDATA")
LB10 = QuickTest

load("Sim_10030_lassoBAugtest1Oct20.RDATA")
LB11 = QuickTest

load("Sim_551_lassoBAugtest1Oct20.RDATA")
LB12 = QuickTest

load("Sim_119_lassoBAugtest1Oct20.RDATA")
LB13 = QuickTest

load("Sim_210_lassoBAugtest1Oct20.RDATA")
LB14 = QuickTest

load("Sim_3133_lassoBAugtest1Oct20.RDATA")
LB15 = QuickTest

load("Sim_51_lassoBAugtest1Oct20.RDATA")
LB16 = QuickTest

load("Sim_800_lassoBAugtest1Oct20.RDATA")
LB17 = QuickTest

load("Sim_756_lassoBAugtest1Oct20.RDATA")
LB18 = QuickTest

load("Sim_7000_lassoBAugtest1Oct20.RDATA")
LB19 = QuickTest

load("Sim_8_lassoBAugtest1Oct20.RDATA")
LB20 = QuickTest


coef.knn.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.knn.mat[i,j,,k] =  c(LB1$coef.knn.mat[i,j,,k], LB2$coef.knn.mat[i,j,,k], LB2$coef.knn.mat[i,j,,k],
								LB4$coef.knn.mat[i,j,,k], LB5$coef.knn.mat[i,j,,k], LB6$coef.knn.mat[i,j,,k],
								LB7$coef.knn.mat[i,j,,k], LB8$coef.knn.mat[i,j,,k], LB9$coef.knn.mat[i,j,,k],
								LB10$coef.knn.mat[i,j,,k], LB11$coef.knn.mat[i,j,,k], LB12$coef.knn.mat[i,j,,k],
								LB13$coef.knn.mat[i,j,,k], LB14$coef.knn.mat[i,j,,k], LB15$coef.knn.mat[i,j,,k],
								LB16$coef.knn.mat[i,j,,k], LB17$coef.knn.mat[i,j,,k], LB18$coef.knn.mat[i,j,,k],
								LB19$coef.knn.mat[i,j,,k], LB20$coef.knn.mat[i,j,,k])
    }
  }
}

coef.CF.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.CF.mat[i,j,,k] =  c(LB1$coef.CF.mat[i,j,,k], LB2$coef.CF.mat[i,j,,k], LB2$coef.CF.mat[i,j,,k],
								LB4$coef.CF.mat[i,j,,k], LB5$coef.CF.mat[i,j,,k], LB6$coef.CF.mat[i,j,,k],
								LB7$coef.CF.mat[i,j,,k], LB8$coef.CF.mat[i,j,,k], LB9$coef.CF.mat[i,j,,k],
								LB10$coef.CF.mat[i,j,,k], LB11$coef.CF.mat[i,j,,k], LB12$coef.CF.mat[i,j,,k],
								LB13$coef.CF.mat[i,j,,k], LB14$coef.CF.mat[i,j,,k], LB15$coef.CF.mat[i,j,,k],
								LB16$coef.CF.mat[i,j,,k], LB17$coef.CF.mat[i,j,,k], LB18$coef.CF.mat[i,j,,k],
								LB19$coef.CF.mat[i,j,,k], LB20$coef.CF.mat[i,j,,k])
    }
  }
}

coef.kmeans.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.kmeans.mat[i,j,,k] =  c(LB1$coef.kmeans.mat[i,j,,k], LB2$coef.kmeans.mat[i,j,,k], LB2$coef.kmeans.mat[i,j,,k],
								LB4$coef.kmeans.mat[i,j,,k], LB5$coef.kmeans.mat[i,j,,k], LB6$coef.kmeans.mat[i,j,,k],
								LB7$coef.kmeans.mat[i,j,,k], LB8$coef.kmeans.mat[i,j,,k], LB9$coef.kmeans.mat[i,j,,k],
								LB10$coef.kmeans.mat[i,j,,k], LB11$coef.kmeans.mat[i,j,,k], LB12$coef.kmeans.mat[i,j,,k],
								LB13$coef.kmeans.mat[i,j,,k], LB14$coef.kmeans.mat[i,j,,k], LB15$coef.kmeans.mat[i,j,,k],
								LB16$coef.kmeans.mat[i,j,,k], LB17$coef.kmeans.mat[i,j,,k], LB18$coef.kmeans.mat[i,j,,k],
								LB19$coef.kmeans.mat[i,j,,k], LB20$coef.kmeans.mat[i,j,,k])
    }
  }
}

coef.rand.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.rand.mat[i,j,,k] =  c(LB1$coef.rand.mat[i,j,,k], LB2$coef.rand.mat[i,j,,k], LB2$coef.rand.mat[i,j,,k],
								LB4$coef.rand.mat[i,j,,k], LB5$coef.rand.mat[i,j,,k], LB6$coef.rand.mat[i,j,,k],
								LB7$coef.rand.mat[i,j,,k], LB8$coef.rand.mat[i,j,,k], LB9$coef.rand.mat[i,j,,k],
								LB10$coef.rand.mat[i,j,,k], LB11$coef.rand.mat[i,j,,k], LB12$coef.rand.mat[i,j,,k],
								LB13$coef.rand.mat[i,j,,k], LB14$coef.rand.mat[i,j,,k], LB15$coef.rand.mat[i,j,,k],
								LB16$coef.rand.mat[i,j,,k], LB17$coef.rand.mat[i,j,,k], LB18$coef.rand.mat[i,j,,k],
								LB19$coef.rand.mat[i,j,,k], LB20$coef.rand.mat[i,j,,k])
    }
  }
}

coef.ind.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.ind.mat[i,j,,k] =  c(LB1$coef.ind.mat[i,j,,k], LB2$coef.ind.mat[i,j,,k], LB2$coef.ind.mat[i,j,,k],
								LB4$coef.ind.mat[i,j,,k], LB5$coef.ind.mat[i,j,,k], LB6$coef.ind.mat[i,j,,k],
								LB7$coef.ind.mat[i,j,,k], LB8$coef.ind.mat[i,j,,k], LB9$coef.ind.mat[i,j,,k],
								LB10$coef.ind.mat[i,j,,k], LB11$coef.ind.mat[i,j,,k], LB12$coef.ind.mat[i,j,,k],
								LB13$coef.ind.mat[i,j,,k], LB14$coef.ind.mat[i,j,,k], LB15$coef.ind.mat[i,j,,k],
								LB16$coef.ind.mat[i,j,,k], LB17$coef.ind.mat[i,j,,k], LB18$coef.ind.mat[i,j,,k],
								LB19$coef.ind.mat[i,j,,k], LB20$coef.ind.mat[i,j,,k])
    }
  }
}

coef.LA.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.LA.mat[i,j,,k] =  c(LB1$coef.LA.mat[i,j,,k], LB2$coef.LA.mat[i,j,,k], LB2$coef.LA.mat[i,j,,k],
								LB4$coef.LA.mat[i,j,,k], LB5$coef.LA.mat[i,j,,k], LB6$coef.LA.mat[i,j,,k],
								LB7$coef.LA.mat[i,j,,k], LB8$coef.LA.mat[i,j,,k], LB9$coef.LA.mat[i,j,,k],
								LB10$coef.LA.mat[i,j,,k], LB11$coef.LA.mat[i,j,,k], LB12$coef.LA.mat[i,j,,k],
								LB13$coef.LA.mat[i,j,,k], LB14$coef.LA.mat[i,j,,k], LB15$coef.LA.mat[i,j,,k],
								LB16$coef.LA.mat[i,j,,k], LB17$coef.LA.mat[i,j,,k], LB18$coef.LA.mat[i,j,,k],
								LB19$coef.LA.mat[i,j,,k], LB20$coef.LA.mat[i,j,,k])
    }
  }
}

coef.LT.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.LT.mat[i,j,,k] =  c(LB1$coef.LT.mat[i,j,,k], LB2$coef.LT.mat[i,j,,k], LB2$coef.LT.mat[i,j,,k],
								LB4$coef.LT.mat[i,j,,k], LB5$coef.LT.mat[i,j,,k], LB6$coef.LT.mat[i,j,,k],
								LB7$coef.LT.mat[i,j,,k], LB8$coef.LT.mat[i,j,,k], LB9$coef.LT.mat[i,j,,k],
								LB10$coef.LT.mat[i,j,,k], LB11$coef.LT.mat[i,j,,k], LB12$coef.LT.mat[i,j,,k],
								LB13$coef.LT.mat[i,j,,k], LB14$coef.LT.mat[i,j,,k], LB15$coef.LT.mat[i,j,,k],
								LB16$coef.LT.mat[i,j,,k], LB17$coef.LT.mat[i,j,,k], LB18$coef.LT.mat[i,j,,k],
								LB19$coef.LT.mat[i,j,,k], LB20$coef.LT.mat[i,j,,k])
    }
  }
}

coef.LE.mat = array(NA, c(10,3,20*50,3))
for (i in 1:10) {
  for (j in 1:3) {
    for (k in 1:3) {
      coef.LE.mat[i,j,,k] =  c(LB1$coef.LE.mat[i,j,,k], LB2$coef.LE.mat[i,j,,k], LB2$coef.LE.mat[i,j,,k],
								LB4$coef.LE.mat[i,j,,k], LB5$coef.LE.mat[i,j,,k], LB6$coef.LE.mat[i,j,,k],
								LB7$coef.LE.mat[i,j,,k], LB8$coef.LE.mat[i,j,,k], LB9$coef.LE.mat[i,j,,k],
								LB10$coef.LE.mat[i,j,,k], LB11$coef.LE.mat[i,j,,k], LB12$coef.LE.mat[i,j,,k],
								LB13$coef.LE.mat[i,j,,k], LB14$coef.LE.mat[i,j,,k], LB15$coef.LE.mat[i,j,,k],
								LB16$coef.LE.mat[i,j,,k], LB17$coef.LE.mat[i,j,,k], LB18$coef.LE.mat[i,j,,k],
								LB19$coef.LE.mat[i,j,,k], LB20$coef.LE.mat[i,j,,k])
    }
  }
}



save(coef.LA.mat, coef.LT.mat, coef.LE.mat, coef.ind.mat, coef.kmeans.mat, coef.CF.mat, coef.rand.mat, coef.knn.mat,
     file = "RescoefLBtest1.RData")	
