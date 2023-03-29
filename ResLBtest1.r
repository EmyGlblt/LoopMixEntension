
load("Sim_2030_lassoBAugtest1Oct20.RDATA")
Quick4.1 = QuickTest

load("Sim_41_lassoBAugtest1Oct20.RDATA")
Quick4.2 = QuickTest

load("Sim_451_lassoBAugtest1Oct20.RDATA")
Quick4.3 = QuickTest

load("Sim_733_lassoBAugtest1Oct20.RDATA")
Quick4.4 = QuickTest

load("Sim_88_lassoBAugtest1Oct20.RDATA")
Quick4.5 = QuickTest

load("Sim_619_lassoBAugtest1Oct20.RDATA")
Quick4.50 = QuickTest


load("Sim_910_lassoBAugtest1Oct20.RDATA")
Quick4.100 = QuickTest

load("Sim_21_lassoBAugtest1Oct20.RDATA")
Quick4.150 = QuickTest

load("Sim_2100_lassoBAugtest1Oct20.RDATA")
Quick4.201 = QuickTest

load("Sim_333_lassoBAugtest1Oct20.RDATA")
Quick4.250 = QuickTest

load("Sim_10030_lassoBAugtest1Oct20.RDATA")
Quick4.300 = QuickTest

load("Sim_551_lassoBAugtest1Oct20.RDATA")
Quick4.350 = QuickTest

load("Sim_119_lassoBAugtest1Oct20.RDATA")
Quick4.401 = QuickTest

load("Sim_210_lassoBAugtest1Oct20.RDATA")
Quick4.450 = QuickTest

load("Sim_3133_lassoBAugtest1Oct20.RDATA")
Quick4.500 = QuickTest

load("Sim_51_lassoBAugtest1Oct20.RDATA")
Quick4.550 = QuickTest

load("Sim_800_lassoBAugtest1Oct20.RDATA")
Quick4.650 = QuickTest

load("Sim_756_lassoBAugtest1Oct20.RDATA")
Quick4.700 = QuickTest

load("Sim_7000_lassoBAugtest1Oct20.RDATA")
Quick4.750 = QuickTest

load("Sim_8_lassoBAugtest1Oct20.RDATA")
Quick4.850 = QuickTest

# Combine meanRSS
# Combine meanRSS
test1meanRSS.knn = rbind(Quick4.1$meanRSSknn, Quick4.2$meanRSSknn, Quick4.3$meanRSSknn, Quick4.4$meanRSSknn,
                              Quick4.5$meanRSSknn, Quick4.50$meanRSSknn, Quick4.100$meanRSSknn, Quick4.150$meanRSSknn, Quick4.201$meanRSSknn,
                              Quick4.250$meanRSSknn, Quick4.300$meanRSSknn, Quick4.350$meanRSSknn, Quick4.401$meanRSSknn, Quick4.450$meanRSSknn,
                              Quick4.500$meanRSSknn, Quick4.550$meanRSSknn, Quick4.650$meanRSSknn, Quick4.700$meanRSSknn, Quick4.750$meanRSSknn,
                              Quick4.850$meanRSSknn)

test1meanRSS.kmeans = rbind(Quick4.1$meanRSSkmeans, Quick4.2$meanRSSkmeans, Quick4.3$meanRSSkmeans, Quick4.4$meanRSSkmeans,
                              Quick4.5$meanRSSkmeans, Quick4.50$meanRSSkmeans, Quick4.100$meanRSSkmeans, Quick4.150$meanRSSkmeans, Quick4.201$meanRSSkmeans,
                              Quick4.250$meanRSSkmeans, Quick4.300$meanRSSkmeans, Quick4.350$meanRSSkmeans, Quick4.401$meanRSSkmeans, Quick4.450$meanRSSkmeans,
                              Quick4.500$meanRSSkmeans, Quick4.550$meanRSSkmeans, Quick4.650$meanRSSkmeans, Quick4.700$meanRSSkmeans, Quick4.750$meanRSSkmeans,
                              Quick4.850$meanRSSkmeans)

test1meanRSS.rand = rbind(Quick4.1$meanRSSrand, Quick4.2$meanRSSrand, Quick4.3$meanRSSrand, Quick4.4$meanRSSrand,
                              Quick4.5$meanRSSrand, Quick4.50$meanRSSrand, Quick4.100$meanRSSrand, Quick4.150$meanRSSrand, Quick4.201$meanRSSrand,
                              Quick4.250$meanRSSrand, Quick4.300$meanRSSrand, Quick4.350$meanRSSrand, Quick4.401$meanRSSrand, Quick4.450$meanRSSrand,
                              Quick4.500$meanRSSrand, Quick4.550$meanRSSrand, Quick4.650$meanRSSrand, Quick4.700$meanRSSrand, Quick4.750$meanRSSrand,
                              Quick4.850$meanRSSrand)

test1meanRSS.kps = rbind(Quick4.1$meanRSSkps, Quick4.2$meanRSSkps, Quick4.3$meanRSSkps, Quick4.4$meanRSSkps,
                                Quick4.5$meanRSSkps, Quick4.50$meanRSSkps, Quick4.100$meanRSSkps, Quick4.150$meanRSSkps, Quick4.201$meanRSSkps,
                              Quick4.250$meanRSSkps, Quick4.300$meanRSSkps, Quick4.350$meanRSSkps, Quick4.401$meanRSSkps, Quick4.450$meanRSSkps,
                              Quick4.500$meanRSSkps, Quick4.550$meanRSSkps, Quick4.650$meanRSSkps, Quick4.700$meanRSSkps, Quick4.750$meanRSSkps,
                              Quick4.850$meanRSSkps)
							  
test1meanRSS.CF = rbind(Quick4.1$meanRSSCF, Quick4.2$meanRSSCF, Quick4.3$meanRSSCF, Quick4.4$meanRSSCF,
                                Quick4.5$meanRSSCF, Quick4.50$meanRSSCF, Quick4.100$meanRSSCF, Quick4.150$meanRSSCF, Quick4.201$meanRSSCF,
                              Quick4.250$meanRSSCF, Quick4.300$meanRSSCF, Quick4.350$meanRSSCF, Quick4.401$meanRSSCF, Quick4.450$meanRSSCF,
                              Quick4.500$meanRSSCF, Quick4.550$meanRSSCF, Quick4.650$meanRSSCF, Quick4.700$meanRSSCF, Quick4.750$meanRSSCF,
                              Quick4.850$meanRSSCF)

test1meanRSS.indiv = rbind(Quick4.1$meanRSSindiv, Quick4.2$meanRSSindiv, Quick4.3$meanRSSindiv, Quick4.4$meanRSSindiv,
                              Quick4.5$meanRSSindiv, Quick4.50$meanRSSindiv, Quick4.100$meanRSSindiv, Quick4.150$meanRSSindiv, Quick4.201$meanRSSindiv,
                              Quick4.250$meanRSSindiv, Quick4.300$meanRSSindiv, Quick4.350$meanRSSindiv, Quick4.401$meanRSSindiv, Quick4.450$meanRSSindiv,
                              Quick4.500$meanRSSindiv, Quick4.550$meanRSSindiv, Quick4.650$meanRSSindiv, Quick4.700$meanRSSindiv, Quick4.750$meanRSSindiv,
                              Quick4.850$meanRSSindiv)

test1meanRSS.LA = rbind(Quick4.1$meanRSSLoopA, Quick4.2$meanRSSLoopA, Quick4.3$meanRSSLoopA, Quick4.4$meanRSSLoopA,
                              Quick4.5$meanRSSLoopA, Quick4.50$meanRSSLoopA, Quick4.100$meanRSSLoopA, Quick4.150$meanRSSLoopA, Quick4.201$meanRSSLoopA,
                              Quick4.250$meanRSSLoopA, Quick4.300$meanRSSLoopA, Quick4.350$meanRSSLoopA, Quick4.401$meanRSSLoopA, Quick4.450$meanRSSLoopA,
                              Quick4.500$meanRSSLoopA, Quick4.550$meanRSSLoopA, Quick4.650$meanRSSLoopA, Quick4.700$meanRSSLoopA, Quick4.750$meanRSSLoopA,
                              Quick4.850$meanRSSLoopA)							  
							  
test1meanRSS.LT = rbind(Quick4.1$meanRSSLoopT, Quick4.2$meanRSSLoopT, Quick4.3$meanRSSLoopT, Quick4.4$meanRSSLoopT,
                              Quick4.5$meanRSSLoopT, Quick4.50$meanRSSLoopT, Quick4.100$meanRSSLoopT, Quick4.150$meanRSSLoopT, Quick4.201$meanRSSLoopT,
                              Quick4.250$meanRSSLoopT, Quick4.300$meanRSSLoopT, Quick4.350$meanRSSLoopT, Quick4.401$meanRSSLoopT, Quick4.450$meanRSSLoopT,
                              Quick4.500$meanRSSLoopT, Quick4.550$meanRSSLoopT, Quick4.650$meanRSSLoopT, Quick4.700$meanRSSLoopT, Quick4.750$meanRSSLoopT,
                              Quick4.850$meanRSSLoopT)

test1meanRSS.LE = rbind(Quick4.1$meanRSSLoopA, Quick4.2$meanRSSLoopA, Quick4.3$meanRSSLoopA, Quick4.4$meanRSSLoopA,
                              Quick4.5$meanRSSLoopA, Quick4.50$meanRSSLoopA, Quick4.100$meanRSSLoopA, Quick4.150$meanRSSLoopA, Quick4.201$meanRSSLoopA,
                              Quick4.250$meanRSSLoopA, Quick4.300$meanRSSLoopA, Quick4.350$meanRSSLoopA, Quick4.401$meanRSSLoopA, Quick4.450$meanRSSLoopA,
                              Quick4.500$meanRSSLoopA, Quick4.550$meanRSSLoopA, Quick4.650$meanRSSLoopA, Quick4.700$meanRSSLoopA, Quick4.750$meanRSSLoopA,
                              Quick4.850$meanRSSLoopA)
							  
# Combine IMSE
test1IMSE.knn = rbind(Quick4.1$IMSEknn2, Quick4.2$IMSEknn2, Quick4.3$IMSEknn2, Quick4.4$IMSEknn2,
                              Quick4.5$IMSEknn2, Quick4.50$IMSEknn2, Quick4.100$IMSEknn2, Quick4.150$IMSEknn2, Quick4.201$IMSEknn2,
                              Quick4.250$IMSEknn2, Quick4.300$IMSEknn2, Quick4.350$IMSEknn2, Quick4.401$IMSEknn2, Quick4.450$IMSEknn2,
                              Quick4.500$IMSEknn2, Quick4.550$IMSEknn2, Quick4.650$IMSEknn2, Quick4.700$IMSEknn2, Quick4.750$IMSEknn2,
                              Quick4.850$IMSEknn2)

test1IMSE.kmeans = rbind(Quick4.1$IMSEkmeans2, Quick4.2$IMSEkmeans2, Quick4.3$IMSEkmeans2, Quick4.4$IMSEkmeans2,
                              Quick4.5$IMSEkmeans2, Quick4.50$IMSEkmeans2, Quick4.100$IMSEkmeans2, Quick4.150$IMSEkmeans2, Quick4.201$IMSEkmeans2,
                                 Quick4.250$IMSEkmeans2, Quick4.300$IMSEkmeans2, Quick4.350$IMSEkmeans2, Quick4.401$IMSEkmeans2, Quick4.450$IMSEkmeans2,
                                 Quick4.500$IMSEkmeans2, Quick4.550$IMSEkmeans2, Quick4.650$IMSEkmeans2, Quick4.700$IMSEkmeans2, Quick4.750$IMSEkmeans2,
                                 Quick4.850$IMSEkmeans2)

test1IMSE.rand = rbind(Quick4.1$IMSErand2, Quick4.2$IMSErand2, Quick4.3$IMSErand2, Quick4.4$IMSErand2,
                            Quick4.5$IMSErand2, Quick4.50$IMSErand2, Quick4.100$IMSErand2, Quick4.150$IMSErand2, Quick4.201$IMSErand2,
                               Quick4.250$IMSErand2, Quick4.300$IMSErand2, Quick4.350$IMSErand2, Quick4.401$IMSErand2, Quick4.450$IMSErand2,
                               Quick4.500$IMSErand2, Quick4.550$IMSErand2, Quick4.650$IMSErand2, Quick4.700$IMSErand2, Quick4.750$IMSErand2,
                               Quick4.850$IMSErand2)

test1IMSE.CF = rbind(Quick4.1$IMSECF2, Quick4.2$IMSECF2, Quick4.3$IMSECF2, Quick4.4$IMSECF2,
                             Quick4.5$IMSECF2, Quick4.50$IMSECF2, Quick4.100$IMSECF2, Quick4.150$IMSECF2, Quick4.201$IMSECF2,
                                Quick4.250$IMSECF2, Quick4.300$IMSECF2, Quick4.350$IMSECF2, Quick4.401$IMSECF2, Quick4.450$IMSECF2,
                                Quick4.500$IMSECF2, Quick4.550$IMSECF2, Quick4.650$IMSECF2, Quick4.700$IMSECF2, Quick4.750$IMSECF2,
                                Quick4.850$IMSECF2)
								
test1IMSE.indiv = rbind(Quick4.1$IMSEindiv, Quick4.2$IMSEindiv, Quick4.3$IMSEindiv, Quick4.4$IMSEindiv,
                              Quick4.5$IMSEindiv, Quick4.50$IMSEindiv, Quick4.100$IMSEindiv, Quick4.150$IMSEindiv, Quick4.201$IMSEindiv,
                              Quick4.250$IMSEindiv, Quick4.300$IMSEindiv, Quick4.350$IMSEindiv, Quick4.401$IMSEindiv, Quick4.450$IMSEindiv,
                              Quick4.500$IMSEindiv, Quick4.550$IMSEindiv, Quick4.650$IMSEindiv, Quick4.700$IMSEindiv, Quick4.750$IMSEindiv,
                              Quick4.850$IMSEindiv)

test1IMSE.LA = rbind(Quick4.1$IMSELoopA, Quick4.2$IMSELoopA, Quick4.3$IMSELoopA, Quick4.4$IMSELoopA,
                              Quick4.5$IMSELoopA, Quick4.50$IMSELoopA, Quick4.100$IMSELoopA, Quick4.150$IMSELoopA, Quick4.201$IMSELoopA,
                              Quick4.250$IMSELoopA, Quick4.300$IMSELoopA, Quick4.350$IMSELoopA, Quick4.401$IMSELoopA, Quick4.450$IMSELoopA,
                              Quick4.500$IMSELoopA, Quick4.550$IMSELoopA, Quick4.650$IMSELoopA, Quick4.700$IMSELoopA, Quick4.750$IMSELoopA,
                              Quick4.850$IMSELoopA)	

test1IMSE.LT = rbind(Quick4.1$IMSELoopT, Quick4.2$IMSELoopT, Quick4.3$IMSELoopT, Quick4.4$IMSELoopT,
                             Quick4.5$IMSELoopT, Quick4.50$IMSELoopT, Quick4.100$IMSELoopT, Quick4.150$IMSELoopT, Quick4.201$IMSELoopT,
                                Quick4.250$IMSELoopT, Quick4.300$IMSELoopT, Quick4.350$IMSELoopT, Quick4.401$IMSELoopT, Quick4.450$IMSELoopT,
                                Quick4.500$IMSELoopT, Quick4.550$IMSELoopT, Quick4.650$IMSELoopT, Quick4.700$IMSELoopT, Quick4.750$IMSELoopT,
                                Quick4.850$IMSELoopT)
								
test1IMSE.LE = rbind(Quick4.1$IMSELoopE, Quick4.2$IMSELoopE, Quick4.3$IMSELoopE, Quick4.4$IMSELoopE,
                             Quick4.5$IMSELoopE, Quick4.50$IMSELoopE, Quick4.100$IMSELoopE, Quick4.150$IMSELoopE, Quick4.201$IMSELoopE,
                                Quick4.250$IMSELoopE, Quick4.300$IMSELoopE, Quick4.350$IMSELoopE, Quick4.401$IMSELoopE, Quick4.450$IMSELoopE,
                                Quick4.500$IMSELoopE, Quick4.550$IMSELoopE, Quick4.650$IMSELoopE, Quick4.700$IMSELoopE, Quick4.750$IMSELoopE,
                                Quick4.850$IMSELoopE)


								
# Combine sumcor
test1sumcor.knn = rbind(Quick4.1$sumcorknn2, Quick4.2$sumcorknn2, Quick4.3$sumcorknn2, Quick4.4$sumcorknn2,
                              Quick4.5$sumcorknn2, Quick4.50$sumcorknn2, Quick4.100$sumcorknn2, Quick4.150$sumcorknn2, Quick4.201$sumcorknn2,
                              Quick4.250$sumcorknn2, Quick4.300$sumcorknn2, Quick4.350$sumcorknn2, Quick4.401$sumcorknn2, Quick4.450$sumcorknn2,
                              Quick4.500$sumcorknn2, Quick4.550$sumcorknn2, Quick4.650$sumcorknn2, Quick4.700$sumcorknn2, Quick4.750$sumcorknn2,
                              Quick4.850$sumcorknn2)

test1sumcor.kmeans = rbind(Quick4.1$sumcorkmeans2, Quick4.2$sumcorkmeans2, Quick4.3$sumcorkmeans2, Quick4.4$sumcorkmeans2,
                                Quick4.5$sumcorkmeans2, Quick4.50$sumcorkmeans2, Quick4.100$sumcorkmeans2, Quick4.150$sumcorkmeans2, Quick4.201$sumcorkmeans2,
                                 Quick4.250$sumcorkmeans2, Quick4.300$sumcorkmeans2, Quick4.350$sumcorkmeans2, Quick4.401$sumcorkmeans2, Quick4.450$sumcorkmeans2,
                                 Quick4.500$sumcorkmeans2, Quick4.550$sumcorkmeans2, Quick4.650$sumcorkmeans2, Quick4.700$sumcorkmeans2, Quick4.750$sumcorkmeans2,
                                 Quick4.850$sumcorkmeans2)

test1sumcor.rand = rbind(Quick4.1$sumcorrand2, Quick4.2$sumcorrand2, Quick4.3$sumcorrand2, Quick4.4$sumcorrand2,
                              Quick4.5$sumcorrand2, Quick4.50$sumcorrand2, Quick4.100$sumcorrand2, Quick4.150$sumcorrand2, Quick4.201$sumcorrand2,
                               Quick4.250$sumcorrand2, Quick4.300$sumcorrand2, Quick4.350$sumcorrand2, Quick4.401$sumcorrand2, Quick4.450$sumcorrand2,
                               Quick4.500$sumcorrand2, Quick4.550$sumcorrand2, Quick4.650$sumcorrand2, Quick4.700$sumcorrand2, Quick4.750$sumcorrand2,
                               Quick4.850$sumcorrand2)

test1sumcor.CF = rbind(Quick4.1$sumcorCF2, Quick4.2$sumcorCF2, Quick4.3$sumcorCF2, Quick4.4$sumcorCF2,
                               Quick4.5$sumcorCF2, Quick4.50$sumcorCF2, Quick4.100$sumcorCF2, Quick4.150$sumcorCF2, Quick4.201$sumcorCF2,
                                Quick4.250$sumcorCF2, Quick4.300$sumcorCF2, Quick4.350$sumcorCF2, Quick4.401$sumcorCF2, Quick4.450$sumcorCF2,
                                Quick4.500$sumcorCF2, Quick4.550$sumcorCF2, Quick4.650$sumcorCF2, Quick4.700$sumcorCF2, Quick4.750$sumcorCF2,
                                Quick4.850$sumcorCF2)

test1sumcor.indiv = rbind(Quick4.1$sumcorindiv1, Quick4.2$sumcorindiv1, Quick4.3$sumcorindiv1, Quick4.4$sumcorindiv1,
                              Quick4.5$sumcorindiv1, Quick4.50$sumcorindiv1, Quick4.100$sumcorindiv1, Quick4.150$sumcorindiv1, Quick4.201$sumcorindiv1,
                              Quick4.250$sumcorindiv1, Quick4.300$sumcorindiv1, Quick4.350$sumcorindiv1, Quick4.401$sumcorindiv1, Quick4.450$sumcorindiv1,
                              Quick4.500$sumcorindiv1, Quick4.550$sumcorindiv1, Quick4.650$sumcorindiv1, Quick4.700$sumcorindiv1, Quick4.750$sumcorindiv1,
                              Quick4.850$sumcorindiv1)

test1sumcor.LA = rbind(Quick4.1$sumcorLoopA1, Quick4.2$sumcorLoopA1, Quick4.3$sumcorLoopA1, Quick4.4$sumcorLoopA1,
                              Quick4.5$sumcorLoopA1, Quick4.50$sumcorLoopA1, Quick4.100$sumcorLoopA1, Quick4.150$sumcorLoopA1, Quick4.201$sumcorLoopA1,
                              Quick4.250$sumcorLoopA1, Quick4.300$sumcorLoopA1, Quick4.350$sumcorLoopA1, Quick4.401$sumcorLoopA1, Quick4.450$sumcorLoopA1,
                              Quick4.500$sumcorLoopA1, Quick4.550$sumcorLoopA1, Quick4.650$sumcorLoopA1, Quick4.700$sumcorLoopA1, Quick4.750$sumcorLoopA1,
                              Quick4.850$sumcorLoopA1)									

test1sumcor.LT = rbind(Quick4.1$sumcorLoopT1, Quick4.2$sumcorLoopT1, Quick4.3$sumcorLoopT1, Quick4.4$sumcorLoopT1,
                               Quick4.5$sumcorLoopT1, Quick4.50$sumcorLoopT1, Quick4.100$sumcorLoopT1, Quick4.150$sumcorLoopT1, Quick4.201$sumcorLoopT1,
                                Quick4.250$sumcorLoopT1, Quick4.300$sumcorLoopT1, Quick4.350$sumcorLoopT1, Quick4.401$sumcorLoopT1, Quick4.450$sumcorLoopT1,
                                Quick4.500$sumcorLoopT1, Quick4.550$sumcorLoopT1, Quick4.650$sumcorLoopT1, Quick4.700$sumcorLoopT1, Quick4.750$sumcorLoopT1,
                                Quick4.850$sumcorLoopT1)

test1sumcor.LE = rbind(Quick4.1$sumcorLoopE1, Quick4.2$sumcorLoopE1, Quick4.3$sumcorLoopE1, Quick4.4$sumcorLoopE1,
                               Quick4.5$sumcorLoopE1, Quick4.50$sumcorLoopE1, Quick4.100$sumcorLoopE1, Quick4.150$sumcorLoopE1, Quick4.201$sumcorLoopE1,
                                Quick4.250$sumcorLoopE1, Quick4.300$sumcorLoopE1, Quick4.350$sumcorLoopE1, Quick4.401$sumcorLoopE1, Quick4.450$sumcorLoopE1,
                                Quick4.500$sumcorLoopE1, Quick4.550$sumcorLoopE1, Quick4.650$sumcorLoopE1, Quick4.700$sumcorLoopE1, Quick4.750$sumcorLoopE1,
                                Quick4.850$sumcorLoopE1)								


								
# Combine accmat
test1accmat.knn = rbind(Quick4.1$accmatknn, Quick4.2$accmatknn, Quick4.3$accmatknn, Quick4.4$accmatknn,
                             Quick4.5$accmatknn, Quick4.50$accmatknn, Quick4.100$accmatknn, Quick4.150$accmatknn, Quick4.201$accmatknn,
                             Quick4.250$accmatknn, Quick4.300$accmatknn, Quick4.350$accmatknn, Quick4.401$accmatknn, Quick4.450$accmatknn,
                             Quick4.500$accmatknn, Quick4.550$accmatknn, Quick4.650$accmatknn, Quick4.700$accmatknn, Quick4.750$accmatknn,
                             Quick4.850$accmatknn)

test1accmat.kmeans = rbind(Quick4.1$accmatkmeans, Quick4.2$accmatkmeans, Quick4.3$accmatkmeans, Quick4.4$accmatkmeans,
                                Quick4.5$accmatkmeans, Quick4.50$accmatkmeans, Quick4.100$accmatkmeans, Quick4.150$accmatkmeans, Quick4.201$accmatkmeans,
                                Quick4.250$accmatkmeans, Quick4.300$accmatkmeans, Quick4.350$accmatkmeans, Quick4.401$accmatkmeans, Quick4.450$accmatkmeans,
                                Quick4.500$accmatkmeans, Quick4.550$accmatkmeans, Quick4.650$accmatkmeans, Quick4.700$accmatkmeans, Quick4.750$accmatkmeans,
                                Quick4.850$accmatkmeans)

test1accmat.rand = rbind(Quick4.1$accmatrand, Quick4.2$accmatrand, Quick4.3$accmatrand, Quick4.4$accmatrand,
                              Quick4.5$accmatrand, Quick4.50$accmatrand, Quick4.100$accmatrand, Quick4.150$accmatrand, Quick4.201$accmatrand,
                              Quick4.250$accmatrand, Quick4.300$accmatrand, Quick4.350$accmatrand, Quick4.401$accmatrand, Quick4.450$accmatrand,
                              Quick4.500$accmatrand, Quick4.550$accmatrand, Quick4.650$accmatrand, Quick4.700$accmatrand, Quick4.750$accmatrand,
                              Quick4.850$accmatrand)

test1accmat.CF = rbind(Quick4.1$accmatCF, Quick4.2$accmatCF, Quick4.3$accmatCF, Quick4.4$accmatCF,
                               Quick4.5$accmatCF, Quick4.50$accmatCF, Quick4.100$accmatCF, Quick4.150$accmatCF, Quick4.201$accmatCF,
                               Quick4.250$accmatCF, Quick4.300$accmatCF, Quick4.350$accmatCF, Quick4.401$accmatCF, Quick4.450$accmatCF,
                               Quick4.500$accmatCF, Quick4.550$accmatCF, Quick4.650$accmatCF, Quick4.700$accmatCF, Quick4.750$accmatCF,
                               Quick4.850$accmatCF)

test1accmat.indiv = rbind(Quick4.1$accmatindiv, Quick4.2$accmatindiv, Quick4.3$accmatindiv, Quick4.4$accmatindiv,
                              Quick4.5$accmatindiv, Quick4.50$accmatindiv, Quick4.100$accmatindiv, Quick4.150$accmatindiv, Quick4.201$accmatindiv,
                              Quick4.250$accmatindiv, Quick4.300$accmatindiv, Quick4.350$accmatindiv, Quick4.401$accmatindiv, Quick4.450$accmatindiv,
                              Quick4.500$accmatindiv, Quick4.550$accmatindiv, Quick4.650$accmatindiv, Quick4.700$accmatindiv, Quick4.750$accmatindiv,
                              Quick4.850$accmatindiv)

test1accmat.LA = rbind(Quick4.1$accmatLoopA, Quick4.2$accmatLoopA, Quick4.3$accmatLoopA, Quick4.4$accmatLoopA,
                              Quick4.5$accmatLoopA, Quick4.50$accmatLoopA, Quick4.100$accmatLoopA, Quick4.150$accmatLoopA, Quick4.201$accmatLoopA,
                              Quick4.250$accmatLoopA, Quick4.300$accmatLoopA, Quick4.350$accmatLoopA, Quick4.401$accmatLoopA, Quick4.450$accmatLoopA,
                              Quick4.500$accmatLoopA, Quick4.550$accmatLoopA, Quick4.650$accmatLoopA, Quick4.700$accmatLoopA, Quick4.750$accmatLoopA,
                              Quick4.850$accmatLoopA)	
							  
test1accmat.LT = rbind(Quick4.1$accmatLoopT, Quick4.2$accmatLoopT, Quick4.3$accmatLoopT, Quick4.4$accmatLoopT,
                               Quick4.5$accmatLoopT, Quick4.50$accmatLoopT, Quick4.100$accmatLoopT, Quick4.150$accmatLoopT, Quick4.201$accmatLoopT,
                               Quick4.250$accmatLoopT, Quick4.300$accmatLoopT, Quick4.350$accmatLoopT, Quick4.401$accmatLoopT, Quick4.450$accmatLoopT,
                               Quick4.500$accmatLoopT, Quick4.550$accmatLoopT, Quick4.650$accmatLoopT, Quick4.700$accmatLoopT, Quick4.750$accmatLoopT,
                               Quick4.850$accmatLoopT)

test1accmat.LE = rbind(Quick4.1$accmatLoopE, Quick4.2$accmatLoopE, Quick4.3$accmatLoopE, Quick4.4$accmatLoopE,
                               Quick4.5$accmatLoopE, Quick4.50$accmatLoopE, Quick4.100$accmatLoopE, Quick4.150$accmatLoopE, Quick4.201$accmatLoopE,
                               Quick4.250$accmatLoopE, Quick4.300$accmatLoopE, Quick4.350$accmatLoopE, Quick4.401$accmatLoopE, Quick4.450$accmatLoopE,
                               Quick4.500$accmatLoopE, Quick4.550$accmatLoopE, Quick4.650$accmatLoopE, Quick4.700$accmatLoopE, Quick4.750$accmatLoopE,
                               Quick4.850$accmatLoopE)
	

yrangemeanRSST1 = range(c(min(test1meanRSS.knn, test1meanRSS.kmeans, test1meanRSS.rand,
                      test1meanRSS.CF, test1meanRSS.indiv,
                      test1meanRSS.LA,
                      test1meanRSS.LT, test1meanRSS.LE, na.rm=T),
                  max(test1meanRSS.knn, test1meanRSS.kmeans, test1meanRSS.rand, 
                      test1meanRSS.CF, test1meanRSS.indiv,
                      test1meanRSS.LA,
                      test1meanRSS.LT, test1meanRSS.LE, na.rm=T))) 

meanRSSmatT1 = cbind(test1meanRSS.knn[,1], test1meanRSS.kmeans[,1], test1meanRSS.rand[,1],
                   test1meanRSS.CF[,1], test1meanRSS.indiv[,1],
                   test1meanRSS.LA[,1],
                   test1meanRSS.LT[,1], test1meanRSS.LE[,1],
                   test1meanRSS.knn[,2], test1meanRSS.kmeans[,2], test1meanRSS.rand[,2], 
                   test1meanRSS.CF[,2], test1meanRSS.indiv[,2],
                   test1meanRSS.LA[,2],
                   test1meanRSS.LT[,2], test1meanRSS.LE[,2],
                   test1meanRSS.knn[,3], test1meanRSS.kmeans[,3], test1meanRSS.rand[,3], 
                   test1meanRSS.CF[,3], test1meanRSS.indiv[,3],
                   test1meanRSS.LA[,3],
                   test1meanRSS.LT[,3], test1meanRSS.LE[,3])				   


yrangeIMSET1 = range(c(min(test1IMSE.knn, test1IMSE.kmeans, test1IMSE.rand,
                      test1IMSE.CF, test1IMSE.indiv,
                      test1IMSE.LA,
                      test1IMSE.LT, test1IMSE.LE, na.rm=T),
                  max(test1IMSE.knn, test1IMSE.kmeans, test1IMSE.rand, 
                      test1IMSE.CF, test1IMSE.indiv,
                      test1IMSE.LA,
                      test1IMSE.LT, test1IMSE.LE, na.rm=T))) 

IMSEmatT1 = cbind(test1IMSE.knn[,1], test1IMSE.kmeans[,1], test1IMSE.rand[,1],
                   test1IMSE.CF[,1], test1IMSE.indiv[,1],
                   test1IMSE.LA[,1],
                   test1IMSE.LT[,1], test1IMSE.LE[,1],
                   test1IMSE.knn[,2], test1IMSE.kmeans[,2], test1IMSE.rand[,2], 
                   test1IMSE.CF[,2], test1IMSE.indiv[,2],
                   test1IMSE.LA[,2],
                   test1IMSE.LT[,2], test1IMSE.LE[,2],
                   test1IMSE.knn[,3], test1IMSE.kmeans[,3], test1IMSE.rand[,3], 
                   test1IMSE.CF[,3], test1IMSE.indiv[,3],
                   test1IMSE.LA[,3],
                   test1IMSE.LT[,3], test1IMSE.LE[,3])	



yrangesumcorT1 = range(c(min(test1sumcor.knn, test1sumcor.kmeans, test1sumcor.rand,
                      test1sumcor.CF, test1sumcor.indiv,
                      test1sumcor.LA,
                      test1sumcor.LT, test1sumcor.LE, na.rm=T),
                  max(test1sumcor.knn, test1sumcor.kmeans, test1sumcor.rand, 
                      test1sumcor.CF, test1sumcor.indiv,
                      test1sumcor.LA,
                      test1sumcor.LT, test1sumcor.LE, na.rm=T))) 

sumcormatT1 = cbind(test1sumcor.knn[,1], test1sumcor.kmeans[,1], test1sumcor.rand[,1],
                   test1sumcor.CF[,1], test1sumcor.indiv[,1],
                   test1sumcor.LA[,1],
                   test1sumcor.LT[,1], test1sumcor.LE[,1],
                   test1sumcor.knn[,2], test1sumcor.kmeans[,2], test1sumcor.rand[,2], 
                   test1sumcor.CF[,2], test1sumcor.indiv[,2],
                   test1sumcor.LA[,2],
                   test1sumcor.LT[,2], test1sumcor.LE[,2],
                   test1sumcor.knn[,3], test1sumcor.kmeans[,3], test1sumcor.rand[,3], 
                   test1sumcor.CF[,3], test1sumcor.indiv[,3],
                   test1sumcor.LA[,3],
                   test1sumcor.LT[,3], test1sumcor.LE[,3])			



yrangeaccmatT1 = range(c(min(test1accmat.knn, test1accmat.kmeans, test1accmat.rand,
                      test1accmat.CF, test1accmat.indiv,
                      test1accmat.LA,
                      test1accmat.LT, test1accmat.LE, na.rm=T),
                  max(test1accmat.knn, test1accmat.kmeans, test1accmat.rand, 
                      test1accmat.CF, test1accmat.indiv,
                      test1accmat.LA,
                      test1accmat.LT, test1accmat.LE, na.rm=T))) 

accmatmatT1 = cbind(test1accmat.knn[,1], test1accmat.kmeans[,1], test1accmat.rand[,1],
                   test1accmat.CF[,1], test1accmat.indiv[,1],
                   test1accmat.LA[,1],
                   test1accmat.LT[,1], test1accmat.LE[,1],
                   test1accmat.knn[,2], test1accmat.kmeans[,2], test1accmat.rand[,2], 
                   test1accmat.CF[,2], test1accmat.indiv[,2],
                   test1accmat.LA[,2],
                   test1accmat.LT[,2], test1accmat.LE[,2],
                   test1accmat.knn[,3], test1accmat.kmeans[,3], test1accmat.rand[,3], 
                   test1accmat.CF[,3], test1accmat.indiv[,3],
                   test1accmat.LA[,3],
                   test1accmat.LT[,3], test1accmat.LE[,3])						   
							   

save(yrangemeanRSST1, meanRSSmatT1, yrangeIMSET1, IMSEmatT1, 
	 yrangesumcorT1, sumcormatT1, yrangesumcorT1, sumcormatT1, yrangesumcorT1, sumcormatT1, yrangeaccmatT1, accmatmatT1,
     file = "ReslassoBtest1.RData")	