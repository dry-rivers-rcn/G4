#!/usr/bin/env Rscript
################################################################################
# GRADES_bias_fix.R
################################################################################
# George Allen, Oct 2019



################################################################################ 
# Define paths: 
wd = "E:/research/2019_10_07_G4/git/G4"
inDir = paste0(wd, "/in")
outDir = paste0(wd, "/out")
biasDir = paste0(inDir, "/bias/australia")
################################################################################

# list files:
biasPaths = list.files(biasDir, pattern="csv", full.names=T)



# for each gauge:
for (i in 1:length(biasPaths)){
  biasTab = read.csv(biasPaths[i], header=T)

  if (i==1){ qMod=biasTab$qmod } else { qMod = c(qMod, biasTab$qmod)}

}


################################################################################
# plot histogram:
hist(log(qMod, 10), 
     xaxt = "n", xlab="Q (cms)", 
     main="Modeled discharge when gauge reads zero flow",
     col="gray")
x = seq(-10, 2, by=2)

axis(1, at=x, labels=paste0("10^", x))
abline(v=log(0.1, 10), col=2, lwd=2)
text(log(0.11, 10), 2e4, "threshold at Q = 0.1 cms", col=2, 
     srt=-90, pos=3, font=2)


################################################################################