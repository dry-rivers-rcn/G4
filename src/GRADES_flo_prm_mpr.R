#!/usr/bin/env Rscript
#*******************************************************************************
# GRADES_flo_prm_mpr.R
#*******************************************************************************
# George H. Allen, 

# install.packages("sf")
# install.packages("ggplot2")

require(sf)
require(ggplot2)


# Define paths: 
wd = "/Users/allenstandard/research/2019_09_20_DryRivRCN_workshop/git/GRADES"
setwd(wd)
inDir = paste0(wd, "/in")
outDir = paste0(wd, "/out")

# unzip file (if needed):
shpPath = list.files(inDir, ".shp", recursive=T, full.names=T)
if (length(file.exists(shpPath)) == 0){
  zipPath = list.files(inDir, ".zip", recursive=T, full.names=T)
  unzip(zipPath, exdir=dirname(zipPath))#exdir=outDir)
  shpPath = list.files(inDir, ".shp", recursive=T, full.names=T)
}

# read in sorted MERIT Hydro + GRADES 07 shapefile for 2013:
shp = st_read(shpPath)

df = as.data.frame(shp)

# get columns with discharge data:
Qcols = grep("^Q", names(df))

zeroFlowLength = apply(df[, Qcols], 1, function(x){length(which(x > 0.5))})
flowPerm = zeroFlowLength/365

pdfOutPath = paste0(outDir, "/fig/NM_shapefile.pdf")
pdf(pdfOutPath, 10, 10)
# plot by stream order:
ggplot() +
  geom_sf(data=shp, lwd=flowPerm*3) +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position="none",
        legend.title = element_blank())

dev.off()
cmd = paste('open', pdfOutPath)
system(cmd)


# write out:
#st_write(shp, shpPathOut)









# set negative flows to zero and find global max:
Qmax = vector()
for (i in 1:length(Qcols)){
  # print(i)
  Q = df[, Qcols[i]]
  negQind = which(Q < 0 | is.na(Q))
  Q[negQind] = 0
  shp[Qcols[i]] = Q 
  
  Qmax[i] = max(Q)
}

globQmax = max(Qmax)

# set up color and line gradients:
colfun = colorRampPalette(c(rgb(0.9,0.9,0.7), rgb(0.6,0.9,1), rgb(0.4,0.6,1),
                            rgb(0.3,0.5,1), rgb(0,0,1), rgb(0,0.1,0.7)))

colRamp = colfun(50)
colSeq = ((seq(0, 1, length.out=length(colRamp)))^7)*globQmax
plot(1:50, col=colRamp, pch=16, cex=15)

lwdRamp = seq(0.15, 12, length.out=50)
lwdSeq = ((seq(0, 1.5, length.out=length(lwdRamp))))*(globQmax)



# for each time step:
for (i in Qcols){
  start_time <- Sys.time()
  
  print(i)
  
  Q =  df[, i]
  #print(length(Q))
  
  # generate colors and line lwds for each record:
  colInt = findInterval(Q, colSeq)+1
  if (length(which(colInt > 50)) > 0){ colInt[colInt > 50] = 50}
  cols = colRamp[colInt]
  
  cols[Q == 0] = rgb(0.8,0.8,0.8)
  
  
  
  if (length(which(is.na(cols))) > 0){ message("Na in cols") }
  lwdInt = findInterval(Q, lwdSeq)+1
  if (length(which(lwdInt > 50)) > 0){ lwdInt[lwdInt > 50] = 50}
  lwds = lwdRamp[lwdInt]
  if (length(which(is.na(lwds))) > 0){ message("Na in lwds") }
  
  jpgOut = paste0(outDir, "/fig/", i, ".jpg")
  jpeg(jpgOut, 12, 10, "in", res=200)
  
  layout(matrix(c(1,2), ncol=2, byrow=T), widths=c(6,1))
  plot(0,0, type='n', main="Q (cms)", bty='n', axes=F, xlab='', ylab='')
  
  # plot map:
  print(ggplot() + 
          geom_sf(data=shp, lwd=lwds, color=cols) +
          coord_sf() +
          theme(plot.background = element_blank(),
                panel.background = element_blank(),
                legend.position="none",
                legend.title = element_blank()))
  
  # add custom color bar:
  par(mar=c(4,4,8,4))
  plot(c(0,10), c(0,globQmax), type='n', 
       main="", bty='n', 
       xaxt='n', yaxt='n', 
       xlab='', ylab='')
  options(scipen=2)
  labs = ((colSeq[round(seq(1, length(colSeq), length.out=10))]))
  labels = formatC(labs, 3)
  axis(2, at=round(seq(0, globQmax, length.out=10)), labels=labels, las=1)
  scale = (length(colRamp)-1)/(globQmax)
  for (j in 1:(length(colSeq)-1)) {
    y = (j-1)/scale
    rect(0,y,10,y+1/scale, col=colRamp[j], border=NA)
  }
  
  mtext("Q (cms)", side=3, line=1, at=-2, font=2)
  d = i-19
  mtext(as.Date(d, origin="2013-01-01"), side=1, line=1, font=3)
  
  dev.off()
  # system(paste("open", jpgOut))
  
  
  print(Sys.time() - start_time)
  
}



