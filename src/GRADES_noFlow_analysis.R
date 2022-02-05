#!/usr/bin/env Rscript
################################################################################
# noFlow_analysis.R
################################################################################
# Primary script developer: George H. Allen
# Feb 5, 2022


# Please cite: 
# Assessing placement bias of the global river gauge network
# Nature Sustainability
# Authors: Corey A. Krabbenhoft, George H. Allen, Peirong Lin, Sarah E. Godsey, 
# Daniel C. Allen, Ryan M. Burrows, Amanda G. DelVecchia, Ken M. Fritz, Margaret
# Shanafield, Amy J. Burgin, Margaret Zimmer, Thibault Datry, Walter K. Dodds, 
# C. Nathan Jones, Meryl C. Mims, Catherin Franklin, John C. Hammond, Samuel C. 
# Zipper, Adam S. Ward, Katie H. Costigan, Hylke E. Beck, and Julian D. Olden


# Description: 
# This code compares zero-flow measurements from global gauge records to 
# simulated streamflows from the GRADES database (Lin et al., 2019, 
# https://doi.org/10.1029/2019WR025287). Using this information, it then 
# predicts when rivers are likely to cease to flow. Finally it calculates 
# no-flow occurrence by aggregating the full GRADES daily streamflow record.  


################################################################################
# required packages 
require(ggplot2)
require(scales)
require(ncdf4)
require(foreign)
require(sf)

################################################################################ 
# Define paths:
wd = "~/XXX/G4"
inDir = paste0(wd, "/in")
outDir = paste0(wd, "/out")
biasDir = paste0(inDir, "/bias/all_bias")
joinPath = paste0(inDir, "/bias/GRADES_join/join_all.csv")

################################################################################
# Read in CSV files created by Peirong Lin at Princeton University. Each CSV 
# is a gauge with the date of a zero-flow observation and the corresponding 
# GRADES streamflow estimate. 

# list files:
biasPaths = list.files(biasDir, pattern="csv", full.names=T) 

# concatenate all zero-flow gauge data into one data frame:
biasTab = read.csv(biasPaths[1], header=T)
bTab = as.data.frame(array(NA, c(5e6, ncol(biasTab))))
names(bTab) = names(biasTab)
j = 1
for (i in 2:length(biasPaths)){
  biasTab = read.csv(biasPaths[i], header=T)
  k = nrow(biasTab) + j
  if (j==k){next}
  bTab[j:(k-1), ] = biasTab
  j = k
}


################################################################################
# compute statistics of GRADES simulated streamflow during no-flow observation:
print(paste("N no-flow observations in GRADES gauges:", nrow(bTab)))
LQ = quantile(bTab$qmod, 0.25)
MQ = median(bTab$qmod)
UQ = quantile(bTab$qmod, 0.75)


################################################################################
# Read in Australian & US EPA no-flow observational data sets: 

# read in Australian & US EPA no-flow measurements. This table was created via 
# GIS analysis by Catherin Franklin (doing several spatial joins in ArcGIS): 
vTab = read.csv(paste0(inDir, "/bias/australia_EPA/australia_epa.csv"), header=T)

# filter out sites with drainage areas less than 25 km2 (min. area of GRADES)
vTab = vTab[-(which(vTab$drainage_k < 25)), ] 

# print out relevant descriptive statistics: 
print(paste("N no-flow observations in validation gauges:", nrow(vTab)))
print(paste("N no-flow observations in EPA:", length(which(vTab$source == "EPA_PROBsites"))))
print(paste("N no-flow observations in Australia:", length(which(vTab$source == "Australian "))))
print(paste("N no-flow unique sites in validation gauges:", length(unique(vTab$site_id))))
print(paste("N no-flow unique sites in EPA:", length(unique(vTab$site_id[vTab$source == "EPA_PROBsites"]))))
print(paste("N no-flow unique sites in Australia:", length(unique(vTab$site_id[vTab$source == "Australian "]))))


################################################################################
# Plots GRADES, Kennard et al. & US EPA distributons: 

# tidy data for ggplot: 
vTab$source = as.factor(vTab$source)
levels(vTab$source) = c("Kennard et al. (2010)", "US EPA")

# join Beck et al. table with Kennard et al. and US EPA tables:
aTab = rbind(data.frame(siteID=bTab$gauge, qmod=bTab$qmod, source="Beck et al. (2020)"), 
             data.frame(siteID=vTab$site_id, qmod=vTab$Model_Q, source=vTab$source))

# Shapiro & # wilcoxon-kruscal wallis tests:
shapiro.test(sample(aTab$qmod[aTab$source=="Beck et al. (2020)"], size = 5000))
shapiro.test(sample(aTab$qmod[aTab$source=="Kennard et al. (2010)"], size = 5000))
shapiro.test(aTab$qmod[aTab$source=="US EPA"])

wilcox.test(aTab$qmod[aTab$source=="Beck et al. (2020)"], aTab$qmod[aTab$source=="US EPA"])
wilcox.test(aTab$qmod[aTab$source=="Beck et al. (2020)"], aTab$qmod[aTab$source=="Kennard et al. (2010)"])

# Descriptive statistics for violin plot: 
X = aggregate(qmod~source, aTab, length)
Y = aggregate(qmod~source, aTab, median)
nObs = cbind(X[-ncol(X)], X[[ncol(X)]], Y[[ncol(Y)]])
names(nObs) = c("source", "N", "median")
nObs$N = paste0("N=", nObs$N)
print(nObs)

# for visualization purposes, remove outliers in plot (0.21% of observations):  
ggTab = aTab[aTab$qmod>1e-5 & aTab$qmod<1e5, ]

# violin plot of all distributions:
par(mar=c(5,5,5,5))
ggplot(ggTab) +
  geom_violin(aes(x=source, y=qmod, fill=source), alpha=0.9) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x)) +
  xlab("") +
  ylab("GRADES Q During No Flow Observation (cms)") +
  scale_fill_brewer(palette = "Spectral") + 
  geom_text(data=nObs, aes(x=source, y=median, label=N), size=3) + 
  guides(size=FALSE) +
  theme (
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(colour="black",size=11),
    axis.text.y = element_text(colour="black",size=11),
    legend.text = element_text(colour="black",size=10),
    legend.title=element_text(colour="black",size=12),
    plot.title=element_text(colour="black",size=14,hjust=0),
    axis.title.x=element_text(colour="black",size=11),
    panel.background = element_rect(fill = "lightgray", colour="black"),
    legend.position = "none" 
  )


################################################################################
# Apply thresholds to calculate flow occurrence for each GRADES segment:
# calculate the following for each year in GRADES:
# % flow occurrence 
# standard deviation of flow occurrence
# coefficient of variability
# temporal trends 

# specify paths:
ncDirIn = paste0(inDir, "/GRADES")
mhDirIn = paste0(inDir, "/MERIT_Hydro/riv")
mhSortDirIn = paste0(inDir, "/MERIT_Hydro_sorted")
mhDirOut = paste0(outDir, "/GRADES_MERIT_sorted")

# list files:
ncPaths = list.files(ncDirIn, pattern="nc", full.names=T)
mhPaths = list.files(mhDirIn, pattern="dbf", full.names=T)
mhSortpaths = list.files(mhSortDirIn, pattern="dbf", full.names=T)
mhOutpaths = sub(mhSortDirIn, mhDirOut, mhSortpaths)[-9] # no greenland


# for each GRADES region: 
for (i in 1:length(ncPaths)){
  print(paste("i=", i))
  ncIn = nc_open(ncPaths[i]) # read in netCDF file:
  start = 1
  end = ncIn$var$Q$varsize[2]
  j = 1
  interval  = 50 
  year = interval
  
  # read in river shapefile dbf file:
  dbf = foreign::read.dbf(mhPaths[i])
  
  # create three tables, one for each flow threshold:
  dbf_LQ = data.frame(dbf)
  dbf_MQ = data.frame(dbf)
  dbf_UQ = data.frame(dbf)
  
  start_time = Sys.time()
  # read in GRADES netCDF in yearly chunks (to prevent memory overflow):
  while (start < end){
    print(paste("chunk", j))
    print("reading in nc file...")
    # for last year: 
    if ((start+interval) > ncIn$var$Q$varsize[2]){
      interval = ncIn$var$Q$varsize[2] - start
    }
    nc = ncvar_get(ncIn, "Q", 
                   start=c(1, start), # starting index of netCDF to read in 
                   count=c(ncIn$var$Q$varsize[1], interval)) # ending index 
    colName = paste0("yr_", substr(paste0("00", j), nchar(j), nchar(j)+2))
    
    # a column giving flow persistence for each quartile:
    print("Calculating flow perminence...")
    dbf_LQ[, colName] = apply(nc, 1, function(x){sum(x > LQ)}) / interval
    dbf_MQ[, colName] = apply(nc, 1, function(x){sum(x > MQ)}) / interval
    dbf_UQ[, colName] = apply(nc, 1, function(x){sum(x > UQ)}) / interval
    
    start = start+interval
    j = j+1
  }
  
  Sys.time() - start_time
  
  # calculate mean, sd, and cv of flow occurence:
  chunkCols = grep("^yr", names(dbf_LQ))
  dbf_LQ$fPermLQ = rowMeans(dbf_LQ[ , chunkCols])
  dbf_MQ$fPermMQ = rowMeans(dbf_MQ[ , chunkCols])
  dbf_UQ$fPermUQ = rowMeans(dbf_UQ[ , chunkCols])
  
  # standard devations (of the chunks):
  dbf_LQ$fPermLQ_sd = apply(dbf_LQ[ , chunkCols], 1, sd)
  dbf_MQ$fPermMQ_sd = apply(dbf_MQ[ , chunkCols], 1, sd)
  dbf_UQ$fPermUQ_sd = apply(dbf_UQ[ , chunkCols], 1, sd)
  
  # coefficient of variation:
  dbf_LQ$fPermLQ_cov = dbf_LQ$fPermLQ_sd/dbf_LQ$fPermLQ
  dbf_MQ$fPermMQ_cov = dbf_MQ$fPermMQ_sd/dbf_MQ$fPermMQ
  dbf_UQ$fPermUQ_cov = dbf_UQ$fPermUQ_sd/dbf_UQ$fPermUQ

  # match COMIDs to sorted shapefile tables: 
  dbf_sorted = foreign::read.dbf(mhSortpaths[i])
  sortInd = match(dbf_sorted$COMID, dbf$COMID)
  
  outDbf_LQ = dbf_LQ[sortInd, grep("^fPerm", names(dbf_LQ))]
  outDbf_MQ = dbf_MQ[sortInd, grep("^fPerm", names(dbf_MQ))]
  outDbf_UQ = dbf_UQ[sortInd, grep("^fPerm", names(dbf_UQ))]
  
  # Calculate IQR:
  IQR = outDbf_LQ$fPermLQ - outDbf_UQ$fPermUQ
  
  # write out shapefile attribute table:
  dbfOut = cbind(dbf_sorted, outDbf_LQ, outDbf_MQ, outDbf_UQ, IQR)
  foreign::write.dbf(dbfOut, mhOutpaths[i])
}


################################################################################
# Generate essential DBFs:

# merge all GRADES data into one data frame:
keep = c("COMID", "strmOrder", "fPermLQ", "fPermMQ", "fPermUQ")
for (i in 1:length(mhOutpaths)){
  print(i)
  df = foreign::read.dbf(mhOutpaths[i])
  dfOut = df[, match(keep, names(df))]
  names(dfOut)[3:5] = c("flOccur25", "flOccur50", "flOccur75")
  foreign::write.dbf(dfOut, mhOutpaths[i])
}

# zip up shapefiles for distribution: 
allPaths = list.files(mhDirOut, full.names=T)
namesNoExt = sub(".dbf", "", mhOutpaths)
for (i in 1:length(mhOutpaths)){
  print(i)
  zipPaths = grep(namesNoExt[i], allPaths, value=T)
  zip(zipfile = namesNoExt[i], files = zipPaths, flags = " a -tzip", 
      zip = "C:/Program Files/7-Zip/7Z")
}