#!/usr/bin/env Rscript
#*******************************************************************************
# ncdf_shp_attr_transfer.R
#*******************************************************************************
# George Allen, Oct 2019

# Transfers discharge estimates from GRADES to MERIT Hydro flowline shapefiles

# install/load libraries:
require(ncdf4)
require(foreign)


# Define paths: 
wd = "E:/research/2019_10_07_G4/git/G4"
inDir = paste0(wd, "/in")
outDir = paste0(wd, "/out")


ncDirIn = paste0(inDir, "/GRADES")
mhDirIn = paste0(inDir, "/MERIT_Hydro/riv")

mhDirOut = paste0(outDir, "/MERIT_GRADES")


# list files:
ncPaths = list.files(ncDirIn, pattern="nc", full.names=T)
mHpaths = list.files(mhDirIn, pattern="dbf", full.names=T)



# for each GRADES region: 
#for (i in 1:length(ncPaths)){

i = 5 # Australia as an example


# read in netCDF file:
ncIn = nc_open(ncPaths[i])

# read in river shapefile dbf file:
dbf = foreign::read.dbf(mHpaths[i], nc)


# read in GRADES netCDF in chunks (to prevent memory overflow):
nc = ncvar_get(ncIn,"Q", 
          start=c(1, ncIn$var$Q$varsize[2]-365), # starting index of netCDF to read in 
          count=c(ncIn$var$Q$varsize[1], 365)) # ending index of netCDF to read in 



# add columns to shapefile attribute table:
dbfOut = data.frame(dbf, nc)

# rename dbf columns:
names(dbfOut) = sub("^X", "Q", names(dbfOut))

# write out shapefile attribute table:
mHpathsOut = sub(mhDirIn, mhDirOut, mHpaths[i])
foreign::write.dbf(dbfOut, mHpathsOut)

#}



