#!/usr/bin/env Rscript
# --------------------------------------------------------------------------------------------
# gauge_placement_analysis.R
# --------------------------------------------------------------------------------------------
# Primary script developer: Julian D. Olden
# Date: 2/15/2022

# Please cite: 
# Assessing placement bias of the global river gauge network
# Nature Sustainability
# Authors: Corey A. Krabbenhoft, George H. Allen, Peirong Lin, Sarah E. Godsey, Daniel C. Allen, Ryan M. Burrows, Amanda G. DelVecchia, Ken M. Fritz, Margaret Shanafield
# Amy J. Burgin, Margaret Zimmer, Thibault Datry, Walter K. Dodds, C. Nathan Jones, Meryl C. Mims, Catherin Franklin, John C. Hammond, Samuel C. Zipper, Adam S. Ward, 
# Katie H. Costigan, Hylke E. Beck, and Julian D. Olden


# Description: 
# This code compares currently gauged river segments to a global river dataset (GRADES) according to 13 geospatial attributes
# Standard bias and Wasserstein distance are used to contrast the statistical distribution of each attribute of gauged versus all river segments 
# to identify types of rivers which are over- or underrepresented by the current gauge network

# --------------------------------------------------------------------------------------------
# required R libraries
library(tidyverse)
library(ggplot2)
library(SimDesign)
library(transport)
library(gridExtra)
library(heatmaply)

# importing data
data <- read.csv('~/XXX/Global_Reach_Attributes.csv', header=TRUE, row.names=1)

# importing names associated with geospatial variables and major freshwater habitat type
VARnames <- read.csv('~/XXX/VARnames.csv', header=FALSE)
FHTnames <- read.csv('~/XXX/FHTnames.csv', header=FALSE)

# --------------------------------------------------------------------------------------------
# Calculating standard bias and Wassenstein distance for gauge reaches vs. all reaches

# sub-setting reaches to those containing gauges and removing reaches (n=22) with missing geospatial data
gagdata<-data %>% 
  filter(!is.na(stationid) & is.na(dor_pc_pva)) %>%
  select(COMID,uparea,order_,dor_pc_pva,slope,tmp_dc_cyr,pre_mm_cyr,crp_pc_use,urb_pc_use,pac_pc_cse,ppd_pk_uav,hft_ix_u09,gdp_ud_usu,fPermMQ) 
# selecting all reaches and removing reaches (n=13,143) with missing missing geospatial data
alldata<-data %>% 
  filter(!is.na(dor_pc_pva)) %>%
  select(COMID,uparea,order_,dor_pc_pva,slope,tmp_dc_cyr,pre_mm_cyr,crp_pc_use,urb_pc_use,pac_pc_cse,ppd_pk_uav,hft_ix_u09,gdp_ud_usu,fPermMQ) 

# calculating variable means for all data
varmeans<-alldata[,-1] %>%
  summarise_all(mean,na.rm=TRUE)

# calculating standardized bias and Wasserstein distance for each variable
all_bias<-matrix(, nrow = dim(gagdata[,-1])[2], ncol = 3)
rownames(all_bias)<-t(VARnames)
all_bias<-cbind(VARnames,all_bias)
all_bias[,2]<-bias(gagdata[,-1],varmeans,type='standardized')
colnames(all_bias)<-c("Variable", "bias", "wasser","Direction")

# note that the first column of gagdata and alldata is omitted b/c it is COMID
for (p in 1:dim(gagdata[,-1])[2]) {
  gagdata_std<-(gagdata[,p+1]-mean(alldata[,p+1]))/sd(alldata[,p+1])
  all_bias[p,3]<-wasserstein1d(gagdata_std,scale(alldata[,p+1]),p=1)
  # creating label if variable has a positive or negative standardized bias
  if (all_bias[p,2]>0) all_bias[p,4]<-"positive"
  else all_bias[p,4]<-"negative"
}

# --------------------------------------------------------------------------------------------
# Producing Figure 2

# Figure 2a
all_bias <- all_bias[order(all_bias$wasser), ] 
all_bias$Variable <- factor(all_bias$Variable, levels = all_bias$Variable)
all_bias$Direction <- factor(all_bias$Direction, levels = c("positive","negative"))
ggplot(all_bias, aes(x=Variable, y=`wasser`, color=Direction, size=wasser)) + 
  geom_point(alpha=1)  + scale_size(range = c(1, 6)) +
  scale_color_manual(values=c("red", "blue"),name="Bias Direction", labels = c("Positive","Negative")) + 
  labs(title="a",y="Wasserstein Distance (Bias)") + 
  coord_flip() +
  guides(size="none") +
  scale_y_continuous(limits=c(0,0.8,0)) +
  scale_fill_manual() +
  theme (
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(colour="black",size=12), 
    axis.text.y = element_text(colour="black",size=12),
    legend.text = element_text(colour="black",size=11),
    legend.title=element_text(colour="black",size=13), 
    plot.title=element_text(colour="black",size=14,hjust=0), 
    axis.title.y=element_blank(), 
    axis.title.x=element_text(colour="black",size=12), 
    panel.background = element_rect(fill = "lightgray", colour="black"),
    #panel.grid.major = element_line(colour = "black"),
    legend.position = c(0.825, 0.1)
  )

# Figure 2b-f

# Plotting variable distributions to highlight some examples
temp<-c(rep("gag",dim(gagdata)[1]),rep("all",dim(alldata)[1]))
temp<-as.matrix(temp)
comdata<-cbind(temp,as.data.frame(rbind(gagdata,alldata)))
colnames(comdata)[1]<-"type"

a<-ggplot(comdata, aes(x = fPermMQ,colour=type)) + stat_ecdf(size=1.5) +
  scale_color_manual(values=c("black","#E69F00"),name="River Segment", labels = c("All","Gauged")) + 
  labs(title="b", x="Flow Permanence", y="Cumulative probability")  +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.33, 0.8),
    panel.background = element_rect(fill = "lightgray"),
    axis.text.x = element_text(colour="black",size=11), 
    axis.text.y = element_text(colour="black",size=11),
    legend.text = element_text(colour="black",size=10),
    legend.title=element_text(colour="black",size=10), 
    #plot.title=element_text(colour="black",size=14,hjust=0), 
    axis.title.y=element_text(colour="black",size=11), 
    axis.title.x=element_text(colour="black",size=11), 
  )

b<-ggplot(comdata, aes(x = log10(dor_pc_pva+1),colour=type)) + stat_ecdf(size=1.5) +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray")) +
  labs(title="d", x="Flow Regulation (log+1) (%)", y="Cumulative probability")  +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightgray"),
    axis.text.x = element_text(colour="black",size=11), 
    axis.text.y = element_text(colour="black",size=11),
    axis.title.y=element_text(colour="black",size=11), 
    axis.title.x=element_text(colour="black",size=11), 
  )

c<-ggplot(comdata, aes(x = hft_ix_u09,colour=type)) + stat_ecdf(size=1.5) +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray")) +
  labs(title="c", x="Human Footprint", y="Cumulative probability")  +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightgray"),
    axis.text.x = element_text(colour="black",size=11), 
    axis.text.y = element_text(colour="black",size=11),
    axis.title.y=element_blank(), 
    axis.title.x=element_text(colour="black",size=11), 
  )

d<-ggplot(comdata, aes(x = tmp_dc_cyr/10,colour=type)) + stat_ecdf(size=1.5) +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray")) +
  labs(title="e", x="Air Temperature (°C)", y="Cumulative probability")   +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightgray"),
    axis.text.x = element_text(colour="black",size=11), 
    axis.text.y = element_text(colour="black",size=11),
    axis.title.y=element_blank(), 
    axis.title.x=element_text(colour="black",size=11), 
  )

# plotting in a 2-by-2 panel
grid.arrange(a,c,b,d)

# --------------------------------------------------------------------------------------------
# Producing Figure S4

# transforming data to aid intepretation
comdata$type <- factor(comdata$type, levels = c("all","gag"))
comdata$uparea_log<-log(comdata$uparea+1)
comdata$slope_log<-log10(comdata$slope+1)
comdata$pre_mm_cyr_log<-log10(comdata$pre_mm_cyr+1)
comdata$crp_pc_use_log<-log10(comdata$crp_pc_use+1)
comdata$urb_pc_use_log<-log10(comdata$urb_pc_use+1)
comdata$pac_pc_cse_log<-log10(comdata$pac_pc_cse+1)
comdata$ppd_pk_uav_log<-log10(comdata$ppd_pk_uav+0.01)
comdata$gdp_ud_usu_log<-log10(comdata$gdp_ud_usu+1)

a<-ggplot(comdata, aes(x = uparea_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black", "#E69F00"),name="River Segment", labels = c("All", "Gauged"))  +
  labs(title="a", x=expression(Catchment~Area~(log+1)~(m^2)), y="Cumulative probability")  +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(
    legend.key.size = unit(0.3, 'cm'),
    legend.title = element_text(size=10),
    panel.grid.minor = element_blank(),
    legend.position = c(0.7, 0.28),
    panel.background = element_rect(fill = "lightgray")
  )
b<-ggplot(comdata, aes(x = order_,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  scale_color_manual(values=c("black","#E69F00")) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="b", x="Stream Order (Strahler)")  
c<-ggplot(comdata, aes(x = slope_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="c", x=expression(Channel~Gradient~(log)~"(%)"))  
d<-ggplot(comdata, aes(x = pre_mm_cyr_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray")) +
  labs(title="d", x=expression(Precipitation~(log+1)~"(mm/year)"), y="Cumulative probability")
e<-ggplot(comdata, aes(x = crp_pc_use_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="e", x="Crop Landuse (log+1) (%)")
f<-ggplot(comdata, aes(x = urb_pc_use_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="f", x="Urban Landuse (log+1) (%)")
g<-ggplot(comdata, aes(x = pac_pc_cse_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray")) +
  labs(title="g", x="Protected Area (log+1) (%)", y="Cumulative probability")
h<-ggplot(comdata, aes(x = ppd_pk_uav_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="h", x=expression(Pop~Density~(log+0.01)~(no.~km^-2)))  
i<-ggplot(comdata, aes(x = gdp_ud_usu_log,colour=type)) + stat_ecdf(size=1.5)  +
  theme_bw() + 
  scale_color_manual(values=c("black","#E69F00")) +
  scale_y_continuous(limits=c(0,1.0),labels = scales::number_format(accuracy = 0.01)) +
  theme(panel.grid.minor = element_blank(),legend.position = "none",panel.background = element_rect(fill = "lightgray"),axis.title.y=element_blank()) +
  labs(title="i", x="Gross Domest Prod (log+1) (USD)")  

# plotting in a 4-by-4 panel
grid.arrange(a,b,c,d,e,f,g,h,i)


# --------------------------------------------------------------------------------------------
# Calculating standard bias and Wassenstein distnace for gauge reaches vs. all reaches
# according to Major Freshwater Habitat Types (Freshwater Ecoregions of the World: Abell et al. 2008)

# creating matrix for results
fht_results<-matrix(, nrow = dim(gagdata[,-1])[2], ncol = 11)
rownames(fht_results)<-t(VARnames)
colnames(fht_results)<-t(FHTnames)

for (j in 1:11) {
  
  # subsetting segments containing gauges and removing 3 reaches with NAs
  gagdata_fht<-data %>%  
    filter(!is.na(stationid) & !is.na(dor_pc_pva) & fmh_cl_cmj==j) %>%
    select(uparea,order_,dor_pc_pva,slope,tmp_dc_cyr,pre_mm_cyr,crp_pc_use,urb_pc_use,pac_pc_cse,ppd_pk_uav,hft_ix_u09,gdp_ud_usu,fPermMQ) 
  
  # selecting the same variables for all global segments
  alldata_fht<-data %>% 
    filter(!is.na(dor_pc_pva) & fmh_cl_cmj==j) %>%
    select(uparea,order_,dor_pc_pva,slope,tmp_dc_cyr,pre_mm_cyr,crp_pc_use,urb_pc_use,pac_pc_cse,ppd_pk_uav,hft_ix_u09,gdp_ud_usu,fPermMQ) 
  
  # calculating standardized bias and Wasserstein distance and test statistics for each variable
  fht_bias<-matrix(, nrow = dim(gagdata_fht)[2], ncol = 3)
  rownames(fht_bias)<-t(VARnames)
  fht_bias<-cbind(VARnames,fht_bias)
  fht_bias[,2]<-(bias(gagdata_fht,varmeans,type='standardized'))
  colnames(fht_bias)<-c("Variable", "bias", "wasser","Direction")
  
  # note that the first column of gagdata and alldata is omitted b/c it is COMID
  for (p in 1:dim(gagdata_fht)[2]) {
    gagdata_fht_std<-(gagdata_fht[,p]-mean(alldata_fht[,p]))/sd(alldata_fht[,p])
    fht_bias[p,3]<-wasserstein1d(gagdata_fht_std,scale(alldata_fht[,p]),p=1)
    if (fht_bias[p,2]<0) fht_bias[p,3]<-fht_bias[p,3]*-1
  }
  
  # output Wasserstein distances
  fht_results[,j]<-fht_bias$wasser  
  
}  

# Producing Figure 3
theme_set(theme_bw())
fht_results<-t((fht_results))

a<-heatmaply(fht_results, 
             dendrogram = "row",
             xlab = "", ylab = "", 
             main = "",
             scale = "none",
             legend = TRUE,
             margins = c(60,100,40,20),
             grid_color = "white",
             grid_width = 0.00001,
             titleX = TRUE,
             hide_colorbar = FALSE,
             colorbar(titlefont=list(size=5)),
             branches_lwd = 0.1,
             label_names = c("FHT", "Variable:", "Value"),
             fontsize_row = 12, fontsize_col = 12,
             labCol = colnames(fht_results),
             labRow = rownames(fht_results),
             # layout(legend.font="Arial"),
             scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
               low = "dark blue", 
               high = "dark red", 
               midpoint = 0, 
               limits = c(-1.5, 1.5),
               name="Wasserstein \nDistance (Bias)"),
             heatmap_layers = theme(axis.line=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black")),
) 

a %>% layout(xaxis=list(tickfont = list(family = "Helvetica")),yaxis=list(tickfont = list(family = "Helvetica")))

# --------------------------------------------------------------------------------------------
# calculate the overall change in global bias in gauge placement (averaged across all variables) 
# if a new gauge were installed

# defining the number of segments
no.seg<-dim(alldata)[1]
permutation_bias<-matrix(,ncol=14,nrow=no.seg)
for (j in 1:no.seg) {
  perdata<-rbind(gagdata, alldata[j,])
  permutation_bias[j,]<-c(alldata[j,1],bias(perdata[,2:(dim(gagdata)[2])],varmeans,type='standardized'))
  print(j)
}

# calculating % change in bias for each variable and overall mean across variables
current_bias<-as.matrix(bias(gagdata[,2:(dim(gagdata)[2])],varmeans,type='standardized'))
temp<-sweep(permutation_bias[,2:14], MARGIN=2,FUN="-", current_bias)
finalbias<-sweep(temp,MARGIN=2,FUN="/", current_bias)*100
finalbias<-cbind(permutation_bias[,1],finalbias,rowMeans(finalbias[,1:13]))
finalbias<-as.data.frame(finalbias)
colnames(finalbias)<-c("COMID", t(VARnames), "MeanBiasChange")