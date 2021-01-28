############################################################################################################
# Create a timeseries of mean OISST in the study area, and a map of max SST
############################################################################################################

library(ncdf4)
library(lubridate)
library(lunar)
library(reshape2)
library(ggplot2)
library(scater)

# Define year and month. 2015-9 for PBF, 2008-18 for lancetfish
moYrs <- expand.grid("mos" = 1:12, "yrs" = 2015:2019) # set years and months of interest here

# Set geographical limits here
myBounds <- c(220, 250, 22, 50)

suppressWarnings(rm(oisst))
for (j in 1:nrow(moYrs)) {
  myYr <- moYrs[j,2]
  myMo <- moYrs[j,1]
  # Retrieve monthly SST from NOAA OISST 
  source("./getoisstMonthly.R")
  sstagg <- getoisstMonthly(myYr, myMo, myBounds, "E:/oisst")
  # Now export
  if (exists("oisst")) {
    oisst <- rbind(oisst, sstagg)
  } else {
    oisst <- sstagg
  }
  print(paste0(myMo, " ", myYr, " is complete"))
}

#####################################################################################################
######################################## Plot monthly SST ###########################################
#####################################################################################################
# Palettes and themes
mycols8 <- colors()[c(473,473,562,71,610,655,653,621,34,34)]
mypalette <- colorRampPalette(mycols8)(255)
myTheme = theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 14), 
                axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14), plot.title = element_text(size = 16), 
                legend.title = element_text(size=14), legend.text = element_text(size=12), strip.text = element_text(size = 14))
pac.coast <- borders("world2", colour="gray50", fill="gray50", xlim = c(100, 260),ylim = c(0, 60))

# Aggregate SST for SCB region 
oisstAgg <- aggregate(sst ~ month + year, subset(oisst, oisst$lonrd >= 239 & oisst$lonrd <= 243 &
                                                   oisst$latrd >=32 & oisst$latrd <=35), FUN = mean, na.rm = TRUE)
# Add in sampling effort
pbf <- read.csv("./pbfSamples.csv", head = TRUE, sep = ",")
oisstAgg2 <- dplyr::inner_join(pbf, oisstAgg, by = "month")
# Plot. sst*3-40 works well to have bars behind SST curves
p3 <- ggplot() + 
  geom_bar(mapping = aes(x = pbf$month, y = pbf$pbf), stat = "identity", fill = "gray") +
  geom_line(mapping = aes(x = oisstAgg2$month, y = oisstAgg2$sst*3-40, group = oisstAgg2$year, 
                          color = factor(oisstAgg2$year)), size = 1.25) + # 2nd axis
  geom_point(mapping = aes(x = oisstAgg2$month, y = oisstAgg2$sst*3-40, group = oisstAgg2$year, 
                           color = factor(oisstAgg2$year)), size = 4) + # 2nd axis
  scale_color_manual("Year", values = c("purple", "blue3", "green", "orange", "red")) +
  scale_y_continuous(name = "Number of Samples", sec.axis = sec_axis(~(. + 40)/3, name = "Mean SST Southern California Bight (C)", 
                     labels = function(b) { paste0(round(b,0), "  ")})) + # The spaces are to stop the axis title over-riding the text
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  xlab("month") +
  theme_bw() + 
  theme(axis.title.y = element_text(vjust = 1.5)) +
  myTheme 
p3

##################################################################################################################################
################################################# Map max SST ######################################################################
##################################################################################################################################
# Make a map showing max SST at any time 2015-9, broader EPO
sstMax <- aggregate(sst ~ lonrd + latrd, oisst, FUN = max, na.rm = TRUE)
p4 <- ggplot()+
  geom_tile(data = sstMax, aes(x = lonrd, y = latrd, fill = sst)) +
  scale_fill_gradientn(colours = mypalette, na.value = NA, limits = c(12, 30.1)) + 
  guides(fill = guide_colorbar(title = "Max SST")) +
  stat_contour(data = sstMax, aes(x = lonrd, y = latrd, z = sst), breaks=c(24), size = 1.1, color = "black", linetype = "longdash") +
  xlab("longitude") + ylab("latitude") +
  pac.coast +
  coord_quickmap(xlim = c(220, 250),ylim = c(22, 50)) +
  theme_bw() +  myTheme
p4
