# Houston Data Viz, 20150606

setwd("C:/Users/Tim/Desktop/houston-june-datajam/")
HospitalPrices <-read.csv("HospitalPrices.csv")

delta_price <- (HospitalPrices$AveragedCoveredCharges - HospitalPrices$AverageTotalPayments)/
  HospitalPrices$AveragedCoveredCharges
HospitalPrices[,"delta_price"] <-delta_price

#Bin Price difference into "perecentiles"
HospitalPrices$group <-as.numeric(cut(HospitalPrices$delta_price, 10))
HospitalPrices$group <-factor(HospitalPrices$group)

library(ggplot2)
library(ggmap)
library(RColorBrewer)

procedures <-as.data.frame(table(HospitalPrices$ID))
HospitalPrices <- merge(HospitalPrices, procedures, by.x="ID", by.y="Var1", all.x=T)

#read in csv with MDC code column:
newdat <-read.csv("prices.csv")
HospitalPrices$MDC <-newdat$MDC_code

drg = levels(HospitalPrices$DRG)
#Most common DRG = 194 [48], 0267 [16], 690 [117], 292 [70]

# subset dataframe to remove groups with
HospitalPrices <-subset(HospitalPrices, group %in% c(8,9,10))

mdc_plot <-subset(HospitalPrices, MDC=="MDC 05")
#mdc_plot2 <-subset(HospitalPrices, (AverageTotalPayments > 2500) & (AverageTotalPayments > 25000))

# Viz 1) US Map
map <-qmap("united states", zoom=4, color="bw", legend = "none")
map1 <-map +  geom_point(data=mdc_plot, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="green", high="yellow")+ggtitle("US_TP-2500")
pdf("US_MDC05.pdf", width = 11, height = 8.5)
print(map1)
dev.off()

# Viz 2) WV Focus
map <-qmap("west virginia", zoom=6, color="bw", legend = "none")
map2 <-map +  geom_point(data=mdc_plot, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="green", high="yellow")+ggtitle("WV_TP-2500")
pdf("WV_MDC05.pdf", width = 11, height = 8.5)
print(map2)
dev.off()


######################### Choropleth Mapping: ###################

#download US count shapefiles from US Census:
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
library(rgdal)
setwd("./US_counties/")
counties <- readOGR(dsn=".",layer = "cb_2014_us_county_20m")
counties <-spTransform(counties, CRS("+proj=longlat +datum=WGS84"))
# Find the correct district for each incident (if the coordinates of the crime data
#  fall within a particular district boundary):
hospital_coords <-subset(HospitalPrices, select =c(lon,lat, ID))
hospital_coords <-hospital_coords[is.na(hospital_coords$lat)!=1,]
coordinates(hospital_coords)<-c("lon","lat")
# Both data sets need to have the same projection 
proj4string(hospital_coords) <- proj4string(counties)
inside.county <- !is.na(over(hospital_coords, as(counties, "SpatialPolygons")))
mean(inside.county) #reality-check to see if hospitals are located inside counties
hospital_coords$county_id <- over(hospital_coords, counties)$COUNTYNS
hospital_coords <-as.data.frame(hospital_coords)

# Add the district names back to the original dataset:
HospitalPrices <-HospitalPrices[is.na(HospitalPrices$lat)!=1,]
HospitalPrices[,"county_id"]<-hospital_coords$county_id

#For plotting the districts as regions on a map, have to add a few extra steps:
county_ids <-factor(counties$COUNTYNS, levels = unique(as.character(counties$COUNTYNS))) #, levels = 
county_ids <-as.data.frame(county_ids)
county_ids[,"index"]<-seq(0,length(levels(county_ids$county_ids))-1,1)
#colnames(county_ids)<-c("county_id", "index")
county_regions <-fortify(counties)
county_regions$id <-as.numeric(county_regions$id)
county_regions <-merge(county_regions, county_ids, by.x="id", by.y="index")

# Summarize data BY County
library(doBy)

drg = levels(HospitalPrices$DRG)
code = drg[70]
#Most common DRG = 194 [48], 0267 [16], 690 [117], 292 [70]
drg_prices <-subset(HospitalPrices, DRG == code)
sum_prices <-summaryBy(AveragedCoveredCharges ~ county_id, data = drg_prices, FUN = mean)
colnames(sum_prices)<-c("county_id", "price")
plot_data <-merge(county_regions, sum_prices, by.y = "county_id",by.x="county_ids",all.x=T, sort=F)
plot_data <-plot_data[order(plot_data$id, plot_data$order),]

## Debug: Make sure coords match to previous regions (order is essential!)
#sum(as.character(county_regions$county_ids) != as.character(plot_data$county_id))
#sum(county_regions$long != plot_data$long)
#sum(county_regions$lat != plot_data$lat)

#Plotting choropleth results (by county)
US <-qmap("united states", zoom=4, color="bw", legend = "none")
US + geom_polygon(data=plot_data, aes(x=long, y=lat, group=county_ids, fill=price, alpha=1), 
                  #color="black", weight=0
                  )+
                   scale_fill_gradient2(midpoint = 40000, low="green", mid = "blue", high="red")+ 
                  ggtitle(paste("Avg Hospital Bill for ", code, sep=""))

# ------Giving NA counties minimum color value (replaces gray counties to obtain seamless maps):
plot_data[(is.na(plot_data$price)==1),"price"]<-min(plot_data$price, na.rm=T)
#---------------------------------------------

# Zoom in on a particular region/state
NY <-qmap("new york", zoom=7, color="bw", legend = "none")
NY + geom_polygon(data=plot_data, aes(x=long, y=lat, group=county_ids, fill=price, alpha=1), 
                  color="black", weight=1)+
  scale_fill_gradient2(midpoint = 40000, low="green", mid = "blue", high="red")+ 
  ggtitle(paste("Avg Hospital Bill for ", code, sep=""))