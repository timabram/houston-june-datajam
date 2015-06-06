# Houston Data Viz

setwd("C:/Users/Tim/Desktop/Houston Data Viz/")
HospitalPrices <-read.csv("HospitalPrices.csv")

delta_price <- (HospitalPrices$AveragedCoveredCharges - HospitalPrices$AverageTotalPayments)/
  HospitalPrices$AveragedCoveredCharges
HospitalPrices[,"delta_price"] <-delta_price

HospitalPrices$group <-as.numeric(cut(HospitalPrices$delta_price, 10))
HospitalPrices$group <-factor(HospitalPrices$group)


library(ggplot2)
library(ggmap)
library(RColorBrewer)

procedures <-as.data.frame(table(HospitalPrices$ID))
HospitalPrices2 <- merge(HospitalPrices, procedures, by.x="ID", by.y="Var1", all.x=T)

#bar plots
ggplot(HospitalPrices, aes(x=State, y=delta_price)) + geom_bar(stat="identity")

outpatient <- subset(HospitalPrices, type=="Outpatient")
inpatient <- subset(HospitalPrices, type!="Outpatient")

ggplot(outpatient, aes(x=State, y=delta_price)) + geom_bar(stat="identity") + ggtitle('Outpatient')
ggplot(inpatient, aes(x=State, y=delta_price)) + geom_bar(stat="identity") + ggtitle('Inpatient')


# US MAP Based on %diff in Medicare Coverage
US <-qmap("united states", zoom=4, color="bw", legend = "none")
US +  geom_point(data=HospitalPrices, aes(x=lon, y=lat, color=group, alpha=0.2)) 

US +  geom_point(data=HospitalPrices2, aes(x=lon, y=lat, color=group, alpha=0.2, size=Freq)) 

#Density Plot:
US + stat_density2d(data=HospitalPrices,aes(x=lon, y=lat, fill=group, alpha=group),
  size=2, bins=4, geom="polygon")

table(HospitalPrices$group)

hos2 <-subset(HospitalPrices2, group %in% c(8, 9, 10))
US +  geom_point(data=hos2, aes(x=lon, y=lat, color=group, alpha=0.2, size=Freq)) 


drg = levels(HospitalPrices$DRG)
hos3 <-subset(hos2, DRG==drg[83])

#Most common DRG = 194 [48], 0267 [16], 690 [117], 292 [70]
code = drg[48]
hos3 <-subset(hos2, DRG==code)
US +  geom_point(data=hos3, aes(x=lon, y=lat, color=group, alpha=0.2, size=Freq)) +
  ggtitle(paste("DRG:", code, sep=""))

US +  geom_point(data=hos3, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  ggtitle(paste("DRG:", code, sep=""))


#Log Difference of price:
delta_price <- (HospitalPrices$AveragedCoveredCharges - HospitalPrices$AverageTotalPayments)/
  HospitalPrices$AveragedCoveredCharges
HospitalPrices2[,"delta_price_log10"] <-log10(delta_price)
HospitalPrices2$group2 <-as.numeric(cut(HospitalPrices2$delta_price_log10, 10))
HospitalPrices2$group2<-factor(HospitalPrices2$group2)
HospitalPrices2$MDC <-factor(HospitalPrices2$MDC)
hos5 <-subset(HospitalPrices2, group2 %in% c(8,9,10))
US +  geom_point(data=hos5, aes(x=lon, y=lat, color=group2, alpha=0.2, size=Freq)) +
  ggtitle(paste("DRG:", code, sep=""))

code = drg[48]
hos3 <-subset(hos5, DRG==code)

hos3 <-subset(hos5, MDC=="MDC 05")

US +  geom_point(data=hos3, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="blue", high="red")+ggtitle("MDC 05")

US +  geom_point(data=hos3, aes(x=lon, y=lat, color=AveragedCoveredCharges, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="blue", high="red")+  ggtitle(paste("DRG:", code, sep=""))


newdat <-read.csv("prices.csv")
HospitalPrices2$MDC <-newdat$MDC_code
# US <-get_map("houston", zoom=4)
# USMap <-ggmap(US, extent = "device", legend="topleft")
# USMap +
#   stat_density2d(
#     aes(x=lon, y=lat, fill=group, alpha=group),
#     size=2, bins=4, data=HospitalPrices,
#     geom="polygon")

ggplot(HospitalPrices, aes(x=lon, y=lat))+
  stat_density2d(aes(fill=group),alpha=0.5,
                 geom="polygon")
  #scale_fill_gradient(colors=rev(brewer.pal(7,"Spectral")))

hos3 <-subset(hos5, MDC=="MDC 04")

hos3 <-subset(hos5, (AverageTotalPayments > 2500) & (AverageTotalPayments > 25000))

map <-qmap("united states", zoom=4, color="bw", legend = "none")
map1 <-map +  geom_point(data=hos3, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="blue", high="red")+ggtitle("US_TP-2500")
pdf("US_TP-2500_2.pdf", width = 11, height = 8.5)
print(map1)
dev.off()

map <-qmap("west virginia", zoom=6, color="bw", legend = "none")
map2 <-map +  geom_point(data=hos3, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="blue", high="red")+ggtitle("WV_TP-2500")
pdf("WV_TP-2500_2.pdf", width = 11, height = 8.5)
print(map2)
dev.off()
