# Houston Data Viz, 20150606

setwd("C:/Users/Tim/Desktop/Houston Data Viz/test/")
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
  scale_colour_continuous(low="blue", high="red")+ggtitle("US_TP-2500")
pdf("US_MDC05.pdf", width = 11, height = 8.5)
print(map1)
dev.off()

# Viz 2) WV Focus
map <-qmap("west virginia", zoom=6, color="bw", legend = "none")
map2 <-map +  geom_point(data=mdc_plot, aes(x=lon, y=lat, color=delta_price, alpha=0.2, size=Freq)) +
  scale_colour_continuous(low="blue", high="red")+ggtitle("WV_TP-2500")
pdf("WV_MDC05.pdf", width = 11, height = 8.5)
print(map2)
dev.off()
