#install.packages('rgeos', type='source')
#install.packages('rgdal', type='source')
library(ggplot2)
library(maptools)
library(rgeos)
#library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)
library(sf)
library(rgdal)
library(dplyr)
library(plotly)


## Shape file downloaded from https://drive.google.com/drive/folders/0ByLjiBJ1xOpuTVRIWThhazBLdWM


shp <- readOGR('D:/Harish/R practice projects/india/Election/india_pc_2019.shp') #With Telangana, Chattisgarh and J&K


str(shp)
plot(shp)
head(shp)

head(shp)
str(shp)

# manually separating some places as per State

shp$PC_NAME[ shp$RId == 3] <- "HAMIRPUR, HIMACHAL PRADESH"
shp$PC_NAME[ shp$RId == 119] <- "HAMIRPUR, UTTAR PRADESH"
shp$PC_NAME[shp$RId == 135] <- "MAHARAJGANJ, UTTAR PRADESH"
shp$PC_NAME[shp$RId == 171] <- "MAHARAJGANJ, BIHAR"
shp$PC_NAME[shp$RId == 181] <- "AURANGABAD, BIHAR"
shp$PC_NAME[shp$RId == 359] <- "AURANGABAD, MAHARASHTRA"
           
#making dataframe

shp.f <- fortify(shp, region="PC_NAME")
head(shp.f)

#roads.df <- merge(fortify(shp), shpq, by="ST_NAME")

nrow(shp.f)
names(shp)
head(shp.f)
Results <- read.csv('D:/Harish/R practice projects/india/election/Data/2019 Results New.csv')
names(Results)
head(Results)
nrow(Results)

#merging the data by constituency

merged <-merge(shp.f,Results, by="id")

final.plot<-merged[order(merged$order), ]


head(merged)

head(final.plot)
tail(final.plot)

names(merged)

#Adding manual colors for each party

cols <- c(
  "TRS" = "darkseagreen3",
  "NDA" = "darkorange",
  "UPA" = "blue",
  "MGB" = "green",
  "YSRC" = "darksalmon",
  "TMC" = "deeppink1",
  "BJD" = "darkorchid2",
  "NA" = "cyan",
  "OTHERS"= "red"
  
 )

# Making final Indian Map

chart <- ggplot()+
  geom_polygon(data = final.plot,aes(x = long, y = lat, group = group, 
  fill = W_Alliance,color=id),color="black",size = 0.25) +
  coord_map() +
  xlab('Longitude')+
  ylab('Latitude') +
  labs(title="Indian Elections - 2019 - Party wise Results", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki") +
  scale_fill_manual(name="id", values = cols) +
  labs(subtitle="Indian Elections Voting % Share Over the Years", 
       y="Voting %", 
       x="Party", 
       title="Scatterplot", 
       caption = "SOURCE: oneindia.com, indiatoday.in, eci.gov.in & wiki")

# prining chart output
print(chart)
