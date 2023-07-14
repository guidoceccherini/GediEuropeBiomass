library(sf)

library(raster)

library(tidyverse)
library(tmap)
library(ggplot2)
library("stringr") 
library(scales)

a <- st_read('GRIDS/GridHex05Deg.shp')

st_crs(a) = 4326


library(ggspatial)
library(colorblindcheck)
library(rcartocolor)





Count19TH <- readr::read_csv('Data/heights_2019_MedianAGB.csv')%>% rename(THM19 = agbd)

aJ <- a %>% left_join(Count19TH, by ='id')



plot(aJ['THM19'])
st_crs(aJ) = 4326

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=THM19), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Biomass 2019")
ggsave("GEDIBiomass2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )





# 22
Count22TH <- readr::read_csv('Data/heights_2022_MedianAGB.csv')%>% rename(THM22 = agbd)

aJ22 <- a %>% left_join(Count22TH, by ='id')



plot(aJ22['THM22'])
st_crs(aJ22) = 4326

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ22,aes(fill=THM22), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Biomass 2022")
ggsave("GEDIBiomass2022.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )


