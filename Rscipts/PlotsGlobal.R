library(sf)

library(raster)

library(tidyverse)
library(tmap)
library(ggplot2)
library("stringr")
library(scales)

a <- st_read('GEDI_GLOBAL/Hexcell500Filter.shp')

st_crs(a) = 4326



plot(a['FID'],breaks = "quantile")
# world <- st_make_valid(world)

# st_write(world,'GEDI_GLOBAL/World.gpkg')

world <- st_read('GEDI_GLOBAL/World.gpkg')
world <- st_cast(world, "MULTIPOLYGON")

# 
# geol_cropped <- st_crop(geol, xmin = 138, xmax = 155, ymin = -32, ymax = -43)
# world.sf  <- world%>% 
#   st_intersection(aJTot)

library(ggspatial)
library(colorblindcheck)
library(rcartocolor)
# 
# ggplot() +
#   annotation_map_tile(type = "osmgrayscale") +
#   # annotation_map_tile(type = "stamenbw") +
#   # annotation_map_tile(type = "cartolight") +
#   geom_sf(data= a,aes(fill=FID), alpha = 0.5)+
#   scale_fill_gradient2(limits=c(-0.015, 0.015), oob=squish) +
#   ggtitle("Hexagons")

ggplot() +
  geom_sf(data = world) +
  geom_sf(data= a,aes(fill=FID), alpha = 0.5)+
  coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  # scale_fill_gradient2(limits=c(-0.015, 0.015), oob=squish) +
  ggtitle("Hexagons")
ggsave("GEDIH.png", width = 25, height = 18, units = "cm",dpi=300,path = 'GEDI_GLOBAL/figures' )






Count19 <- readr::read_csv('GEDI_GLOBAL/heights_2019_C.csv')%>% rename(Tot19=rh98 )
Count19T <- readr::read_csv('GEDI_GLOBAL/heights_2019_Cgt5.csv')%>% rename(Trees = rh98)
Count19f <- readr::read_csv('GEDI_GLOBAL/heights_2019_Cf.csv')%>% rename(Count19 = rh98)
Count19TH <- readr::read_csv('GEDI_GLOBAL/heights_2019_Median.csv')%>% rename(THM19 = rh98)

Count19f$Count19 <- str_replace(Count19f$Count19, "\\{", "")
Count19f$Count19 <- str_replace(Count19f$Count19, "\\}", "")
Count19f$Count19 <- str_replace(Count19f$Count19, ".*_", "")


Count19f1 <- Count19f %>%
  mutate(rn = row_number()) %>%
  # separate_rows(Count19, sep="=\\s*\\n*")  %>%
  separate_rows(Count19, sep=",\\s*\\n*") %>%
  mutate(Count19 = str_remove_all(Count19, '"')) %>%
  separate(Count19, into = c("vars1", "vars2"), sep="\\s*=\\s*") %>% 
  pivot_wider(names_from = vars1, values_from = vars2, 
              values_fill = list(vars2 = '')) %>%
  select(-rn)%>%
  select(-'_count')%>%
  select(-'null')%>%
  select(-`system:index`)%>%
  select(-`.geo`)%>%
  select(-'0.0')%>%
  # rename('0' = `0.0`)%>%
  rename('5' = `1.0`)%>%
  rename('15' = `2.0`)%>%
  rename('25' = `3.0`)%>%
  rename('35' = `4.0`)%>%
  rename('45' = `5.0`)%>%
  rename('55' = `6.0`)%>%
  rename('65' = `7.0`)%>%
  rename('75' = `8.0`)%>%
  rename('85' = `9.0`)


# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.lte(1),0);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(1).and(heights_2019_Class.lte(5)),1);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(5).and(heights_2019_Class.lte(15)),2);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(15).and(heights_2019_Class.lte(25)),3);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(25).and(heights_2019_Class.lte(35)),4);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(35).and(heights_2019_Class.lte(45)),5);
# heights_2019_Class = heights_2019_Class.where(heights_2019_Class.gt(45),6);



Count19f1 <- Count19f1 %>% mutate_if(is.character,as.numeric)

Count19f1Tot <- colSums(Count19f1[,2:10], na.rm =T)

Count19f1Tot <- bind_rows(Count19f1Tot)

Count19f1Tot_long <- Count19f1Tot %>%
  pivot_longer(
    c("5",  "15", "25", "55", "45", "35", "65", "75", "85"),
    names_to = "H", values_to = "values"
  )
    
Count19f1Tot_long <- Count19f1Tot_long %>% mutate_if(is.character,as.numeric)
Count19f1Tot_long2 <- Count19f1Tot_long  %>% mutate(v19 = values/ sum(Count19f1Tot_long$values, na.rm =T))

p<-ggplot(data=Count19f1Tot_long, aes(x=H, y=values)) +
  geom_bar(stat="identity")
p
p2<-ggplot(data=Count19f1Tot_long, aes(x=H, y=values)) +
  # geom_bar(stat="identity")+
  geom_density(stat="identity", fill=NA) 
p2

aJ <- a %>% left_join(Count19, by ='FID')
aJ <- aJ %>% left_join(Count19T, by ='FID')
aJ <- aJ %>% left_join(Count19TH, by ='FID')
aJ <- aJ %>% left_join(Count19f1, by ='FID')



plot(aJ['Tot19'])
st_crs(aJ) = 4326

ggplot() +
  geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Tot19), alpha = 0.5)+
  coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Samples 2019")
ggsave("GEDITot2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'GEDI_GLOBAL/figures' )




plot(aJ['Trees'])
ggplot() +
  geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Trees), alpha = 0.5)+
  coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Trees 2019")
ggsave("GEDITrees2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'GEDI_GLOBAL/figures' )



aJ <- aJ %>% mutate(Ratio19 = Trees/Tot19)%>%dplyr::filter(Tot19 > 50000)

plot(aJ['Ratio19'])

ggplot() +
  geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Ratio19), alpha = 0.5)+
  coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Ratio Tree vs Total 2019")
ggsave("GEDIRatio2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'GEDI_GLOBAL/figures' )




