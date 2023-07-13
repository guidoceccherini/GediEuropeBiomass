library(sf)

library(raster)

library(tidyverse)
library(tmap)
library(ggplot2)
library("stringr")
library(scales)
library("rnaturalearth")
library(ggspatial)
library(colorblindcheck)
library(rcartocolor)
library(MetBrewer)





a <- st_read('GRIDS/GridHex05Deg.shp')

Count19 <- readr::read_csv('Data/TreeCoverHRL05.csv')%>% rename(TC18=b1)

aJ <- a %>% left_join(Count19, by ='id')%>% filter(TC18>0)%>% drop_na(TC18)


st_crs(aJ) = 4326

plot(aJ['TC18'],breaks = "quantile")

a2 <- st_read('GRIDS/GridHex1Deg.shp')

Count192 <- readr::read_csv('Data/TreeCoverHRL1.csv')%>% rename(TC18=b1)

aJ2 <- a2 %>% left_join(Count192, by ='id')%>% filter(TC18>0)%>% drop_na(TC18)


st_crs(aJ2) = 4326




# world <- st_make_valid(world)

# st_write(world,'GEDI_GLOBAL/World.gpkg')

spdf_SL <- ne_countries(country = 'Slovenia',returnclass='sf',scale= 'large')
# write_sf(spdf_SL, 'SloveniaBorders.shp')





europe <- ne_countries(continent = 'europe',returnclass='sf',scale= 'large')
europe <- europe %>% dplyr::filter(name!='Russia') %>%
  dplyr::filter(name!='Ukraine') %>%
  dplyr::filter(name!='Belarus') %>%
  dplyr::filter(name!='Norway') %>%
  dplyr::filter(name!='Macedonia') %>%
  dplyr::filter(name!='Albania') %>%
  dplyr::filter(name!='Kosovo')%>%
  dplyr::filter(name!='Serbia')%>%
  dplyr::filter(name!='San Marino')%>%
  dplyr::filter(name!='Monaco')%>%
  dplyr::filter(name!='Andorra')%>%
  dplyr::filter(name!='Montenegro')%>%
  dplyr::filter(name!='Bosnia and Herz.')%>%
  dplyr::filter(name!='Moldova')%>%
  dplyr::filter(name!='Gibraltar')%>%
  dplyr::filter(name!='Vatican')%>%
  dplyr::filter(name!='Moldova')%>%
  dplyr::filter(name!='Malta')%>%
  dplyr::filter(name!='Jersey')%>%
  dplyr::filter(name!='Isle of Man')%>%
  dplyr::filter(name!='Åland')%>%
  dplyr::filter(name!='Faeroe Is.')%>%
  dplyr::filter(name!='Guernsey')%>%
  dplyr::filter(name!='United Kingdom')%>%
  dplyr::filter(name!='Ireland')%>%
  dplyr::filter(name!='Switzerland')%>%
  dplyr::filter(name!='Lithuania')%>%
  dplyr::filter(name!='Liechtenstein')%>%
  dplyr::filter(name!='Estonia')%>%
  dplyr::filter(name!='Latvia')%>%
  dplyr::filter(name!='Sweden')%>%
  dplyr::filter(name!='Finland')%>%
  dplyr::filter(name!='Denmark')%>%
  dplyr::filter(name!='Iceland')

# 
# world <- st_as_sf(rnaturalearth::countries110)
# europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')

# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,55,-25,55,-25,29),byrow = T,ncol = 2)))

europe.clipped <- suppressWarnings(st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe))))

# geol_cropped <- st_crop(geol, xmin = 138, xmax = 155, ymin = -32, ymax = -43)
# world.sf  <- world%>% 
#   st_intersection(aJTot)


# 
# ggplot() +
#   annotation_map_tile(type = "osmgrayscale") +
#   # annotation_map_tile(type = "stamenbw") +
#   # annotation_map_tile(type = "cartolight") +
#   geom_sf(data= a,aes(fill=FID), alpha = 0.5)+
#   scale_fill_gradient2(limits=c(-0.015, 0.015), oob=squish) +
#   ggtitle("Hexagons")
# 
# ggplot() +
#   geom_sf(data = europe.clipped) +
#   geom_sf(data= aJ,aes(fill=TC18), alpha = 0.5)+
#   # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
#   scale_fill_viridis_b(limits=c(0,1)) +
#   ggtitle("Tree Cover EU HRL Copernicus")
# ggsave("EU_HRL.png", width = 25, height = 18, units = "cm",dpi=300,path = 'GEDI_GLOBAL/figures' )



# PLOT GEDI



ggplot() +
  geom_sf(data = europe.clipped) +
  geom_sf(data= aJ,aes(fill=TC18*100), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,100)) +
  ggtitle("Percentage Forest Cover EU HRL Copernicus")
ggsave("EU_HRL.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )


ggplot() +
  geom_sf(data = europe.clipped) +
  geom_sf(data= aJ2,aes(fill=TC18*100), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,100)) +
  ggtitle("Percentage Forest Cover EU HRL Copernicus")
ggsave("EU_HRL2.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )


