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






Count19 <- readr::read_csv('Data//heights_2019_C.csv')%>% rename(Tot19=rh98 )
Count19T <- readr::read_csv('Data/heights_2019_Cgt5.csv')%>% rename(Trees = rh98)
Count19f <- readr::read_csv('Data/heights_2019_Cf.csv')%>% rename(Count19 = rh98)
Count19TH <- readr::read_csv('Data/heights_2019_Median.csv')%>% rename(THM19 = rh98)

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
  dplyr::select(-rn)%>%
  # select(-'_count')%>%
  dplyr::select(-'null')%>%
  dplyr::select(-`system:index`)%>%
  dplyr::select(-`.geo`)%>%
  dplyr::select(-'0.0')%>%
  dplyr::select(-left)%>%
  dplyr::select(-right)%>%
  dplyr::select(-bottom)%>%
  dplyr::select(-top)%>%
  
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

aJ <- a %>% left_join(Count19, by ='id')
aJ <- aJ %>% left_join(Count19T, by ='id')
aJ <- aJ %>% left_join(Count19TH, by ='id')
aJ <- aJ %>% left_join(Count19f1, by ='id')



plot(aJ['Tot19'])
st_crs(aJ) = 4326

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Tot19), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Samples 2019")
ggsave("GEDITot2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )




plot(aJ['Trees'])
ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Trees), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Trees 2019")
ggsave("GEDITrees2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )



aJ <- aJ %>% mutate(Ratio19 = Trees/Tot19)%>%dplyr::filter(Tot19 > 10000)

plot(aJ['Ratio19'])

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ,aes(fill=Ratio19*100), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,100)) +
  ggtitle("GEDI Ratio Tree vs Total 2019")
ggsave("GEDIRatio2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )


ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ|> dplyr:: filter(THM19>0),aes(fill=THM19), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,30)) +
  ggtitle("GEDI Mean Canopy height 2019")
ggsave("GEDI_CH_2019.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )





# ////////////////////////////////////

# 2022

Count22 <- readr::read_csv('Data//heights_2022_C.csv')%>% rename(Tot22=rh98 )
Count22T <- readr::read_csv('Data/heights_2022_Cgt5.csv')%>% rename(Trees = rh98)
Count22f <- readr::read_csv('Data/heights_2022_Cf.csv')%>% rename(Count22 = rh98)
Count22TH <- readr::read_csv('Data/heights_2022_Median.csv')%>% rename(THM22 = rh98)

Count22f$Count22 <- str_replace(Count22f$Count22, "\\{", "")
Count22f$Count22 <- str_replace(Count22f$Count22, "\\}", "")
Count22f$Count22 <- str_replace(Count22f$Count22, ".*_", "")


Count22f1 <- Count22f %>%
  mutate(rn = row_number()) %>%
  # separate_rows(Count22, sep="=\\s*\\n*")  %>%
  separate_rows(Count22, sep=",\\s*\\n*") %>%
  mutate(Count22 = str_remove_all(Count22, '"')) %>%
  separate(Count22, into = c("vars1", "vars2"), sep="\\s*=\\s*") %>% 
  pivot_wider(names_from = vars1, values_from = vars2, 
              values_fill = list(vars2 = '')) %>%
  dplyr::select(-rn)%>%
  # select(-'_count')%>%
  dplyr::select(-'null')%>%
  dplyr::select(-`system:index`)%>%
  dplyr::select(-`.geo`)%>%
  dplyr::select(-'0.0')%>%
  dplyr::select(-left)%>%
  dplyr::select(-right)%>%
  dplyr::select(-bottom)%>%
  dplyr::select(-top)%>%
  
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


Count22f1 <- Count22f1 %>% mutate_if(is.character,as.numeric)

Count22f1Tot <- colSums(Count22f1[,2:10], na.rm =T)

Count22f1Tot <- bind_rows(Count22f1Tot)

Count22f1Tot_long <- Count22f1Tot %>%
  pivot_longer(
    c("5",  "15", "25", "55", "45", "35", "65", "75", "85"),
    names_to = "H", values_to = "values"
  )

Count22f1Tot_long <- Count22f1Tot_long %>% mutate_if(is.character,as.numeric)
Count22f1Tot_long2 <- Count22f1Tot_long  %>% mutate(v22 = values/ sum(Count22f1Tot_long$values, na.rm =T))

p<-ggplot(data=Count22f1Tot_long, aes(x=H, y=values)) +
  geom_bar(stat="identity")
p
p2<-ggplot(data=Count22f1Tot_long, aes(x=H, y=values)) +
  # geom_bar(stat="identity")+
  geom_density(stat="identity", fill=NA) 
p2

aJ22 <- a %>% left_join(Count22, by ='id')
aJ22 <- aJ22 %>% left_join(Count22T, by ='id')
aJ22 <- aJ22 %>% left_join(Count22TH, by ='id')
aJ22 <- aJ22 %>% left_join(Count22f1, by ='id')



plot(aJ22['Tot22'])
st_crs(aJ22) = 4326

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ22,aes(fill=Tot22), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Samples 2022")
ggsave("GEDITot2022.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )




plot(aJ22['Trees'])
ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ22,aes(fill=Trees), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_b() +
  ggtitle("GEDI Trees 2022")
ggsave("GEDITrees2022.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )



aJ22 <- aJ22 %>% mutate(Ratio22 = Trees/Tot22)%>%dplyr::filter(Tot22 > 10000)

plot(aJ22['Ratio22'])

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ22,aes(fill=Ratio22*100), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,100)) +
  ggtitle("GEDI Ratio Tree vs Total 2022")
ggsave("GEDIRatio2022.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )


ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJ22|> dplyr:: filter(THM22>0),aes(fill=THM22), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_viridis_c(limits=c(0,30)) +
  ggtitle("GEDI Mean Canopy height 2022")
ggsave("GEDI_CH_2022.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )

# differences in TH

aJTot <- aJ|> st_drop_geometry() |> left_join(aJ22|> st_drop_geometry() |> dplyr::select(id,THM22,Ratio22 ), by = 'id')
aJTot <-  aJTot |> left_join(aJ |> dplyr::select(id), by = 'id')

aJTot <- aJTot |> mutate(DeltaT = THM22 - THM19)
aJTot <- aJTot |> mutate(DeltaArea = Ratio22 - Ratio19)

aJTot <- st_as_sf(aJTot)

ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJTot,aes(fill=DeltaT), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_gradient2(limits=c(-2, 2), oob=squish) +
  ggtitle("GEDI Delta Canopy height 2022 2019")
ggsave("GEDI_CH_Delta22_19.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )



ggplot() +
  # geom_sf(data = world) +
  geom_sf(data= aJTot,aes(fill=DeltaArea*100), alpha = 0.5)+
  # coord_sf( ylim = c(-52, 53),xlim = c(-135, 160))+
  scale_fill_gradient2(limits=c(-10, 10), oob=squish) +
  ggtitle("GEDI Delta ForestCover 2022 2019")
ggsave("GEDI_CH_DeltaArea22_19.png", width = 25, height = 18, units = "cm",dpi=300,path = 'Figures' )
