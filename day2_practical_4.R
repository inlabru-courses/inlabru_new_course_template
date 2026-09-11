## ----child="practicals/spatial_data_types_areal.qmd"--------------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

load(here::here("datasets/pollutionhealthdata.RData"))





## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(sf)
library(ggplot2)
library(scico)


## -----------------------------------------------------------------------------
resp_cases <- merge(GGHB.IZ, pollutionhealthdata, by = "IZ")


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(dplyr)
resp_cases <- resp_cases %>% 
  mutate(SMR = observed/expected, .by = year )


## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| fig-align: center
ggplot()+
  geom_sf(data=resp_cases,aes(fill=SMR))+
  facet_wrap(~year)+scale_fill_scico(palette = "roma")



## ----child="practicals/spatial_data_types_geostats.qmd"-----------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# For plotting
library(mapview)
library(ggplot2)
library(scico) # for colouring palettes

# Data manipulation
library(dplyr)




## -----------------------------------------------------------------------------
#| message: false
#| warning: false

load(here::here("datasets/pcod.RData"))




## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(sf)
pcod_sf =   st_as_sf(pcod_df, coords = c("lon","lat"), crs = 4326)


## -----------------------------------------------------------------------------
pcod_sf_proj <- st_transform(pcod_sf, crs = 32609)
st_crs(pcod_sf_proj)$units


## -----------------------------------------------------------------------------
pcod_sf_proj = st_transform(pcod_sf_proj,
                            gsub("units=m","units=km",
                                 st_crs(pcod_sf_proj)$proj4string)) 
st_crs(pcod_sf_proj)$units


## -----------------------------------------------------------------------------
pcod_sf = st_transform(pcod_sf,
                       crs = "+proj=utm +zone=9 +datum=WGS84 +no_defs +type=crs +units=km" )
st_crs(pcod_sf)$units



## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
#| message: false
#| warning: false
#| 
pcod_sf %>% 
  filter(year== 2017) %>%
  mutate(present = as.factor(present)) %>%
mapview(zcol = "present",
        layer.name = "Occupancy status of Pacific Cod in 2017")




## -----------------------------------------------------------------------------
#| message: false
#| warning: false

library(terra)
depth_r <- rast(qcs_grid, type = "xyz")
depth_r


## -----------------------------------------------------------------------------
crs(depth_r) <- crs(pcod_sf)


## -----------------------------------------------------------------------------
#| fig-width: 8 
#| fig-height: 8
#| fig-align: center  


library(tidyterra)

ggplot()+ 
  geom_spatraster(data=depth_r$depth)+
  geom_sf(data=pcod_sf,aes(color=factor(present))) +
  facet_wrap(~year)+
    scale_color_manual(name="Occupancy status for the Pacific Cod",
                     values = c("black","orange"),
                     labels= c("Absence","Presence"))+
  scale_fill_scico(name = "Depth",
                   palette = "nuuk",
                   na.value = "transparent" )




## ----child="practicals/spatial_data_types_points.qmd"-------------------------

## -----------------------------------------------------------------------------
#| message: false
#| warning: false

# For plotting
library(ggplot2)
library(scico) # for colouring palettes

# Data manipulation
library(dplyr)

# Spatial Data manipulation
library(sf)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| eval: false
#| 

# shp_SGC <-  st_read("datasets/SG_CairngormsNationalPark/SG_CairngormsNationalPark_2010.shp",quiet =T)
# 


## -----------------------------------------------------------------------------
load(here::here("datasets/SG_CairngormsNationalPark_2010.RData"))


## -----------------------------------------------------------------------------
shp_SGC <- shp_SGC %>% st_transform(crs = 27700)
st_crs(shp_SGC)$units


## -----------------------------------------------------------------------------
shp_SGC <- st_transform(shp_SGC,gsub("units=m","units=km",st_crs(shp_SGC)$proj4string)) 
st_crs(shp_SGC)$units


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
ggplot()+
  geom_sf(data=shp_SGC)



## -----------------------------------------------------------------------------
ringlett <- read.csv("datasets/bnm_ringlett.csv")
head(ringlett)


## -----------------------------------------------------------------------------
ringlett_sf <- ringlett %>% st_as_sf(coords = c("x","y"),crs = "+proj=longlat +datum=WGS84") 




## -----------------------------------------------------------------------------
ringlett_CNP <- ringlett_sf[shp_SGC,] # crop to mainland


## -----------------------------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
ggplot()+
  geom_sf(data=shp_SGC)+
  geom_sf(data=ringlett_CNP)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
#| fig-width: 4
#| fig-height: 4
#| fig-align: center
library(terra)
elevation_r <- rast("datasets/Scotland_elev.tiff")
crs(elevation_r) = crs(shp_SGC)
plot(elevation_r)


## -----------------------------------------------------------------------------
elevation_r <- elevation_r %>% scale()


## -----------------------------------------------------------------------------
#| fig-width: 6
#| fig-height: 4
#| fig-align: center

elev_CNP <- terra::crop(elevation_r,shp_SGC,mask=T)
plot(elev_CNP)



