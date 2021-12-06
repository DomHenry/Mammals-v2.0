## ________________________________________________________________________

## Title:
## Purpose:
## Author:
## Date:

## Libraries
library(sf)
library(tidyverse)
library(lubridate)
library(gghighlight)
library(gridExtra)
library(ggpubr)
library(glue)
## ________________________________________________________________________

## Load occurrence data
sdm_dir <- "data output/sdm data processing"
load(glue("{sdm_dir}/{sppselect}/occ_data_clean.RData"))

source("helper functions/check_bat_spp.R")
(is_bat <- check_bat_spp(sppselect))

# Investigate duplicates --------------------------------------------------

# Add lat and long columns 
source("helper functions/sfc_as_cols.R")
occ_data_sf <- sfc_as_cols(occ_data_sf) %>% 
  select(-geometry,geometry)

glimpse(occ_data_sf)

## Print duplicates when occ ID is removed
janitor::get_dupes(st_drop_geometry(occ_data_sf)%>% select(-occurrence_id))

## Proportion unique records
print(glue::glue(
  "{round(nrow(distinct(occ_data_sf %>% 
  select(-occurrence_id)))/nrow(occ_data %>% 
  select(-occurrence_id))*100,2)}% UNIQUE RECORDS"))

## Write duplicates to file (non-distinct values)
occ_data %>% 
  group_by_at(vars(-occurrence_id)) %>% 
  filter(n() > 1) %>%  # keep non-distinct values only (i.e. duplicates)
  ungroup() %>% 
  arrange(order,
          genus,
          scientific_name,
          decimal_latitude ,decimal_longitude) %>% 
  mutate_at(vars(core:buff3), as.character) %>% 
  write_csv(glue("{sdm_dir}/{sppselect}/duplicate_records_{sppselect}.csv"))

## Remove spatial duplicates from remainder of analysis (select distinct points) 
occ_data_sf <- occ_data_sf %>% 
  distinct(geometry, .keep_all = TRUE)


# Temporal plots ----------------------------------------------------------

## Add date variables
occ_data_sf <- occ_data_sf %>% 
  mutate(year = ifelse(year == "-9999", NA, year)) %>% 
  mutate(year_date = dmy(str_c("01","01",year, sep = "-"))) %>% 
  mutate(decade = as.factor(year(floor_date(year_date, years(10)))))

undated <- occ_data_sf %>% 
  filter(is.na(year_date)) %>% 
  nrow()

## Set ggplot theme
ptheme <- theme(axis.text.x = element_text(angle = 90, size = 16),
                axis.text.y = element_text(size = 16),
                axis.title = element_text(size = 16),
                title = element_text(size = 18))

## Occurrence records frequency counts
occ_data_sf %>% 
  ggplot(aes(x = year_date)) +
  geom_bar() +
  ylab("Count of occurrence records") +
  xlab ("") +
  labs(title = sppselect,
       subtitle = glue("{undated} undated records from a total of {nrow(occ_data_sf)}"))+
  ptheme

ggsave(glue("{sdm_dir}/{sppselect}/occ_frequency_date_all_{sppselect}.jpg"), height = 9, width = 12)

occ_data_sf %>% 
  filter(year_date > "1980-01-01") %>% 
  ggplot(aes(x = year_date)) +
  geom_bar() +
  ylab("Count of occurrence records") +
  xlab ("") +
  labs(title = sppselect,
       subtitle = glue("{undated} undated records"))+
  scale_x_date(date_breaks = "3 year", date_labels = "%Y")+
  ptheme

ggsave(glue("{sdm_dir}/{sppselect}/occ_frequency_date_1980_{sppselect}.jpg"), height = 9, width = 12)

# Spatial plots -----------------------------------------------------------

## Import spatial data
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
za <- st_read("data input/RSA_fixed.shp",crs = latlongCRS)

if(is_bat){
  
  range <- st_read(glue("{sdm_dir}/{sppselect}/IUCN_{sppselect}.shp"))
  
} else if (!is_bat){
  
  range <- st_read(glue("{sdm_dir}/{sppselect}/QDS_{sppselect}.shp"))
  
  
}

## Create inset box
occ_bbox <- st_make_grid(st_buffer(occ_data_sf, 0.3), n = 1) # Make a grid with one cell

## Set theme
ptheme <- theme(legend.text = element_text(size = 22),
                legend.title = element_text(size = 24),
                axis.text = element_text(size = 18),
                plot.title = element_text(size = 24))

p1 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.3), size = 0.5)+
  geom_sf(data = range, fill = NA, col = "dodgerblue", size = 0.8) +
  geom_sf(data = occ_data_sf, size = 1.5, col = "black")+
  theme_bw()+
  ggtitle(glue("{sppselect}: {nrow(occ_data_sf)} occurrence points"))+
  ptheme 
  
ggsave(glue("{sdm_dir}/{sppselect}/occ_map.jpg"), plot = p1, height = 12, width = 16)

p2 <- ggplot()+
  geom_sf(data = za, fill = NA, size = 0.5)+
  geom_sf(data =  occ_data_sf, aes(color = decade), size = 3)+
  scale_color_viridis_d(na.value = alpha("grey",0.7), direction = 1)+
  guides(color = guide_legend(title="Decade",
                              override.aes = list(size = 8)))+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  ptheme +
  ggtitle(glue("{sppselect}"))

ggsave(glue("{sdm_dir}/{sppselect}/occ_map_decadal.jpg"), plot = p2, height = 12, width = 16)

p3 <- ggplot() +
  geom_sf(data = za, fill = alpha("grey",0.3), size = 0.5)+
  geom_sf(data = occ_bbox, fill = NA, col = "dodgerblue", size = 1.2) +
  geom_sf(data = occ_data_sf, size = 1.5, col = "black")+
  theme_bw()+
  ggtitle(glue("{sppselect}"))+
  ptheme 

p4 <- ggplot()+
  geom_sf(data = za, fill = alpha("grey",0.3), size = 0.5)+
  geom_sf(data = occ_data_sf, size = 1.5, col = "black")+
  coord_sf(xlim = c(st_bbox(occ_bbox)$xmin,st_bbox(occ_bbox)$xmax),
           ylim = c(st_bbox(occ_bbox)$ymin,st_bbox(occ_bbox)$ymax))+
  theme_bw()+
  theme(panel.border = element_rect(colour = "dodgerblue", fill=NA, size=1.6))+
  ptheme

p5 <- grid.arrange(grobs = list(p3,p4), nrow = 1)
ggsave(glue("{sdm_dir}/{sppselect}/occ_map_inset.jpg"), plot = p5, height = 12, width = 16)


