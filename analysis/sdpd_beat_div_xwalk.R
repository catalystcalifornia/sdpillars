#### Set up work space ####
library(dplyr)
library(RPostgreSQL)
library(sf)
library(tidyr)
library(tidyverse)
library(sf)
library(leaflet)

options(scipen=999)

# Connect to postgres

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")


# read in tables from postgres----

stops<-dbGetQuery(con, "SELECT * FROM rel_stops") ## use THIS to make the xwalk

div<-dbGetQuery(con, "SELECT * FROM sangis_sdpd_divisions_2023")%>%
  dplyr::select(-geom)

div_sf<-st_read(con, query="SELECT * FROM sangis_sdpd_divisions_2023")

beats<-st_read(con, query = "SELECT * FROM sangis_sdpd_beats_2023")%>%
  mutate(beat=as.character(beat))

# join divisions to the beats table just to get the full division name ----

beats_div<-beats%>%
  left_join(div%>%dplyr::select(div_num, div_name), by=c("div"="div_num"))%>%
  dplyr::select(beat, name, div, div_name)%>%
  as.data.frame()%>%
  dplyr::select(-geom)%>%
  rename('beat_name' = 'name')%>%
  group_by(beat)%>%
  slice(1)%>%
  mutate(beat_name=ifelse(beat %in% "511", 'BARRIO LOGAN', beat_name))%>% # there was a data entry error where beat=511 showed up twice, once with the beat_name included, and once where beat_name was NA. So I manually re-added the beat_name
  ungroup()

# join to stops data ----

df<-stops%>%
  left_join(beats_div%>%select(beat, div, div_name), by=c("beat"="beat"))%>%
  mutate(div=ifelse(beat_name %in% 'La Jolla Village 125', 1, div))%>% #recode La Jolla Village 125, which doesn't have a division assignment even though it is basically the same as La Jolla 124, which is assigned to Nothern division. 
  mutate(div_name=ifelse(beat_name %in% 'La Jolla Village 125', "NORTHERN", div_name))%>%
  select( beat_name, beat, div_name, div)

# make sure that only unique beats are left in the final df

df<-unique(df[,c(1:4)])%>%
  filter(!is.na(beat))

#### lets map the original beats that don't join to a division ####

# CONCLUDING it is OK that the la jolla cove slivers are not assigned to a division. 

na<-beats_div%>%
  filter(is.na(div_name)) # beats ==0 | 9 

beat_na<-beats%>%
  filter(beat==0 |beat==9)

# transform for mapping

beat_na<-st_transform(beat_na, 4326)
div_sf<-st_transform(div_sf, 4326)
beats<-st_transform(beats, 4326)

# map

beat_popup<-paste0("Beat Number: ", beats$beat)
div_popup<-paste0(div_sf$div_name,"<br>","Division: ", div_sf$div_num)

leaflet()%>%
  
  addMapPane("beat", zIndex=490)%>%
  addMapPane("div", zIndex=490)%>%
  addMapPane("na", zIndex=490)%>%
  
  # beat layer

  addPolygons(data=beats, 
              weight = 2, smoothFactor = 0.8, fill=F,
              opacity = 1,
              color = "red",
              fillOpacity =1,
              popup=beat_popup,
               options = pathOptions(pane = "beat")) %>%
  
# division layer
  
  addPolygons(data=div_sf,
               popup=div_popup,
              color="yellow", weight=2, opacity = 1, fill=F,
              group="SDPD Division",
               options = pathOptions(pane = "div")) %>%
  
  
  # NA layer
  
  addPolygons(data=beat_na,
              color="blue", weight=2, opacity = 1, fill=F,
              group="NA Beat",
              options = pathOptions(pane = "na")) %>%
  
  
   #layer control
    addLayersControl(overlayGroups =
                       c("SDPD Division", "NA Beat"), 
                  options = layersControlOptions(collapsed = FALSE))%>%
  
     #hide best starts by default
  hideGroup( "NA Beat")%>%
  
  #base and view
  addProviderTiles("CartoDB.PositronNoLabels")


#### Finalize and push to postgres ####

# set column types
charvect = rep('varchar', ncol(df)) 

# add df colnames to the character vector

names(charvect) <- colnames(df)

##### Export Data #####

dbWriteTable(con,  "sdpd_beat_div_xwalk", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE sdpd_beat_div_xwalk  IS 'Crosswalk of SDPD divisions to beats 
based off the SDPD RIPA Stop Data for 2022. NOTE that the beat La Jolla village 125 was manually assigned to the 
Northern Division because it is in the same area as La Jolla 124 beat which is already assigned to the Northern Division.
The original data has no division for La Jolla Village 125.
R script used to create xwalk and import table: W:/Project/RJS/Pillars/R/Data and Geo Prep/sdpd_beat_div_xwalk.R';

COMMENT ON COLUMN sdpd_beat_div_xwalk.beat_name IS 'SDPD Beat name';
COMMENT ON COLUMN sdpd_beat_div_xwalk.beat IS 'SDPD beat number';
COMMENT ON COLUMN sdpd_beat_div_xwalk.div_name IS 'SDPD Division name';
COMMENT ON COLUMN sdpd_beat_div_xwalk.div IS 'SDPD division number';")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
