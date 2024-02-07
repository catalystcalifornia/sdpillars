## Load Libraries

#install packages if not already installed

# Analysis Task : number and % of FI stops where a search took place, field interview card completed, and no contraband was found by division and race --------

list.of.packages <- c("tidyverse","RPostgreSQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Packages ##
library(tidyverse)
library(RPostgreSQL)


## data setup ##
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# Import data that we need
rel_persons_fi_card <- dbGetQuery(con, "SELECT * FROM rel_persons_recode") %>% select(stop_id, person_id, stop_year, fi_card_flag)
rel_races_recode <- dbGetQuery(con, "SELECT * FROM rel_races_recode") %>% select(stop_id, person_id, nh_race, sswana_flag, aian_flag, nhpi_flag)
rel_stops_cfs_beat <- dbGetQuery(con, "SELECT * FROM rel_stops") %>% select(stop_id, stop_in_response_to_cfs, beat, beat_name)
rel_persons_searches_contraband  <- dbGetQuery(con, "SELECT * FROM rel_persons_searches") %>% select(stop_id, search, contraband)
xwalk<-dbGetQuery(con, "SELECT * FROM sdpd_beat_div_xwalk")

## Step 1: Filter for officer-initiated stops, year = 2022, where a search took place and stop resulted in Identification Card Completed

## Step 2: Of these stops, group the total stop count by division and race.

## Step 3: Of the stops in Step 1, filter for whether a contraband was found, then group total and by race by division. 

## Step 4: Calculate Rate

## Now use the person data and find count by division -----------------------------------------

## merge relevant df's and filter for 2022 officer-initiated stops that resulted in FI card completed
rel_stops_fi_card_cfs_person <- rel_persons_fi_card %>% left_join(rel_stops_cfs_beat) %>% filter(stop_year == "2022" & stop_in_response_to_cfs == "0" & fi_card_flag == "1")

## merge with races
rel_stops_fi_card_cfs_person_race <- rel_stops_fi_card_cfs_person %>% left_join(rel_races_recode)

## merge filtered 2022 Officer-initiated stops with relational df that has searches and contraband. Filter for search = 1

rel_stops_fi_card_cfs_person_race_search <- rel_stops_fi_card_cfs_person_race %>% left_join(rel_persons_searches_contraband) %>% filter(search == "1")

# merge df with beats and division

df <- rel_stops_fi_card_cfs_person_race_search %>% left_join(xwalk) %>% filter(!is.na(div_name))

# ## df  now has our unit of analysis: 2022 officer-initiated stop --------

# total stops where a search was conducted and FI card completed by division --------

df_stops_div_total <- df  %>% mutate(race = "total") %>% group_by(div, div_name, race) %>% summarize(stop_count = n()) 

df_stops_div_nh_race <- df  %>%  group_by(div, div_name, nh_race) %>% summarize(stop_count = n()) %>% rename(race = nh_race)

df_sswana_div_race <- df %>% filter(sswana_flag == "1") %>% group_by(div, div_name, sswana_flag) %>% summarize(stop_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
"1" = "sswana"))

df_aian_div_race <- df %>% filter(aian_flag == "1") %>% group_by(div, div_name, aian_flag) %>% summarize(stop_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
"1" = "aian"))

df_nhpi_div_race <- df %>% filter(nhpi_flag == "1") %>% group_by(div, div_name, nhpi_flag) %>% summarize(stop_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
"1" = "nhpi"))

## combine all searched FI card stops
df_stops_div_race <- rbind(df_stops_div_total, df_stops_div_nh_race, df_sswana_div_race, df_aian_div_race,df_nhpi_div_race)


# contraband not found by race and division for stops with a search and FI card completed --------------------------------------------

df_stops_div_total_contraband <- df  %>% mutate(race = "total") %>%  filter(contraband == "0") %>% group_by(div, div_name, race) %>% summarize(no_contraband_count = n()) 

df_stops_div_nh_race_contraband <- df %>% filter(contraband == "0") %>% group_by(div, div_name, nh_race) %>% summarize(no_contraband_count = n()) %>% rename(race = nh_race)

df_sswana_div_race_contraband <- df %>% filter(sswana_flag == "1" & contraband == "0") %>% group_by(div, div_name, sswana_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
"1" = "sswana"))

df_aian_div_race_contraband <- df %>% filter(aian_flag == "1" & contraband == "0") %>% group_by(div, div_name, aian_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
"1" = "aian"))

df_nhpi_div_race_contraband <- df %>% filter(nhpi_flag == "1" & contraband == "0") %>% group_by(div, div_name, nhpi_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
"1" = "nhpi"))

## combine all no contraband stops
df_stops_div_race_contraband <- rbind(df_stops_div_total_contraband, df_stops_div_nh_race_contraband, df_sswana_div_race_contraband, df_aian_div_race_contraband,df_nhpi_div_race_contraband)


### merge both df's: stop count and no contraband count, make no contraband count 0, and then calculate rate

df_final <- df_stops_div_race %>% left_join(df_stops_div_race_contraband) %>% mutate(no_contraband_count = ifelse(is.na(no_contraband_count), 0, no_contraband_count), 
  no_contraband_prc_rate = (no_contraband_count/stop_count) * 100)

# create level: FI Card

df_final <- df_final %>% mutate(level = "Field Interview Card Completed")


# Export ------------------------------------------------------------------

dbWriteTable(con,  "report_hitrates_div_race_ficard_person", df_final,
          overwrite = TRUE, row.names = FALSE)

#write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE report_hitrates_div_race_ficard_person  IS 'No discovery of contraband by race and total for all officer-initiated stops that resulted in a search in 2022 and Field Interview Card Completed by division.
Analysis includes total and race calculation. 
R script used to calculate rates and import table: W:/Project/RJS/Pillars/R/analysis/report_hitrates_div_race_ficard_person.R

QA document: 
W:/Project/RJS/Pillars/Documentation/QA_report_hitrates_div_race_ficard_person.docx
NHPI, SSWANA(MESA) and AIAN are alone or in combination with Latinx.';

COMMENT ON COLUMN report_hitrates_div_race_ficard_person.div IS 'San Diego Division Number'; 
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.div_name IS 'San Diego Division Name'; 
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.race IS 'Perceived race of person stopped as indicated by officer';
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.stop_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search and FI card completed';
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.no_contraband_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search, FI card completed, and no contraband was found';
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.no_contraband_prc_rate IS 'no hit rate for stops that resulted in a search and FI card completed';
COMMENT ON COLUMN report_hitrates_div_race_ficard_person.level IS 'Level of analysis: Field Interview Card Completed';
")

# send table comment + column metadata
dbSendQuery(con, table_comment)












