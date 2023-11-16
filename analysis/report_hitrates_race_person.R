
# Task: Look at low hit/contraband discovery rate for officer-initiated stops that involve a search in 2022.

## Packages ##
library(tidyverse)
library(RPostgreSQL)


## data setup ##
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")


## Step 1: Filter for officer-initiated stops, year = 2022, and where a search took place

## Step 2: Of these stops, group the total count by all of SDPD, divide by total population and calculate rate 

## Step 3: Of the stops in step 1, filter for contraband = 0, group total count by all of SDPD

# Import data that we need
rel_persons <- dbGetQuery(con, "SELECT * FROM rel_persons_recode") %>% select(stop_id, person_id, stop_year)
rel_races_recode <- dbGetQuery(con, "SELECT * FROM rel_races_recode") %>% select(stop_id, person_id, nh_race, sswana_flag, aian_flag, nhpi_flag)
rel_stops_cfs <- dbGetQuery(con, "SELECT * FROM rel_stops") %>% select(stop_id, stop_in_response_to_cfs)
rel_persons_searches <- dbGetQuery(con, "SELECT * FROM rel_persons_searches") %>% select(stop_id, person_id, search, contraband)

## merge relevant df's and filter for 2022 officer-initiated stops

rel_persons_cfs <- rel_persons %>% left_join(rel_stops_cfs) %>% filter(stop_year == "2022" & stop_in_response_to_cfs == "0")

## merge with races
rel_persons_cfs_race <- rel_persons_cfs %>% left_join(rel_races_recode)


# merge filtered 2022 Officer-initiated stops with searches relational df table --------

df <- rel_persons_cfs_race %>% left_join(rel_persons_searches) %>% filter(search == "1")

## total officer-initiated stops where a search was conducted by race

df_total <-  df %>% mutate(race = "total") %>% group_by(race) %>% summarize(searches_count = n()) 

df_stops_nh_race <- df %>% group_by(nh_race) %>% summarize(searches_count = n()) %>% rename(race = nh_race) %>% filter(!is.na(race)) # filter out 1 obs where no race is coded

df_sswana_race <- df %>% filter(sswana_flag == "1") %>% group_by(sswana_flag) %>% summarize(searches_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
"1" = "sswana"))

df_aian_race <- df %>% filter(aian_flag == "1") %>% group_by(aian_flag) %>% summarize(searches_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
"1" = "aian"))

df_nhpi_race <- df %>% filter(nhpi_flag == "1") %>% group_by(nhpi_flag) %>% summarize(searches_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
"1" = "nhpi"))

## combine all searched FI card stops
df_stops_race <- rbind(df_total, df_stops_nh_race, df_sswana_race, df_aian_race,df_nhpi_race) 


# total officer-initiated stops where a search and no contraband was found was conducted by race --------

df_total_contraband <-  df %>% mutate(race = "total") %>% filter(contraband == "0") %>%  group_by(race) %>% summarize(no_contraband_count = n()) 

df_stops_nh_race_contraband <- df %>% filter(contraband == "0") %>% group_by(nh_race) %>% summarize(no_contraband_count = n()) %>% rename(race = nh_race) %>% filter(!is.na(race)) # filter out 1 obs where no race is coded

df_sswana_race_contraband <- df %>% filter(sswana_flag == "1" & contraband == "0") %>% group_by(sswana_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
                                                                                                                                                                      "1" = "sswana"))
df_aian_race_contraband <- df %>% filter(aian_flag == "1" & contraband == "0") %>% group_by(aian_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
                                                                                                                                                              "1" = "aian"))
df_nhpi_race_contraband <- df %>% filter(nhpi_flag == "1" & contraband == "0") %>% group_by(nhpi_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
                                                                                                                                                              "1" = "nhpi"))

## combine all searched FI card stops
df_stops_race_contraband <- rbind(df_total_contraband, df_stops_nh_race_contraband, df_sswana_race_contraband, df_aian_race_contraband,df_nhpi_race_contraband) 

# merge with contraband, then calculate rate
df_rate <- df_stops_race %>% left_join(df_stops_race_contraband) %>% mutate(no_contraband_rate = no_contraband_count/searches_count * 100)

# filter for total
#df_no_contraband <- df_rate %>% mutate(label = "No Contraband or Evidence Found") %>% rename(count = no_contraband_count, rate = no_contraband_prc_rate)

# calculate contraband
#df_contraband <- df_rate %>% filter(race == "total") %>% mutate(label = "Contraband or Evidence Found", count = stop_count - no_contraband_count, rate = 100-no_contraband_prc_rate) %>% #select(-no_contraband_count, -no_contraband_prc_rate)

# calculate level

df_final <- df_rate %>% mutate(level = "SDPD")

# Export ------------------------------------------------------------------

#dbWriteTable(con,  "report_hitrates_race_person", df_final, 
#          overwrite = TRUE, row.names = FALSE)

#write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE report_hitrates_race_person  IS 'No discovery of contraband by race for all officer-initiated stops that resulted in a search in 2022
R script used to calculate rates and import table: W:/Project/RJS/Pillars/R/analysis/report_hitrates_race_person.R

QA document: 
W:/Project/RJS/Pillars/Documentation/QA_report_hitrates_race_person.docx
NHPI, SSWANA(MESA) and AIAN are alone or in combination with Latinx.';

COMMENT ON COLUMN report_hitrates_race_person.race IS 'Perceived race of person stopped as indicated by officer';
COMMENT ON COLUMN report_hitrates_race_person.searches_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search';
COMMENT ON COLUMN report_hitrates_race_person.no_contraband_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search and no contraband found';
COMMENT ON COLUMN report_hitrates_race_person.no_contraband_rate IS 'no hit rate for stops that resulted in a search';
COMMENT ON COLUMN report_hitrates_race_person.level IS 'Level of analysis: SDPD';
")

# send table comment + column metadata
#dbSendQuery(con, table_comment)




