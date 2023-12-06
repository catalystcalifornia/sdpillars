# Task: % of FI card stops where a search took place that resulted in no contraband by race

## Packages ##
library(tidyverse)
library(RPostgreSQL)

## data setup ##
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

## Step 1: Filter for officer-initiated stops, year = 2022, where a search took place and stop resulted in Identification Card

## Step 2: Of these stops, group the total count by all of SDPD, divide by total population and calculate rate 

## Step 3: Of the stops in step 1, filter for contraband = 0, group total count by all of SDPD

# Import data that we need
rel_persons_fi_card <- dbGetQuery(con, "SELECT * FROM rel_persons_recode") %>% select(stop_id, person_id, stop_year, fi_card_flag)
rel_races_recode <- dbGetQuery(con, "SELECT * FROM rel_races_recode") %>% select(stop_id, person_id, nh_race, sswana_flag, aian_flag, nhpi_flag)
rel_stops_cfs <- dbGetQuery(con, "SELECT * FROM rel_stops") %>% select(stop_id, stop_in_response_to_cfs)
rel_persons_searches_contraband <- dbGetQuery(con, "SELECT * FROM rel_persons_searches") %>% select(stop_id, search, contraband)

## merge relevant df's and filter for 2022 officer-initiated stops where the stop resulted in FI card
rel_stops_fi_card_cfs_person  <- rel_persons_fi_card %>% left_join(rel_stops_cfs) %>% filter(stop_year == "2022" & stop_in_response_to_cfs == "0" & fi_card_flag == "1")

## merge with races
rel_stops_fi_card_cfs_person_race <- rel_stops_fi_card_cfs_person  %>% left_join(rel_races_recode)

## merge filtered 2022 Officer-initiated stops with relational df that has searches and contraband. This now has our unit of analysis: officer-initiated stops in 2022 that resulted in a search and FI card completed

df <- rel_stops_fi_card_cfs_person_race %>% left_join(rel_persons_searches_contraband) %>% filter(search == "1")


# total officer-initiated stops where a search was conducted by race --------

df_total <-  df %>% mutate(race = "total") %>% group_by(race) %>% summarize(stop_count = n()) 

df_stops_nh_race <- df%>% group_by(nh_race) %>% summarize(stop_count = n()) %>% rename(race = nh_race) %>% filter(!is.na(race)) # filter out 1 obs where no race is coded

df_sswana_race <- df %>% filter(sswana_flag == "1") %>% group_by(sswana_flag) %>% summarize(stop_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
                                                                                                                                                                            "1" = "sswana"))

df_aian_race <- df %>% filter(aian_flag == "1") %>% group_by(aian_flag) %>% summarize(stop_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
                                                                                                                                                                    "1" = "aian"))

df_nhpi_race <- df %>% filter(nhpi_flag == "1") %>% group_by(nhpi_flag) %>% summarize(stop_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
                                                                                                                                                                    "1" = "nhpi"))

## combine all searched FI card stops
df_stops_race <- rbind(df_total, df_stops_nh_race, df_sswana_race, df_aian_race,df_nhpi_race) 

# total officer-initiated stops where a search was conducted, field interview card was completed, and no contraband was found by race --------

df_total_contraband <-  df %>% mutate(race = "total") %>% filter(contraband == "0") %>%  group_by(race) %>% summarize(no_contraband_count = n()) 

df_stops_nh_race_contraband <- df %>% filter(contraband == "0") %>% group_by(nh_race) %>% summarize(no_contraband_count = n()) %>% rename(race = nh_race) %>% filter(!is.na(race)) # filter out 1 obs where no race is coded

df_sswana_race_contraband <- df %>% filter(sswana_flag == "1" & contraband == "0") %>% group_by(sswana_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = sswana_flag) %>% mutate(race = recode(race,
                                                                                                                                                                      "1" = "sswana"))
df_aian_race_contraband <- df %>% filter(aian_flag == "1" & contraband == "0") %>% group_by(aian_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = aian_flag) %>% mutate(race = recode(race,
                                                                                                                                                              "1" = "aian"))
df_nhpi_race_contraband <- df %>% filter(nhpi_flag == "1" & contraband == "0") %>% group_by(nhpi_flag) %>% summarize(no_contraband_count = n()) %>% rename(race = nhpi_flag) %>% mutate(race = recode(race,
                                                                                                                                                              "1" = "nhpi"))


# combine all searched FI card stops -------------------------------------


df_stops_race_contraband <- rbind(df_total_contraband, df_stops_nh_race_contraband, df_sswana_race_contraband, df_aian_race_contraband,df_nhpi_race_contraband) 


# merge with contraband, then calculate rate
df_rate <- df_stops_race %>% left_join(df_stops_race_contraband) %>% mutate(no_contraband_prc_rate = no_contraband_count/stop_count * 100)


# filter for race groups we want
df_final <- df_rate %>% filter(race %in% c("nh_white", "nh_black", "nh_asian", "nh_twoormor", "latinx", "aian", "nhpi", "sswana", "total", "nh_nhpi", "nh_aian")) %>% arrange(no_contraband_prc_rate)

# create level: FI Card

df_final <- df_final %>% mutate(level = "Field Interview Card Completed")


# Export ------------------------------------------------------------------

#dbWriteTable(con,  "report_hitrates_race_ficard_person", df_final, 
#            overwrite = TRUE, row.names = FALSE)

#write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE report_hitrates_race_ficard_person  IS 'No discovery of contraband by race for all officer-initiated stops that resulted in a search in 2022 and field interview card completed. 
R script used to calculate rates and import table: W:/Project/RJS/Pillars/R/analysis/report_hitrates_race_ficard_person.R

QA document: 
W:/Project/RJS/Pillars/Documentation/QA_report_hitrates_race_ficard_person.docx
NHPI, SSWANA(MESA) and AIAN are alone or in combination with Latinx.';

COMMENT ON COLUMN report_hitrates_race_ficard_person.race IS 'Perceived race of person stopped as indicated by officer';
COMMENT ON COLUMN report_hitrates_race_ficard_person.stop_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search and field interview card completed';
COMMENT ON COLUMN report_hitrates_race_ficard_person.no_contraband_count IS 'Officer-initiated stops per racial group or total based on RIPA data that resulted in a search, field interview card completed, and no contraband was found';
COMMENT ON COLUMN report_hitrates_race_ficard_person.no_contraband_prc_rate IS 'no hit rate for stops that resulted in a search and field interview card completed';
COMMENT ON COLUMN report_hitrates_race_ficard_person.level IS 'Level of analysis: Field Interview Card Completed';
")

# send table comment + column metadata
#dbSendQuery(con, table_comment)




