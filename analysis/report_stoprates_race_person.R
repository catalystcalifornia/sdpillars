# Task: Calculating stop rates by race in city-wide SDPD ----------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
source("W:\\RDA Team\\R\\credentials_source.R")
conn <- connect_to_db("rjs_pillars")

# import data

rel_races_recode <- dbGetQuery(conn, "SELECT * FROM data.rel_races_recode")

rel_stops_recode <- dbGetQuery(conn, "SELECT * FROM data.rel_stops_recode")

rel_stops <- dbGetQuery(conn, "SELECT * FROM data.rel_stops")

rel_persons <- dbGetQuery(conn, "SELECT * FROM data.rel_persons")

population_race_sd_city <- dbGetQuery(conn, "SELECT * FROM data.population_race_sd_city") %>% select(-rate)

# Data Cleaning/Merging ---------------------------------------------------

## filter for call for service stops
cfs <- rel_stops %>% select(stop_id, stop_in_response_to_cfs) %>% left_join(rel_stops_recode %>% select(stop_id, stop_year))

## merge persons with race rel tbales to get persons_race
rel_persons_race <- left_join(rel_persons, rel_races_recode)

## merge persons_race with cfs,  filter for officer-initiated stops ONLY
rel_persons_race_cfs <- left_join(rel_persons_race, cfs) %>% filter(stop_in_response_to_cfs == "0" & stop_year == "2022")

# Officer-initiated Stops for each race group: NH-White, NH-Black, NH-Asian, Latinx, --------

## nh white, nh black, nh asian, latinx, nh two or more
nh_race_stops = rel_persons_race_cfs %>% group_by(nh_race) %>% summarize(stop_count = n()) %>% rename(race = nh_race)

# Officer-initiated Stops for AIAN, NHPI, and MENA, Latinx-inclusive

## ALL SSWANA
sswana_stops =  rel_persons_race_cfs %>% filter(sswana_label == "sswana") %>% group_by(sswana_label) %>% summarize(stop_count = n()) %>% rename(race = sswana_label)

## ALL AIAN
aian_stops =  rel_persons_race_cfs %>% filter(aian_label == "aian") %>% group_by(aian_label) %>% summarize(stop_count = n()) %>% rename(race = aian_label)

##ALL NHPI
nhpi_stops =  rel_persons_race_cfs %>% filter(nhpi_label == "nhpi") %>% group_by(nhpi_label) %>% summarize(stop_count = n()) %>% rename(race = nhpi_label)

## Total Stops
stop_count = rel_persons_race_cfs %>% mutate(total_label = "total") %>% group_by(total_label) %>% summarize(stop_count = n()) %>% rename(race = total_label)

## bind everything together
race_stops <- bind_rows(nh_race_stops, sswana_stops, aian_stops, nhpi_stops, stop_count)



# merge population with race stops count and get stop rates per 1,000 and % of out of total count


# merge with population table

stop_total <-race_stops %>%filter(race=='total')%>%mutate(stop_total_count =stop_count)%>%select(stop_total_count)

df_all_stops <- left_join(population_race_sd_city, race_stops) %>% cbind(stop_total) %>%  mutate(stop_per1k_rate = (stop_count/count*1000),
                                                                                                 stop_prc_rate = (stop_count/stop_total_count * 100)) %>% rename(pop_count = count, pop_moe = moe) %>% arrange(desc(stop_per1k_rate)) %>% select(-geoid, -name)

# filter for race groups we want
df_final <- df_all_stops %>% filter(race %in% c("nh_white", "nh_black", "nh_asian", "nh_twoormor", "latinx", "aian", "nhpi", "sswana", "total", "nh_nhpi", "nh_aian"))

# create level: all of SDPD

df_final <- df_final %>% mutate(level = "SDPD")

##### Export Data #####

#dbWriteTable(conn,  "report_stoprates_race_person", df_final, 
#             overwrite = TRUE, row.names = FALSE)

#write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE report_stoprates_race_person  IS 'Number of stops per 1K of same race and % of officer initiated stops out of ALL; includes all officer-initiated stops in 2022 for all of SDPD.
R script used to calculate rates and import table: W:/Project/RJS/Pillars/R/analysis/report_stoprates_race_person.R

QA document: 
W:/Project/RJS/Pillars/Documentation/QA_stoprates_race_person.docx
NHPI, SSWANA(MESA) and AIAN are alone or in combination with Latinx.';

COMMENT ON COLUMN report_stoprates_race_person.race IS 'Perceived race of person stopped as indicated by officer';
COMMENT ON COLUMN report_stoprates_race_person.pop_count IS 'Estimated count of racial group or total based on city level ACS data';
COMMENT ON COLUMN report_stoprates_race_person.pop_moe IS 'Margin of Error of estimated racial group count or total';
COMMENT ON COLUMN report_stoprates_race_person.stop_count IS 'Officer-initiated stops per racial group or total based on RIPA data';
COMMENT ON COLUMN report_stoprates_race_person.stop_total_count IS 'Total Officer-initiated stops based on RIPA data';
COMMENT ON COLUMN report_stoprates_race_person.stop_per1K_rate IS 'Number of officer-initiated stops per 1K of same race population';
COMMENT ON COLUMN report_stoprates_race_person.stop_prc_rate IS '% of officer-initiated stops out of all officer initiated stops';
COMMENT ON COLUMN report_stoprates_race_person.level IS 'all of SDPD';
")

# send table comment + column metadata
#dbSendQuery(conn = conn, table_comment)

