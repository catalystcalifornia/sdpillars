# Task: Analyzing stop rates for gang enforcement SDPD citywide -----------

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


# Task: Rate of gang enforcement stops by race (per 1K people of same race)

## filter for call for service stops,gang enforcement, and year 2022
cfs_gang <- rel_stops %>% select(stop_id, stop_in_response_to_cfs) %>% left_join(rel_stops_recode %>% select(stop_id, stop_year, unit_flag))


## merge persons with race rel tbales to get persons_race
rel_persons_race <- left_join(rel_persons, rel_races_recode)


## merge persons_race with cfs,  filter for officer-initiated stops ONLY, Gang enforcement stops, year 2022
rel_persons_race_cfs_gang <- left_join(rel_persons_race, cfs_gang) %>% filter(stop_in_response_to_cfs == "0" & unit_flag == "1" & stop_year == "2022")



# Officer-initiated Gang Enforcement Stops for each race group: NH-White, NH-Black, NH-Asian, Latinx, --------

## nh white, nh black, nh asian, latinx, nh two or more
nh_race_gang_stops = rel_persons_race_cfs_gang %>% group_by(nh_race) %>% summarize(stop_count = n()) %>% rename(race = nh_race)

# Officer-initiated Gang Enforcement Stops for AIAN, NHPI, and MENA, Latinx-inclusive

## ALL SSWANA
sswana_gang_stops =  rel_persons_race_cfs_gang %>% filter(sswana_label == "sswana") %>% group_by(sswana_label) %>% summarize(stop_count = n()) %>% rename(race = sswana_label)

## ALL AIAN
aian_gang_stops =  rel_persons_race_cfs_gang %>% filter(aian_label == "aian") %>% group_by(aian_label) %>% summarize(stop_count = n()) %>% rename(race = aian_label)

##ALL NHPI
nhpi_gang_stops =  rel_persons_race_cfs_gang %>% filter(nhpi_label == "nhpi") %>% group_by(nhpi_label) %>% summarize(stop_count = n()) %>% rename(race = nhpi_label)

## Total Stops
total_gang_stops = rel_persons_race_cfs_gang %>% mutate(total_label = "total") %>% group_by(total_label) %>% summarize(stop_count = n()) %>% rename(race = total_label)

## bind everything together
race_gang_stops <- bind_rows(nh_race_gang_stops, sswana_gang_stops, aian_gang_stops, nhpi_gang_stops, total_gang_stops)



# merge with population table

stop_total <-race_gang_stops%>%filter(race=='total')%>%mutate(stop_total_count =stop_count)%>%select(stop_total_count)


# merge with population table and total stop count table
df_gang_stops <- left_join(population_race_sd_city, race_gang_stops) %>%cbind(stop_total)%>% mutate(stop_count = ifelse(is.na(stop_count), 0, stop_count), stop_per1k_rate = (stop_count/count*1000),
                                                                                                    stop_prc_rate = (stop_count/stop_total_count * 100)) %>% rename(pop_count = count, pop_moe = moe) %>%  arrange(desc(stop_per1k_rate)) %>% select(-geoid, -name)

# filter for race groups we want
df_final <- df_gang_stops %>% filter(race %in% c("nh_white", "nh_black", "nh_asian", "nh_twoormor", "latinx", "aian", "nhpi", "sswana", "total", "nh_nhpi", "nh_aian"))

# create level: Gang enforcement

df_final <- df_final %>% mutate(level = "Gang Enforcement Assignment")


##### Export Data #####

#dbWriteTable(conn,  "report_stoprates_race_gang_person", df_final, 
#           overwrite = TRUE, row.names = FALSE)

#write comment to table, and column metadata

table_comment <- paste0("COMMENT ON TABLE report_stoprates_race_gang_person  IS 'Number of gang enforced stops per 1K of same race population and % of all gang enforcement stops; includes gang enforcement officer-initiated stops for 2022 in San Diego city-wide.
R script used to calculate rates and import table: W:/Project/RJS/Pillars/R/analysis/report_stoprates_race_gang_person.R

QA document: 
W:/Project/RJS/Pillars/Documentation/QA_stoprates_race_gang_person.docx
NHPI, SSWANA(MESA) and AIAN are alone or in combination with Latinx.';

COMMENT ON COLUMN report_stoprates_race_gang_person.race IS 'Perceived race of person stopped as indicated by officer';
COMMENT ON COLUMN report_stoprates_race_gang_person.pop_count IS 'Estimated count of racial group or total based on city level ACS data';
COMMENT ON COLUMN report_stoprates_race_gang_person.pop_moe IS 'Margin of Error of estimated racial group count or total';
COMMENT ON COLUMN report_stoprates_race_gang_person.stop_count IS 'Gang enforced officer-initiated stops per racial group or total based on RIPA data';
COMMENT ON COLUMN report_stoprates_race_gang_person.stop_total_count IS 'Total gang enforced officer-initiated stops based on RIPA data';
COMMENT ON COLUMN report_stoprates_race_gang_person.stop_per1K_rate IS 'Number of gang enforced officer-initiated stops per 1K of same race population';
COMMENT ON COLUMN report_stoprates_race_gang_person.stop_prc_rate IS '% of gang enforced officer-initiated stops out of all gang enforced stops. Uses same denominator of 484 for all rates';   COMMENT ON COLUMN report_stoprates_race_gang_person.level IS 'Gang Enforcement Assignment';                   
                        ")

# send table comment + column metadata
#dbSendQuery(conn = conn, table_comment)
<<<<<<< HEAD

=======
>>>>>>> 46241b6962ad2474c1412b76e7354a691fc4b8d7
