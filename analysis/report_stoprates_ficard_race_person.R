# Analysis: Rate of stops per 1K people of same race or per 100 stops for field-interview cards for all of SDPD

# Method 1: # of officer-initiated stops that only result in a FI card per 1K people of the same race
# Method 2: % of stops of each race that only result in FI card out of all officer-initiated stops

# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# Rate of stops per 1K people of same race or per 100 stops for field-interview cards for all of SDPD

# import data

race <- dbGetQuery(con, "SELECT * FROM data.rel_races_recode")
stops_re <- dbGetQuery(con, "SELECT * FROM data.rel_stops_recode")
stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
person <- dbGetQuery(con, "SELECT * FROM rel_persons")
person_re<-dbGetQuery(con, "SELECT * FROM rel_persons_recode")
pop_city<-dbGetQuery(con, "SELECT * FROM population_race_sd_city")%>%
  rename("nh_race"="race")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

#join all needed tables

person_stops<-stops%>%
  left_join(person_re)%>%
  left_join(race)

####Method 1: # of officer-initiated stops that only result in a FI card per 1K people of the same race-------------

# aggregate and analyze: NH only

df1_nh<-person_stops%>%
  group_by(nh_race, fi_card_flag)%>%
  mutate(count=n())%>%
  slice(1)%>%
  filter(fi_card_flag==1)%>%
  filter(!is.na(nh_race))%>%
  mutate(level="Field interview card completed")%>%
  select(nh_race, level, fi_card_flag, count)%>%
  left_join(pop_city, by=c("nh_race"="nh_race"))%>%
  rename("count"="count.x",
         "total"="count.y",
    "race"="nh_race")%>%
  mutate(rate=count/total*1000)%>%
  ungroup()%>%
  select(race, level, total, count, rate)%>%
  filter(!grepl('nh_sswana', race)) # we don't have a nh_sswana population estimate

# Aggregate and analyze for AIAN/NHPI

df1_other<-person_stops%>%
  select(aian_flag, sswana_flag, nhpi_flag, fi_card_flag) %>% 
  pivot_longer(cols=1:3, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  group_by(race)%>%
  filter(count==1)%>%
  group_by(race, fi_card_flag)%>%
  mutate(count=n())%>%
  slice(1)%>%
  filter(fi_card_flag==1)%>%
  mutate(level="Field interview card completed")%>%
  select(race, level, fi_card_flag, count)%>%
  left_join(pop_city, by=c("race"="nh_race"))%>%
  rename("count"="count.x",
         "total"="count.y")%>%
  mutate(rate=count/total*1000)%>%
  ungroup()%>%
  select(race, level,  total, count, rate)
  
# Final combine and push to postgres

df<-rbind(df1_nh, df1_other)

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_ficard_race_pop_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_ficard_race_pop_person  IS 'Rate of stops (rate is per 1k of total population) of each race that only result in FI card out of total city population for each racial group
in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_ficard_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_ficard_race_person.docx';

COMMENT ON COLUMN report_stoprates_ficard_race_pop_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_stoprates_ficard_race_pop_person.total IS 'Total number of people within each racial group in SD city.
Denominator for rate calculation.';
COMMENT ON COLUMN report_stoprates_ficard_race_pop_person.count IS 'Count of people within each racial group stopped for an 
officer initiated stop that resulted in only a field interview card. Numerator for rate calculation.';
COMMENT ON COLUMN report_stoprates_ficard_race_pop_person.rate IS 'Rate per 1k of people stopped for an officer initiated stop resulting in only a field interview card
out of total population (per 1k people) within each racial group';
COMMENT ON COLUMN report_stoprates_ficard_race_pop_person.level IS 'Level of analysis';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)


#### Method 2: % of stops of each race that only result in FI card out of all officer-initiated stops ----------

# NH 

df2_nh<-person_stops%>%
  group_by(nh_race)%>%
  mutate(total=n())%>%
  group_by(nh_race, fi_card_flag)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  filter(fi_card_flag==1)%>%
  filter(!is.na(nh_race))%>%
  mutate(level="Field interview card completed")%>%
  ungroup()%>%
  select(nh_race, level, total, count,rate)%>%
  rename("race"="nh_race")

# AIAN/SSWANA/NHPI

df2_other<-person_stops%>%
  select(aian_flag, sswana_flag, nhpi_flag, fi_card_flag) %>% 
  pivot_longer(cols=1:3, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  group_by(race)%>%
  filter(count==1)%>%
  mutate(total=n())%>%
  group_by(race, fi_card_flag)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  filter(fi_card_flag==1)%>%
  mutate(level="Field interview card completed")%>%
ungroup()%>%
  select(race, level, total, count,rate)

# Final combine and push to postgres

df<-rbind(df2_nh, df2_other)%>%
  arrange(-rate)

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_ficard_race_offit_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_ficard_race_offit_person  IS 'Rate of stops of each race that only result in FI card out of all officer-initiated stops 
in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_ficard_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_ficard_race_person.docx';

COMMENT ON COLUMN report_stoprates_ficard_race_offit_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_stoprates_ficard_race_offit_person.total IS 'Total number of people stopped for an officer initiated stop within each racial group.
Denominator for rate calculation.';
COMMENT ON COLUMN report_stoprates_ficard_race_offit_person.count IS 'Count of people within each racial group stopped for an 
officer initiated stop that resulted in only a field interview card. Numerator for rate calculation.';
COMMENT ON COLUMN report_stoprates_ficard_race_offit_person.rate IS 'Rate of people stopped for an officer initiated stop resulting in only a field interview card
out of total officer initiated stops within each racial group';
COMMENT ON COLUMN report_stoprates_ficard_race_offit_person.level IS 'Level of analysis';

")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
