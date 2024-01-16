# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# Rate of stops resulting only in FI cards by race for each SDPD division out of total officer initiated stops for each racial group within each SDPD division

# import data

race <- dbGetQuery(con, "SELECT * FROM data.rel_races_recode")
stops_re <- dbGetQuery(con, "SELECT * FROM data.rel_stops_recode")
stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
person <- dbGetQuery(con, "SELECT * FROM rel_persons")
person_re<-dbGetQuery(con, "SELECT * FROM rel_persons_recode")

beat_div_xwalk<-dbGetQuery(con, "SELECT * FROM sdpd_beat_div_xwalk")%>%
  filter(beat!=999)%>%
  group_by(beat, beat_name)%>%
  slice(1)

div<-dbGetQuery(con, "SELECT * FROM sangis_sdpd_divisions_2023")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

#join all needed tables

person_stops<-stops%>%
  left_join(person_re)%>%
  left_join(race)%>%
  left_join(beat_div_xwalk)

# calculate counts and rates for total division (not by race)

df_tot<-person_stops%>%
  group_by(div, div_name)%>%
  mutate(total=n())%>%
  group_by(div, div_name, fi_card_flag)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  filter(fi_card_flag==1)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(level="Field interview card completed",
         race="total")%>%
  select(div, div_name, level, race, total, count, rate)%>%
  filter(!is.na(div))

#aggregate and analyze : NH ONLY

df_nh<-person_stops%>%
  group_by(div, div_name, nh_race)%>%
  mutate(total=n())%>%
  group_by(div, div_name, nh_race, fi_card_flag)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  filter(fi_card_flag==1)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(level="Field interview card completed")%>%
  select(div, div_name, level, nh_race, total, count, rate)%>%
  filter(!is.na(div))%>%
  rename("race"="nh_race")

# aggregate and analyze: AIAN/SSWANA/NHPI

df_other<-person_stops%>%
  select(aian_flag, sswana_flag, nhpi_flag, fi_card_flag, div, div_name) %>% 
  pivot_longer(cols=1:3, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  group_by(div, div_name, race)%>%
  filter(count==1)%>%
  mutate(total=n())%>%
  group_by(div, div_name, race, fi_card_flag)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  filter(fi_card_flag==1)%>%
  mutate(level="Field interview card completed")%>%
  ungroup()%>%
  select(div, div_name, level, race, total, count, rate)%>%
  filter(!is.na(div))

# final combine and push to postgres

df<-rbind(df_tot, df_nh, df_other)%>%
  arrange(div)

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3,4), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_div_ficard_race_person", df,
            overwrite = TRUE, row.names = FALSE,
            field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_div_ficard_race_person  IS '% of stops of each race by stop result, but only when the stop result was
a FI card only out of total officer initiated stops for each racial group in each SDPD division
in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_div_ficard_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_ficard_race_person.docx';

COMMENT ON COLUMN report_stoprates_div_ficard_race_person.div IS 'SDPD division number';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.div_name IS 'SDPD division name';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.total IS 'Total number of officer initiated stops made for each racial group by SDPD division.
Denominator for rate calculation.';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.count IS 'Count of people within each racial group stopped for an 
officer initiated stop that resulted in only a field interview card. Numerator for rate calculation.';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.rate IS '% of people stopped for an officer initiated stop 
resulting in only a field interview card by race and SDPD division out of total officer-initiated stops within each racial group by SDPD division';
COMMENT ON COLUMN report_stoprates_div_ficard_race_person.level IS 'Level of analysis';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
