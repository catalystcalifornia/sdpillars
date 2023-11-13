# Analysis: Calculate/visualize the percentage of officer-initiated stops that result in no action or warning by race.

# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# import data

race <- dbGetQuery(con, "SELECT * FROM data.rel_races_recode")
stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
person <- dbGetQuery(con, "SELECT * FROM data.rel_persons")
result<-dbGetQuery(con, "SELECT * FROM rel_persons_result")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

# now join person level data to stops table

person_stops<-stops%>%
  left_join(result)%>%
  left_join(race)

# Aggregate and analyze: NH race------------------------

# denominator: total officer-initiated stops within each racial group

df_nh<-person_stops%>%
  group_by(nh_race)%>%
  mutate(total=n())%>%
  group_by(nh_race, result_simple)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  select(nh_race, result_simple, total, count, rate)%>%
 filter(!is.na(nh_race))%>% # filter out NA race because these are people with 7 perceived number of races
# filter(nh_race!="nh_sswana" & nh_race!="nh_aian" & nh_race!="nh_nhpi")%>%
  rename("race"="nh_race")

# Aggregate and analyze: AIAN/SSWANA/NHPI race------------------------

df_other<-person_stops%>%
  select(result_simple, aian_flag, sswana_flag, nhpi_flag) %>% 
  pivot_longer(cols=2:4, names_to="race", values_to="count")%>%
  filter(count==1)%>% 
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  group_by(race)%>%
  mutate(total=n())%>%
  group_by(result_simple, race)%>%
  mutate(count=sum(count),
         rate=count/total*100)%>%
  slice(1)%>%
  select( race, result_simple, total, count, rate)

# Final combine and push to postgres-------------

df<-rbind(df_nh, df_other)%>%
  mutate(level='SDPD')%>%
  arrange(race, -rate)%>%
  select( race, level, result_simple, total, count, rate)

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_result_race_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_result_race_person  IS '% of stops by stop result (alone not in combination with any other result)
and race (SDPD-wide) in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_result_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_result_race_person.docx';

COMMENT ON COLUMN report_stoprates_result_race_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_stoprates_result_race_person.level IS 'Level of analysis';
COMMENT ON COLUMN report_stoprates_result_race_person.result_simple IS 'Stop result (alone not in combination with anything else)';
COMMENT ON COLUMN report_stoprates_result_race_person.total IS 'Total officer initiated stops within that racial group. Denominator for rate calc.';
COMMENT ON COLUMN report_stoprates_result_race_person.count IS 'Count of people stopped in each stop result. Numerator for rate calculation.';
COMMENT ON COLUMN report_stoprates_result_race_person.rate IS 'Rate of people stopped within each racial group by stop result';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
