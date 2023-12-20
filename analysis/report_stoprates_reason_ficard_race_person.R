## Analysis: Looking at all the stop reasons that resulted in an FI card only --overall, and by race. This would be at the person level?

# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# import data

stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
stops_re <- dbGetQuery(con, "SELECT * FROM data.rel_stops_recode")
reason<-dbGetQuery(con, "SELECT * FROM rel_persons_reason")
race <- dbGetQuery(con, "SELECT * FROM data.rel_races_recode")


# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

# now join person level data and stop duration data to stops table, and filter for assignment = 'gang enforcement'

person_stops<-stops%>%
  left_join(stops_re)%>%
  left_join(reason)%>%
  left_join(race)

# Stop reasons resulting in FI card OVERALL-----------------------

# Denominator: all officer-initiated stops resulting in only an FI card

df<-person_stops%>%
  filter(fi_card_flag==1)%>%
mutate(total=n())%>%
  group_by(reason_simple)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  mutate(level="Field interview card completed")%>%
  arrange(-rate)%>%
  mutate(race="total")%>%
  select(race, level,reason_simple,  total, count,rate)
  

# Stop reasons resulting in FI card BY RACE------------------------

# NH 

df_nh<-person_stops%>%
  filter(fi_card_flag==1)%>%
  group_by(nh_race)%>%
  mutate(total=n())%>%
  group_by(nh_race,reason_simple)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  mutate(level="Field interview card completed")%>%
  select(nh_race, level,reason_simple,  total, count,rate)%>%
rename("race"="nh_race")%>%
  arrange(race, -rate)

# AIAN/SSWANA/NHPI


df_other<-person_stops%>%
  select(reason_simple, fi_card_flag, aian_flag, sswana_flag, nhpi_flag) %>% 
  filter(fi_card_flag==1)%>%
  pivot_longer(cols=3:5, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  group_by(race)%>%
  mutate(total=n())%>%
  group_by(race, reason_simple)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  mutate(level="Field interview card completed")%>%
  select(race, level,reason_simple,  total, count,rate)


# Final combine push to postgres --------------------

df<-rbind(df, df_nh, df_other)%>%
  arrange(race,-rate)

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_reason_ficard_race_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_reason_ficard_race_person  IS 'Rate of stops
that result only in FI card by stop reason out of all officer-initiated stops. 
The stops are officer-initiated stops only for 2022.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_reason_ficard_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_reason_ficard_race_person.docx';

COMMENT ON COLUMN report_stoprates_reason_ficard_race_person.level IS 'Level of analysis';
COMMENT ON COLUMN report_stoprates_reason_ficard_race_person.reason_simple IS 'Simplified reason for the stop by unique person stopped. 
People can only be stopped for 1 unique reason category, but they may be stopped for different rationales within that reason category';
COMMENT ON COLUMN report_stoprates_reason_ficard_race_person.total IS 'Total people stopped for officer initiated stops that resulted in only an FI card for that racial group';
COMMENT ON COLUMN report_stoprates_reason_ficard_race_person.count IS 'Total number of people stopped for officer initiated stops resulting only in an FI card within each racial group and stop reason';
COMMENT ON COLUMN report_stoprates_reason_ficard_race_person.rate IS 'Percent of people stopped for each stop reason that resulted only in a FI card by racial group';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
