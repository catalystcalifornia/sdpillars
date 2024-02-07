## ANALYSIS: Calculating time SDPD officers spent on FI cards and time spent by race

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
duration<- dbGetQuery(con, "SELECT * FROM data.rel_stop_duration_capped")
race <- dbGetQuery(con, "SELECT * FROM data.rel_stops_race")
result<-dbGetQuery(con, "SELECT * FROM data.rel_stops_result")
pop<-dbGetQuery(con, "SELECT * FROM population_race_sdpd")

beat_div_xwalk<-dbGetQuery(con, "SELECT * FROM sdpd_beat_div_xwalk")%>%
  filter(beat!=999)

div<-dbGetQuery(con, "SELECT * FROM sangis_sdpd_divisions_2023")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

# now join other needed tables to stops table

person_stops<-stops%>%
  left_join(stops_re)%>%
   left_join(result)%>%
  left_join(race)%>%
  left_join(duration, by=c("stop_id"="stop_id"))%>%
  left_join(beat_div_xwalk)

# Analyze: Time spent on FI result OVERALL -----------------------

df_overall<-person_stops%>%
  group_by(div_name)%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(div_name, stop_result_simple)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  filter(beat!='999')%>%
  select(div, div_name, stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  mutate(race="total")%>%
  select(div_name, div, race, stop_result_simple, duration_total, duration_count, duration_rate)%>%
  arrange(div, race, -duration_rate)

# Analyze: Time spent on FI result BY RACE: NH -----------------------

# denominator: total amount of time spent on officer-initiated stops within each racial group

df_nh<-person_stops%>%
   group_by(div_name, stop_nh_race)%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(div_name, stop_result_simple, stop_nh_race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  filter(beat!='999')%>%
  select(div_name, div, stop_nh_race, stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  # filter(!grepl("nh_aian|nh_sswana|nh_sswana", stop_nh_race))%>%
  arrange(-duration_rate)%>%
  rename("race"="stop_nh_race")%>%
  filter(!is.na(race))%>% # filter out NA race these are people with 7+ perceived number of races
arrange(div, race, -duration_rate)

# Analyze: Time spent on FI card result BY RACE: AIAN/SSWANA/NHPI----------------

df_other<-person_stops%>%
  filter(beat!='999')%>%
  select(div_name, div, stop_result_simple, aian_flag, sswana_flag, nhpi_flag, stop_duration_capped) %>% 
  pivot_longer(cols=4:6, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  group_by(div_name, race)%>%
    mutate(duration_total=sum(stop_duration_capped))%>%
    group_by(div_name, race, stop_result_simple)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  slice(1)%>%
  select(div_name, div, race, stop_result_simple, duration_total, duration_count, duration_rate)

# Final combine of all tables--------------------

df<-rbind(df_overall, df_nh, df_other)%>%
  arrange(div, race, -duration_rate)%>%
  mutate(level="SDPD")%>%
  select(div_name, div, race, level, stop_result_simple, duration_total, duration_count, duration_rate)


# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3,4,5), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_div_result_race_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_div_result_race_person  IS '% of time spent on each stop result for each
race by SDPD division out of total time spent on all officer-initiated stops for SDPD overall by division and by race in 2022. 
The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_div_result_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_ficard_race_stop.docx';
COMMENT ON COLUMN report_timespent_div_result_race_person.level IS 'Level of analysis.';
COMMENT ON COLUMN report_timespent_div_result_race_person.div_name IS 'Division name.';

COMMENT ON COLUMN report_timespent_div_result_race_person.div IS 'Division number.';

COMMENT ON COLUMN report_timespent_div_result_race_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_timespent_div_result_race_person.stop_result_simple IS 'Recoded stop results. Shows stops where this was the ONLY result unless there were two or more which is grouped together';
COMMENT ON COLUMN report_timespent_div_result_race_person.duration_total IS 'Total amount of time spent on officer initiated stops for each racial group and by division. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_div_result_race_person.duration_count IS 'Total amount of time spent on officer initiated stops by result (alone not in combination) within each racial group and by division. Numerator for rate calculation.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_div_result_race_person.duration_rate IS 'Percent time spent on stops by result and by race';



")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
