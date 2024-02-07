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


# Analyze: Time spent on FI result BY RACE: NH -----------------------

# denominator: total amount of time spent on officer-initiated stops resulting in a FI card in each SDPD division

df_nh<-person_stops%>%
  filter(stop_result_simple=="Field interview card completed")%>%
   group_by(div_name)%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(div_name, stop_nh_race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  filter(beat!='999')%>%
  select(div_name, div, stop_nh_race, stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  arrange(-duration_rate)%>%
  rename("race"="stop_nh_race")%>%
  filter(!is.na(race))%>% # filter out NA race these are people with 7+ perceived number of races
arrange(div, -duration_rate)

# Analyze: Time spent on FI card result BY RACE: AIAN/SSWANA/NHPI----------------

df_other<-person_stops%>%
  filter(beat!='999')%>%
  filter(stop_result_simple=="Field interview card completed")%>%
  select(div_name, div, stop_result_simple, aian_flag, sswana_flag, nhpi_flag, stop_duration_capped) %>% 
  pivot_longer(cols=4:6, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  group_by(div_name)%>%
 left_join(df_nh%>%ungroup()%>%distinct(div, div_name, duration_total)%>%select(div, div_name, duration_total))%>%
    group_by(div_name, race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  slice(1)%>%
  select(div_name, div, race, stop_result_simple, duration_total, duration_count, duration_rate)

# Final combine of all tables--------------------

df<-rbind(df_nh, df_other)%>%
  arrange(div, -duration_rate)%>%
  mutate(level="Field Interview Card Completed")%>%
  select(div_name, div, race, level, stop_result_simple, duration_total, duration_count, duration_rate)


# Join population data to final table ------------

df<-df%>%
  left_join(pop)%>%
  select(-div_num)%>%
  rename("pop_count"="count",
         "pop_rate"="rate")%>%
  mutate(pop_rate=pop_rate*100)

# Push to postgres ------------------------

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3,4,5), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_div_ficard_race_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_div_ficard_race_person  IS '% of time spent on FI card stops in each SDPD division by race out of all 
time spent on FI card stops in each SDPD division. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_div_ficard_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_ficard_race_stop.docx';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.level IS 'Level of analysis.';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.div_name IS 'Division name.';

COMMENT ON COLUMN report_timespent_div_ficard_race_person.div IS 'Division number.';

COMMENT ON COLUMN report_timespent_div_ficard_race_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.stop_result_simple IS 'Recoded stop results. Shows stops where this was the ONLY result unless there were two or more which is grouped together';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.duration_total IS 'Total amount of time spent on FI card stops by division. These are stops that result only in a FI card. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.duration_count IS 'Total amount of time spent on FI card stops (alone not in combination) within each racial group in each SDPD division. Numerator for rate calculation.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.duration_rate IS 'Percent time spent on FI card stops by race and SDPD division';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.pop_count IS 'Total population for each racial group';
COMMENT ON COLUMN report_timespent_div_ficard_race_person.pop_rate IS 'Percent of each racial group out of total population';



")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
