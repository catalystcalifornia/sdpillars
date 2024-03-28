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

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

# now join other needed tables to stops table

person_stops<-stops%>%
  left_join(stops_re)%>%
   left_join(result)%>%
  left_join(race)%>%
  left_join(duration, by=c("stop_id"="stop_id"))


# Analyze: Time spent on FI result OVERALL -----------------------

df_overall<-person_stops%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(stop_result_simple)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  select(stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  mutate(race="total")%>%
  select(race, stop_result_simple, duration_total, duration_count, duration_rate)

# Analyze: Time spent on FI result BY RACE: NH -----------------------

# denominator: total amount of time spent on officer-initiated stops within each racial group

df_nh<-person_stops%>%
   group_by(stop_nh_race)%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(stop_result_simple, stop_nh_race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  select(stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  # filter(!grepl("nh_aian|nh_sswana|nh_sswana", stop_nh_race))%>%
  arrange(-duration_rate)%>%
  rename("race"="stop_nh_race")%>%
  filter(!is.na(race)) # filter out NA race these are people with 7+ perceived number of races

# Analyze: Time spent on FI card result BY RACE: AIAN/SSWANA/NHPI----------------

df_other<-person_stops%>%
  select(stop_result_simple, aian_flag, sswana_flag, nhpi_flag, stop_duration_capped) %>% 
  pivot_longer(cols=2:4, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  group_by(race)%>%
    mutate(duration_total=sum(stop_duration_capped))%>%
    group_by(race, stop_result_simple)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  slice(1)%>%
  select(race, stop_result_simple, duration_total, duration_count, duration_rate)

# Final combine and push to postgres --------------------

df<-rbind(df_overall, df_nh, df_other)%>%
  arrange(race, -duration_rate)%>%
  mutate(level="SDPD")%>%
  select(race, level, stop_result_simple, duration_total, duration_count, duration_rate)

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_result_race_stop", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_result_race_stop  IS '% of time spent on FI card stops out of all 
officer-initiated stops for SDPD overall and by race in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_ficard_race_stop.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_ficard_race_stop.docx';
COMMENT ON COLUMN report_timespent_result_race_stop.level IS 'Level of analysis.';
COMMENT ON COLUMN report_timespent_result_race_stop.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_timespent_result_race_stop.stop_result_simple IS 'Recoded stop results. Shows stops where this was the ONLY result unless there were two or more which is grouped together';
COMMENT ON COLUMN report_timespent_result_race_stop.duration_total IS 'Total amount of time spent on officer initiated stops for each racial group. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_result_race_stop.duration_count IS 'Total amount of time spent on officer initiated stops by result (alone not in combination) within each racial group. Numerator for rate calculation.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_result_race_stop.duration_rate IS 'Percent time spent on stops by result and by race';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)

# ADDITIONAL POSTGRES TABLE: FIcard stops by race ---------------------
# using all officer-initiated stops resulting in a FIcard as denominator----------------------

# NH 

df_nh<-person_stops%>%
  filter(fi_card_flag==1)%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(stop_nh_race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  select(stop_result_simple, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  arrange(-duration_rate)%>%
  rename("race"="stop_nh_race")%>%
  filter(!is.na(race))%>% # filter out NA race these are people with 7+ perceived number of races
  select(race, duration_total, duration_count, duration_rate)

#  AIAN/SSWANA/NHPI----------------

fi_total<-df_nh%>%ungroup()%>%select(duration_total)%>%distinct()


df_other<-person_stops%>%
  select(fi_card_flag, aian_flag, sswana_flag, nhpi_flag, stop_duration_capped) %>% 
  filter(fi_card_flag==1)%>%
  pivot_longer(cols=2:4, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  cbind(fi_total)%>%
  group_by(race)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  slice(1)%>%
  select(race, duration_total, duration_count, duration_rate)


# Final combine and push to postgres --------------------

df<-rbind(df_nh, df_other)%>%
  ungroup()%>%
  mutate(level="Field interview card completed")%>%
  select(race, level, duration_total, duration_count, duration_rate)%>%
  arrange(-duration_rate)

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_ficard_race_stop", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_ficard_race_stop  IS '% of time spent on FI card stops by race out of all 
officer-initiated stops that resulted in a FI Card in 2022. The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_ficard_race_stop.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_ficard_race_stop.docx';
COMMENT ON COLUMN report_timespent_ficard_race_stop.level IS 'Level of analysis.';
COMMENT ON COLUMN report_timespent_ficard_race_stop.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_timespent_ficard_race_stop.duration_total IS 'Total amount of time spent on officer initiated stops that resulted in a FI Card. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_ficard_race_stop.duration_count IS 'Total amount of time spent on officer initiated stops where result was only a field interview card within each racial group. Numerator for rate calculation.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_ficard_race_stop.duration_rate IS 'Percent time spent on officer-initiated stops resulting only in a field interview card by race';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
