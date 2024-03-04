## ANALYSIS: Analyze what are most common reasons for arrests without warrant by race for arrests without warrant 
# made by gang enforcement assignment

# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# import data

stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
stops_re <- dbGetQuery(con, "SELECT * FROM data.rel_stopresult")
race <- dbGetQuery(con, "SELECT * FROM data.rel_races_recode")
result<-dbGetQuery(con, "SELECT * FROM data.rel_persons_result")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis
# filter for assignment == gang enforcement for this analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0 & assignment=="Gang enforcement")

# now join other needed tables to stops table

person_stops<-stops%>%
  left_join(result)%>%
  left_join(race)%>%
  filter(result_simple=="Custodial Arrest without warrant")%>%
  left_join(stops_re)


# Analysis------------------------

# filter out only stops that resulted in 'arrest without warrant' and aggregate over-all (not by race)-----------------------

df_tot<-person_stops%>%
  filter(result=="Custodial Arrest without warrant")%>%
  mutate(total=n())%>%
  group_by(resulttext)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(level="Gang Enforcement Assignment",
         nh_race="total")%>%
  select(level, nh_race, result, resulttext, total, count, rate)%>%
  arrange(nh_race, -rate)%>%
  rename("race"="nh_race")%>%
  slice(1)


# filter out only stops that resulted in 'arrest without warrant' and aggregate stop reason by race for NH groups only

df<-person_stops%>%
  filter(result=="Custodial Arrest without warrant")%>%
  group_by(nh_race)%>%
  mutate(total=n())%>%
  group_by(nh_race, resulttext)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  mutate(level="Gang Enforcement Assignment")%>%
  select(level, nh_race, result, resulttext, total, count, rate)%>%
  arrange(nh_race, -rate)%>%
  rename("race"="nh_race")%>%
  slice(1)

# filter out only stops that resulted in 'arrest without warrant' and aggregate stop reason by race for SSWANA/AIAN/NHPI groups only

df_other<-person_stops%>%
  filter(result=="Custodial Arrest without warrant")%>%
  select(result, resulttext, aian_flag, sswana_flag, nhpi_flag) %>% 
  pivot_longer(cols=3:5, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  filter(count==1)%>% #only keep where the flag ==1 because those are people who are aian/sswana/nhpi
  group_by(race)%>%
  mutate(total=n())%>%
  group_by(race, resulttext)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  mutate(level="Gang Enforcement Assignment")%>%
  select(level, race, result, resulttext, total, count, rate)%>%
  arrange(race, -rate)%>%
  slice(1)

# Combine tables and push to postgres-----------------------

df<-rbind(df_tot, df, df_other)%>%
  arrange(race,-rate)

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3,4), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "stoprates_arrest_gang_race_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE stoprates_arrest_gang_race_person  IS 'Rate of stops that resulted in 
Arrest without warrant by specific 
result reasons (resulttext) overall and by race for officer-initiated stops made by officers assigned to gang enforcement in 2022.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\stoprates_arrest_gang_race_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_stoprates_arrest_gang_race_person.docx';

COMMENT ON COLUMN stoprates_arrest_gang_race_person.level IS 'Level of analysis';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.race IS 'Race. All racial groups are exclusive of Latinx except for AIAN which is alone or in combination with latinx. Note that SSWANA/NHPI is missing from this table because there were no stops that resulted in an arrest by an officer assigned to gange enforcement for those groups.';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.resulttext IS 'More detailed reason behind stop. For this analysis, gives detail for why the the result was an arrest without a warrant';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.result IS 'Result of stop. For this analysis results are filtered to only arrest without warrant';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.total IS 'Total people stopped for officer initiated stops by officers assigned to gang enforcement that resulted in a arrest without a warrant';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.count IS 'Total number of people stopped for officer initiated stops by officers assigned to gang enforcement that resulted in specific arrest reasons';
COMMENT ON COLUMN stoprates_arrest_gang_race_person.rate IS 'Percent of people stopped that resulted with an arrest without a warrant by race and specific arrest reasons';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
