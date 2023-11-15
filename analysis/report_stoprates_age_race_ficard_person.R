### Analysis: Race distribution of field interview card stops by age group

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
person_re <- dbGetQuery(con, "SELECT * FROM rel_persons_recode")
person <- dbGetQuery(con, "SELECT * FROM data.rel_persons")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)


# Analyze ------------------------

# Prep age data

person_age<-stops%>%
  left_join(person)%>%
  select(stop_id, person_id, Year, assignment, perceived_age)%>%
  mutate(age_bracket=ifelse(perceived_age <18, '17 and under',
                            ifelse(perceived_age <=24 & perceived_age>=18, '18-24',
                                   ifelse(perceived_age <=34 & perceived_age>24, '25-34',
                                          ifelse(perceived_age <=44 & perceived_age>34, '35-44',
                                                 ifelse(perceived_age <=54 & perceived_age>44, '45-54',
                                                        ifelse(perceived_age <=64 & perceived_age>54, '55-64',
                                                               ifelse(perceived_age >=65, '65 and older', 'Blank'))))))))
# Join race & result data & filter for "field interview card" as ONLY result of stop

person_age_result<-person_age%>%
  left_join(person_re)%>%
  left_join(race)%>%
  filter(fi_card_flag==1)


# Aggregate and calculate --for nh race only. 

df_nh<-person_age_result%>%
  group_by(age_bracket)%>%
  mutate(total=n())%>%
  group_by(age_bracket, nh_race)%>%
  mutate(count=n(),
         rate=count/total*100)%>%
  slice(1)%>%
  select(age_bracket, nh_race, fi_card_flag, total, count, rate)%>%
  # filter(!grepl('nh_sswana|nh_aian',nh_race))%>%
  rename("race"="nh_race")

# now aggregate and calculate but only for sswana/aian/nhpi

df_other<-person_age_result%>%
  select(age_bracket, fi_card_flag, aian_flag, sswana_flag, nhpi_flag) %>% 
  group_by(age_bracket)%>%
  mutate(total = n()) %>%
  pivot_longer(cols=3:5, names_to="race", values_to="count")%>%
  mutate(race=ifelse(race %in% "sswana_flag", 'sswana',
                     ifelse(race %in% "aian_flag", 'aian', "nhpi")))%>%
  group_by(age_bracket, race)%>%
  mutate(count=sum(count),
         rate=count/total*100)%>%
  slice(1)%>%
  select(age_bracket, race, fi_card_flag, total, count, rate)

# combine both df into one final df a

df<-rbind(df_nh,  df_other)%>%
  arrange(age_bracket, -rate)%>%
  mutate(level="Field interview card completed")%>%
  select(age_bracket, race, level, total, count, rate)

# Push to postgres ----------------

# make sure no trailing spaces anywhere

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_stoprates_age_race_ficard_person", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_stoprates_age_race_ficard_person  IS 'Distribution of race demographics within
each age bracket for people stopped by police where the only stop result was a field interview card in 2022. 
The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_stoprates_age_race_ficard_person.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_stoprates_age_race_ficard_person.docx';

COMMENT ON COLUMN report_stoprates_age_race_ficard_person.age_bracket IS 'Age-bracket';
COMMENT ON COLUMN report_stoprates_age_race_ficard_person.level IS 'Level of analysis';
COMMENT ON COLUMN report_stoprates_age_race_ficard_person.race IS 'Race. All groups are exclusive of latinx except for AIAN, SSWANA and NHPI which are alone or in combination with other races.';
COMMENT ON COLUMN report_stoprates_age_race_ficard_person.total IS 'Total number of people stopped for a field interview card only within each age group. Denominator for rate calculation.';
COMMENT ON COLUMN report_stoprates_age_race_ficard_person.count IS 'Count of people stopped where result was only a field interview card within each age-bracket and racial group. Numerator for rate calculation.';
COMMENT ON COLUMN report_stoprates_age_race_ficard_person.rate IS 'Rate of people stopped within each racial group within each age bracket out of total people in each age bracket where only stop result was a field interview card';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
