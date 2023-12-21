## Analysis: calculating time spent on gang enforcement assignment stops that result in just a FI card
# Prep ------------------------

# Load Packages
library(tidyverse)
library(RPostgreSQL)
library(dplyr)
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rjs_pillars")

# import data

stops <- dbGetQuery(con, "SELECT * FROM data.rel_stops")
duration<- dbGetQuery(con, "SELECT * FROM data.rel_stop_duration_capped")
result<-dbGetQuery(con, "SELECT * FROM rel_stops_result")

# add year column to stops data and filter for 2022 and cfs==0 to use throughout analysis

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

# now join person level data and stop duration data to stops table, and filter for assignment = 'gang enforcement'

person_stops<-stops%>%
  left_join(result)%>%
  left_join(duration,by=c("stop_id"="stop_id"))%>%
  filter(assignment=="Gang enforcement")

# Aggregate and calculate -----------------------

# denominator: time spent on all officer-initiated stops by gang assignment

df<-person_stops%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(stop_result_simple)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  slice(1)%>%
  select(assignment, stop_result_simple, duration_total, duration_count, duration_rate)%>%
  # filter(!grepl("Contacted parent/legal guardian or other person responsible for the minor|Citation for infraction|Custodial Arrest pursuant to outstanding warrant|Custodial Arrest without warrant|In-field cite and release|Psychiatric hold|Two or More Results", result))%>%
  arrange(-duration_rate)%>%
  mutate(level="Gang enforcement assignment")%>%
  select(level, stop_result_simple, duration_total, duration_count, duration_rate )

# Final push to postgres --------------------

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_ficard_gang_stop", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_ficard_gang_stop  IS 'Rate of time spent on stops by stop result
out of all officer-initiated stops done by the gang enforcement assignment in 2022. 
The stops are officer-initiated stops only.
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_ficard_gang_stop.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_ficard_gang_stop.docx';

COMMENT ON COLUMN report_timespent_ficard_gang_stop.level IS 'Level of analysis';

COMMENT ON COLUMN report_timespent_ficard_gang_stop.stop_result_simple IS 'Stop result alone (not in combination with other stop results)';
COMMENT ON COLUMN report_timespent_ficard_gang_stop.duration_total IS 'Total amount of time spent on officer initiated stops. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_ficard_gang_stop.duration_count IS 'Total amount of time spent on officer initiated stops by stop result.  Numerator for rate calculation. Note this is using capped stop duration times that adjust for outliers.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_ficard_gang_stop.duration_rate IS 'Percent time spent on stops by result and by race';
")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)



  
