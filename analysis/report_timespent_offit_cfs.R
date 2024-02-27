## ANALYSIS: Calculating time SDPD officers spent on officer-initiated stops and calls for service

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

# add year column to stops data and filter for 2022 

stops$Year = substr(stops$date_stop, 1, 4)
stops<-stops%>%
  filter(Year=='2022')

# now join the duration table to the stops table

stops<-stops%>%
  left_join(duration, by=c("stop_id"="stop_id"))


# Analyze: Time spent on officer initiated stops -----------------------

df<-stops%>%
  mutate(duration_total=sum(stop_duration_capped))%>%
  group_by(stop_in_response_to_cfs)%>%
  mutate(duration_count=sum(stop_duration_capped),
         duration_rate=duration_count/duration_total*100)%>%
  select(stop_in_response_to_cfs, duration_total, duration_count, duration_rate)%>%
  slice(1)%>%
  mutate(label=ifelse(stop_in_response_to_cfs %in% 0, "Officer-initiated stop", "Call for service"),
         level="SDPD")%>%
  select(stop_in_response_to_cfs, label, level, duration_total,duration_count, duration_rate)

# Push table to postgres ----------------------------

# make sure no trailing spaces anywhere in the df

names(df) <- gsub(" ", "", names(df))

df[df == " "] <- ""

# set column types

charvect = rep("numeric", ncol(df)) #create vector that is "varchar" for the number of columns in df

charvect <- replace(charvect, c(1,2,3), c("varchar"))

# add df colnames to the character vector

names(charvect) <- colnames(df)

# push to postgres

dbWriteTable(con,  "report_timespent_offit_cfs", df, 
             overwrite = TRUE, row.names = FALSE,
             field.types = charvect)

# add meta data

table_comment <- paste0("COMMENT ON TABLE report_timespent_offit_cfs  IS '% of time spent on calls for service and officer initiated stops
out of all stops for SDPD in 2022. 
R script: W:\\Project\\RJS\\Pillars\\R\\analysis\\report_timespent_offit_cfs.R
QA document: 
W:\\Project\\RJS\\Pillars\\Documentation\\QA_report_timespent_offit_cfs.docx';
COMMENT ON COLUMN report_timespent_offit_cfs.stop_in_response_to_cfs IS 'Binary variable indicating if a stop was a call for service (1) or an officer initiated stop (0)';
COMMENT ON COLUMN report_timespent_offit_cfs.duration_total IS 'Total amount of time spent on all stops for SDPD in 2022. Note this is using capped stop duration times that adjust for outliers. Denominator for rate calc. See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_offit_cfs.duration_count IS 'Total amount of time spent on officer initiated stops by whether the stop was an officer initiated stop or a call for service.  Numerator for rate calculation.  See W:\\Project\\RJS\\Pillars\\R\\Data and Geo Prep\\duration_outlier_analysis.html for full methodology on how duration outliers are adjusted.';
COMMENT ON COLUMN report_timespent_offit_cfs.duration_rate IS 'Percent time spent on officer initiated stops or calls for service';
COMMENT ON COLUMN report_timespent_offit_cfs.label IS 'Label for if the data is for calls for service or officer initiated stop';
COMMENT ON COLUMN report_timespent_offit_cfs.level IS 'Level of analysis';

                        ")

# send table comment + column metadata
dbSendQuery(conn = con, table_comment)
