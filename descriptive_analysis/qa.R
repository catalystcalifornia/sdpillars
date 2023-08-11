qa_beats<-beats%>%group_by(beat,div)%>%summarise(count=n())
unique_beats<-length(unique(qa_beats$beat))
beats_div_qa<-qa_beats%>%
  left_join(div, by=c("div"="div_num"))
qa<-beats_div_qa%>%
  left_join(beats_div,by="beat")


qa<-beats_stop%>%
  filter(is.na(div_name))


qa<-beats_stop%>%
  filter(Year=='2022')%>%
  group_by(div_name,stop_in_response_to_cfs)%>%
  summarise(Count=n(),
            'Average Stop Time (Mins)' = mean(stopduration, na.rm = FALSE),
            'Median Stop Time (Mins)'=   median(stopduration,na.rm = FALSE),
            'Min Stop Time (Mins)'=min(stopduration,na.rm = FALSE),
            'Max Stop Time (Mins)'=max(stopduration,na.rm = FALSE),
            'Total Time (Mins)'= sum(stopduration)
  )

qa<-total<-stops%>%
  filter(stop_in_response_to_cfs==0 & Year=='2022')%>%nrow()

qa<-stops%>%
  filter(stop_in_response_to_cfs==0 & Year=='2022')%>%
  group_by(assignment)%>%
  summarise(Count=n())
qa_total<-beats_stop%>%
  filter(Year=='2022')%>%
  group_by(div_name)%>%
  summarise(Total=n())
  
qa<-beats_stop%>%
  filter(Year=='2022')%>%
  group_by(div_name,stop_in_response_to_cfs)%>%
  summarise(Count=n())%>%
  left_join(qa_total)%>%
  mutate(Percent=Count/Total*100)

qa<-reason_p %>%
  group_by(stop_id, person_id) %>% 
  summarise(count=n())%>%
  nrow()

reason_p%>%filter(is.na(reason_for_stop))%>%nrow()


reason_p_dedup<-reason_p%>%
  group_by(stop_id,person_id,reason_for_stop,assignment)%>%
  summarise(duplicates=n())

result_p_qa<-person%>%
  
  # join result table to person table so we can filter for year / cfs
  
  left_join(beats_stop%>%select(stop_id, Year, stop_in_response_to_cfs,assignment,div_name))%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)%>%
  
  # left join results -filtered for year/cfs - to get stop reason
  
  left_join(result%>%select(stop_id, person_id, result),by=c("stop_id","person_id"))%>%
  
  select(Year, stop_id, person_id, result,assignment,div_name)

qa_total<-result_p_qa%>%
  group_by(div_name)%>%
  summarise(Total=n())

qa<-result_p_qa%>%
  left_join(qa_total,by=c("div_name"))%>%
  group_by(result,div_name)%>%
  summarise(Count=n(),
            Percent=Count/min(Total)*100)

# bring in action table
actions<-dbGetQuery(con, "SELECT * FROM rel_actions")

# filter for search actions
actions_search<-actions%>%
  filter(grepl('search|Search', action))

search_dedup<-search%>%
  mutate_all(na_if,"")%>%
  filter(!is.na(basis_for_search))%>%
  group_by(stop_id,person_id)%>%
  summarise(count_basis=n())
# a person can have more than 1 basis for a search

#dedup search actions
actions_search_dedup<-actions_search%>%
  filter(consented!="N")%>%
  group_by(stop_id,person_id)%>%
  summarise(count_searches=n())

#join to basis for the search, is it always captured?
actions_basis<-actions_search_dedup%>%
  full_join(search_dedup, by=c("stop_id","person_id"))

sum(is.na(actions_basis$count_searches)) # 5 nulls based on actions of searches
sum(is.na(actions_basis$count_basis)) # 9561 based on basis

# nulls for count basis are people where consent was provided but no search was recorded--do they show up in contraband?

contra_p<-contra%>%
  filter(contraband!="None")%>%
  group_by(stop_id, person_id)%>%
  summarise(count_contra=n())

actions_basis_contra<-actions_basis%>%
  left_join(contra_p)

actions_basis_contra%>%filter(is.na(count_basis) & !is.na(count_contra))%>%nrow()
#1684 where contraband found but no search basis reported, use actions taken not basis for search

actions_basis_contra%>%filter(is.na(count_searches) & !is.na(count_contra))%>%nrow()

person_age%>%filter(is.na(perceived_age))

min(person_age$perceived_age)

max(person_age$perceived_age)

median(person_age$perceived_age)

View(person_age[c("perceived_age", "age_bracket")])

qa_race<-race%>%
  left_join(beats_stop, by="stop_id")%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)

qa_all<-qa_race%>%
  mutate(Total=n())%>%
  group_by(nh_race)%>%
  summarise(Percent=n()/min(Total)*100)

qa_gu<-qa_race%>%filter(assignment=="Gang enforcement")%>%
  mutate(Total=n())%>%
  group_by(nh_race)%>%
  summarise(Percent=n()/min(Total)*100)

qa_all<-qa_race%>%
  summarise(NHPI=sum(nhpi_flag,na.rm=TRUE),
            SSWANA=sum(sswana_flag,na.rm=TRUE),
            AIAN=sum(aian_flag,na.rm=TRUE))

qa_gu<-qa_race%>%filter(assignment=="Gang enforcement")%>%
  summarise(NHPI=sum(nhpi_flag,na.rm=TRUE),
            SSWANA=sum(sswana_flag,na.rm=TRUE),
            AIAN=sum(aian_flag,na.rm=TRUE))

div_total<-qa_race%>%group_by(div_name)%>%
  summarise(Total=n())

qa<-qa_race%>%group_by(div_name)%>%
  summarise(NHPI=sum(nhpi_flag,na.rm=TRUE),
            SSWANA=sum(sswana_flag,na.rm=TRUE),
            AIAN=sum(aian_flag,na.rm=TRUE))%>%
  left_join(div_total, by="div_name")%>%
  mutate(NHPI_rate=NHPI/Total,
         SSWANA_rate=SSWANA/Total,
        AIAN_rate=AIAN/Total,
         )


qa<-qa_race%>%group_by(div_name,nh_race)%>%
  summarise(Count=n())%>%
  left_join(div_total, by="div_name")%>%
  mutate(Percent=Count/Total*100)

qa<-beats_stop%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0)%>%
  group_by(beat_name)%>%
  summarise(count=n())

qa_gu<-beats_stop%>%
  filter(Year=='2022' & stop_in_response_to_cfs == 0 & assignment=='Gang enforcement')%>%
  group_by(beat_name)%>%
  summarise(count=n())

quantile(x)
