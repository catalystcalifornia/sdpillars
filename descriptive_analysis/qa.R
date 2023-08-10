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