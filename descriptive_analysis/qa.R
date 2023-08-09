qa_beats<-beats%>%group_by(beat,div)%>%summarise(count=n())
unique_beats<-length(unique(qa_beats$beat))
beats_div_qa<-qa_beats%>%
  left_join(div, by=c("div"="div_num"))

