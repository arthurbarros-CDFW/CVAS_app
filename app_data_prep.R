#Script to pull CVAS data from access database and clean
#load packages
library(tidyverse)
library(dplyr)

#######################################################################
# (1) recreate first_count table for pasting
#######################################################################

open_date<-as.Date("2022-07-16")
close_date<-as.Date("2022-12-31")

first_counts<-tblCountSurv %>%
  left_join(tblCountDetail, by = "CountSurvId")%>%
  left_join(tblGeoLoc,by='GeoLocId')%>%
  mutate(SurveyDate = as.Date(SurveyDate, format = "%m/%d/%Y"))%>%
  mutate(Start=as.POSIXct(Start,format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(Start=format(Start,format='%H:%M'))%>%
  mutate(Finish=as.POSIXct(Finish,format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(Finish=format(Finish,format='%H:%M'))%>%
  # Filter the survey dates
  filter(as.Date(SurveyDate) >= as.Date(open_date) & as.Date(SurveyDate) <= as.Date(close_date)) %>%
  # Group by required columns and calculate sum of Anglers
  group_by(GeoLocDE, SurveyDate, Start, Finish, Day_Type = ifelse(weekdays(SurveyDate) %in% c("Saturday", "Sunday"), "WE", "WD"), LocSort) %>%
  summarise(B_Anglers = sum(coalesce(B_Anglers, 0)), S_Anglers = sum(coalesce(S_Anglers, 0))) %>%
  # Order the result
  arrange(LocSort)

first_counts<-dplyr::select(first_counts,section=GeoLocDE,SurveyDate,Start,Finish,Day_Type,B_Anglers,S_Anglers)

first_counts<-first_counts%>%
  pivot_longer(
    cols=B_Anglers:S_Anglers,
    names_to = "Method",
    values_to = "Anglers"
  )
first_counts$Method<-ifelse(first_counts$Method=='B_Anglers','B','S')

#boating and shore surveys that are tracked separately in expansion lookups for 12.1 and 12.0D
targets<-c('12.0D','12.1')
first_counts$section<-ifelse(first_counts$section%in%targets,paste(first_counts$section,first_counts$Method,sep=''),first_counts$section)

#next group and sum by section x month
first_counts<-first_counts%>%
  group_by(section,SurveyDate,Start,Finish,Day_Type)%>%
  summarise(Anglers=sum(Anglers))

first_counts$month<-month(first_counts$SurveyDate,label=T,abbr=F)

saveRDS(first_counts,'outputs/first_counts.rds')

#######################################################################
# (2) recreate second_count table for pasting
#######################################################################
second_counts<-tblGeoLoc %>%
  right_join(tbl2ndCountSurv %>%
               left_join(tbl2ndCountDetail, by = "CountSurvId"),
             by = "GeoLocId") %>%
  mutate(SurveyDate = as.Date(SurveyDate, format = "%m/%d/%Y"))%>%
  mutate(Start=as.POSIXct(Start,format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(Start=format(Start,format='%H:%M'))%>%
  mutate(Finish=as.POSIXct(Finish,format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(Finish=format(Finish,format='%H:%M'))%>%
  # Filter the survey dates
  filter(as.Date(SurveyDate) >= as.Date(open_date) & as.Date(SurveyDate) <= as.Date(close_date)) %>%
  # Group by required columns and calculate sum of Anglers
  group_by(GeoLocDE, SurveyDate, Start, Finish, Day_Type = ifelse(weekdays(SurveyDate) %in% c("Saturday", "Sunday"), "WE", "WD"), LocSort) %>%
  summarise(B_Anglers = sum(coalesce(B_Anglers, 0)), S_Anglers = sum(coalesce(S_Anglers, 0))) %>%
  # Order the result
  arrange(LocSort)

second_counts<-dplyr::select(second_counts,section=GeoLocDE,SurveyDate,Start,Finish,Day_Type,B_Anglers,S_Anglers)

second_counts<-second_counts%>%
  pivot_longer(
    cols=B_Anglers:S_Anglers,
    names_to = "Method",
    values_to = "Anglers"
  )
second_counts$Method<-ifelse(second_counts$Method=='B_Anglers','B','S')

#boating and shore surveys that are tracked separately in expansion lookups for 12.1 and 12.0D
targets<-c('12.0D','12.1')
second_counts$section<-ifelse(second_counts$section%in%targets,paste(second_counts$section,second_counts$Method,sep=''),second_counts$section)


second_counts<-second_counts%>%
  group_by(section,SurveyDate,Start,Finish,Day_Type)%>%
  summarise(Anglers=sum(Anglers))

second_counts$month<-month(second_counts$SurveyDate,label=T,abbr=F)

saveRDS(second_counts,'outputs/second_counts.rds')

#######################################################################
# (3) recreate roving table for pasting
#######################################################################
roving<-select(tblIvDetail,IvDetId,IvSurvId,IvPage,IvLine,NumberAnglers,FST,HrsFished,MethodId,tgtSpecies=SpeciesId)%>%
  left_join(select(tblCatch,CatchId,IvDetId,Kept,Released,SpeciesId))%>%
  left_join(select(tblSpecies,SpeciesId,SpeciesDE),by="SpeciesId")%>%
  left_join(select(tblIvSurv,IvSurvId,GeoLocId,SurveyDate),by="IvSurvId")%>%
  left_join(select(tblGeoLoc,GeoLocId,GeoLocDE))%>%
  left_join(select(tblMethod,MethodId,MethodName))%>%
  mutate(SurveyDate = as.Date(SurveyDate, format = "%m/%d/%Y"))%>%
  mutate(FST=as.POSIXct(FST,format="%m/%d/%Y %H:%M:%S"))%>%
  mutate(FST=format(FST,format='%H:%M'))%>%
  filter(as.Date(SurveyDate) >= as.Date(open_date) & as.Date(SurveyDate) <= as.Date(close_date))

roving$Day_Type<-ifelse(weekdays(roving$SurveyDate)%in% c("Saturday", "Sunday"), "WE", "WD")
roving$TotalHours<-roving$NumberAnglers*roving$HrsFished

roving<-dplyr::select(roving,section=GeoLocDE,SurveyDate,Day_Type,Page=IvPage,IvLine,Anglers=NumberAnglers,FST,HoursFished=HrsFished,tgtSpecies,SpeciesCaught=SpeciesDE,Kept,Released,TotalHours,MethodName)

roving$section<-ifelse(roving$section=='BF','7.1B',roving$section)

#Pull out by-catch to remove from roving and use later
bycatch_targets<-c(1,11)
by_catch<-roving%>%
  filter(!tgtSpecies%in%bycatch_targets & SpeciesCaught=='CS')

by_catch_filter<-unique(select(by_catch,section,SurveyDate,Day_Type,Page,IvLine,SpeciesCaught))

roving<-roving%>%anti_join(by_catch_filter)
saveRDS(roving,'outputs/roving.rds')

#######################################################################
# (4) recreate ratio table for pasting
#######################################################################
ratio_prep<-unique(select(roving,section,SurveyDate,Day_Type,IvLine,Page,TotalHours,MethodName,MethodName,tgtSpecies))

target_sections<-c('12.0D','12.1')
target_method<-c('Boat','Guided boat party')

ratio_prep<-ratio_prep%>%
  mutate(MethodName=ifelse(MethodName%in%target_method,'B','S'))
ratio_prep$section<-ifelse(ratio_prep$section %in%target_sections,paste(ratio_prep$section,ratio_prep$MethodName,sep=''),ratio_prep$section)
ratio_prep$section<-ifelse(ratio_prep$section=='BF','7.1B',ratio_prep$section)

Total_effort<-ratio_prep%>%
  group_by(section,SurveyDate,Day_Type)%>%
  summarise(Total_Effort=sum(TotalHours))
CS_effort<-ratio_prep%>%
  filter(tgtSpecies%in%c(15,11))%>%
  group_by(section,SurveyDate,Day_Type)%>%
  summarise(CS_Effort=sum(TotalHours))
ratio<-Total_effort%>%
  left_join(CS_effort)

ratio$ratio_cs<-ifelse(is.na(ratio$CS_Effort)==T,0,ratio$CS_Effort/ratio$Total_Effort)

saveRDS(ratio,'outputs/ratio.rds')

#######################################################################
# by-catch
#######################################################################
by_catch<-select(by_catch,section,SurveyDate,Day_Type,Page,IvLine,SpeciesCaught,Kept,Released,TotalHours,MethodName)
by_catch_surveys<-unique(select(by_catch,section,SurveyDate))
by_catch_effort<-roving%>%
  inner_join(by_catch_surveys)%>%
  filter(tgtSpecies!=11)
by_catch_effort<-unique(select(by_catch_effort,SurveyDate,section,Day_Type,Page,IvLine,TotalHours,MethodName))
# Check if by_catch_effort is not empty before creating the SpeciesCaught column
if (nrow(by_catch_effort) > 0) {
  by_catch_effort$SpeciesCaught <- ' '
  by_catch_effort$Kept=0
  by_catch_effort$Released=0
} else {
  # If by_catch_effort is empty, create the SpeciesCaught column without assigning values
  by_catch_effort$SpeciesCaught <- character(0)
  by_catch_effort$Kept=numeric(0)
  by_catch_effort$Released=numeric(0)
}

by_catch<-by_catch%>%rbind(by_catch_effort)

print("data prep finished")

saveRDS(by_catch,'outputs/by_catch.rds')