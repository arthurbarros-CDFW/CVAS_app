tgt_species<-c(11) #here set target species number

sum(!weekdays(seq(open_date, close_date, "days")) %in% c("Saturday", "Sunday"))
survey_days<-data.frame(month=months(seq(open_date, close_date, "days")),day=seq(open_date, close_date, "days"))
survey_days$Day_Type<-ifelse(!weekdays(survey_days$day) %in% c("Saturday", "Sunday"),'WD','WE')
survey_daytypes<-survey_days%>%
  group_by(month,Day_Type)%>%
  dplyr::summarise(day_tally=n())

head(survey_daytypes)

avg_dt_effort<-count_expansions%>%
  left_join(select(ratio,-Day_Type))

#deal with opening days
selected_year<-format(open_date,"%Y")
opener_sections<-c('5','6','7.1B','7.1S')
opening_dates<-c(as.Date(paste(selected_year,'07-16',sep='-')),
                 as.Date(paste(selected_year,'07-16',sep='-')),
                 as.Date(paste(selected_year,'08-01',sep='-')),
                 as.Date(paste(selected_year,'08-01',sep='-')))
opening_df<-data.frame(section=opener_sections,SurveyDate=opening_dates)

openers_efforts<-avg_dt_effort%>%
  inner_join(opening_df)
openers_efforts$section<-paste(openers_efforts$section,'opener')

avg_dt_effort<-avg_dt_effort%>%
  anti_join(opening_df)
avg_dt_effort<-avg_dt_effort%>%rbind(openers_efforts)

avg_dt_effort<-avg_dt_effort%>%
  left_join(survey_daytypes)

#fix opening day tallys
avg_dt_effort$day_tally<-ifelse(avg_dt_effort$section%in%c('5','6') & avg_dt_effort$month=='July' & avg_dt_effort$Day_Type=='WE',avg_dt_effort$day_tally-1,avg_dt_effort$day_tally)

avg_dt_effort$day_tally<-ifelse(avg_dt_effort$section%in%c('7.1S','7.1B') & avg_dt_effort$month=='August' & avg_dt_effort$Day_Type=='WD',avg_dt_effort$day_tally-1,avg_dt_effort$day_tally)

avg_dt_effort$day_tally<-ifelse(avg_dt_effort$section%in%c('7.1S opener','7.1B opener','5 opener','6 opener'),1,avg_dt_effort$day_tally)

#first_count
first_effort<-avg_dt_effort%>%
  filter(count=='First')
first_effort$cs_effort<-first_effort$ratio_cs*first_effort$angler_effort
first_effort$cs_effort[is.na(first_effort$cs_effort)]<-0

first_effort<-first_effort%>%
  group_by(month,section,Day_Type,count,day_tally)%>%
  summarise(avg_cs_effort=mean(cs_effort))

first_effort$effort_expanded<-first_effort$avg_cs_effort*first_effort$day_tally

#multi_count
multi_effort<-avg_dt_effort
multi_effort$cs_effort<-multi_effort$ratio_cs*multi_effort$angler_effort
multi_effort$cs_effort[is.na(multi_effort$cs_effort)]<-0

multi_effort<-multi_effort%>%
  group_by(month,SurveyDate,section,Day_Type,day_tally)%>%
  summarise(avg_cs_effort=mean(cs_effort))

multi_effort<-multi_effort%>%
  group_by(month,section,Day_Type,day_tally)%>%
  summarise(avg_cs_effort=mean(avg_cs_effort))

multi_effort$effort_expanded<-multi_effort$avg_cs_effort*multi_effort$day_tally
multi_effort$count<-'Multiple'

expanded_efforts<-first_effort%>%rbind(multi_effort)
str(data.frame(expanded_efforts))

daily_effort<-avg_dt_effort
daily_effort$CS_angler_effort<-daily_effort$angler_effort*daily_effort$ratio_cs
daily_effort<-daily_effort%>%
  mutate(CS_angler_effort=ifelse(is.na(CS_angler_effort),0,CS_angler_effort))%>%
  group_by(SurveyDate,month,section,Day_Type)%>%
  dplyr::summarise(multi_effort_average=mean(CS_angler_effort))

monthly_effort<-daily_effort%>%
  group_by(month,section)%>%
  summarise(sum_hours=sum(multi_effort_average))

target_sections<-c('12.0D','12.1')
target_method<-c('Boat','Guided boat party')

roving_prep<-roving%>%
  mutate(MethodName=ifelse(MethodName%in%target_method,'B','S'))
roving_prep$section<-ifelse(roving_prep$section %in%target_sections,
                            paste(roving_prep$section,
                                  roving_prep$Method,sep=''),roving_prep$section)

hours_targeting_CS<-unique(select(roving_prep,section,SurveyDate,Day_Type,tgtSpecies,Page,IvLine,TotalHours))%>%
  filter(tgtSpecies%in%tgt_species)%>%
  group_by(section,SurveyDate,Day_Type)%>%
  summarise(TotalHours=sum(TotalHours,na.rm=T))

CS_captured<-unique(select(roving_prep,section,SurveyDate,Day_Type,tgtSpecies,SpeciesCaught,IvLine,Kept,Released,TotalHours))%>%
  filter(SpeciesCaught=='CS', tgtSpecies==11)%>%
  group_by(section,SurveyDate,Day_Type)%>%
  summarise(Kept=sum(Kept),Released=sum(Released))

daily_cpue<-hours_targeting_CS%>%left_join(CS_captured)
daily_cpue$catch_CPUE<-(daily_cpue$Kept+daily_cpue$Released)/daily_cpue$TotalHours
daily_cpue$harvest_CPUE<-(daily_cpue$Kept)/daily_cpue$TotalHours
daily_cpue$released_CPUE<-(daily_cpue$Released)/daily_cpue$TotalHours

daily_cpue[is.na(daily_cpue)] <- 0

daily_cpue<-unique(select(ungroup(count_expansions),SurveyDate,section,Day_Type))%>%
  left_join(daily_cpue)
daily_cpue[is.na(daily_cpue)] <- 0

opening_df<-data.frame(section=opener_sections,SurveyDate=opening_dates)

openers_cpue<-daily_cpue%>%
  inner_join(opening_df)
openers_cpue$section<-paste(openers_cpue$section,'opener')

daily_cpue<-daily_cpue%>%
  anti_join(opening_df)
daily_cpue<-daily_cpue%>%rbind(openers_cpue)

avg_cpue<-daily_cpue%>%
  group_by(month=month(SurveyDate,label=T,abbr=F),section,Day_Type,year=year(daily_cpue$SurveyDate))%>%
  summarise(avg_catch_cpue=mean(catch_CPUE),avg_harvest_cpue=mean(harvest_CPUE),avg_released_cpue=mean(released_CPUE))

avg_cpue<-avg_cpue%>%
  left_join(expanded_efforts)

total_catch<-avg_cpue
total_catch$Total_DT_Catch<-total_catch$avg_cs_effort*total_catch$avg_catch_cpue*total_catch$day_tally

total_catch$Total_DT_Harvest<-total_catch$avg_cs_effort*total_catch$avg_harvest_cpue*total_catch$day_tally

total_catch$Total_DT_Released<-total_catch$avg_cs_effort*total_catch$avg_released_cpue*total_catch$day_tally

bycatch_expanded<-by_catch%>%
  group_by(SurveyDate,section,Day_Type)%>%
  summarise(TotalHours=sum(TotalHours),Kept=sum(Kept),Released=sum(Released))

bycatch_expanded$catch_CPUE<-(bycatch_expanded$Kept+bycatch_expanded$Released)/bycatch_expanded$TotalHours
bycatch_expanded$harvest_CPUE<-(bycatch_expanded$Kept)/bycatch_expanded$TotalHours
bycatch_expanded$released_CPUE<-(bycatch_expanded$Released)/bycatch_expanded$TotalHours

bycatch_expanded$month<-month(bycatch_expanded$SurveyDate,label = T,abbr = F)

#create zeros for all other date sections
empty_bycatch<-unique(select(roving_prep,section,SurveyDate,Day_Type,TotalHours))%>%
  group_by(SurveyDate,section,Day_Type)%>%
  summarise(TotalHours=sum(TotalHours))
empty_bycatch$Kept=0
empty_bycatch$Released=0
empty_bycatch$catch_CPUE=0
empty_bycatch$month<-month(empty_bycatch$SurveyDate,label = T,abbr = F)
empty_bycatch$harvest_CPUE<-0
empty_bycatch$released_CPUE<-0

empty_bycatch<-empty_bycatch%>%
  anti_join(bycatch_expanded,by=c('SurveyDate','section'))

bycatch_expanded<-bycatch_expanded%>%
  rbind(empty_bycatch)

avg_bycatch<-bycatch_expanded%>%
  group_by(month,section,Day_Type,year=year(SurveyDate))%>%
  summarise(avg_DT_catchCPUE=mean(catch_CPUE),
            avg_DT_harvestCPUE=mean(harvest_CPUE),
            avg_DT_releasedCPUE=mean(released_CPUE))

#bring in effort data
avg_bycatch<-avg_bycatch%>%
  left_join(select(avg_dt_effort,section,SurveyDate,Day_Type,angler_effort,ratio_cs))

avg_bycatch$ratio_bycatch<-1-avg_bycatch$ratio_cs
avg_bycatch$bycatch_effort<-avg_bycatch$angler_effort*avg_bycatch$ratio_bycatch

#by-catch totals
total_bycatch<-avg_bycatch%>%
  group_by(month,section,Day_Type,year)%>%
  summarise(avg_DT_catchCPUE=mean(avg_DT_catchCPUE,rm.na=T),
            avg_DT_effort=mean(bycatch_effort,rm.na=T),
            avg_DT_harvestCPUE=mean(avg_DT_harvestCPUE,na.rm=T),
            avg_DT_releasedCPUE=mean(avg_DT_releasedCPUE,na.rm=T))
total_bycatch<-total_bycatch%>%left_join(survey_daytypes)
total_bycatch$total_bycatch_effort<-total_bycatch$avg_DT_effort*total_bycatch$day_tally

total_bycatch$avg_DT_catch<-total_bycatch$avg_DT_catchCPUE*total_bycatch$avg_DT_effort
total_bycatch$total_DT_catch<-total_bycatch$avg_DT_catch*total_bycatch$day_tally

total_bycatch$total_harvest<-(total_bycatch$avg_DT_effort*total_bycatch$avg_DT_harvestCPUE)*total_bycatch$day_tally

total_bycatch$total_released<-(total_bycatch$avg_DT_effort*total_bycatch$avg_DT_releasedCPUE)*total_bycatch$day_tally

summary_bycatch<-total_bycatch%>%
  group_by(month,section,year)%>%
  summarise(bycatch_total_catch=sum(total_DT_catch),
            bycatch_total_harvest=sum(total_harvest),
            bycatch_total_released=sum(total_released),
            days_in_month=sum(day_tally))

summary_data<-total_catch%>%
  group_by(month,section,count,year)%>%
  summarise(total_angler_effort=sum(effort_expanded),
            targeted_total_catch=sum(Total_DT_Catch),
            targeted_total_harvest=sum(Total_DT_Harvest),
            targeted_total_released=sum(Total_DT_Released),
            days_in_month=sum(day_tally)            )

summary_data<-summary_data%>%left_join(summary_bycatch) #join in by-catch summary

write.csv(summary_data,'outputs/summary_data.csv',row.names = F)
saveRDS(summary_data,'outputs/summary_data.rds')

daily_summary<-daily_effort%>%left_join(daily_cpue)

print("summaries finished")

saveRDS(daily_summary,'outputs/daily_summary.rds')
saveRDS(survey_daytypes,'outputs/survey_daytypes.rds')
