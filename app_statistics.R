daily_tallys<-survey_daytypes
  
monthly_stats<-summary_data

#rounding values
monthly_stats$total_angler_effort<-round(monthly_stats$total_angler_effort)
monthly_stats$targeted_total_harvest<-floor(monthly_stats$targeted_total_harvest)
monthly_stats$targeted_total_catch<-floor(monthly_stats$targeted_total_catch)

monthly_stats$bycatch_total_harvest[is.na(monthly_stats$bycatch_total_harvest)]<-0
monthly_stats$bycatch_total_catch[is.na(monthly_stats$bycatch_total_catch)]<-0

########################################################
###Estimate Daily Hours
########################################################
daily_summary$catch<-daily_summary$Kept+daily_summary$Released
daily_summary$catch_squared<-daily_summary$catch^2
daily_summary$kept_squared<-daily_summary$Kept^2
daily_summary$released_squared<-daily_summary$Released^2

#get daily expanded effort squared for variance estimates
daily_summary$me_average_squared<-daily_summary$multi_effort_average^2

daily_summary$hrs_fish_harvest<-daily_summary$Kept*daily_summary$multi_effort_average
daily_summary$hrs_fish_released<-daily_summary$Released*daily_summary$multi_effort_average
daily_summary$hrs_fish_catch<-daily_summary$catch*daily_summary$multi_effort_average

########################################################
###Get monthly stats from daily records
########################################################

monthly_hours<-daily_summary%>%
  group_by(month,section,year=year(SurveyDate))%>%
  dplyr::summarise(sum_hours=sum(multi_effort_average,na.rm=T),
                   sum_hours_squared=sum(me_average_squared,na.rm=T),
                   sum_catch_squared=sum(catch_squared,na.rm=T),
                   sum_kept_squared=sum(kept_squared,na.rm=T),
                   sum_released_squared=sum(released_squared,na.rm=T),
                   sum_hours_catch=sum(hrs_fish_catch,na.rm=T),
                   sum_hours_harvest=sum(hrs_fish_harvest,na.rm=T),
                   sum_hours_released=sum(hrs_fish_released,na.rm=T))

########################################################
###Day type variance estimates
########################################################

WD_variance<-daily_summary%>%
  filter(Day_Type=='WD')%>%
  group_by(month,section,year=year(SurveyDate))%>%
  dplyr::summarise(wd_variance=var(multi_effort_average,na.rm=T))

WE_variance<-daily_summary%>%
  filter(Day_Type=='WE')%>%
  group_by(month,section,year=year(SurveyDate))%>%
  dplyr::summarise(we_variance=var(multi_effort_average,na.rm=T))

daytype_variance<-WD_variance%>%left_join(WE_variance)

tally_surveys<-daily_summary%>%
  group_by(section,month,Day_Type,year=year(SurveyDate))%>%
  dplyr::summarise(tally_surveys=n())
tally_surveys$Day_Type<-paste(tally_surveys$Day_Type,'_surveys',sep='')

tally_surveys_wide<-tally_surveys%>%
  pivot_wider(names_from = Day_Type,
              values_from = tally_surveys)

daily_tallys$Day_Type<-paste(daily_tallys$Day_Type,'_monthly_total',sep='')
daily_tallys<-daily_tallys%>%
  pivot_wider(names_from = Day_Type,
              values_from = day_tally)

tally_surveys<-tally_surveys_wide%>%left_join(daily_tallys)

tally_surveys$total_num_surveys<-tally_surveys$WD_surveys+tally_surveys$WE_surveys

########################################################
###Estimate Effort Variance
########################################################

#what is VARTAH field (cell AK28) from monthly expanders? ANS: variance total angler hours
effort_variance<-daytype_variance%>%left_join(tally_surveys)
effort_variance$monthly_total<-effort_variance$WD_monthly_total+effort_variance$WE_monthly_total

#the following equation should try and match equation 13 from the West Inc CVAS review
#however, I don't think it is a correct interpretation, right now (6/17/2024) it matches with the cell AK28 from monthly excel expanders
effort_variance<-effort_variance%>%
  mutate(VARTAH=monthly_total^2*(1/(monthly_total^2))*(WD_monthly_total*(WD_monthly_total-WD_surveys)*wd_variance/WD_surveys+WE_monthly_total*(WE_monthly_total-WE_surveys)*we_variance/WE_surveys))


########################################################

monthly_hours<-monthly_hours%>%left_join(tally_surveys)

monthly_hours$total_days<-monthly_hours$WD_monthly_total+monthly_hours$WE_monthly_total

monthly_hours$FPCF<-(monthly_hours$total_days-monthly_hours$total_num_surveys)/monthly_hours$total_days

#####################################################################

monthly_stats$total_harvest<-monthly_stats$targeted_total_harvest+monthly_stats$bycatch_total_harvest

monthly_stats$hpue<-(monthly_stats$total_harvest)/monthly_stats$total_angler_effort

monthly_stats$cpue<-(monthly_stats$targeted_total_catch)/monthly_stats$total_angler_effort

monthly_stats$rpue<-(monthly_stats$targeted_total_released)/monthly_stats$total_angler_effort

#####################################################################

monthly_stats<-monthly_stats%>%
  left_join(monthly_hours)%>%
  left_join(effort_variance)

########################################################
###Estimate VarianceCPUE
########################################################

monthly_stats<-monthly_stats%>%
  mutate(VARCPUE_CATCH=(total_num_surveys/sum_hours)^2*FPCF*(sum_catch_squared+cpue^2*sum_hours_squared-2*cpue*sum_hours_catch)/(total_num_surveys*(total_num_surveys-1)))%>%
  mutate(VARCPUE_HARVST=(total_num_surveys/sum_hours)^2*FPCF*(sum_kept_squared+hpue^2*sum_hours_squared-2*hpue*sum_hours_harvest)/(total_num_surveys*(total_num_surveys-1)))%>%
  mutate(VARCPUE_RELEASE=(total_num_surveys/sum_hours)^2*FPCF*(sum_released_squared+rpue^2*sum_hours_squared-2*rpue*sum_hours_released)/(total_num_surveys*(total_num_surveys-1)))

monthly_stats<-monthly_stats%>%
  mutate(VARAHSP=(VARTAH))

monthly_stats<-monthly_stats%>%
  mutate(VARCATCH_CATCH=total_angler_effort^2*VARCPUE_CATCH+cpue^2*VARTAH-VARCPUE_CATCH*VARAHSP)%>%
  mutate(VARCATCH_HARVEST=total_angler_effort^2*VARCPUE_HARVST+hpue^2*VARTAH-VARCPUE_HARVST*VARAHSP)%>%
  mutate(VARCATCH_RELEASE=total_angler_effort^2*VARCPUE_RELEASE+rpue^2*VARTAH-VARCPUE_RELEASE*VARAHSP)

########################################################
###summary tablle
########################################################

monthly_summary<-dplyr::select(monthly_stats,year,month,section,count,
                               catch=targeted_total_catch,
                               catch_var=VARCATCH_CATCH,
                               harvest=targeted_total_harvest,
                               harvest_var=VARCATCH_HARVEST,
                               released=targeted_total_released,
                               released_var=VARCATCH_RELEASE,
                               effort=total_angler_effort,
                               effort_var=VARTAH)

########################################################
###outputting
########################################################

monthly_summary$month<-ordered(monthly_summary$month,levels=month)

saveRDS(monthly_summary,'outputs/monthly_stats.rds')
