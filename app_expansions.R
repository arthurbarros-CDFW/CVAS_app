
exp_lookup$WE<-as.numeric(exp_lookup$WE)
exp_lookup$WD<-as.numeric(exp_lookup$WD)
exp_lookup$both<-as.numeric(exp_lookup$both)

#create a function to calculate the average effort for the target surveys month, site-group, and hours using the exp_lookup table
effort_average<-function(row){
  joined_data<-exp_lookup%>%
    filter(month==row$month & site_grouping==row$EDM_code & hours %in% c(row$start_hour:row$finish_hour))
  exp_factor<-if(row$Day_Type=="WD"){
    mean(joined_data$WD)/100
  }else if(row$Day_Type=="WE"){
    mean(joined_data$WE)/100
  }else if(row$Day_Type=='both'){
    mean(joined_data$both)/100
  }
}

#######################################################################
# 1st Count expansions
#######################################################################

first_counts<-first_counts%>%left_join(edm_lookup)
first_counts$start_hour<-as.numeric(format(as.POSIXct(first_counts$Start,format="%H:%M"),format='%H'))
first_counts$finish_hour<-as.numeric(format(as.POSIXct(first_counts$Finish,format="%H:%M"),format='%H'))
first_counts$month<-format(as.Date(first_counts$SurveyDate, format="%Y-%m-%d"),"%B")

#group_by edm site grouping, month and date and sum anglers
first_counts_grouped<-first_counts%>%
  group_by(month,SurveyDate,start_hour,finish_hour,Day_Type,EDM_code,section)%>%
  dplyr::summarise(Total_Anglers=sum(Anglers))

start<-Sys.time()
first_counts_grouped$expansion_factor<-sapply(1:nrow(first_counts_grouped),function(i){
  effort_average(first_counts_grouped[i,])
})
finish<-Sys.time()
finish-start

#Calculate total angler effort
first_expansions<-first_counts_grouped%>%
  mutate(angler_effort=Total_Anglers/expansion_factor)
first_expansions$count<-'First'
#######################################################################
# 2nd Count expansions
#######################################################################
second_counts<-second_counts%>%left_join(edm_lookup)
second_counts$start_hour<-as.numeric(format(as.POSIXct(second_counts$Start,format="%H:%M"),format='%H'))
second_counts$finish_hour<-as.numeric(format(as.POSIXct(second_counts$Finish,format="%H:%M"),format='%H'))
second_counts$month<-format(as.Date(second_counts$SurveyDate, format="%Y-%m-%d"),"%B")

#group_by edm site grouping, month and date and sum anglers
second_counts_grouped<-second_counts%>%
  group_by(month,SurveyDate,start_hour,finish_hour,Day_Type,EDM_code,section)%>%
  dplyr::summarise(Total_Anglers=sum(Anglers))

start<-Sys.time()
second_counts_grouped$expansion_factor<-sapply(1:nrow(second_counts_grouped),function(i){
  effort_average(second_counts_grouped[i,])
})
finish<-Sys.time()
finish-start

#Calculate total angler effort
second_expansions<-second_counts_grouped%>%
  mutate(angler_effort=Total_Anglers/expansion_factor)
second_expansions$count<-'Second'

count_expansions<-first_expansions%>%
  rbind(second_expansions)

print("expansions finished")

saveRDS(count_expansions,'outputs/count_expansions.rds')