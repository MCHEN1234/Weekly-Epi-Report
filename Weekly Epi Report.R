#load libraries
library(tidyverse)
library(googlesheets4)
library(zoo)
library(lubridate)
library(dplyr)
library(data.table)

#load case data
gs4_deauth()
daily_numbers <- read_sheet("https://docs.google.com/spreadsheets/d/1ad7-09_Jn6AxsdkVPE33T-iLfGpPRmd3piXQqFiVeas/edit#gid=0",
                            sheet = "PT Data", col_names = TRUE) %>%
  select(pruid, prname, date, update, numtotal, numrecover, numdeaths, numtoday, numdeathstoday, numactive) %>%
  mutate(date=as.Date(date),"EST")

#load population data
load('Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/PROCEDURES/Templates/12. 8 pm PCO Bullets/latest_can_pop.rda')
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
table1a <- daily_numbers %>% #select original dataframe to work with
  select(pruid,prname,date,numtotal,numtoday) %>% #isolate relevant columns
  arrange(prname,date) %>% #sort by province and date
  mutate(Average = rollmean(numtoday,k=7,fill=NA,align=c("right"))) %>% #create variable for rolling average
  left_join(latest_can_pop,by=c("prname"="Jurisdiction")) %>% #join with population data
  mutate(incrcaseper = (numtoday/Population)*100000) %>% #create variable for rate (incremental)
  mutate(cumcaseper = (numtotal/Population)*100000)  %>%  #create variable for rate (cumulative)
  mutate(start_of_week=floor_date(date, unit = "weeks")) %>%
  mutate(end_of_week=date(start_of_week)+6) %>%
  mutate(Week=paste(str_sub(months(start_of_week),1,3),"-",day(start_of_week), " to ", str_sub(months(end_of_week),1,3),"-",day(end_of_week)),
  Week_before=paste(str_sub(months(date(start_of_week)-7),1,3),"-",day(date(start_of_week)-7), " to ", str_sub(months(date(end_of_week)-7),1,3),"-",day(date(end_of_week)-7)))%>%
  group_by(Week, prname)%>%
  mutate(weeklytotal=sum(numtoday))

table1b <- table1a %>%
  
  
  
  