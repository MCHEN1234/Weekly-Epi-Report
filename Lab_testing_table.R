library(PHACTrendR)
library(cansim)
library(dplyr)
library(tidyr)
library(lubridate)
library(zoo) #need this for rollsumr function below
library(scales)
library(stringr)
library(hms)
library(readr)
library(janitor)

#import incremental salt data
salt_raw<-PHACTrendR::import_SALT_data()
###

#rename and extract the date variables
SALT <- salt_raw %>%
  select(Report.Date,Jurisdiction,Tests.Performed,Positive.Test.Results,Percent.Positive.Test.Results, Latest.Update.Date) %>%
  rename(tests_performed=Tests.Performed,
         positive_tests=Positive.Test.Results,
         percent_positive=Percent.Positive.Test.Results) %>%
  mutate(update_date = as.Date(str_sub(Latest.Update.Date, 1, 10)),
         Date = as.Date(str_sub(Report.Date, 1, 10)),
         Time = as_hms(str_sub(Report.Date, 13, 20)),
         datetime = strptime(paste(Date, Time), "%Y-%m-%d%H:%M:%S"),
         positive_tests = ifelse (!is.na(positive_tests), positive_tests, round(tests_performed*(percent_positive/100))),  #some PTs (AB, ON) only report % positive
         percent_positive = ifelse (!is.na(percent_positive), percent_positive, round((positive_tests/tests_performed)*100, digits = 3)))

#create week variables and delete data after the cut off date
SALT2 <- SALT %>%
  select(-Latest.Update.Date,-update_date)%>%
  mutate(Start_of_week=floor_date(Date, "week"),
         End_of_week=date(Start_of_week)+6,
         Week=paste(str_sub(months(Start_of_week),1,3),"-",day(Start_of_week), " to ", str_sub(months(End_of_week),1,3),"-",day(End_of_week)),
         Week_before=paste(str_sub(months(date(Start_of_week)-7),1,3),"-",day(date(Start_of_week)-7), " to ", str_sub(months(date(End_of_week)-7),1,3),"-",day(date(End_of_week)-7))) %>%
  filter(Date <= floor_date(max(Date), "week")-1) %>% #delete any data after the cut off
  mutate(Current_week=ifelse(date(Date)+7 <= max(Date),"No","Yes")) %>%
  arrange(Jurisdiction,datetime)

#calculate the cumulative total tests performed
SALT3 <- SALT2 %>%
  group_by(Jurisdiction) %>%
  mutate(cumulative_tests = sum(tests_performed)) %>%
  filter(Date>="2021-01-23") #Issues with historical data missing for some PTs - only taking last two weeks data for now.

#merge in the population data
Provincial <- SALT3 %>%
  left_join(latest_can_pop, by="Jurisdiction") %>% #left join will return all rows in SALT2 and all columns from both SALT2 and latest_can_pop.
  filter(Current_week=="Yes")

#calculate variables needed in the table
Provincial2 <- Provincial %>%
  group_by(Jurisdiction, Week, Population, cumulative_tests) %>%
  summarise(total_days_reported=n(),
            week_tests_performed = sum(tests_performed),
            week_positive_tests = sum(positive_tests),
            .groups="drop_last") %>%
  mutate(week_percent_positive=round(week_positive_tests/week_tests_performed,digits = 4),
         Avg_tests_per_day=round(week_tests_performed/total_days_reported,digits = 4),
         Avg_tests_per_1000pop = round(Avg_tests_per_day/Population*1000,digits = 4)) %>%
 ungroup()


#setting national pop to actual value, as issues when summing all PTs when some don't report up to latest date
canada_pop<-latest_can_pop$Population[latest_can_pop$Jurisdiction=="Canada"]

#get Canadian totals by summing all the provinces
National_table <- Provincial2 %>%
  select(Week, Jurisdiction, cumulative_tests, week_tests_performed, week_positive_tests, Avg_tests_per_day, Avg_tests_per_1000pop, total_days_reported) %>%
  summarise(across(where(is.numeric),sum), #calculate the sum of numeric variables listed in the SELECT statement
            .groups="drop_last") %>%
  mutate(Population=canada_pop, #instead of using the summed up population, we will use the the population we defined for Canada
         Jurisdiction="Canada",
         week_percent_positive = week_positive_tests/week_tests_performed,
         Avg_tests_per_1000pop = Avg_tests_per_day/Population*1000) %>%
  select( Jurisdiction, cumulative_tests, Population, week_tests_performed, Avg_tests_per_day, Avg_tests_per_1000pop, week_percent_positive, total_days_reported)

#keep the relevant variables in the PT data set and order the variables in the same order as the National_table data set
PT_table <- Provincial2 %>%
  select(Jurisdiction, cumulative_tests, Population, week_tests_performed, Avg_tests_per_day, Avg_tests_per_1000pop, week_percent_positive, total_days_reported)


#combine PT and national data
PT_national_table <- rbind(PT_table,National_table) %>%
  group_by(Jurisdiction) %>%
  select(-Population, -total_days_reported)

juriorder <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut","Canada")

Final_weekly_table <- PT_national_table %>%
  mutate(week_percent_positive=ifelse(round(week_percent_positive,digits=4) < 0.001, percent(week_percent_positive,accuracy = 0.01), percent(week_percent_positive,accuracy = 0.1)),
         cumulative_tests = number(cumulative_tests,big.mark = " " ,accuracy = 1),
         week_tests_performed = number(week_tests_performed,big.mark = " " ,accuracy = 1),
         Avg_tests_per_day = number(Avg_tests_per_day,big.mark = " " ,accuracy = 1),
         Avg_tests_per_1000pop = number(Avg_tests_per_1000pop,big.mark = " " ,accuracy = 0.01),
         Jurisdiction = factor(Jurisdiction,levels = juriorder)) %>%
  arrange(Jurisdiction) %>%
  #select(Jurisdiction, Population,Total_tests_week, Total_pos_week, Percent_pos_week, Num_neg_per_pos, Interpretation,Avg_tests_per_day, Avg_tests_per_1000pop) %>%
  rename(`Province/Territory`=Jurisdiction,
         `Total number of tests performed` = cumulative_tests,
         `7 day difference` = week_tests_performed,
         `Weekly percentage of tests positive`= week_percent_positive,
         `Average # of tests performed daily`= Avg_tests_per_day,
         `Average # of tests performed daily per 1 000 pop'n` = Avg_tests_per_1000pop)

write_csv(Final_weekly_table,paste0("Y:\PHAC\IDPCB\CIRID\VIPS-SAR\EMERGENCY PREPAREDNESS AND RESPONSE HC4\EMERGENCY EVENT\WUHAN UNKNOWN PNEU - 2020\EPI SUMMARY\Trend analysis\_Current\Weekly Epi Report\Weekly_table_",Today,".csv"))
