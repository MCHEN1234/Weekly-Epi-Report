library(metabaser)
library(tidyverse)
library(ggplot2)
library(scales)

#place login credentials in quotes for username and password. alternative access method will eventually be implemented
handle<- metabaser::metabase_login(base_url = "https://discover-metabase.hres.ca/api",
                                   database_id = 2, # phac database
                                   username = "",
                                   password = "")

qry_cases_raw <- metabaser::metabase_query(handle, "select phacid, phacreporteddate, episodedate, earliestdate, pt, age_years, agegroup10, agegroup20, onsetdate, earliestlabcollectiondate, sex, gender, sexgender, coviddeath, hosp, icu, exposure_cat, exposure_linked, closecontactcase, outbreak_associated from all_cases;")

table1 <- qry_cases_raw %>%
  select(earliestdate,exposure_linked) %>%
  group_by(earliestdate) %>%
  count(exposure_linked,name="Count") 

table2 <- qry_cases_raw %>%
  select(earliestdate,closecontactcase) %>%
  group_by(earliestdate) %>%
  count(closecontactcase,name="Count")

table2$closecontactcase <- factor(table2$closecontactcase,exclude=NULL,
                                  levels=c(NA,"other","not asked","unknown","no","yes"),
                                  labels=c("NA","other","not asked","unknown","no","yes"))

table3 <- qry_cases_raw %>%
  select(earliestdate,outbreak_associated) %>%
  group_by(earliestdate) %>%
  count(outbreak_associated,name="Count")

table3$outbreak_associated <- factor(table3$outbreak_associated,exclude=NULL,
                                     levels=c(NA,"not assessed","unknown","no","yes"),
                                     labels=c("NA","not assessed","unknown","no","yes"))

plot1 <- ggplot(table1,
                aes(fill=factor(exposure_linked,levels=c("blank","unknown","3+","2","1","0")),x=earliestdate,y=Count)) +
  geom_bar(position="stack",stat="identity") +
  scale_x_date(date_breaks = ("2 months"),labels = date_format("%B %d, %Y"),expand = c(0,0)) +
  scale_y_continuous(label=comma,expand = c(0, 0), limits = c(0, NA)) +
  labs(title="Count by earliestdate: Day and exposure_linked",fill="expsure_linked") +
  theme(
    plot.title=element_text(face="bold",hjust=0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    legend.title = element_blank(),
    legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, reverse=T))
plot(plot1)

plot2 <- ggplot(table2,
                aes(fill=factor(closecontactcase,levels=c("NA","other","not asked","unknown","no","yes")),x=earliestdate,y=Count)) +
  geom_bar(position="stack",stat="identity") +
  scale_x_date(date_breaks = ("2 months"),labels = date_format("%B %d, %Y"),expand = c(0,0)) +
  scale_y_continuous(label=comma,expand = c(0, 0), limits = c(0, NA)) +
  labs(title="Count by closecontactcase responses over time",fill="closecontactcase") +
  theme(
    plot.title=element_text(face="bold",hjust=0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    legend.title = element_blank(),
    legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, reverse=T))
plot(plot2)

plot3 <- ggplot(table3,
                aes(fill=factor(outbreak_associated,levels=c("NA","not assessed","unknown","no","yes")),x=earliestdate,y=Count)) +
  geom_bar(position="stack",stat="identity") +
  scale_x_date(date_breaks = ("2 months"),labels = date_format("%B %d, %Y"),expand = c(0,0)) +
  scale_y_continuous(label=comma,expand = c(0, 0), limits = c(0, NA)) +
  labs(title="Count by earliestdate: Day and exposure_linked",fill="expsure_linked") +
  theme(
    plot.title=element_text(face="bold",hjust=0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    legend.title = element_blank(),
    legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1, reverse=T))
plot(plot3)