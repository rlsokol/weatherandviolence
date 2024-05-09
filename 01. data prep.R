###############################################################################
#  Updated version of the code for the analysis in:                           #
#                                                                             #
#   "Extreme weather and violence:                                            #
#    A case study in Wayne County, Michigan, United States"                   #
#                                                                             #
#  Update: 11 April 2024                                                      #
###############################################################################

###############################################################################
# 01 LOAD AND CLEAN THE DATASETS                                              # 
###############################################################################

# LOAD PACKAGES 
library(dplyr); library(dlnm); library(tidyr); library(purrr); library(splines);
library(ggplot2)
library(lubridate)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

##############
# LOAD DATA  #
##############

##############
# Storm data #
##############

# load data
storm1 <-read.csv("C:/Users/rlsokol/Dropbox (University of Michigan)/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Storm Data/Wayne County, MI/Wayne_storm.csv")
storm2 <-read.csv("C:/Users/rlsokol/Dropbox (University of Michigan)/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Storm Data/Wayne County, MI/Wayne_county_mi_storm2.csv")

storm <- storm1 %>%
  bind_rows(storm2)
storm <- storm %>%
  mutate(County = "Wayne County", storm = 1)
table(storm$EVENT_TYPE)

# put date in format YYYY-MM-DD
storm$Date_begin <- as.Date(storm$BEGIN_DATE, format = "%m/%d/%Y")
storm$Date_end <- as.Date(storm$END_DATE, format = "%m/%d/%Y")

    # restrict dates to 2018-2023 for extreme weather type frequency
    storm_restrict <- storm %>%
      filter(Date_begin>="2018-01-01" & Date_begin<="2023-12-31")
    table(storm_restrict$EVENT_TYPE)
    
    #remove records with same Date_begin and EVENT_TYPE
    storm_restrict <- storm_restrict %>%
      select(Date_begin, EVENT_TYPE)
    storm_restrict_dist <- distinct(storm_restrict)
    
    # frequency by year
    storm_restrict_dist$year <- year(storm_restrict_dist$Date_begin)
    table(storm_restrict_dist$EVENT_TYPE, storm_restrict_dist$year)
    table(storm_restrict_dist$EVENT_TYPE)

# keep relevant vars
storm <- storm %>%
  select(County, Date_begin, Date_end, storm)

# storm duration

  # calculate the number of days between Date_begin and Date_end
  storm$days_between <- ((storm$Date_end - storm$Date_begin) + 1)
  
  #limit max duration of storm to 7
  storm <- storm %>%
    mutate(days_btw = case_when(
      is.na(days_between) ~ NA,
      days_between==1 ~ 1,
      days_between==2 ~ 2,
      days_between==3 ~ 3,
      days_between==4 ~ 4,
      days_between==5 ~ 5,
      days_between==6 ~ 6,
      days_between>=7 ~ 7,
    ))

# expand the records for incidents that last multiple days, limited to 7 days duration
expanded_df_limit <- storm %>%
  mutate(Date = map2(Date_begin, days_btw, ~ seq(as.Date(.x), length.out = .y, by = "day"))) %>%
  unnest(Date)

  # keep relevant vars
  expanded_df_limit<-expanded_df_limit %>%
    select(Date, County, storm, days_between, days_btw)

  # Keep only one occurrence of each unique date observation
  storm_final_limit<- expanded_df_limit[!duplicated(expanded_df_limit$Date), ]

# expand the records for incidents that last multiple days, unlimited duration
expanded_df_unlimit <- storm %>%
  mutate(Date = map2(Date_begin, days_between, ~ seq(as.Date(.x), length.out = .y, by = "day"))) %>%
  unnest(Date)

  # keep relevant vars
  expanded_df_unlimit<-expanded_df_unlimit %>%
    select(Date, County, storm, days_between, days_btw)

  # keep only one occurrence of each unique date observation
  storm_final_unlimit<- expanded_df_unlimit[!duplicated(expanded_df_unlimit$Date), ]

  str(expanded_df_limit)
  

####################
# Temperature data #
####################

# load data
temp1 <-read.csv("C:/Users/rlsokol/Dropbox (University of Michigan)/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Weather Station Data/Wayne County, MI/Wayne_County_mi_daily.csv")
temp2 <-read.csv("C:/Users/rlsokol/Dropbox (University of Michigan)/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Weather Station Data/Wayne County, MI/Wayne_county_mi_daily2_TMAX.csv")

temp <- temp1 %>%
  bind_rows(temp2)
temp <- temp %>%
  mutate(County = "Wayne County")
temp <- temp %>%
  mutate(Date = as.Date(DATE))
temp <- temp %>%
  select(County, Date, TMAX)
temp <- temp %>%
  group_by(Date, County) %>%
  summarise(max_temp = mean(TMAX, na.rm=TRUE))

# convert to C
temp <- temp %>%
  mutate(max_temp_c = (max_temp-32)*(5/9))

######################
# Gun violence data  #
######################

# load data
gva <- read.csv("C:/Users/rlsokol/Dropbox (University of Michigan)/GVA Incidents/GVA 2014-2023.csv")

# restrict to Wayne County; incidents with injury or death
gva<-gva %>%
  filter((County=="Wayne County") &
          State=="Michigan" & 
          (Injured>=1 | Killed>=1))

# calculate number of incidents per day
gva<-gva %>%
  group_by(Date, County) %>%
  summarise(incidents = n())
gva<-gva %>%
  mutate(Date=as.Date(Date))

################## 
# Calendar data  #
##################

# create new dataset with every day from 1-1-14 to 12-31-23
library(lubridate)
date_sequence <- seq(from = ymd("2014-01-01"), to = ymd("2023-12-31"), by = "days")
date_dataframe <- data.frame(Date = date_sequence)

# create time index variable for each day of year from 2014-2023
date_dataframe <- date_dataframe %>%
  arrange(Date) %>%
  mutate(day_index = row_number())
  #add county
  date_dataframe <- date_dataframe %>%
    mutate(County = "Wayne County")

##################################
# Child maltreatment data        # 
##################################

# read in substantiation data
substantiate<- read.csv("J:/DataLab_Sandbox/rebeccah/weather study/daily_substantiate.csv")

substantiate$Date<-as.Date(substantiate$complaint_date)
substantiate<-substantiate %>%
  rename(County=county_name)
substantiate<-substantiate %>%
  filter(County=="Wayne")
substantiate <- substantiate %>%
  mutate(County = case_when(
    County == "Wayne" ~ "Wayne County"))

substantiate <- substantiate %>%
  select(Date, County, substantiate)

# read in allegation data
alleg<- read.csv("J:/DataLab_Sandbox/rebeccah/weather study/daily_complaints.csv")
alleg$Date<-as.Date(alleg$complaint_date)
alleg<-alleg %>%
  rename(County=county_name)
alleg<-alleg %>%
  filter(County=="Wayne")
alleg<-alleg %>%
  rename(mtxt_numb=n)
alleg <- alleg %>%
  mutate(County = case_when(
    County == "Wayne" ~ "Wayne County"))

alleg <- alleg %>%
  select(Date, County, mtxt_numb)

# read in unassigned data
unassigned<- read.csv("J:/DataLab_Sandbox/rebeccah/weather study/daily_unassigned.csv")

unassigned$Date<-as.Date(unassigned$complaint_date)
unassigned<-unassigned %>%
  rename(County=county_name)
unassigned<-unassigned %>%
  filter(County=="Wayne")
unassigned <- unassigned %>%
  mutate(County = case_when(
    County == "Wayne" ~ "Wayne County"))
unassigned <- unassigned %>%
  filter(Date>="2014-01-01" & Date<="2023-12-31")

unassigned <- unassigned %>%
  select(Date, County, unassigned_number)

# read in hotline data
hotline<- read.csv("J:/DataLab_Sandbox/rebeccah/weather study/daily_hotline.csv")

hotline$Date<-as.Date(hotline$complaint_date)
hotline<-hotline %>%
  rename(County=county_name)
hotline<-hotline %>%
  filter(County=="Wayne")
hotline <- hotline %>%
  mutate(County = case_when(
    County == "Wayne" ~ "Wayne County"))
hotline <- hotline %>%
  filter(Date>="2014-01-01" & Date<="2023-12-31")

hotline <- hotline %>%
  select(Date, County, hotline_number)

####################
# MERGE DATASETS   #
####################
  
# merge date and gva
gva_date <- gva %>%
  right_join(date_dataframe, by=c("Date", "County"))
gva_date$incidents[is.na(gva_date$incidents)] <- 0

gva_date <- gva_date %>%
  mutate(Date=as.Date(Date),
         doy = yday(Date),
         month = month(Date),
         year = year(Date))
gva_date$weekday <- weekdays(gva_date$Date)

# merge gva_date to substantiate data
gva_date_subst <- gva_date %>%
  left_join(substantiate, by=c("Date", "County"))

# merge gva_date_subst to allegation data
gva_date_subst_alleg <- gva_date_subst %>%
  left_join(alleg, by=c("Date", "County"))

# merge gva_date_subst_alleg to unassigned data
gva_date_subst_alleg_unas <- gva_date_subst_alleg %>%
  left_join(unassigned, by=c("Date", "County"))

# merge gva_date_subst_alleg_unas to hotline data
gva_date_mtxt <- gva_date_subst_alleg_unas %>%
  left_join(hotline, by=c("Date", "County"))

# merge gva_date_mtxt to temp data
gva_date_mtxt_temp <- gva_date_mtxt %>%
  left_join(temp, by=c("Date", "County"))

#Create proportion unassigned
gva_date_mtxt_temp$unassigned_number[is.na(gva_date_mtxt_temp$unassigned_number)] <- 0
gva_date_mtxt_temp <- gva_date_mtxt_temp %>%
  mutate(falsepos = unassigned_number/(unassigned_number + mtxt_numb))
gva_date_mtxt_temp$falsepos[is.na(gva_date_mtxt_temp$falsepos)] <- 0

# merge gva_date_mtxt_temp to storm data

  # limit max duration of storm to 7
  data_limit <- gva_date_mtxt_temp %>%
    left_join(storm_final_limit, by=c("Date", "County"))

    # replace NA with 0
    data_limit$storm[is.na(data_limit$storm)] <- 0
    data_limit$substantiate[is.na(data_limit$substantiate)] <- 0
    data_limit$unassigned_number[is.na(data_limit$unassigned_number)] <- 0
  
  # unlimit max duration of storm to 7
  data_unlimit <- gva_date_mtxt_temp %>%
    left_join(storm_final_unlimit, by=c("Date", "County"))
  
    # replace NA with 0
    data_unlimit$storm[is.na(data_unlimit$storm)] <- 0
    data_unlimit$substantiate[is.na(data_unlimit$substantiate)] <- 0
    data_unlimit$unassigned_number[is.na(data_unlimit$unassigned_number)] <- 0

    
    
    
    
#########################
# STRATIFY BY YEARS     #
#########################
    
# total study period, 2018-2023
    
  # limit max duration of storm to 7 
    data_limit_1823<- data_limit %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")

  # limit max duration of storm to 7 
    data_unlimit_1823<- data_unlimit %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")
    
    
# pre-onset, 2018-2019
    
  # limit max duration of storm to 7 
    data_limit_1819<- data_limit %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")

  # limit max duration of storm to 7 
    data_unlimit_1819<- data_unlimit %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")    
    
    
# onset, March 2020-2021
    
  # limit max duration of storm to 7 
    data_limit_onset<- data_limit %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")

  # limit max duration of storm to 7 
    data_unlimit_onset<- data_unlimit %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")
    
# post-onset, 2022-2023
    
  # limit max duration of storm to 7 
    data_limit_2223<- data_limit %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")

  # limit max duration of storm to 7 
    data_unlimit_2223<- data_unlimit %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")

# retain final datasets
  
rm(list=ls()[! ls() %in% c("data_limit_1823","data_unlimit_1823",
                           "data_limit_1819", "data_unlimit_1819",
                           "data_limit_onset", "data_unlimit_onset",
                           "data_limit_2223", "data_unlimit_2223")])
