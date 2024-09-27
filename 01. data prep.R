###############################################################################
#  Updated version of the code for the analysis in:                           #
#                                                                             #
#  Climate change-induced extreme weather events alter the frequency of       #
#  firearm incidents and child maltreatment cases in Wayne County, Michigan   #
#                                                                             #
#  Update: 23 September 2024                                                  #
###############################################################################

###############################################################################
# 01 LOAD AND CLEAN THE DATASETS                                              # 
###############################################################################

# LOAD PACKAGES 
library(dplyr); library(dlnm); library(tidyr); library(purrr); library(splines);
library(ggplot2)
library(lubridate)
library(misty)
library(FluMoDL) #for attributable number

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
storm1 <-read.csv("C:/Users/rlsokol/University of Michigan Dropbox/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Storm Data/Wayne County, MI/wayne_storm.csv")
storm2 <-read.csv("C:/Users/rlsokol/University of Michigan Dropbox/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Storm Data/Wayne County, MI/Wayne_county_mi_storm2.csv")

storm <- storm1 %>%
  bind_rows(storm2)
storm <- storm %>%
  mutate(County = "Wayne County", storm = 1)
table(storm$EVENT_TYPE)

# put date in format YYYY-MM-DD
storm$Date_begin <- as.Date(storm$BEGIN_DATE, format = "%m/%d/%Y")
storm$Date_end <- as.Date(storm$END_DATE, format = "%m/%d/%Y")

# restrict dates to 2018-2023 for extreme weather type frequency
storm <- storm %>%
  filter(Date_begin>="2018-01-01" & Date_begin<="2023-12-31")
table(storm$EVENT_TYPE)

# remove lakeshore flooding for sensitivity analyses (comment out for main analyses)
# storm <- storm %>%
#  filter(EVENT_TYPE!="Lakeshore Flood")

# generate storm type indicators
storm <- storm %>%
  mutate(Wind=case_when(
                        EVENT_TYPE == "High Wind" ~ 1,
                        EVENT_TYPE == "Thunderstorm Wind" ~ 1,
                        EVENT_TYPE == "Tornado" ~ 1, 
                        .default = as.numeric(0)
  ))
storm <- storm %>%
  mutate(Flood=case_when(
                        EVENT_TYPE == "Flash Flood" ~ 1,
                        EVENT_TYPE == "Flood" ~ 1,
                        EVENT_TYPE == "Lakeshore Flood" ~ 1, 
                        .default = as.numeric(0)
  ))
storm <- storm %>%
  mutate(Winter_weather=case_when(
                        EVENT_TYPE == "Cold/Wind Chill" ~ 1,
                        EVENT_TYPE == "Heavy Snow" ~ 1, 
                        EVENT_TYPE == "Ice Storm" ~ 1, 
                        EVENT_TYPE == "Winter Storm" ~ 1,
                        .default = as.numeric(0)
  ))
storm <- storm %>%
  mutate(Heat=case_when(
                        EVENT_TYPE == "Heat" ~ 1,
                        .default = as.numeric(0)
  ))
storm <- storm %>%
  mutate(Hail=case_when(
                        EVENT_TYPE == "Hail" ~ 1,
                        .default = as.numeric(0)
  ))

    
    
    #remove records with same Date_begin and event (i.e., multiple locations)
    storm_restrict <- storm %>%
      select(Date_begin, Date_end, storm, Wind, Flood, Winter_weather, Heat, Hail, County)
    storm_restrict <- distinct(storm_restrict)
      # Check duplicate storms
        storm_restrict_dup<- df.duplicated(Date_begin, data=storm_restrict)
    
    # frequency by year
        storm_restrict$year <- year(storm_restrict$Date_begin)
    
        table(storm_restrict$Wind, storm_restrict$year)
        table(storm_restrict$Wind)
        
        table(storm_restrict$Flood, storm_restrict$year)
        table(storm_restrict$Flood)
        
        table(storm_restrict$Winter_weather, storm_restrict$year)
        table(storm_restrict$Winter_weather)
        
        table(storm_restrict$Heat, storm_restrict$year)
        table(storm_restrict$Heat)
        
        table(storm_restrict$Hail, storm_restrict$year)
        table(storm_restrict$Hail)


# keep relevant vars
storm_restrict <- storm_restrict %>%
  select(County, Date_begin, Date_end, storm, Wind, Flood, Winter_weather, Heat, Hail)

# storm duration

  # calculate the number of days between Date_begin and Date_end
  storm_restrict$days_between <- ((storm_restrict$Date_end - storm_restrict$Date_begin) + 1)
  
  #limit max duration of storm to 7
  storm_restrict <- storm_restrict %>%
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

# STORM FINAL DATA (regardless of incident type): expand the records for incidents that last multiple days, limited to 7 days duration
expanded_df_limit <- storm_restrict %>%
  mutate(Date = map2(Date_begin, days_btw, ~ seq(as.Date(.x), length.out = .y, by = "day"))) %>%
  unnest(Date)

  # Keep only one occurrence of each unique date observation
  storm_final_limit<- expanded_df_limit[!duplicated(expanded_df_limit$Date), ]

# STORM FINAL DATA, SENSITIVITY: expand the records for incidents that last multiple days, unlimited duration
expanded_df_unlimit <- storm_restrict %>%
  mutate(Date = map2(Date_begin, days_between, ~ seq(as.Date(.x), length.out = .y, by = "day"))) %>%
  unnest(Date)

  # keep only one occurrence of each unique date observation
  storm_final_unlimit<- expanded_df_unlimit[!duplicated(expanded_df_unlimit$Date), ]

# WIND FINAL DATA
wind_final <- expanded_df_limit %>%
  filter(Wind==1)

  # Keep only one occurrence of each unique date observation
  wind_final<- wind_final[!duplicated(wind_final$Date), ]

# FLOOD FINAL DATA
flood_final <- expanded_df_limit %>%
  filter(Flood==1)

  # Keep only one occurrence of each unique date observation
  flood_final<- flood_final[!duplicated(flood_final$Date), ]
    
####################
# Temperature data #
####################

# load data
temp1 <-read.csv("C:/Users/rlsokol/University of Michigan Dropbox/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Weather Station Data/Wayne County, MI/Wayne_County_mi_daily.csv")
temp2 <-read.csv("C:/Users/rlsokol/University of Michigan Dropbox/SSW-Partners for Youth Safety and Well-Being/Climate change and violence/Climate Hazards and Firearm Violence/DATA/County Weather Station Data/Wayne County, MI/Wayne_county_mi_daily2_TMAX.csv")

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
gva <- read.csv("C:/Users/rlsokol/University of Michigan Dropbox/Rebeccah Sokol/GVA Incidents/GVA 2014-2023.csv")

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

# merge gva_date_mtxt to temp data
gva_date_mtxt_temp <- gva_date_subst_alleg %>%
  left_join(temp, by=c("Date", "County"))

#Create proportion unconfirmed 
gva_date_mtxt_temp$substantiate[is.na(gva_date_mtxt_temp$substantiate)] <- 0
gva_date_mtxt_temp <- gva_date_mtxt_temp %>%
  mutate(falsepos_sub = (mtxt_numb-substantiate)/(mtxt_numb))
gva_date_mtxt_temp$falsepos_sub[is.na(gva_date_mtxt_temp$falsepos_sub)] <- 0

# merge gva_date_mtxt_temp to storm data

  # limit max duration of storm to 7
  data_limit <- gva_date_mtxt_temp %>%
    left_join(storm_final_limit, by=c("Date", "County"))

    # replace NA with 0
    data_limit$storm[is.na(data_limit$storm)] <- 0
    data_limit$substantiate[is.na(data_limit$substantiate)] <- 0
  
  # unlimit max duration of storm to 7
  data_unlimit <- gva_date_mtxt_temp %>%
    left_join(storm_final_unlimit, by=c("Date", "County"))
  
    # replace NA with 0
    data_unlimit$storm[is.na(data_unlimit$storm)] <- 0
    data_unlimit$substantiate[is.na(data_unlimit$substantiate)] <- 0
    
  # WIND: limit max duration of storm to 7
  data_wind <- gva_date_mtxt_temp %>%
    left_join(wind_final, by=c("Date", "County"))

    # replace NA with 0
    data_wind<-data_wind %>%
      mutate(storm=case_when(
                             Wind == 1 ~ 1,
                             is.na(Wind) ~ 0,
                             Wind == 0 ~ 0
      ))
    data_wind$Wind[is.na(data_wind$Wind)] <- 0
    data_wind$Flood[is.na(data_wind$Flood)] <- 0
    data_wind$Hail[is.na(data_wind$Hail)] <- 0
    data_wind$Heat[is.na(data_wind$Heat)] <- 0
    data_wind$Wind[is.na(data_wind$Wind)] <- 0
    data_wind$Winter_weather[is.na(data_wind$Winter_weather)] <- 0
    data_wind$substantiate[is.na(data_wind$substantiate)] <- 0
    
  # FLOOD: limit max duration of storm to 7
  data_flood <- gva_date_mtxt_temp %>%
    left_join(flood_final, by=c("Date", "County"))

    # replace NA with 0
    data_flood<-data_flood %>%
      mutate(storm=case_when(
                             Flood == 1 ~ 1,
                             is.na(Flood) ~ 0,
                             Flood == 0 ~ 0
                            ))
    data_flood$Wind[is.na(data_flood$Wind)] <- 0
    data_flood$Flood[is.na(data_flood$Flood)] <- 0
    data_flood$Hail[is.na(data_flood$Hail)] <- 0
    data_flood$Heat[is.na(data_flood$Heat)] <- 0
    data_flood$Wind[is.na(data_flood$Wind)] <- 0
    data_flood$Winter_weather[is.na(data_flood$Winter_weather)] <- 0
    data_flood$substantiate[is.na(data_flood$substantiate)] <- 0
    
#########################
# STRATIFY BY YEARS     #
#########################
    
# total study period, 2018-2023
    
  # limit max duration of storm to 7 
    data_limit_1823<- data_limit %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")

  # unlimit max duration of storm 
    data_unlimit_1823<- data_unlimit %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")
    
  # wind, limit max duration of storm to 7 
    data_wind_1823<- data_wind %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")
    
  # flood, limit max duration of storm to 7 
    data_flood_1823<- data_flood %>%
      filter(Date>="2018-01-01" & Date<="2023-12-31")
    
# pre-onset, 2018-2019
    
  # limit max duration of storm to 7 
    data_limit_1819<- data_limit %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")

  # unlimit max duration of storm  
    data_unlimit_1819<- data_unlimit %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")    
    
  # wind, limit max duration of storm to 7 
    data_wind_1819<- data_wind %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")
    
  # flood, limit max duration of storm to 7 
    data_flood_1819<- data_flood %>%
      filter(Date>="2018-01-01" & Date<="2019-12-31")
    
    
# onset, March 2020-2021
    
  # limit max duration of storm to 7 
    data_limit_onset<- data_limit %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")

  # unlimit max duration of storm 
    data_unlimit_onset<- data_unlimit %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")
    
  # wind, limit max duration of storm to 7 
    data_wind_onset<- data_wind %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")
    
  # flood, limit max duration of storm to 7 
    data_flood_onset<- data_flood %>%
      filter(Date>="2020-03-11" & Date<="2021-03-10")
    
# post-onset, 2022-2023
    
  # limit max duration of storm to 7 
    data_limit_2223<- data_limit %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")

  # unlimit max duration of storm 
    data_unlimit_2223<- data_unlimit %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")
    
  # wind, limit max duration of storm to 7 
    data_wind_2223<- data_wind %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")
    
  # flood, limit max duration of storm to 7 
    data_flood_2223<- data_flood %>%
      filter(Date>="2022-01-01" & Date<="2023-12-31")
    
# sensitivity, 2022 only
    
  # limit max duration of storm to 7 
    data_limit_2022<- data_limit %>%
      filter(Date>="2022-01-01" & Date<="2022-12-31")


# retain final datasets
  
rm(list=ls()[! ls() %in% c("data_limit_1823","data_unlimit_1823", "data_wind_1823", "data_flood_1823",
                           "data_limit_1819", "data_unlimit_1819","data_wind_1819", "data_flood_1819",
                           "data_limit_onset", "data_unlimit_onset","data_wind_onset", "data_flood_onset",
                           "data_limit_2223", "data_unlimit_2223", "data_wind_2223", "data_flood_2223",
                           "data_limit_2022")])
