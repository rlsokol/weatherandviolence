###############################################################################
#  Updated version of the code for the analysis in:                           #
#                                                                             #
#  Climate change-induced extreme weather events alter the frequency of       #
#  firearm incidents and child maltreatment cases in Wayne County, Michigan   #
#                                                                             #
#  Update: 23 September 2024                                                  #
###############################################################################

###############################################################################
# 02 ANALYSES                                                                 # 
###############################################################################



################
# SELECT DATA  #
################

# comment out unused datasets

data <- data_limit_1823
#data <- data_limit_1819
#data <- data_limit_onset
#data <- data_limit_2223

#data <- data_unlimit_1823
#data <- data_unlimit_1819
#data <- data_unlimit_onset
#data <- data_unlimit_2223

#data <- data_flood_1823
#data <- data_flood_1819
#data <- data_flood_onset
#data <- data_flood_2223 #(too few floods in 2022-23; n=1)

#data <- data_wind_1823
#data <- data_wind_1819
#data <- data_wind_onset #(too few wind events in 2020-21; n=5)
#data <- data_wind_2223

#data <- data_limit_2022

  # data descriptives
  mean(data$incidents)
  mean(data$substantiate)
  sum(data$storm)
  sum(data$substantiate)
  sum(data$incidents)
  
  data_yr<-data%>%
    group_by(year)%>%
    summarise(n_storm = sum(storm))

#####################
# DEFINE ARGUMENTS  #
#####################

# comment out arguments for unused datasets
  # notation:
    # n_y = number of years included
    # df_y = knots per year for seasonal and long term trend
    # n_lag = days of lag

# MAIN ANALYSES

  # main analyses, 6 years (data_limit_1823, data_unlimit_1823, 
  #                         data_flood_1823, data_wind_1823)
   argvar_extremeweather<-list(fun="lin")
   arglag_extremeweather<-list(fun="poly",degree=3)  
   n_y = 6
   df_y = 7
   n_lag = 7
  
  # main analyses, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223,
  #                         data_flood_1819, data_wind_1819,
  #                         data_flood_2223, data_wind_2223)
  # argvar_extremeweather<-list(fun="lin")
  # arglag_extremeweather<-list(fun="poly",degree=3)  
  # n_y = 2
  # df_y = 7
  # n_lag = 7
    
  # main analyses, 1 year (data_limit_onset, data_unlimit_onset,
  #                        data_flood_onset, data_wind_onset,
  #                        data_limit_2022)
  # argvar_extremeweather<-list(fun="lin")
  # arglag_extremeweather<-list(fun="poly",degree=3) 
  # n_y = 1
  # df_y = 7
  # n_lag = 7
  
# SENSITIVITY: QUADRATIC CROSS BASIS

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=2)  
  #  n_y = 6
  #  df_y = 7
  #  n_lag = 7
   
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=2)  
  #  n_y = 2
  #  df_y = 7
  #  n_lag = 7
    
  #  sensitivity, 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=2) 
  #  n_y = 1
  #  df_y = 7  
  #  n_lag = 7
    
    
# SENSITIVITY: QUARTIC CROSS BASIS

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=4)  
  #  n_y = 6
  #  df_y = 7
  #  n_lag = 7
  
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  # argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=4)  
  #  n_y = 2
  #  df_y = 7
  #  n_lag = 7
    
  #  sensitivity 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=4) 
  #  n_y = 1
  #  df_y = 7 
  #  n_lag = 7
    
    
# SENSITIVITY ANALYSES: 6 knots/yr

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 6
  #  df_y = 6
  #  n_lag = 7
  
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 2
  #  df_y = 6
  #  n_lag = 7
    
  #  sensitivity, 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3) 
  #  n_y = 1
  #  df_y = 6
  #  n_lag = 7
    
# SENSITIVITY ANALYSES: 8 knots/yr

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 6
  #  df_y = 8
  #  n_lag = 7
  
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 2
  #  df_y = 8
  #  n_lag = 7
     
  #  sensitivity, 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3) 
  #  n_y = 1
  #  df_y = 8
  #  n_lag = 7
   
# SENSITIVITY ANALYSES: 5 day lag

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 6
  #  df_y = 7
  #  n_lag = 5
  
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 2
  #  df_y = 7
  #  n_lag = 5
     
  #  sensitivity, 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3) 
  #  n_y = 1
  #  df_y = 7
  #  n_lag = 5
   
# SENSITIVITY ANALYSES: 10 day lag

  #  sensitivity, 6 years (data_limit_1823, data_unlimit_1823)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 6
  #  df_y = 7
  #  n_lag = 10
  
  #  sensitivity, 2 years (data_limit_1819, data_unlimit_1819, 
  #                         data_limit_2223, data_unlimit_2223)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3)  
  #  n_y = 2
  #  df_y = 7
  #  n_lag = 10
     
  #  sensitivity, 1 year (data_limit_onset, data_unlimit_onset)
  #  argvar_extremeweather<-list(fun="lin")
  #  arglag_extremeweather<-list(fun="poly",degree=3) 
  #  n_y = 1
  #  df_y = 7
  #  n_lag = 10
  
#######################
# CREATE CROSS-BASES  #
#######################

cb1.exweath<- crossbasis(data$storm, lag=n_lag, argvar=argvar_extremeweather,
                                                  arglag=arglag_extremeweather)

cb2.temp<- crossbasis(data$max_temp_c, lag=0, argvar=list(fun="ns", df=4),
                                                  arglag=list(fun="lin"))


##########################
# FIREARM VIOLENCE MODEL #
##########################
  
  # run the model      
  model_fa <- glm(incidents ~ 
                 cb1.exweath +
                 cb2.temp +
                 ns(day_index, df_y*n_y) +  
                 weekday,
                 family=quasipoisson(), data)
  residuals <- residuals(model_fa,type="partial")
  pacf(residuals)

  # reduce the model
  cp_fa <- crosspred(cb1.exweath,model_fa,at=0:1,bylag=0.2, cumul=TRUE)
  
  # plot the lag-specific results
  plot(cp_fa, "slices", var=1, col=3, ylab="Risk Ratio", xlab="Lag (in days)", ylim = c(0, 2),
       ci.arg=list(density=15,lwd=2),
       main="Lag−response curve of the association between extreme weather and firearm violence")
  
  # plot the cumulative effects
  # Increase left margin on plot (col=4 for 2018-2023; col=2 for all else)
  par(mar = c(5, 5, 1, 1) + 0.1)  # Increase left margin
  plot(cp_fa, "slices", var=1, col=2, cumul=TRUE, ylab="Cumulative RR", xlab="Lag (in days)", 
       ylim = c(0, 2),
       cex.axis = 2, cex.lab = 2,
       lwd = 5)
  abline(h = 1, lwd = 2) 
  
  # obtain cumulative RR and 95% CI
  cp_fa$allRRfit["1"]
  cbind(cp_fa$allRRlow, cp_fa$allRRhigh)["1",]
  
  # plot residuals
  residuals <- residuals(model_fa,type="partial")
  pacf(residuals)
  
  # global backward attributable risk of extreme weather (number and fraction)
  attrdl(data$storm, cb1.exweath, data$incidents, model_fa, type = "an", cen=0, dir="back")
  attrdl(data$storm, cb1.exweath, data$incidents, model_fa, type = "af", cen=0, dir="back")
  
  # empirical confidence intervals 
  afsim <- attrdl(data$storm, cb1.exweath, data$incidents, model_fa, 
                  type = "an", dir="back",
                  sim=TRUE, nsim=1000, cen=0) 
  quantile(afsim,c(2.5,97.5)/100)
  
  afsim <- attrdl(data$storm, cb1.exweath, data$incidents, model_fa, 
                  type = "af", dir="back",
                  sim=TRUE, nsim=1000, cen=0) 
  quantile(afsim,c(2.5,97.5)/100)
  
############################
# CHILD MALTREATMENT MODEL #
############################
  
  # run the model      
  model_mtxt <- glm(substantiate ~ 
                 cb1.exweath +
                 cb2.temp +
                 ns(day_index, df_y*n_y) +  
                 weekday,
                 family=quasipoisson(), data)
  residuals <- residuals(model_mtxt,type="partial")
  pacf(residuals)
  
  # reduce the model
  cp_mtxt <- crosspred(cb1.exweath,model_mtxt,at=0:1,bylag=0.2, cumul=TRUE)
  
  # plot the lag-specific results
  plot(cp_mtxt, "slices", var=1, col=3, ylab="Risk Ratio", xlab="Lag (in days)", ylim = c(0, 2),
       ci.arg=list(density=15,lwd=2),
       main="Lag−response curve of the association between extreme weather and firearm violence")
  
  # plot the cumulative effects
  # Increase left margin on plot (col=4 for 2018-2023; col=2 for all else)
  par(mar = c(5, 5, 1, 1) + 0.1)  # Increase left margin
  plot(cp_mtxt, "slices", var=1, col=2, cumul=TRUE, ylab="Cumulative RR", xlab="Lag (in days)", 
       ylim = c(0, 2),
       cex.axis = 2, cex.lab = 2,
       lwd = 5)
  abline(h = 1, lwd = 2) 
  
  # obtain cumulative RR and 95% CI
  cp_mtxt$allRRfit["1"]
  cbind(cp_mtxt$allRRlow, cp_mtxt$allRRhigh)["1",]
  
  # plot residuals
  residuals <- residuals(model_mtxt,type="partial")
  pacf(residuals)
  
  # global backward attributable risk of extreme weather (number and fraction)
  attrdl(data$storm, cb1.exweath, data$substantiate, model_mtxt, type = "an", cen=0, dir="back")
  attrdl(data$storm, cb1.exweath, data$substantiate, model_mtxt, type = "af", cen=0, dir="back")
  
  # empirical confidence intervals 
  afsim <- attrdl(data$storm, cb1.exweath, data$substantiate, model_mtxt, 
                  type = "an", dir="back",
                  sim=TRUE, nsim=1000, cen=0) 
  quantile(afsim,c(2.5,97.5)/100)
  
  afsim <- attrdl(data$storm, cb1.exweath, data$substantiate, model_mtxt, 
                  type = "af", dir="back",
                  sim=TRUE, nsim=1000, cen=0) 
  quantile(afsim,c(2.5,97.5)/100)

#################################
# UNASSIGNED MALTREATMENT MODEL #
#################################
  
  # run the model      
  model_unas <- glm(falsepos_sub ~ 
                 cb1.exweath +
                 cb2.temp +
                 ns(day_index, df_y*n_y) +  
                 weekday,
                 family=quasipoisson(), data)
  residuals <- residuals(model_unas,type="partial")
  pacf(residuals)
  
  # reduce the model
  cp_unas <- crosspred(cb1.exweath,model_unas,at=0:1,bylag=0.2, cumul=TRUE)
  
  # plot the lag-specific results
  plot(cp_unas, "slices", var=1, col=3, ylab="Risk Ratio", xlab="Lag (in days)", ylim = c(0, 2),
       ci.arg=list(density=15,lwd=2),
       main="Lag−response curve of the association between extreme weather and unassigned child maltreatment cases")
  
  # plot the cumulative effects
  # Increase left margin on plot
  par(mar = c(5, 5, 1, 1) + 0.1)  # Increase left margin
  plot(cp_unas, "slices", var=1, col=2, cumul=TRUE, ylab="Cumulative RR", xlab="Lag (in days)", 
       ylim = c(0, 2),
       cex.axis = 2, cex.lab = 2,
       lwd = 5)
  abline(h = 1, lwd = 2) 
  
  # obtain cumulative RR and 95% CI
  cp_unas$allRRfit["1"]
  cbind(cp_unas$allRRlow, cp_unas$allRRhigh)["1",]

  
 
