###############################################################################
#  Updated version of the code for the analysis in:                           #
#                                                                             #
#  Climate change-induced extreme weather events alter the frequency of       #
#  firearm incidents and child maltreatment cases in Wayne County, Michigan   #
#                                                                             #
#  Update: 23 September 2024                                                  #
###############################################################################

###############################################################################
# 03 FIGURES                                                                  # 
###############################################################################

data <- data_limit_1823

# Extract dates where weathertoplot == 1
weather_dates <- subset(data, storm==1)$Date

######################################
# DESCRIPTIVE FIGURES FROM MODELS    #
######################################

# Note: Run model from 02.analysis code using data_limit_1823

predicted_fa <- predict.glm(model_fa, se=T, data.frame=data_forpred, type="response")
newdf_fa <- cbind(data.frame(predicted_fa$fit), data %>% filter(day_index >= 1469))

predicted_mtxt <- predict.glm(model_mtxt, se=T, data.frame=data_forpred, type="response")
newdf_mtxt <- cbind(data.frame(predicted_mtxt$fit),data%>%filter(day_index>=1469))


# faceted plot

# firearm data
newdf_fa2 <- newdf_fa %>%
  select(Date, incidents, predicted_fa.fit)
newdf_fa2 <- newdf_fa2 %>%
  rename(observed = incidents)
newdf_fa2 <- newdf_fa2 %>%
  rename(predicted = predicted_fa.fit)
newdf_fa2 <- newdf_fa2 %>%
  mutate(outcome = "Firearm violence incidents")

# maltreatment data
newdf_mtxt2 <- newdf_mtxt %>%
  select(Date, substantiate, predicted_mtxt.fit)
newdf_mtxt2 <- newdf_mtxt2 %>%
  rename(observed = substantiate)
newdf_mtxt2 <- newdf_mtxt2 %>%
  rename(predicted = predicted_mtxt.fit)
newdf_mtxt2 <- newdf_mtxt2 %>%
  mutate(outcome = "Child maltreatment cases")

#append data
newdf <- newdf_fa2 %>%
  bind_rows(newdf_mtxt2)

newdf$outcome<-factor(newdf$outcome,
                      levels = c("Firearm violence incidents", 
                                 "Child maltreatment cases"))

p <- 
  ggplot(newdf) + 
  geom_vline(xintercept = as.numeric(weather_dates), color = "gray", 
             linetype = "solid", linewidth = 0.05) +
  geom_line(aes(x = Date, y = observed, color = "Observed"), linewidth = 0.01) +
  geom_line(aes(x = Date, y = predicted, color = "Predicted"), linewidth = 0.5) +
  ylab("Outcome count") +
  xlab("Year") +
  scale_y_continuous(limits = c(0, 35)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("#D55E00", "#56B4E9"), 
                     labels = c("Observed", 
                                "Model predicted")) +
  labs(color = "") + 
  theme(panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "bottom",
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 11)) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  facet_grid(rows = vars(outcome)) +
  theme(strip.text = element_text(size = 14))
p


