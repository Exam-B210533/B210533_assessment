
#---------------------------------------------
#load packages for data loading and preparation
#---------------------------------------------

#install.packages("summarytools")
#install.packages("gridGraphics")
library(NHSRdatasets)
library(tidyverse)
library(here)
library(knitr)
library(lubridate)
library(caret)
library(summarytools)

#---------------------
# Load NHSRdataset
#---------------------

# The data set is loaded from the NHSRdataset library 
data(ons_mortality)
mort <- ons_mortality
class(mort)

#--------------------------------
# Look at the ons_mortality data
#--------------------------------

# Examine the data frame 
glimpse(mort)
head(mort)
tail(mort)
# Look at the unique values in each of the Catgeory columns
unique_cat1 = unique(mort[c("category_1")])
kable(unique_cat1)
unique_cat2 = unique(mort[c("category_2")])
kable(unique_cat2)
# Look at the dates to identify the earliest and latest observations
min(mort$date, na.rm = TRUE)
max(mort$date, na.rm = TRUE)

#-------------------------------------------------------
# Create a four column table to view the category 2 data
#-------------------------------------------------------

# The Category_2 list is long and would be better presented as four columns side by side
unique_cat2 = unique(mort[c("category_2")])
unique_cat2_list <- data.frame(unique_cat2, freq=1:42)
kable(
  list(unique_cat2[1:10,], unique_cat2[11:20,],unique_cat2[21:31,],unique_cat2[32:42,]),
  booktabs = TRUE
)

#-------------------------
# Look for missing values
#-------------------------

# This code with look count the NA values in each column 
mort %>% 
  map(is.na) %>%
  map(sum)

#--------------------------------------
# Create an index column for reference
#--------------------------------------

# A row of integers is added as a new column using the data frame row number
mort <- rowid_to_column(mort, "index")

#----------------------------------------
# Tabulate the raw data and save the file
#----------------------------------------

# The kable function is used to create a table formatted for the Markdown document
kable(head(mort, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","),
      caption= "ONS England and Wales Mortality Data Full Data Set, 2010 - 2019")

# The data frame is saved to the RawData folder
write_csv(mort, here("RawData", "ons_mortality.csv"))

#---------------------------------------------------------------------
# Map the 5 year average for each week to the all ages mortality data
#---------------------------------------------------------------------

# Filter by Category_2 to create a subset for "average of same week over 5 years"
mort_5year_avg <- filter(mort,category_2 == "average of same week over 5 years")
glimpse(mort_5year_avg)
tail(mort_5year_avg)

mort_total <- filter(mort,category_1 == "Total deaths" & category_2 == "all ages")
glimpse(mort_total)

# Average data has 521 observations, while all deaths has 535 observations
# Last date for 5-year average observation is 27-12-2019, so need to remove "all deaths" observations after this date 
# Create a subset of the data for "all deaths" and "all ages" before this date
mort_all <- filter(mort,category_1 == "Total deaths" & category_2 == "all ages" & date < "2019-12-31")
glimpse(mort_all)

# Append the 5 year average column to the 
mort_all$mort_avg = mort_5year_avg$counts

#---------------------------------------------------------------------
# Calculate the variance between actual deaths and the 5 year average
#---------------------------------------------------------------------

# calculate the variance between actual and 5 year average for each week
mort_all$variance_from_avg = mort_all$counts / mort_all$mort_avg

# create a year column to support analysis by week and year
mort_all$year = as.character(year(mort_all$date))

# create subset of data with relevant data columns
mort_all <- mort_all[, c("index", "week_no", "year", "date", "counts", "mort_avg", "variance_from_avg")]
head(mort_all)

#---------------------------------------
# Check again for missing values
#---------------------------------------

mort_all %>% 
  map(is.na) %>%
  map(sum)

#---------------------------------------
# Tabulate the subset and save the file
#---------------------------------------

kable(head(mort_all, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","),
      caption= "Training Data: England and Wales Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_all, here("RawData", "ons_mortality_ENG_1019.csv"))

#---------------------------------
# Create subset for training data
#---------------------------------

# Calculate the proportion of the data subset to use for testing purposes
prop<-(1-(15/nrow(mort_all)))
print(prop)
# Set a seed to be able to replicate the random generator in future
set.seed(432)
#Partition the raw data into the test and training data.
train_index <- createDataPartition(mort_all$index, p = prop, 
                                  list = FALSE, 
                                  times = 1)
head(train_index)
# All records that are in the train_index are assigned to the training data.
mort_alltrain <- mort_all[ train_index,]
nrow(mort_alltrain)
# There are 509 records in the training set

#----------------------------------------------
# Tabulate the training data and save the file
#----------------------------------------------

kable(head(mort_alltrain, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","),
      caption= "Training Data: England and Wales Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltrain, here("Data", "ons_mortality_ENG_1019_training.csv"))

#-----------------------------
# Create subset for test data
#-----------------------------

# Allocate all records that are not assigned to the training data subset
mort_alltest  <- mort_all[-train_index,]
nrow(mort_alltest)
#There are 12 records in the test data. 

#-------------------------------------------
# Create subset for Marker to test the tool
#-------------------------------------------

# One row needs to be set aside for use by the assignment markers to test and evaluate your data-capture tool.
mort_alltestMarker  <- mort_alltest[1,]

kable(head(mort_alltestMarker, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","),
      caption= "Markers Test Data: England and Wales Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltestMarker, here("Data", "ons_mortality_ENG_1019_test_marker.csv"))

#-------------------------------------------------
# Tabulate the test data subset and save the file
#-------------------------------------------------

# Remove from the subset the row saved for the Marker
mort_alltest  <- mort_alltest[2:nrow(mort_alltest),]

kable(head(mort_alltrain, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","),
      caption= "Test Data: England and Wales Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltest, here("Data", "ons_mortality_ENG_1019_test.csv"))


#---------------------------------
#load packages for visualisation
#---------------------------------

#install.packages("gridGraphics")
#install.packages("summarytools")
library(scales)
library(cowplot)
library(rlang)
library(gridGraphics)

#----------------------------------------------------------
# Exploring seasonal differences, create seasonal datasets
#----------------------------------------------------------

# Seasons are established using the astronomical calendar using the dates of the solstice and equinox. 
# The result is that Spring (13 weeks), Summer (13 weeks), Autumn (13 weeks) and Winter (13 weeks)
mort_all$year <- as.character(mort_all$year)
mort_spring <- subset(mort_all, week_no >= 12 & week_no < 25)
mort_summer <- subset(mort_all, week_no >= 25 & week_no < 38)
mort_autumn <- subset(mort_all, week_no >= 38 & week_no < 51)
mort_winter <- subset(mort_all, week_no < 12 | week_no >= 51)

#-----------------------------------------------
# Remove unnecessary data from winter data set
#-----------------------------------------------

# The winter season runs from week 50 to week 11 inclusive. 
# Because this is split across two years it necessary to take a closer look at the data and group the weeks into the corresponding winter season. 
kable(head(mort_winter, n = 15), 
      digits = 3, 
      format.args = list(big.mark = ","))
kable(tail(mort_winter, n = 10), 
      digits = 3, 
      format.args = list(big.mark = ","))
# The tail end of winter 2009-10 and start of winter 2019-20 need to be removed
mort_winter <- mort_winter[-c(1:11),]
mort_winter <- head(mort_winter,-2)
str(mort_winter)
# There are now 118 observations from winter 2010-2011 to winter 2018-19. 
# By grouping the weeks by seasons the number of winter seasons is now 9 years, compared to 10 years spring, summer and autumn seasons. 
# This means there should be 117 observations (13 weeks by 9 years) but there are 118. Further investigation is needed.
# Use table function to look at the count of each week in the week_no column
table(mort_winter$week_no)
# This shows that there are 9 years of observations for the weeks in the winter season, however one year has 53 weeks.
# This is unexpected and need to find the 53 week observation. 
# Create a conditional subset of the data to find the specific row.  
mort_winter[which(mort_winter$week_no == 53), ]      
# 2016 has a week 53.  
# 2016 is a leap year. This combined with the specific starting date of the year, has caused the year to have 53 weeks. 
# This happens only once every 71 years. https://www.rosecalendars.co.uk/news/the-reason-and-logic-behind-this-phenomenon/ 
# To make the year comparable to the winter season in each of the other years in the winter season data frame (with equal number of observations), the 2016 winter season will include week 52-53, 2016 and week 1 - 11, 2017.
# Therefore, necessary remove the line of data for week 50, 2016. 
mort_winter_final <- subset(mort_winter, index != 8946) 
table(mort_winter_final$week_no)
# The data now has 117 observations with each years winter season of 13 weeks.  

#---------------------------------
# Save data for seasonal analysis
#---------------------------------

write_csv(mort_winter_final, here("Data", "winter_mortality.csv"))
write_csv(mort_spring, here("Data", "spring_mortality.csv"))
write_csv(mort_summer, here("Data", "summer_mortality.csv"))
write_csv(mort_autumn, here("Data","autumn_mortality.csv"))


#------------------------
# Descriptive statistics
#------------------------

# Descriptive statistics for Spring data variance
mort_spring_statdesc <- descr(
  mort_spring$variance_from_avg, 
  stats = "common"
)

# Descriptive statistics for Summer data variance
mort_summer_statdesc <- descr(
  mort_summer$variance_from_avg, 
  stats = "common"
)

# Descriptive statistics for Autumn data variance
mort_autumn_statdesc <- descr(
    mort_autumn$variance_from_avg, 
    stats = "common"
  )

# Descriptive statistics for Winter data variance
mort_winter_statdesc <- descr(
  mort_winter_final$variance_from_avg, 
  stats = "common"
)

mort_seasonalvar_statdesc <- cbind(spring = mort_spring_statdesc, summer = mort_summer_statdesc, autumn = mort_autumn_statdesc, winter = mort_winter_statdesc)
colnames(mort_seasonalvar_statdesc) <- c("spring", "summer", "autumn", "winter")    # Applying column names
mort_seasonalvar_statdesc 

kable(mort_seasonalvar_statdesc, 
      digits = 3, 
      format.args = list(big.mark = ","))

#---------------
# Spring visual
#---------------


head(mort_spring)
spring_line_plot <- ggplot(mort_spring, aes(x = week_no, y = variance_from_avg)) + 
  geom_line(aes(color = year)) +
  scale_y_continuous(labels=scales::percent, limits = c(0.6, 1.4)) +
  scale_x_continuous(breaks = pretty_breaks(n = 13)) + 
  geom_hline(yintercept=1.0, colour = "darkgrey") +
  geom_hline(yintercept=1.4, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.3, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.2, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.1, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.9, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.8, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.7, colour = "grey", linetype=2) +
  labs(title="Spring",
       y="actual as % of average", x="week number")
spring_line_plot

#---------------
# Summer visual
#---------------

tail(mort_summer)
summer_line_plot <- ggplot(mort_summer, aes(x = week_no, y = variance_from_avg)) + 
  geom_line(aes(color = year)) +
  scale_y_continuous(labels=scales::percent, limits = c(0.6, 1.4)) + 
  scale_x_continuous(breaks = pretty_breaks(n = 13)) +
  geom_hline(yintercept=1.0, colour = "darkgrey") +
  geom_hline(yintercept=1.4, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.3, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.2, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.1, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.9, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.8, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.7, colour = "grey", linetype=2) +
  labs(title="Summer", 
       y="actual as % of average", x="week number")
summer_line_plot

#---------------
# Autumn visual
#---------------

head(mort_autumn)
autumn_line_plot <- ggplot(mort_autumn, aes(x = week_no, y = variance_from_avg)) + 
  geom_line(aes(color = year)) +
  scale_y_continuous(labels=scales::percent, limits = c(0.6, 1.4)) + 
  scale_x_continuous(breaks = pretty_breaks(n = 13)) +
  geom_hline(yintercept=1.0, colour = "darkgrey") +
  geom_hline(yintercept=1.4, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.3, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.2, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.1, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.9, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.8, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.7, colour = "grey", linetype=2) +
  labs(title="Autumn", 
       y="actual as % of average", x="week number")
autumn_line_plot

#---------------
# Winter visual
#---------------

mort_winter_final$week_no <- as.character(mort_winter_final$week_no)
str(mort_winter_final)
tail(mort_winter_final)
# The seasonal weeks are allocated to the year 
mort_winter_final <- mort_winter_final %>% 
  mutate(season = rep(c("2010","2011","2012","2013","2014","2015","2016","2017","2018"), each = 13)) 

winter_line_plot <- ggplot() +
  ## week is discrete, so you need to specify group
  geom_line(mort_winter_final, mapping = aes(week_no, variance_from_avg, color = season, group = season)) +
  ## set axis breaks
  scale_x_discrete(limits = as.character(c(51:52, 1:11))) +
  scale_y_continuous(labels=scales::percent, limits = c(0.6, 1.4)) +
  geom_hline(yintercept=1.0, colour = "darkgrey") +
  geom_hline(yintercept=1.4, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.3, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.2, colour = "grey", linetype=2) +
  geom_hline(yintercept=1.1, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.9, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.8, colour = "grey", linetype=2) +
  geom_hline(yintercept=0.7, colour = "grey", linetype=2) +
  labs(
    title="Winter", 
    y="actual as % of average", x="week number")
winter_line_plot

#---------------------------------------
# Combine the plots into a single figure
#---------------------------------------

# Create grid of seasonal plots without legends
seasonal_deaths_variance_from_average <- plot_grid(
  spring_line_plot + theme(legend.position="none"), 
  summer_line_plot + theme(legend.position="none"), 
  autumn_line_plot + theme(legend.position="none"), 
  winter_line_plot + theme(legend.position="none"))

# extract the legend from one of the plots
# Its possible to use one legend because each set of data is in the same order, starting with the same year
legend <- get_legend(
  # create some space to the left of the legend
  summer_line_plot + theme(legend.box.margin = margin(0, 0, 12))
)

# add the legend to the combined graphs giving it space rel_widths.
seasonal_deaths_variance_w_legend <- plot_grid(seasonal_deaths_variance_from_average, legend, rel_widths = c(1, .15))

# Create a title for the combined graphs
title <- ggdraw() + 
  draw_label(
    "Actual mortality as a percentage of 5-year average, by season 2010 - 2019",
    fontface = 'bold',
    x = 0,
    hjust = 0,
  ) +
  draw_label(
    "Source: NHSRDatasets",
    x = 1, 
    y = 1, 
    hjust = 1, 
    vjust = 1,
    size = 10,
    colour = "darkgrey"
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(15, 20, 10, 10)
  )

# Add the title to the combined graphs with the legend 
plot_grid(
  title, seasonal_deaths_variance_w_legend,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1))

