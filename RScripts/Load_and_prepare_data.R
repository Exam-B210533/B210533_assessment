#---------------------
#load packages
#---------------------

library(NHSRdatasets)
library(tidyverse)
library(here)
library(knitr)
library(scales)
library(lubridate)
library(caret)
library(cowplot)

#---------------------
# Load NHSRdataset
#---------------------

data(ons_mortality)
mort <- ons_mortality

#--------------------------------
# Look at the ons_mortality data
#--------------------------------

glimpse(mort)
head(mort)
tail(mort)
unique(mort[c("category_1")])
unique(mort[c("category_2")])
min(mort$date, na.rm = TRUE)
max(mort$date, na.rm = TRUE)

#--------------------------------------
# Create an index column for reference
#--------------------------------------

mort <- rowid_to_column(mort, "index")

#----------------------------------------
# Tabulate the raw data and save the file
#----------------------------------------

mort %>%
  # Set the period column to show in month-year format
  mutate_at(vars(date), format, "%b-%y") %>% 
  # Set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(counts), comma) %>%
  # Show the first 10 rows
  head(10) %>%
  # Format as a table
  kable()

write_csv(mort, here("RawData", "ons_mortality.csv"))

#---------------------------------------------------------------------
# Map the 5 year average for each week to the all ages mortality data
#---------------------------------------------------------------------

# Filter by Category_2 to create a subset for "average of same week over 5 years"
mort_5year_avg <- filter(mort,category_2 == "average of same week over 5 years")
glimpse(mort_5year_avg)
tail(mort_5year_avg)

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
mort_all$year = year(mort_all$date)

# create subset of data with relevant data columns
mort_all <- mort_all[, c("index", "week_no", "year", "date", "counts", "mort_avg", "variance_from_avg")]
head(mort_all)

#---------------------------------------
# Tabulate the subset and save the file
#---------------------------------------

mort_all %>%
  # set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(counts, mort_avg), comma) %>%
  # reduce decimal places 
  mutate_at(vars(variance_from_avg), round, 2)  %>%
  # show the first 10 rows
  head(10) %>%
  # format as a table
  kable(caption= "England Mortality Data and Variance from Average, 2010 - 2019")

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

mort_alltrain %>%
  # set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(counts, mort_avg), comma) %>%
  # reduce decimal places 
  mutate_at(vars(variance_from_avg), round, 2)  %>%
  # show the first 10 rows
  head(10) %>%
  # format as a table
  kable(caption= "Training Data: England Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltrain, here("Data", "ons_mortality_ENG_1019_training.csv"))

#-----------------------------
# Create subset for test data
#-----------------------------

# Allocate all records that are not assigned to the training data subset
mort_alltest  <- mort_all[-train_index,]
nrow(mort_alltest)
#There are 12 records in your test data. One row needs to be set aside for use by the assignment markers
#can test and evaluate your data-capture tool.
mort_alltestMarker  <- mort_alltest[1,]

# Tabulate the Markers row for testing
mort_alltestMarker %>%
  # set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(counts, mort_avg), comma) %>%
  # reduce decimal places 
  mutate_at(vars(variance_from_avg), round, 2)  %>%
  # show the first 10 rows
  head(10) %>%
  # format as a table
  kable(caption= "Markers Test Data: England Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltestMarker, here("Data", "ons_mortality_ENG_1019_test_marker.csv"))

#-------------------------------------------------
# Tabulate the test data subset and save the file
#-------------------------------------------------

# Remove from the subset the row saved for the Marker
mort_alltest  <- mort_alltest[2:nrow(mort_alltest),]

mort_alltest %>%
  # set the numeric columns to have a comma at the 1000's place
  mutate_at(vars(counts, mort_avg), comma) %>%
  # reduce decimal places 
  mutate_at(vars(variance_from_avg), round, 2)  %>%
  # show the first 10 rows
  head(10) %>%
  # format as a table
  kable(caption= "Test Data: England Mortality Data and Variance from Average, 2010 - 2019")

write_csv(mort_alltest, here("Data", "ons_mortality_ENG_1019_test.csv"))
