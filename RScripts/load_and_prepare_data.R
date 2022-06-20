#---------------------
#load packages
#---------------------

# load the packages needed for data preparation and visualisation
library(NHSRdatasets)
library(tidyverse)
library(here)
library(knitr)
library(scales)
library(lubridate)
library(caret)
library(cowplot)

#--------------------------------
# Load ons_mortality NHSRdataset
#--------------------------------

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