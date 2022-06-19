#---------------------
# Load packages
#---------------------
install.packages("dataMeta")
library(dataMeta)
library (tidyverse)
library(here)

#------------------
# Read in the data
#------------------

collected_data=read_csv(here("RawData", "collected_data_final.csv"))

#-----------------
#Look at the data
#-----------------

glimpse(collected_data) 
tail(collected_data)

#---------------
# Create Linker
#---------------

# Create the variable descriptions
variable_description <- c("The index column that allows us to link the data collected to the original ons_mortality data in the 'RawData' folder.",
                          "The week in the year that this activity relates to, with numbers starting from January.",
                          "The year that this activity relates to.", 
                          "The date of the Friday of the week that this activity relates to.", 
                          "The count of deaths in that week.",  
                          "The average count of deaths in the same week using data from the previous 5-years",
                          "The difference between the actual deaths in the week compared to the 5-year average as a proportion (double)",
                          "The consent from the end-user to process and share the data collected with the data capture tool.")
print(variable_description)

# View the data types
glimpse(collected_data) 

# There are six quantitative values variables and two fixed values (allowable values or codes) variables.
variable_type <- c(0, 0, 0, 1, 0, 0, 0, 1)
print(variable_type)

linker<-build_linker(collected_data, variable_description, variable_type)
print(linker)

#---------------------
# Create Data Dictionary 
#---------------------

dictionary <- build_dict(my.data = collected_data, linker = linker)
glimpse(dictionary)

#---------------------
# Save Data Dictionary 
#---------------------

write_csv(dictionary, here("RawData", "collected_data_dictionary.csv"))

#-----------------------------------
# Create main_string for attributes
#-----------------------------------
 
main_string <- "This data describes the NHS England Mortality data and running 5 years averages from the *NHSRdatasets* package collected by the data capture tool."

#----------------------------------
# Incorporate attributes as metada
#----------------------------------

complete_collected_data <- incorporate_attr(my.data = collected_data, data.dictionary = dictionary,
                                           main_string = main_string)
#Change the author name
attributes(complete_collected_data)$author[1]<-"Shona McElroy"
complete_collected_data
attributes(complete_collected_data)

#----------------------------------------
# Save the complete_collected_data with attributes
#----------------------------------------

save_it(complete_collected_data, here("RawData", "complete_collected_data"))
