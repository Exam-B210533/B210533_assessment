# Title: Using R on in Noteable
# Author details: Author: Mairead Bermingham, Contact details: mairead.bermingham@ed.ac.uk
# Script and data info: This script provides an example of using R as a calculator, and to collect BMI data entered at the console, and finally saveing the data as a csv file. 
# Data consists of numerical data.
# Data was collected was inputted on the 21 May 2022. 
# Copyright statement: This script is the product of University of Edinburgh.

#Here is an example of using R as a calculator to perform a numerical operation. 

(30000000 + 70000000)^2 - 1*10/400000000


# Using the c() and cbind() base R functions to combine data values and objects respectively. 
#Here we are going to use the c() function from the *base* R package to collect data entered at the console. 
#The c() function in R programming stands for combine. We are then going to name each collection of data and 
#perform a numerical operation to calculate BMI. We will then use the `cbind()` to combine the Height,  Weight and BMI objects. 
#The `cbind()` *base* R function combine R objects by rows or columns. 
#In this example, we conduct an analogous analysis to working in a spreadsheet. 

Height <- c(1.6, 1.8, 2.0, 2.5) # height data in m
Weight <- c(50, 60, 64, 95)     # weight data in kg
BMI <-   Weight/(Height^2)      # BMI
cbind(Height , Weight, BMI) # column bind, like spreadsheet


# Using readr from tidyverse to write the BMI data to file¶
# tidyverse is a collection of essential R packages for data science. 
#readr provides a fast and friendly way to read rectangular data from delimited files, 
#such as comma-separated values (CSV) and tab-separated values (TSV). 
#A data frame is a table or a two-dimensional array-like structure in which each column contains 
# values of one variable and each row contains one set of values from each column. 
#The function data.frame() creates data frames.

#Load the R packages that you will need to run the next few lines of R code.
library(tidyverse)
library(readr)

#Create a data frame from the BMI data.
Data<-data.frame(cbind(Height , Weight, BMI))
view(Data)
head(Data)
write_csv(Data, file="./Data/BMIData.csv")
#That is the BMI data frame saved to the working 'Data' folder.
   