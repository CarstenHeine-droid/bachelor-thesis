####################
# Topic: Bachelorthesis - world development indicator processing
# Author: Carsten Heine
# Comment: Education and Fertility
# Date: 30.12.2023
####################

################
# Step 0 Load Packages
################

# clean environment
rm(list=ls())

# load packages
library(tidyverse) # for data cleaning
library(countrycode) # for adapting country codes


######################
# Step 1 Load Data
######################

# load data
wdi <- read_csv(file.path("raw_data","3c4e965e-4b41-4bf4-a9c4-57a7618d2102_Series - Metadata.csv"),
                         col_names = TRUE,na = "..", n_max = 798)

######################
# Step 1.1 clean Data
######################

# delete empty spaces in column names
colnames(wdi)<-make.names(colnames(wdi),unique = TRUE)

# rename year variables
names(wdi) = sub(pattern = "X", replacement = "", x = names(wdi))
names(wdi) = sub(pattern = "..YR.*",replacement = "", x = names(wdi))

# convert column names to lower case letters
colnames(wdi) <- tolower(colnames(wdi))


# adjust data to long format
wdi <- wdi %>%
  # select relevant columns
  select(-c("series.code","country.name")) %>%
  pivot_longer(3:32,names_to = "year",values_to = "values") %>%
  # create columns education and fertility
  pivot_wider(names_from = "series.name",values_from = "values") %>%
  # rename key variables
  rename("education" = "School enrollment, secondary, female (% net)") %>%
  rename("fertility" = "Fertility rate, total (births per woman)") %>%
  rename(countrycode = country.code)

######################
# Step 1.1 Save Data
######################

write.csv(wdi,file.path("analysis_data","wdi.csv"), row.names = FALSE)
















