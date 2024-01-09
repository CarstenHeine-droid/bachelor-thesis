####################
# Topic: Bachelorthesis - FLFP and GDP preprocessing
# Author: Carsten Heine
# Comment: Preprocessing of the Penn World Table data and the ILO modelled estimates data
# Date: 06.11.2023
####################

################
# Step 0 Load Packages
################
# clean environment
rm(list=ls())

# load packages
library(readxl) # to read in excel files
library(plm) # for panel data analysis
library(tidyverse) # for more intelligible code
library(psych) # for descriptive analysis of the data
library(car) # for multicollinearity check
library(faraway) # to check for influential observations
library(openxlsx) # to save data as excel files

######################
# Step 1.0 Load Data
######################

# load data from the Penn World Table
pen <- read_excel(file.path("raw_data","pwt1001.xlsx"), sheet = "Data")

#load ILO modelled estimates data
ilo <- read_csv(file.path("raw_data","EAP_2WAP_SEX_AGE_RT_A-filtered-2023-11-02.csv"))

######################
# Step 1.1 Clean Pen World Table Data
######################

# clean the Pen World Table dataset
pen <- pen %>%
  mutate(gdppc = rgdpe/pop) %>% # create gdp per capita ppp variable
  select(c("countrycode","country","year","gdppc","pop")) %>% # select relevant variables
  subset(year >= 1990) %>% # filter for years 1990-2019
  arrange("countrycode","year") # order dataset

# group the countries into income groups following world bank classification in 2019 (https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2019-2020)
pen <- pen %>%
  group_by(countrycode) %>% 
  mutate(income_group = case_when(gdppc < 1025 & year == 2019 ~ "low_income",
                                gdppc <= 3995 & year == 2019 ~ "lower_middle_income",
                                gdppc <= 12375 & year == 2019 ~ "upper_middle_income",
                                gdppc > 12375 & year == 2019 ~ "high_income" )) %>%
  mutate(region = case_when(countrycode %in% c("AFG","BGD","BTN","IND","MDV","NPL","PAK","LKA") 
                            ~ "South Asia",
                            countrycode %in% c("AGO","BEN","BWA","BEN","BFA","BDI","CMR","CPV",
                                               "CAF","TCD","COM","ZAR","COG","COD","CIV","GLQ","ERI","ETH","GAB",
                                               "GMB","GHA","GIN","GNB","GUY","HTI","HND","KEN","LSO","LBR",
                                               "MDG","MWI","MLI","MRT","MUS","ESW","MOZ","NAM","NER",
                                               "NGA","RWA","STP","SEN","SLE","SYC","SUD","SOM","ZAF","TZA",
                                               "TGO","UGA","ZMB","ZWE","GNQ","SDN","SWZ")
                            ~ "Sub-Saharan Africa",
                            countrycode %in% c("AIA","ATG","ARG","ABW","BHS","BRB","BLZ","BOL","BRA","VGB",
                                               "CYM","CHL","COL","CRI","CUB","DOM","DMA","ECU","SLV","SXM",
                                               "GRD","GTM","JAM","MEX","NIC","PAN","PRY","PER","PRI","MSR","4CUW",
                                               "KNA","LCA","VCT","SSD","SUR","TTO","TCA","URY","VEN","VIR","GNQ") 
                            ~ "Latin America & Carribean",
                            countrycode %in% c("DZA","BHR","DJI","EGY","IRN","IRQ","ISR","KWT","LBN","LBY",
                                               "JOR","MAR","OMN","QAT","SAU","SYR","TUN","ARE","YEM","PSE","CUW")
                            ~ "Middle East & North Africa",
                            countrycode %in% c("AUS","BRN","KHM","CHN","FJI","HKG","IDN","JPN","LAO",
                                               "KOR","MAC","MYS","MNG","MMR","NZL","PHL","TWN","THA","SGP","VNM")
                            ~ "East Asia & Pacific",
                            countrycode %in% c("ALB","ARM","AUT","AZE","BLR","BEL","BIH","BGR",
                                               "HRV","CYP","CZE","DNK","EST","FIN","FRA","GEO",
                                               "DEU","GRC","HUN","ISL","IRL","ITA","LVA","LTU",
                                               "LUX","MDA","MNE","NLD","MKD","NOR","POL","PRT",
                                               "ROU","RUS","SRB","SVK","SVN","ESP","SWE","CHE",
                                               "TJK","TUR","TKM","UKR","GBR","UZB","KAZ","KGZ","MLT")
                            ~ "Europe & Central Asia",
                            countrycode %in% c("BMU","CAN","USA")
                            ~ "North America",
                            TRUE ~ "Other")) %>%
  #create a variable distinguishing OECD and other high income countries
  mutate(oecd = case_when(countrycode %in% c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN",
                                             "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR",
                                             "LVA","LTU","LUX","MEX","NLD","NZL","NOR","PRT","POL","SVK",
                                             "SVN","ESP","SWE","CHE","TUR","USA","GBR")
                          ~ "OECD",
                          TRUE ~ "non-OECD"))
# check if all countries where assigned to a region
check <- pen %>%
  subset(region == "Other")
check

# create the income_group variable
# create a data frame containing countries,the income_group and region they pertain to.
pen_b <- pen %>%
  subset(year == 2019) %>%
  select(countrycode,country,income_group,region,oecd) 

# merge pen_b and pen to obtain a income_group column without NAs
pen <- pen %>% 
  select(-c("income_group","region","oecd","country")) %>%
  inner_join(pen_b, by = c("countrycode"))

######################
# Step 1.2 Clean ILO Data
######################

# clean ILO modeled estimates data set
ilo <- ilo %>%
  # rename variables for intelligibility 
  rename(country = ref_area.label) %>%
  rename(sex = sex.label) %>%
  rename(age_group = classif1.label) %>%
  rename(year = time) %>%
  rename(lfpr = obs_value) %>%
  rename(obs_status = obs_status.label) %>%
  # subset for years and sex of interest
  subset(year > 1989 & year < 2020 & sex == "Sex: Female") %>% 
  rename(flfpr = lfpr) %>%
  # select variables of interest
  select(country,age_group,year,flfpr) %>% 
  # convert flfpr into variable from 0-1 instead of 0-100
  mutate(flfpr = flfpr/100)


######################
# Step 2 Merge Data Sets
######################

# merge the ilo_est and pen data sets
 
# check which countries are accounted for in both data sets in order to avoid problems 
# because country names are written differently 
pen_c <- unique(pen$country)
ilo_c <- unique(ilo$country)
intersect(pen_c,ilo_c) # 154 countries seem to appear in both data sets
mismatch <- symdiff(pen_c,ilo_c) 
mismatch

# consulting the mismatch list we find that 13 countries are not matched because their names are written differently in the two data sets. 
# these countries are: 
# "Bolivia (Plurinational State of)" and "Bolivia" 
# "D.R. of the Congo" and "Congo, Democratic Republic of the" 
# "Czech Republic" and "Czechia"
# "China, Hong Kong SAR" and "Hong Kong, China" 
# "Iran (Islamic Republic of)" and "Iran, Islamic Republic of"
# "Republic of Korea" and "Korea, Republic of" 
# "Lao People's DR" and "Lao People's Democratic Republic"
# "State of Palestine" and "Occupied Palestinian Territory"
# "Turkey" and "Türkiye"
# "Taiwan" and "Taiwan, China" 
# "U.R. of Tanzania: Mainland" and "Tanzania, United Republic of"
# "St. Vincent and the Grenadines" and "Saint Vincent and the Grenadines" 
# "Venezuela (Bolivarian Republic of)" and "Venezuela, Bolivarian Republic of"

# rename the countries in the ILO data set
ilo$country[ilo$country == "Bolivia"] <- "Bolivia (Plurinational State of)"
ilo$country[ilo$country == "Congo, Democratic Republic of the"] <- "D.R. of the Congo"
ilo$country[ilo$country == "Czechia"] <- "Czech Republic"
ilo$country[ilo$country == "Hong Kong, China"] <- "China, Hong Kong SAR"
ilo$country[ilo$country == "Iran, Islamic Republic of"] <- "Iran (Islamic Republic of)"
ilo$country[ilo$country == "Korea, Republic of"] <- "Republic of Korea"
ilo$country[ilo$country == "Lao People's Democratic Republic"] <- "Lao People's DR"
ilo$country[ilo$country == "Occupied Palestinian Territory"] <- "State of Palestine"
ilo$country[ilo$country == "Türkiye"] <- "Turkey"
ilo$country[ilo$country == "Taiwan, China"] <- "Taiwan"
ilo$country[ilo$country == "Tanzania, United Republic of"] <- "U.R. of Tanzania: Mainland"
ilo$country[ilo$country == "Saint Vincent and the Grenadines"] <- "St. Vincent and the Grenadines"
ilo$country[ilo$country == "Venezuela, Bolivarian Republic of"] <- "Venezuela (Bolivarian Republic of)"

# check if we have 13 more matches 
ilo_c <- unique(ilo$country)
intersect(pen_c,ilo_c) # 167 countries are matched now

# merge the ilo and pen data sets
pen_ilo <- inner_join(pen,ilo,by = c("country","year")) 
 
#####################
# Step 3 Descriptive Statistics
#####################

describe(pen_ilo)


# Here I create a table similiar to Gaddis 2014 to see if the data I use is comparable to theirs
# check female labor force participation per region in 2008 to compare it with Gaddis (2014)
des_a <- pen_ilo %>%
  # subset to the main age_group Gaddis (2014) uses
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2008) %>%
  select(-c("country","age_group")) %>%
  # convert into wide format
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2008 = "2008")%>%
  group_by(region) %>%
  summarise(across(av_flfpr_2008,mean))

# subset in high income OECD and non-OECD
des_b <- pen_ilo %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & 
           year == 2008 & 
           income_group == "high_income") %>%
  select(-c("country","age_group","region")) %>%
  # merge the oecd column and the income_group column
  unite(income_group_oecd,c("oecd","income_group")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2008 = "2008") %>%
  group_by(income_group_oecd) %>%
  summarise(across(av_flfpr_2008,mean)) %>%
  rename(region = income_group_oecd)

# get the average over all countries
des_c <- pen_ilo %>%
  ungroup() %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2008) 

av_all <- data.frame(region = "all",
  av_flfpr_2008 = mean(des_c$flfpr)
  )

# add the three tables together
av_flfpr_2008 <- rbind(des_a,des_b,av_all)
av_flfpr_2008

# create the descriptive statistics also for 2019 
# give descriptive data on FLFP and GDP in 2019 per region
des_d <-  pen_ilo %>%
  # subset to the main age_group Gaddis (2014) uses
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2019) %>%
  select(-c("country","age_group")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2019 = "2019")%>%
  group_by(region) %>%
  summarise(across(c("av_flfpr_2019","gdppc"),mean))

# subset in high income OECD and non-OECD
des_e <- pen_ilo %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & 
           year == 2019 & 
           income_group == "high_income") %>%
  select(-c("country","age_group","region")) %>%
  # merge the oecd column and the income_group column
  unite(region,c("oecd","income_group")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2019 = "2019") %>%
  group_by(region) %>%
  summarise(across(c("av_flfpr_2019","gdppc"),mean))

des_f <- pen_ilo %>%
  ungroup() %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2019) 
av_all_2019 <- data.frame(region = "all",
                     av_flfpr_2019 = mean(des_f$flfpr)
)
av_all_2019$gdppc <- mean(des_f$gdppc)

# add the three tables together
av_flfpr_2019 <- rbind(des_d,des_e,av_all_2019)

# save descriptive statistics as csv file
write.xlsx(av_flfpr_2008,file.path("tables_graphics","av_flfpr_2008.xlsx"))
write.xlsx(av_flfpr_2019,file.path("tables_graphics","av_flfpr_2019.xlsx"))

######################
# Step 4.1 Create data sets for three age cohorts
######################

# women aged 25-54, 1990 - 2010 (because this is almost the same group of people Gaddis (2014) looked at) 
# and select only 5 year periods to avoid the influence of volatility
pen_ilo_a1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr_lag"),~(scale(.) %>% as.vector))%>%

# save data
write.csv(pen_ilo_a1,file.path("analysis_data","flfp_a1.csv"), row.names = FALSE)

# women aged 15-24, 1990 - 2010
pen_ilo_a2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-24") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  #mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%


# save data
write.csv(pen_ilo_a2,file.path("analysis_data","flfp_a2.csv"), row.names = FALSE)

# women aged 15-64, 1990 - 2010
pen_ilo_a3 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  #mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector)) %>%

# save data
write.csv(pen_ilo_a3,file.path("analysis_data","flfp_a3.csv"), row.names = FALSE)

# women aged 25-54, 1990 - 2019
pen_ilo_b1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2)  # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%

# save data
write.csv(pen_ilo_b1,file.path("analysis_data","flfp_b1.csv"), row.names = FALSE)

# women aged 15-24, 1990 - 2019
pen_ilo_b2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-24") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2)  # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%

# save data
write.csv(pen_ilo_b2,file.path("analysis_data","flfp_b2.csv"), row.names = FALSE)

# women aged 15-64, 1990 - 2019
pen_ilo_b3 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%

# save data
write.csv(pen_ilo_b3,file.path("analysis_data","flfp_b3.csv"), row.names = FALSE)

# women aged 15-64, 1990 - 2019 all years
pen_ilo_b3_ay <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64") %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%

# save data
write.csv(pen_ilo_b3_ay,file.path("analysis_data","flfp_b3_ay.csv"), row.names = FALSE)

######################
# Step 4.2 Subset OECD Members
######################

# OECD members,  women aged 25-54, 1990 - 2010 
oecd_a1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54" & oecd == "OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp 

# save data
write.csv(oecd_a1,file.path("analysis_data","oecd_a1.csv"), row.names = FALSE)

# OECD members, women aged 15-64, 1990 - 2010
oecd_a2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64" & oecd == "OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp

# save data
write.csv(oecd_a2,file.path("analysis_data","oecd_a2.csv"), row.names = FALSE)

# OECD members, women aged 25-54, 1990 - 2019
oecd_b1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54" & oecd == "OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp

# save data
write.csv(oecd_b1,file.path("analysis_data","oecd_b1.csv"), row.names = FALSE)

# non_OECD members, women aged 15-64, 1990 - 2019
oecd_b2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64" & oecd == "OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp

# save data
write.csv(oecd_b2,file.path("analysis_data","oecd_b2.csv"), row.names = FALSE)

######################
# Step 4.3 Subset Non-OECD Members
######################

# non-OECD members,  women aged 25-54, 1990 - 2010 
n_oecd_a1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54" & oecd == "non-OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp 

# save data
write.csv(n_oecd_a1,file.path("analysis_data","n_oecd_a1.csv"), row.names = FALSE)

# non-OECD members,  women aged 15-64, 1990 - 2010 
n_oecd_a2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64" & oecd == "non-OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp 

# save data
write.csv(n_oecd_a2,file.path("analysis_data","n_oecd_a2.csv"), row.names = FALSE)

# non_OECD members, women aged 25-54, 1990 - 2019
n_oecd_b1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54" & oecd == "non-OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp

# save data
write.csv(n_oecd_b1,file.path("analysis_data","n_oecd_b1.csv"), row.names = FALSE)

# non_OECD members, women aged 15-64, 1990 - 2019
n_oecd_b2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64" & oecd == "non-OECD") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) # create the square of the natural logarithm of gdp per capita ppp

# save data
write.csv(n_oecd_b2,file.path("analysis_data","n_oecd_b2.csv"), row.names = FALSE)

######################
# Step 4.4 Create graph for flfpr over time per region
######################
# we use the following data : # women aged 15-64, 1990 - 2019 every five years 

# create weights for each country using the population variable in the Penn World Table
# first we calculate the total population of all countries per year 
t_pop_year <- pen_ilo_b3 %>%
  group_by(year) %>%
  summarise(total_pop = sum(pop))

# create the weights by dividing the individual countries population with the total population
pen_ilo_b4 <- inner_join(pen_ilo_b3,t_pop_year, by = c("year")) %>%
  mutate(weight = pop/total_pop)

# create the weighted mean of the flpfr per region
flfpr_regio <- pen_ilo_b4 %>%
  group_by(year,region) %>%
  summarise(w_mean_flfpr = sum(flfpr*weight)/sum(weight)) 

# plot flfpr over years per region
flfpr_regio %>%
  ggplot(aes( x = year, y = w_mean_flfpr, color = region)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Female Labor Force Participation Rate") +
  labs(title = "Female Labor Force Participation Rate per Region over Time", color = "World Regions") 
