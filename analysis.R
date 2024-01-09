####################
# Topic: Bachelorthesis - FLFP and GDP preprocessing
# Author: Carsten Heine
# Comment: U-Shape without culture
# Date: 06.11.2023
####################

################
# Step 0 load packages
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
library(textreg) # for exportation to Word

######################
# Step 1 Clean Datasets
######################

# load data from the Penn World Table
pen <- read_excel(file.path("raw_data","pwt1001.xlsx"), sheet = "Data")

# clean the Pen World Table dataset
pen <- pen %>%
  mutate(gdppc = rgdpe/pop) %>% # create gdp per capita ppp variable
  select(c("countrycode","country","year","rgdpe","gdppc","pop")) %>% # select relevant variables
  subset(year >= 1990) %>% # filter for years 1990-2019
  arrange("countrycode","year") # order dataset

# group the countries into income groups following world bank classification in 2019 (https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2019-2020)
pen <- pen %>%
  group_by(countrycode) %>% 
  mutate(income_group = case_when(rgdpe < 1025 & year == 2019 ~ "low_income",
                                rgdpe <= 3995 & year == 2019 ~ "lower_middle_income",
                                rgdpe <= 12375 & year == 2019 ~ "upper_middle_income",
                                rgdpe > 12375 & year == 2019 ~ "high_income" )) %>%
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
                                               "JOR","MAR","OMN","QAT","SAU","SYR","TUN","ARE","YEM","PSE")
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
  #create a variable distinguishing OECD and oher high income countries
  mutate(oecd = case_when(countrycode %in% c("AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST","FIN",
                                             "FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN","KOR",
                                             "LVA","LTU","LUX","MEX","NLD","NZL","NOR","PRT","POL","SVK",
                                             "SVN","ESP","SWE","CHE","TUR","USA","GBR")
                          ~ "OECD",
                          TRUE ~ "non-OECD"))
# check if all countries where assigned to a region
check <- pen %>%
  subset(region == "Other")

# create a dataframe containing countries,the income_group and region they pertain to.
pen_b <- pen %>%
  subset(year == 2019) %>%
  select(countrycode,country,income_group,region,oecd) 

# merge pen_b and pen to obtain a income_group column without NAs
pen <- pen %>% 
  select(-c("income_group","region","oecd","country")) %>%
  inner_join(pen_b, by = c("countrycode"))


# clean the ILO modelled estimates dataset
ilo <- read_csv(file.path("raw_data","EAP_2WAP_SEX_AGE_RT_A-filtered-2023-11-02.csv"))

# clean ILO modelled estimates dataset
ilo <- ilo %>%
  # rename variables for intelligibility 
  rename(country = ref_area.label) %>%
  rename(sex = sex.label) %>%
  rename(age_group = classif1.label) %>%
  rename(year = time) %>%
  rename(lfpr = obs_value) %>%
  rename(obs_status = obs_status.label) %>%
  subset(year > 1989 & year < 2020 & sex == "Sex: Female") %>% # subset for years and sex of interest
  rename(flfpr = lfpr) %>%
  select(country,age_group,year,flfpr) %>% # select variables of interest
  # convert flfpr into variable from 0-1 instead of 0-100
  mutate(flfpr = flfpr/100)


######################
# Step 2 Merge Datasets
######################

# merge the ilo_est and pen datasets
 
# check which countries are accounted for in both datasets in order to avoid problems 
# because country names are written differently 
pen_c <- unique(pen$country)
ilo_c <- unique(ilo$country)
intersect(pen_c,ilo_c) # 154 countries seem to appear in both datasets
not_matched <- symdiff(pen_c,ilo_c) 

# consulting the non_matched list we find that 13 countries are not matched because their names are written differently in the two datasets. 
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

# rename the countries in the ILO dataset
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
pen_c <- unique(pen$country)
ilo_c <- unique(ilo$country)
intersect(pen_c,ilo_c) # 167 countries are matched now

# merge the ilo and pen datasets
pen_ilo <- inner_join(pen,ilo,by = c("country","year")) 
 
#####################
# Step 3 descriptive statistics
#####################

describe(pen_ilo)

# check female labor force participation per region in 2008 to compare it with Gaddis (2014)
des_a <- pen_ilo %>%
  # subset to the main age_group Gaddis (2014) uses
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2008) %>%
  select(-c("country","age_group","rgdpe")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2008 = "2008")%>%
  group_by(region) %>%
  summarise(across(av_flfpr_2008,mean))

# subset in high income OECD and non-OECD
des_b <- pen_ilo %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & 
           year == 2008 & 
           income_group == "high_income") %>%
  select(-c("country","age_group","rgdpe","region")) %>%
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
# give descriptive data on FLFP and GDP in 2019 per region
des_d <-  pen_ilo %>%
  # subset to the main age_group Gaddis (2014) uses
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2019) %>%
  select(-c("country","age_group","rgdpe")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2008 = "2019")%>%
  group_by(region) %>%
  summarise(across(c("av_flfpr_2008","gdppc"),mean))

# subset in high income OECD and non-OECD
des_e <- pen_ilo %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & 
           year == 2019 & 
           income_group == "high_income") %>%
  select(-c("country","age_group","rgdpe","region")) %>%
  # merge the oecd column and the income_group column
  unite(region,c("oecd","income_group")) %>%
  pivot_wider(names_from = year, values_from = flfpr) %>%
  rename(av_flfpr_2008 = "2019") %>%
  group_by(region) %>%
  summarise(across(c("av_flfpr_2008","gdppc"),mean))

des_f <- pen_ilo %>%
  ungroup() %>%
  subset(age_group == "Age (Aggregate bands): 25-54" & year == 2019) 
av_all_2019 <- data.frame(region = "all",
                     av_flfpr_2008 = mean(des_f$flfpr)
)
av_all_2019$gdppc <- mean(des_f$gdppc)

# add the three tables together
av_flfpr_2019 <- rbind(des_d,des_e,av_all_2019)

######################
# Step 4.1 Create dataset for three age cohorts
######################

# women aged 25-54, 1990 - 2010 (because this is almost the same group of people Gaddis (2014) looked at) 
# and select only 5 year periods to avoid the influence of volatility
pen_ilo_a1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr_lag"),~(scale(.) %>% as.vector))%>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

# women aged 15-24, 1990 - 2010
pen_ilo_a2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-24") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  #mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

# women aged 15-64, 1990 - 2010
pen_ilo_a3 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  #mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector)) %>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

# women aged 25-54, 1990 - 2019
pen_ilo_b1 <- pen_ilo %>% 
  subset(age_group == "Age (Aggregate bands): 25-54") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

# women aged 15-24, 1990 - 2019
pen_ilo_b2 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-24") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

# women aged 15-64, 1990 - 2019
pen_ilo_b3 <- pen_ilo %>% 
  subset(age_group == "Age (Youth, adults): 15-64") %>%
  subset(year == 1990 | year == 1995 | year == 2000 | year == 2005 | year == 2010 | year ==2015 | year == 2019) %>%
  select(-age_group) %>%
  # add lag variable for flfpr
  group_by(countrycode)%>%
  arrange(countrycode,year) %>%
  mutate(flfpr_lag = dplyr::lag(flfpr, n = 1)) %>%
  ungroup() %>%
  # create the natural logarithm of the gdp variables
  mutate(lg_gdppc = log(gdppc)) %>%
  mutate(lg_gdppc_sq = (lg_gdppc)^2) %>% # create the square of the natural logarithm of gdp per capita ppp
  # and standardize the variables (https://www.statology.org/standardize-data-in-r/)
  # mutate_at(c("lg_gdppc","lg_gdppc_sq","flfpr","flfpr_lag"),~(scale(.) %>% as.vector))%>%
  # we group by countrycode to facilitate the usage of time-fixed effects later
  group_by(countrycode)

######################
# Step 4.2 Create graph for flfpr over time per region
######################

# create weights for each country using the population variable in the Penn World Table

t_pop_year <- pen_ilo_b3 %>%
  group_by(year) %>%
  summarise(total_pop = sum(pop))

pen_ilo_b3 <- inner_join(pen_ilo_b3,t_pop_year, by = c("year")) %>%
  mutate(weight = pop/total_pop)

flfpr_regio <- pen_ilo_b3 %>%
  group_by(year,region) %>%
  summarise(w_mean_flfpr = sum(flfpr*weight)/sum(weight)) 
flfpr_regio

flfpr_regio %>%
  ggplot(aes( x = year, y = w_mean_flfpr, color = region)) +
  geom_point() +
  geom_line() +
  xlab("Year") +
  ylab("Female Labor Force Participation Rate") +
  labs(title = "Female Labor Force Participation Rate per Region over Time", color = "World Regions") 


# Step 5 Analyse Dataset
######################

# create the panel data sets
p.data_a1 <- pdata.frame(pen_ilo_a1, index = c("countrycode","year"))
p.data_a2 <- pdata.frame(pen_ilo_a2, index = c("countrycode","year"))
p.data_a3 <- pdata.frame(pen_ilo_a3, index = c("countrycode","year"))
p.data_b1 <- pdata.frame(pen_ilo_b1, index = c("countrycode","year"))
p.data_b2 <- pdata.frame(pen_ilo_b2, index = c("countrycode","year"))
p.data_b3 <- pdata.frame(pen_ilo_b3, index = c("countrycode","year"))


#####################
# Step 5.1 OLS Analysis
#####################

# pooled analysis
ols_a1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_a1)
ols_a2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_a2)
ols_a3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_a3)
summary(ols_a1)
summary(ols_a2) # in the young group of people the effect is stronger,which might be due to an increase in education in the last decades
summary(ols_a3)

ols_b1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_b1) 
ols_b2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_b2) 
ols_b3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = pen_ilo_b3) 
summary(ols_b1)
summary(ols_b2)
summary(ols_b3)
# we see that, as the theory predicts, the U-shape hypothesis is supported in a pooled cross sectional model
# the estimators in the larger dataset are larger, hinting at the fact that more recently collected data supports the hypothesis more


# visualize the idiosyncratic U-shape
pen_ilo_b3  %>%
  ggplot(aes(x = lg_gdppc, y = flfpr)) +
  geom_point() +
  stat_smooth(method = "lm", formula = y ~ poly(x,2), color = "red") +
  xlab("Log GDP per capita PPP") +
  ylab("Female Labor Force Participation Rate") +
  labs(title = "U-shape of Female Labor Force Participation")

# calculate the turning points for the ols regressions
coef_a1 <- coef(ols_a1)
beta_1 <- coef_a1[2]
beta_2 <- coef_a1[3]
tp_a1 <- exp(-beta_1/(2*beta_2))
tp_a1

coef_a2 <- coef(ols_a2)
beta_1 <- coef_a2[2]
beta_2 <- coef_a2[3]
tp_a2 <- exp(-beta_1/(2*beta_2))
tp_a2

coef_a3 <- coef(ols_a3)
beta_1 <- coef_a3[2]
beta_2 <- coef_a3[3]
tp_a3 <- exp(-beta_1/(2*beta_2))
tp_a3

coef_b1 <- coef(ols_b1)
beta_1 <- coef_b1[2]
beta_2 <- coef_b1[3]
tp_b1 <- exp(-beta_1/(2*beta_2))
tp_b1

coef_b2 <- coef(ols_b2)
beta_1 <- coef_b2[2]
beta_2 <- coef_b2[3]
tp_b2 <- exp(-beta_1/(2*beta_2))
tp_b2

coef_b3 <- coef(ols_b3)
beta_1 <- coef_b3[2]
beta_2 <- coef_b3[3]
tp_b3 <- exp(-beta_1/(2*beta_2))
tp_b3

ols_tp <- cbind(tp_a1,tp_a2,tp_a3,tp_b1,tp_b2,tp_b3)
######################
# Step 5.2 Analyse Dataset
######################

# fixed effect model (https://libguides.princeton.edu/R-Panel)
# (http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html)
# with time fixed effect(https://www.econometrics-with-r.org/10.4-regression-with-time-fixed-effects.html)
fixed_a1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_a1, method = "within", index = c("countrycode"))
fixed_a2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_a2, method = "within", index = c("countrycode"))
fixed_a3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_a3, method = "within", index = c("countrycode"))

summary(fixed_a1)
summary(fixed_a2)
summary(fixed_a3)

fixed_b1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_b1, method = "within", index = c("countrycode"))
fixed_b2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_b2, method = "within", index = c("countrycode"))
fixed_b3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + as.factor(year), data = p.data_b3, method = "within", index = c("countrycode"))

summary(fixed_b1)
summary(fixed_b2)
summary(fixed_b3)

# calculate the turning points for the fixed regressions
coef_a1 <- coef(fixed_a1)
beta_1 <- coef_a1[1]
beta_2 <- coef_a1[2]
tp_a1 <- exp(-beta_1/(2*beta_2))
tp_a1

coef_a2 <- coef(fixed_a2)
beta_1 <- coef_a2[1]
beta_2 <- coef_a2[2]
tp_a2 <- exp(-beta_1/(2*beta_2))
tp_a2

coef_a3 <- coef(fixed_a3)
beta_1 <- coef_a3[1]
beta_2 <- coef_a3[2]
tp_a3 <- exp(-beta_1/(2*beta_2))
tp_a3

coef_b1 <- coef(fixed_b1)
beta_1 <- coef_b1[1]
beta_2 <- coef_b1[2]
tp_b1 <- exp(-beta_1/(2*beta_2))
tp_b1

coef_b2 <- coef(fixed_b2)
beta_1 <- coef_b2[1]
beta_2 <- coef_b2[2]
tp_b2 <- exp(-beta_1/(2*beta_2))
tp_b2

coef_b3 <- coef(fixed_b3)
beta_1 <- coef_b3[1]
beta_2 <- coef_b3[2]
tp_b3 <- exp(-beta_1/(2*beta_2))
tp_b3

fixed_tp <- cbind(tp_a1,tp_a2,tp_a3,tp_b1,tp_b2,tp_b3)
# the u-shape hypothesis persists in the fixed effects model but the estimators shrink
# and all of the year dummies are positive and significant
# again we see that with more data, the estimators are larger. The R^2 stays more or less the same.


# take a look at the coefficients for all countries
fixed_b4 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + countrycode + as.factor(year), 
                         data = pen_ilo_b3)

summary(fixed_b4)
coef_b4 <- summary(fixed_b4)$coef
sig_coef <- coef_b4[coef_b4[,4] <= 0.05, 4]
sig_coef <- data.frame(sig_coef)
sig_coef %>%
  mutate(coef = row_number())

sig_coef$coefficients<- as.character(row.names(sig_coef))
sig_coef <- sig_coef %>%
  # filter for countrycode variables/coefficients
  filter(str_detect(coefficients,"countrycode")) %>%
  # create a column with only the countrycodes
  mutate(countrycode = substr(coefficients,12,14))


# check for multicollinearity - variation inflation factor(proportion by which the variance of an estimator increases due to the inclusion of a particular covariate)
# this works best for OLS models 

# Assuming we've already fit our plm() model...
design.matrix <- as.data.frame(model.matrix(fixed_b3))

# Get the time-demeaned response variable, flfpr
design.matrix$flfpr <- plm::Within(
  plm::pdata.frame(pen_ilo_b3, index = "countrycode")$flfpr)

# Fit the OLS model on the demeaned dataset
m3.ols <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq , data = design.matrix)

# Calculate VIF scores
car::vif(m3.ols)

# multicollinearity seems to be present - how to deal with it?

# Homoscedasticity 
# calculate the fitted values as the difference between the observed values and the model residuals for plm

fitted.values <- as.numeric(pen_ilo_b3$flfpr - residuals(fixed_b3))
plot(fitted.values, residuals(fixed_b3),
     bty = 'n', xlab = 'Fitted Values', ylab = 'Residuals',
     main = 'Residuals vs. Fitted')
abline(h = 0, col = 'red', lty = 'dashed')

# some heteroscedasticity seems present - specifically in realtively large positive or negative residual values

# check for influential observations - which leverage do individual observations have?(http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html)

# calculate the projections matrix
X <- model.matrix(fixed_b1)
P = X %*% solve(t(X) %*% X) %*% t(X)

# Create `labs` (labels) for 1 through 1169 observations
halfnorm(diag(P), labs = 1:1169, ylab = 'Leverages', nlab = 1)
pen_ilo_b1[1134,] # value of venezuela in 2019 seems to have a great influence
pen_ilo_b1[pen_ilo_b1$countrycode == "VEN",] # Probably because the GDPPC falls rapidly from 2015 to 2019

######################
# Step 5.3 GMM models
######################

# https://www.youtube.com/watch?v=zOzhJ2nyXAw
# create the lag variabels in the data set because the lag() function of the model cannot deal with jumps in time

#p.data_b3 <- p.data_b3 %>%
  # group_by(countrycode) 
  # mutate(flfpr_lag2 = lag(flfpr,2)) %>%
  # mutate(flfpr_lag3 = lag(flfpr,3)) %>%
  # mutate(flfpr_lag4 = lag(flfpr,4)) %>%
  # mutate(flfpr_lag5 = lag(flfpr,5)) %>%
  # mutate(lg_gdppc_lag1 = lag(lg_gdppc,1)) %>%
  # mutate(lg_gdppc_lag2 = lag(lg_gdppc,2)) %>%
  # mutate(lg_gdppc_lag3 = lag(lg_gdppc,3)) %>%
  # mutate(lg_gdppc_lag4 = lag(lg_gdppc,4)) %>%
  # mutate(lg_gdppc_sq_lag1 = lag(lg_gdppc_sq,1)) %>%
  # mutate(lg_gdppc_sq_lag2 = lag(lg_gdppc_sq,2)) %>%
  # mutate(lg_gdppc_sq_lag3 = lag(lg_gdppc_sq,3)) %>%
  # mutate(lg_gdppc_sq_lag4 = lag(lg_gdppc_sq,4)) %>%
  # mutate(lg_gdppc_sq_lag5 = lag(lg_gdppc_sq,5))

# the lag function in the GMM models should work if we group the datasets according to the countrycode
p.data_a1 <- p.data_a1 %>% group_by(countrycode)
p.data_a2 <- p.data_a2 %>% group_by(countrycode)
p.data_a3 <- p.data_a3 %>% group_by(countrycode)

p.data_b1 <- p.data_b1 %>% group_by(countrycode)
p.data_b2 <- p.data_b2 %>% group_by(countrycode)
p.data_b3 <- p.data_b3 %>% group_by(countrycode)

# Gaddis (2014) accepts the model if there is first order autocorrelation (p<0.05) 
# but not second order autocorrelation (p>0.1) and
# we should not be able to reject the null hypothesis (joint validity of instruments) of the sargan hansen test(p > 0.1).


gmm_a1 <- pgmm(flfpr ~ lg_gdppc  + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 2:3) + lag(lg_gdppc_sq, 2:3) + lag(flfpr, 3:4), # instruments
               data = p.data_a1,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_a1)

# lag structure: gdp variables - 2:3 order lags; flfpr - 3:4 order lags

gmm_a2 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1)  # normal covariates
               |lag(lg_gdppc, 2:3) + lag(lg_gdppc_sq, 2:3) + lag(flfpr, 3:4), # instruments
               data = p.data_a2,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_a2)
# lag structure: no specification found

gmm_a3 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1)  # normal covariates
               |lag(lg_gdppc, 2:3) + lag(lg_gdppc_sq, 2:3) + lag(flfpr, 3:4), # instruments
               data = p.data_a3,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_a3)
# lag structure: gdp variables - 2:3 order lags; flfpr - 3:4 order lags

gmm_b1 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1)  # normal covariates
               |lag(lg_gdppc, 2) + lag(lg_gdppc_sq, 2) + lag(flfpr, 2:3), # instruments
               data = p.data_b1,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_b1) # lag structure: gdp variables - 2 order lags; flfpr - 2:3 order lags


gmm_b2 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1)  # normal covariates
               |lag(lg_gdppc, 4:5) + lag(lg_gdppc_sq, 4:5) + lag(flfpr, 4), # instruments
               data = p.data_b2,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_b2) # lag structure: gdp variables - 4:5 order lags; flfpr - 4 order lags

gmm_b3 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1)  # normal covariates
                 |lag(lg_gdppc, 3:4) + lag(lg_gdppc_sq, 3:4) + lag(flfpr, 4:5), # instruments
                 data = p.data_b3,
                 effect = "twoways",
                 model = "twostep",
                 collapse = FALSE,
                 transformation = "d",
                 fsm = "I")
summary(gmm_b3)  # lag structure: gdp variables - 3:4 order lags; flfpr - 4:5 order lags

# Gaddis accepts the model if there is first order autocorrelation (p<0.05) 
# but not second order autocorrelation (p>0.1) and
# we should not be able to reject the null hypothesis (joint validity of instruments) of the sargan hansen test(p > 0.1).

# calculate the turning points for the fixed regressions
coef_a1 <- coef(gmm_a1)
beta_1 <- coef_a1[1]
beta_2 <- coef_a1[2]
tp_a1 <- exp(-beta_1/(2*beta_2))
tp_a1

coef_a2 <- coef(gmm_a2)
beta_1 <- coef_a2[1]
beta_2 <- coef_a2[2]
tp_a2 <- exp(-beta_1/(2*beta_2))
tp_a2

coef_a3 <- coef(gmm_a3)
beta_1 <- coef_a3[1]
beta_2 <- coef_a3[2]
tp_a3 <- exp(-beta_1/(2*beta_2))
tp_a3

coef_b1 <- coef(gmm_b1)
beta_1 <- coef_b1[1]
beta_2 <- coef_b1[2]
tp_b1 <- exp(-beta_1/(2*beta_2))
tp_b1

coef_b2 <- coef(gmm_b2)
beta_1 <- coef_b2[1]
beta_2 <- coef_b2[2]
tp_b2 <- exp(-beta_1/(2*beta_2))
tp_b2

coef_b3 <- coef(gmm_b3)
beta_1 <- coef_b3[1]
beta_2 <- coef_b3[2]
tp_b3 <- exp(-beta_1/(2*beta_2))
tp_b3

gmm_tp <- cbind(tp_a1,tp_a2,tp_a3,tp_b1,tp_b2,tp_b3)

# random walk https://machinelearningmastery.com/gentle-introduction-random-walk-times-series-forecasting-python/ 