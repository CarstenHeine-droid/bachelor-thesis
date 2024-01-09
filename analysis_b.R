####################
# Topic: Bachelorthesis Analysis
# Author: Carsten Heine
# Comment: Analysis
# Date: 13.12.2023
####################

######################
# Step 0 load packages and datasets
######################

# clean environment
rm(list=ls())

# load packages
library(plm) # for panel data analysis
library(tidyverse) # for more intelligible code
library(car) # for multicollinearity check
library(faraway) # to check for influential observations
library(stargazer) # to export regression results
library(lmtest) # to check if heteroskedasticity is present in OLS models

######################
# Step 1 load data
######################

flfp_a1 <- read_csv(file.path("analysis_data","flfp_a1.csv"), col_names = TRUE)
flfp_a2 <- read_csv(file.path("analysis_data","flfp_a2.csv"), col_names = TRUE)
flfp_a3 <- read_csv(file.path("analysis_data","flfp_a3.csv"), col_names = TRUE)

flfp_b1 <- read_csv(file.path("analysis_data","flfp_b1.csv"), col_names = TRUE)
flfp_b2 <- read_csv(file.path("analysis_data","flfp_b2.csv"), col_names = TRUE)
flfp_b3 <- read_csv(file.path("analysis_data","flfp_b3.csv"), col_names = TRUE)
flfp_b3_ay <- read_csv(file.path("analysis_data","flfp_b3_ay.csv"), col_names = TRUE) # contains data for all years

wvs <- read_csv(file.path("analysis_data","wvs.csv"), col_names = TRUE)

wdi <- read_csv(file.path("analysis_data","wdi.csv"), col_names = TRUE)

sig_coef <- read_csv(file.path ("analysis_data","significant_country_coefficients.csv"), col_names = TRUE)

######################
# Step 2 Merge pen_ilo, wvs and wdi data
######################

# check which countries do not match
coun_1 <- unique(flfp_b3$countrycode) 
coun_2 <- unique(wvs$countrycode) 
coun_3 <- unique(wdi$countrycode)

match1 <- intersect(coun_1,coun_2)
match <- intersect(intersect(coun_1,coun_2),coun_3)# see matching countries
# 88 countries included in data set

non_match <- c(setdiff(coun_1,coun_2),setdiff(coun_2,coun_1)) # see not matching countries
setdiff(match1,coun_3) # Taiwan is not in the 

intersect(coun_1,non_match) # countries in flfp_b3 not in wvs # 78
intersect(coun_2,non_match) # countries in wvs and not in flfp_b3 # 5
setdiff(match1,coun_3) # Taiwan is not in the WDI database

# create a panel data set for wvs data
flfp_wvs <- inner_join(flfp_b3_ay, wvs, by = c("countrycode","year"), copy = FALSE) %>% # 170 observations 
  # We want to treat the wvs data as panel data with years 2007,2013,2019
  # These are the central years of the respective waves of the world value survey
  # We combine all observations of one wave into one year
  # We base this on the assumption that 
  # culture and religion do not change significantly over the course of 2-3 years.
  mutate(year_b = case_when(year %in% c(2004,2005,2006,2007,2008,2009)
                            ~ 2007,
                            year %in% c(2010,2011,2012,2013,2014,2015,2016)
                            ~ 2013,
                            year %in% c(2017,2018,2019,2020,2021,2022,2023)
                            ~ 2019,
                            .default = 0)) %>%
  # create a dummy for Muslim countries
  mutate(muslim = ifelse(religion_wvs == "muslim",1,0)) %>%
  # add the wid data on fertility and education
  inner_join(wdi, by = c("countrycode","year"), copy = FALSE)



######################
# Step 3 OLS/ Fixed effect regressions with WVS data for three years 2007/2013/2019
######################

# The goal of this section is to get a first picture of how the 
# patriarchal culture variable affects flfpr
# We are using an unbalanced panel data set with a low number of observations
# Therefore, results need to be interpreted cautiously
# The improvement to the old model is that we introduce the pat_cul variable


# create panel data
p.data_c <- pdata.frame(flfp_wvs, index = c("countrycode","year_b"))

######################
# Step 3.1 pooled OLS regressions
######################

# general pooled OLS model
ols_c1 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq , data = flfp_wvs)
summary(ols_c1)

# Breusch-Pagan Test to see if OLS assumption holds
bptest(ols_c1)
# because p-value < 0,05 we reject the null hypothesis of homoscedasticity -> model is biased

# controlling for patriarchal culture
ols_c2 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul, data = flfp_wvs)
summary(ols_c2)
# effect of gdp completely disappears

ols_c3 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul + religion_wvs + religiosity + fertility, data = flfp_wvs)
summary(ols_c3)
# effect of pat_culture reduces 

ols_c4 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul + religion_pew + religiosity + fertility, data = flfp_wvs)
summary(ols_c4)

# using Muslim dummy
ols_c5 <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul + muslim + religiosity + fertility, data = flfp_wvs)
summary(ols_c5)

stargazer(ols_c1,ols_c2,ols_c3,ols_c5, type = "html", 
          title = "Pooled OLS Regression - FLFPR and Culture ",
          dep.var.caption = "Female Labor Force Participation Rate",
          #covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared","Patriarchal Culture",
          #                     "buddhist","christian","hindu","muslim","orthodox","other","muslim_dummy","fertility"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_pat_cul.htm")

######################
# Step 3.2 Country Fixed effect with patriarchal culture 
######################

# fixed effect models - probably not promising because culture barely changes over time
fixed_c1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , data = p.data_c, model = "within")
summary(fixed_c1)

# controlling for patriarchal culture
fixed_c2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul, data = p.data_c, model = "within")
summary(fixed_c2)
bptest(fixed_c2)

fixed_c3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + pat_cul + religiosity + fertility, data = p.data_c, model = "within")
summary(fixed_c3)

# that the results are insignificant might be due to the fact that we only have few data points

######################
# Step 4 Create Categorical variable for culture
######################

# The goal of this section is to group countries according to there level of culture.
# Once we grouped the countries, we will be able to use the data from 1900-2019 which enables us to 
# have a larger and ,therefore, more convincing balanced panel data set. 
# Because we assume that culture barely changes over time we will group countries now into different cultural groups
# I will look at different groupings for robustness reasons

# I also create a pat_cul variable for the larger data set
# Again, one key assumption is that cultural values barely change over time so that we can 
# use patriarchal values measured in 2007 or 2013 or 2019 to account for a variation in countries 
# culture throughout the period under investigation 1990-2019. 

# pat_cul for large data set
# for countries with more than one pat_cul variable I'll use an unweighted average

pat_cul <- flfp_wvs %>%
  group_by(countrycode) %>%
  summarize(mpat_cul = mean(pat_cul))

 # First we take the highest value of each country measured and compare it to the minimum
 gr_cul <- flfp_wvs %>%
   group_by(countrycode) %>%
   summarize(max_cul = max(pat_cul),min_cul = min(pat_cul))
 # plot histograms next to each other
 
 par(mfrow=c(1,2))
 hist(gr_cul$max_cul, breaks = 20)
 hist(gr_cul$min_cul, breaks = 20)
 
 # grouping 1 - according to histogram of min values
 # grouping 2 - according to histogram of max values
 # grouping 3 - according to the most recent measurement available
 
 gr_cul_1 <- gr_cul %>%
   # grouping 1 - min values
   mutate(cult_1 = case_when(min_cul > 2.9 ~ 3,
                                 min_cul > 2.2 ~ 2,
                                 min_cul > 0 ~ 1,
                                 .default = 99)) %>%
   # grouping 2 - max values
   mutate(cult_2 = case_when(max_cul > 2.9 ~ 3,
                               max_cul > 2.1 ~ 2,
                               max_cul > 0 ~ 1,
                               .default = 99)) %>%
   select(-c("min_cul","max_cul"))

# For countries with more than one observation I'll take the average value of patriarchal culture

# grouping 3 - newest year
gr_cul_2 <- flfp_wvs %>%
  group_by(countrycode) %>%
  slice_max(year_b)

# check histogram
hist(gr_cul_2$pat_cul, breaks = 40)

# add grouping 3 and 4
gr_cul_2 <- gr_cul_2 %>%
  mutate(cult_3 = case_when(pat_cul > 2.9 ~ 3,
                              pat_cul > 1.9 ~ 2,
                              pat_cul > 0 ~ 1,
                              .default = 99)) %>%
  # grouping 4 should be more fine grained and will contain 4 groups
  mutate(cult_4 = case_when(pat_cul > 3.0 ~ 5,
                            pat_cul > 2.7 ~ 4,
                            pat_cul > 2.4 ~ 3,
                            pat_cul > 1.9 ~ 2,
                            pat_cul > 0 ~ 1,
                            .default = 99)) %>%
  select(countrycode,cult_3,cult_4)

# add grouping 5 based on the averaged pat_cul variable
hist(pat_cul$mpat_cul, breaks = 40)
gr_cul_3  <- pat_cul %>%
  mutate(cult_5 = case_when(mpat_cul > 2.83 ~ 3,
                            mpat_cul > 1.8 ~ 2,
                            mpat_cul > 0 ~ 1,
                            .default = 99)) %>%
  mutate(cult_6 = case_when(mpat_cul > 2.69 ~ 3,
                            mpat_cul > 2.1 ~ 2,
                            mpat_cul > 0 ~ 1,
                            .default = 99))
 
# check which countries were put into which group
cult_groups <- data.frame(cbind(gr_cul_1,gr_cul_2,gr_cul_3)) %>% 
  select(-c("countrycode.1","countrycode.2"))

# check how many countries are in each group
coun_num <- cult_groups %>%
  select(-countrycode) %>%
  reframe(across(c(cult_1,cult_2,cult_3,cult_5,cult_6),table)) 
coun_num <- rbind(coun_num,c(NA,NA,NA,NA,NA),c(NA,NA,NA,NA,NA)) %>% 
  cbind(table(cult_groups$cult_4)) %>%
  select(-Var1) %>% 
  rename(cult_4 = Freq)
  
coun_num
# the amount of countries in the first three groups

# Add cultural groups,religion to the larger data set (flfp_b3)

# create religion variable
reli1 <- flfp_wvs %>%
  distinct(countrycode,religion_wvs)
reli2 <- flfp_wvs %>%
  distinct(countrycode,religion_pew)

# merge cultural groups with the main dataset (flfp_b3, 1990-2019, women aged 15-64)
flfp_d <- flfp_b3 %>%
  # add grouping 1 and 2
  inner_join(gr_cul_1, by = "countrycode", copy = FALSE) %>%
  # add grouping 3 and 4
  inner_join(gr_cul_2, by = "countrycode", copy = FALSE) %>%
  # add grouping 4 and 5
  inner_join(gr_cul_3, by = "countrycode", copy = FALSE) %>%
  # add religion variable
  inner_join(reli1, by = "countrycode", copy = FALSE) %>%
  inner_join(reli2, by = "countrycode", copy = FALSE) %>%
  # create dummy for Muslim
  mutate(muslim = if_else(religion_wvs == "muslim",1,0)) %>%
  # add the fertility variables
  inner_join(wdi, by = c("countrycode","year"), copy = FALSE)

# observations are reduced from 1169 to 616

# we will run the regression with all groupings to check for robustness

# check how countries are ordered into groups
flfp_cul_group <- flfp_d %>%
  select(countrycode,cult_1,cult_2,cult_3,cult_4,cult_5,cult_6,religion_wvs,religion_pew) %>%
  distinct()

######################
# Step 5 Models with categorical culture variable
######################

######################
# Step 5.1 OLS Models
######################

ols_std <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_d)
summary(ols_std)

# Grouping 1 OLS
ols_d1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_1 , data = flfp_d)
ols_d2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_1 + region + religion_wvs + fertility, data = flfp_d)
ols_d3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_1 + region + religion_pew + fertility, data = flfp_d)
ols_d4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_1 + region + muslim + fertility, data = flfp_d)

summary(ols_d1)
summary(ols_d2)
summary(ols_d3)
summary(ols_d4)

# Grouping 2 OLS
ols_e1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_2 , data = flfp_d)
ols_e2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_2 + region + religion_wvs + fertility, data = flfp_d)
ols_e3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_2 + region + religion_pew + fertility, data = flfp_d)
ols_e4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_2 + region + muslim + fertility, data = flfp_d)

summary(ols_e1)
summary(ols_e2)
summary(ols_e3)
summary(ols_e4)

# Grouping 3 OLS
ols_f1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 , data = flfp_d)
ols_f2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + religion_wvs + fertility, data = flfp_d)
ols_f3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + religion_pew + fertility, data = flfp_d)
ols_f4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + muslim + fertility, data = flfp_d)

summary(ols_f1)
summary(ols_f2)
summary(ols_f3)
summary(ols_f4)

stargazer(ols_std,ols_f1,ols_f2,ols_f4,type = "html", 
          title = "OLS - FLFPR and Cultural Groups",
          dep.var.caption = "Female Labor Force Participation Rate",
          #covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared","Patriarchal Culture",
          #                     "buddhist","christian","hindu","muslim","orthodox","other","muslim_dummy","fertility"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_cult_c.htm")

# Grouping 4 OLS
ols_g1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_4 , data = flfp_d)
ols_g2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_4 + religion_wvs + region + fertility, data = flfp_d)
ols_g3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_4 + religion_pew + region + fertility, data = flfp_d)
ols_g4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_4 + muslim + region + fertility, data = flfp_d)

summary(ols_g1)
summary(ols_g2)
summary(ols_g3)
summary(ols_g4)

# Grouping 5 OLS
ols_h1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_5 , data = flfp_d)
ols_h2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_5 + region + religion_wvs + fertility, data = flfp_d)
ols_h3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_5 + region + religion_pew + fertility, data = flfp_d)
ols_h4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_5 + region + muslim + fertility, data = flfp_d)

summary(ols_h1)
summary(ols_h2)
summary(ols_h3)
summary(ols_h4)

# Grouping 6 OLS
ols_i1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_6 + region , data = flfp_d)
ols_i2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_6 + region + religion_wvs + fertility, data = flfp_d)
ols_i3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_6 + region + religion_pew + fertility, data = flfp_d)
ols_i4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_6 + region + muslim + fertility, data = flfp_d)

summary(ols_i1)
summary(ols_i2)
summary(ols_i3)
summary(ols_i4)

# In OLS all groupings have a similar effect
# If only culture is included it negatively influences FLFP
# This tell us that a more patriarchal culture causes a lower female labor force participation
# Once we include religion, culture is not significant anymore
# This could hint at a mediation effect of culture through religion

######################
# Step 5.3 Fixed Effect Models
######################

# create panel data
p.data_d <- pdata.frame(flfp_d, index = c("countrycode","year"))

# standard model
fixed_d1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = p.data_d, model = "within")
summary(fixed_d1)
# the U-shape is significant

# Grouping 
fixed_da1 <- plm(flfpr ~ lg_gdppc*cult_1 + lg_gdppc_sq*cult_1 , data = p.data_d, model = "within")
fixed_db1 <- plm(flfpr ~ lg_gdppc*cult_2 + lg_gdppc_sq*cult_2 , data =p.data_d, model = "within")
fixed_dc1 <- plm(flfpr ~ lg_gdppc*cult_3 + lg_gdppc_sq*cult_3 , data = p.data_d, model = "within")
fixed_dd1 <- plm(flfpr ~ lg_gdppc*cult_4 + lg_gdppc_sq*cult_4 , data = p.data_d, model = "within")
fixed_de1 <- plm(flfpr ~ lg_gdppc*cult_5 + lg_gdppc_sq*cult_5 , data = p.data_d, model = "within")
fixed_df1 <- plm(flfpr ~ lg_gdppc*cult_6+ lg_gdppc_sq*cult_6 , data = p.data_d, model = "within")
fixed_dg1 <- plm(flfpr ~ lg_gdppc*religion_wvs+ lg_gdppc_sq*religion_wvs , data = p.data_d, 
                 model = "within")

summary(fixed_da1)
summary(fixed_db1)
summary(fixed_dc1)
summary(fixed_dd1)
summary(fixed_de1)
summary(fixed_df1)
summary(fixed_dg1)

# Controlling for religion
# grouping 1
fixed_da2 <- plm(flfpr ~ lg_gdppc*cult_1 +lg_gdppc_sq*cult_1 + 
                  lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_d, model = "within")
fixed_da3 <- plm(flfpr ~ lg_gdppc*cult_1 +lg_gdppc_sq*cult_1 + 
                  lg_gdppc*religion_pew + lg_gdppc_sq*religion_pew + fertility, data = p.data_d, model = "within")
fixed_da4 <- plm(flfpr ~ lg_gdppc*cult_1 +lg_gdppc_sq*cult_1 + 
                  lg_gdppc*muslim + lg_gdppc_sq*muslim + fertility, data = p.data_d, model = "within")
summary(fixed_da2)
summary(fixed_da3)
summary(fixed_da4)

#grouping 2
fixed_db2 <- plm(flfpr ~ lg_gdppc*cult_2 +lg_gdppc_sq*cult_2 + 
                  lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility , data = p.data_d, model = "within")
fixed_db3 <- plm(flfpr ~ lg_gdppc*cult_2 +lg_gdppc_sq*cult_2 + 
                  lg_gdppc*religion_pew + lg_gdppc_sq*religion_pew + fertility , data = p.data_d, model = "within")
fixed_db4 <- plm(flfpr ~ lg_gdppc*cult_2 +lg_gdppc_sq*cult_2 + 
                  lg_gdppc*muslim + lg_gdppc_sq*muslim + fertility , data = p.data_d, model = "within")

summary(fixed_db2)
summary(fixed_db3)
summary(fixed_db4)

#grouping 3
fixed_dc2 <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                  lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_d, model = "within")
fixed_dc3 <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                  lg_gdppc*religion_pew +lg_gdppc_sq*religion_pew + fertility, data = p.data_d, model = "within")
fixed_dc4 <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                   lg_gdppc*muslim +lg_gdppc_sq*muslim + fertility, data = p.data_d, model = "within")

summary(fixed_dc2)
summary(fixed_dc3)
summary(fixed_dc4)

stargazer(fixed_d1,fixed_dc1,fixed_dc2,fixed_dc4,type = "html", 
          title = "Country Fixed Effect - FLFPR and Cultural Groups",
          dep.var.caption = "Female Labor Force Participation Rate",
          #covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared","Patriarchal Culture",
          #                    "buddhist","christian","hindu","muslim","orthodox","other","muslim_dummy","fertility"),
          notes.label = "Significance Level",
          out = "regression_tables/fixed_cult_c.htm")

#grouping 4
fixed_dd2 <- plm(flfpr ~ lg_gdppc*cult_4 +lg_gdppc_sq*cult_4 + 
                   lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_d, model = "within")
fixed_dd3 <- plm(flfpr ~ lg_gdppc*cult_4 +lg_gdppc_sq*cult_4 + 
                   lg_gdppc*religion_pew + lg_gdppc_sq*religion_pew + fertility, data = p.data_d, model = "within")
fixed_dd4 <- plm(flfpr ~ lg_gdppc*cult_4 +lg_gdppc_sq*cult_4 + 
                   lg_gdppc*muslim+ lg_gdppc_sq*muslim + fertility , data = p.data_d, model = "within")

summary(fixed_dd2)
summary(fixed_dd3)
summary(fixed_dd4)

#grouping 5
fixed_de2 <- plm(flfpr ~ lg_gdppc*cult_5 +lg_gdppc_sq*cult_5 + 
                   lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_d, model = "within")
fixed_de3 <- plm(flfpr ~ lg_gdppc*cult_5 +lg_gdppc_sq*cult_5 + 
                   lg_gdppc*religion_pew + lg_gdppc_sq*religion_pew + fertility, data = p.data_d, model = "within")
fixed_de4 <- plm(flfpr ~ lg_gdppc*cult_5 +lg_gdppc_sq*cult_5 + 
                   lg_gdppc*muslim+ lg_gdppc_sq*muslim + fertility , data = p.data_d, model = "within")

summary(fixed_de2)
summary(fixed_de3)
summary(fixed_de4)

#grouping 6
fixed_dg2 <- plm(flfpr ~ lg_gdppc*cult_6 +lg_gdppc_sq*cult_6 + 
                   lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_d, model = "within")
fixed_dg3 <- plm(flfpr ~ lg_gdppc*cult_6 +lg_gdppc_sq*cult_6 + 
                   lg_gdppc*religion_pew + lg_gdppc_sq*religion_pew + fertility, data = p.data_d, model = "within")
fixed_dg4 <- plm(flfpr ~ lg_gdppc*cult_6 +lg_gdppc_sq*cult_6 + 
                   lg_gdppc*muslim+ lg_gdppc_sq*muslim + fertility , data = p.data_d, model = "within")

summary(fixed_dg2)
summary(fixed_dg3)
summary(fixed_dg4)

######################
# Step 6 Use subset of countries that had significant country effects in the original analysis
######################

flfp_sig <- flfp_d %>%
  inner_join(sig_coef, by = "countrycode", copy = FALSE) # observations reduced to 602

# create panel data frame
p.data_sig <- pdata.frame(flfp_sig, index = c("countrycode","year"))

# standard model OLS
ols_sig <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_sig)
summary(ols_sig)

# Grouping 3 OLS
ols_sig_3a <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 , data = flfp_sig)
ols_sig_3b <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + religion_wvs + fertility, data = flfp_sig)
ols_sig_3c <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + religion_pew + fertility, data = flfp_sig)
ols_sig_3d <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq + cult_3 + region + muslim + fertility, data = flfp_sig)

summary(ols_sig_3a)
summary(ols_sig_3b)
summary(ols_sig_3c)
summary(ols_sig_3d)

# grouping 3 fixed effect

# standard model fixed effect
fixed_sig <-  plm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = p.data_sig , model = "within")
summary(fixed_sig)
# grouping 3 Fixed Effect
fixed_sig_3a <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                   lg_gdppc*religion_wvs + lg_gdppc_sq*religion_wvs + fertility, data = p.data_sig , model = "within")
fixed_sig_3b <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                   lg_gdppc*religion_pew +lg_gdppc_sq*religion_pew + fertility, data = p.data_sig , model = "within")
fixed_sig_3c <- plm(flfpr ~ lg_gdppc*cult_3 +lg_gdppc_sq*cult_3 + 
                   lg_gdppc*muslim +lg_gdppc_sq*muslim + fertility, data = p.data_sig , model = "within")

summary(fixed_sig_3a)
summary(fixed_sig_3b)
summary(fixed_sig_3c)

######################
# Step 7 Further Analysis if required
######################
# split the sample into Muslim and non Muslim countries since "Muslim" seems to have a strong effect
p.data_d4 <- p.data_d %>%
  subset(muslim == 0)
p.data_d5 <- p.data_d %>%
  subset(muslim == 1)

# non muslim countries
ols_nm <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + mpat_cul, data = p.data_d4, 
              model = "pooling")
fixed_nm <- plm(flfpr ~ lg_gdppc*mpat_cul + lg_gdppc_sq*mpat_cul, data = p.data_d4, 
                model = "within")
summary(ols_nm)
bptest(ols_nm)
summary(fixed_nm)
bptest(fixed_nm)

# Muslim countries
ols_m <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq + mpat_cul, data = p.data_d5, 
               model = "pooling")
fixed_m <- plm(flfpr ~ lg_gdppc*cult_3 + lg_gdppc_sq*cult_3, data = p.data_d5, 
                 model = "within")
summary(ols_m)
bptest(ols_m)
summary(fixed_m)
bptest(fixed_m)
