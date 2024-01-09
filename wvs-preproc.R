####################
# Topic: Bachelorthesis - culture and religion preprocessing
# Author: Carsten Heine
# Comment: WVS
# Date: 06.11.2023
####################

################
# Step 0 load packages
################

# clean environment
rm(list=ls())
# load packages
library(tidyverse) # for data cleaning
library(countrycode) # for adapting country codes
library(readxl) # to read excel files
library(stevemisc) # to reverse code variables


######################
# Step 1 load data
######################

# read data in
wvs7 <- read_csv(file.path("raw_data","WVS_Cross-National_Wave_7_csv_v5_0.csv"), col_names = TRUE)
wvs6 <- read_delim(file.path("raw_data","WV6_Data_csv_v20201117.csv"), delim = ";",  col_names = TRUE)
wvs5 <- read_delim(file.path("raw_data","WV5_Data_csv_v20180912.csv"), delim = ";", col_names = TRUE)


# load Pew Research data (necessary for checking religion compositions later)
pew <- read_excel(file.path("raw_data","Religious_Composition_by_Country_2010-2050.xlsx"), sheet = "rounded_percentage")
# https://www.pewresearch.org/religion/interactives/religious-composition-by-country-2010-2050/ (29.12.2023)

######################
# Step 1.1 Clean Wave 7
######################

# variables of interest (Q33, Q29, Q30,q6,q289)
names(wvs7) <- tolower(colnames(wvs7))
wvs7 <- wvs7 %>%
  select(a_wave,a_year,c_cow_num,q33,q29,q30,q6,q289,b_country) %>% # the lower the values the more do the participants disagree
  rename(wave = a_wave) %>%
  rename(job_scar = q33) %>% # When jobs are scarce, men should have more right to a job than women 
  rename(pol_lead = q29) %>% # On the whole, men make better political leaders than women do
  rename(uni = q30) %>% # A university education is more important for a boy than for a girl
  rename(rel1 = q6) %>% # For each of the following, indicate how important it is in your life. Would you say it is ...
  rename(rel2 = q289) %>% # Do you belong to a religion or religious denomination? If yes, which one?
  rename(cow = c_cow_num) %>%
  rename(year = a_year) %>%
  # we subset for year <= 2019 because we only have GDP data until 2019 ( we might argue that culture changes very little over the course of three years such that we assume all cultural values where taken 2019)
  # subset(year <= 2019) %>%
  # replace missing values -1 (Don't know),-2 (No answer/refused),
  # -3 (Not applicable),-4 (Not asked), -5 (Missing; Not applicable for other reasons)
  mutate(job_scar = replace(job_scar, job_scar %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(pol_lead = replace(pol_lead, pol_lead %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(uni = replace(uni, uni %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(rel1 = replace(rel1, rel1 %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel2 = replace(rel2, rel2 %in% c(-1,-2,-3,-5), NA))

# check missing values
sum(is.na(wvs7$job_scar)) # 690 missing values
sum(is.na(wvs7$pol_lead)) # 2500 missing values
sum(is.na(wvs7$uni)) # 1367 missing values
sum(is.na(wvs7$rel1)) # 889 missing values
sum(is.na(wvs7$rel2)) # 1349 missing values
# compared to the total number of observations number of missing values is small
# and thus is no problem for the analysis
unique(wvs7$uni)
wvs7 <- wvs7 %>%
  # delete rows with missing values
  subset(!is.na(job_scar) & !is.na(pol_lead) & 
           !is.na(uni) & !is.na(rel1) & !is.na(rel2)) %>%
  # rescale the job_scar variable because its scale is from 1-5; strongly agree - strongly disagree
  # we rescale it to the scale the other two variables have: 1-4; strongly agree - strongly disagree
  # I use the following formula to rescale: y = ((X-Xmin)/Xrange)*n 
  # X is the variable of interest, Xmin is the minimum value, Xrange is the difference between the highest and lowest value and n is the highest value of the new scale
  mutate(job_scar = ((job_scar - 1)/4)*4) %>%
  # reverse code items to make interpretation later easier
  mutate(job_scar = 5 - job_scar) %>% 
  mutate(pol_lead = 5 - pol_lead) %>% 
  mutate(uni = 5 - uni) %>% 
  # create variable pat_cul as row_mean of the three variables above 
  mutate(pat_cul = (job_scar+pol_lead+uni)/3)


# check how many observations per country are available
count_country_7 <- wvs7 %>%
  group_by(cow) %>%
  summarize(count = n())

# convert to ISO3 codes check if all country codes will be converted correctly
wvs7$countrycode <- countrycode(wvs7$cow, "cown", "iso3c", nomatch = NA)
# 6, 201, 202, 348, 446, 714 are not matched

# check country that where not matched with codebook for wave 7
wvs7[wvs7$cow==714,"b_country"] # b_country = 344 which corresponds to Hong Kong
wvs7[wvs7$cow==446,"b_country"] # b_country = 446 which corresponds to Macao SAR
wvs7[wvs7$cow==6,"b_country"] # b_country = 630 which corresponds to Puerto Rico
wvs7[wvs7$cow==348,"b_country"] # b_country = 688 which corresponds to Serbia
wvs7[wvs7$cow==201,"b_country"] # b_country = 826 which corresponds to Great Britain
wvs7[wvs7$cow==202,"b_country"] # b_country = 909 which corresponds to Northern Ireland

# add non matched countries
# Macao = MAC
# Hong Kong = HKG
# Puerto Rico = PRI
# Serbia = SRB
# Great Britain = GBR
# Northern Ireland = GBR
wvs7$countrycode[wvs7$cow==714] <- "HKG"
wvs7$countrycode[wvs7$cow==446] <- "MAC"
wvs7$countrycode[wvs7$cow==6] <- "PRI"
wvs7$countrycode[wvs7$cow==348] <- "SRB"
wvs7$countrycode[wvs7$cow %in% c(201,202)] <- "GBR"

#check results
unique(wvs7$countrycode)

# select only ISO3C countrycodes
wvs7 <- wvs7 %>%
  select(-c("cow","b_country"))

######################
# Step 1.2 Clean Wave 6
######################
names(wvs6) <- tolower(colnames(wvs6))

# something seems to be coded wrongly with variable v144. There is another variable v144g, which contains the same values 7 magnitudes smaller. 
wvs6$v144 <- wvs6$v144/10000000
identical(wvs6$v144g, wvs6$v144g)

wvs6 <- wvs6 %>%
  select(v1,cow,v45,v51,v52,v144g,v9,v2,v262) %>% # the lower the values the more do the participants disagree
  rename(wave=v1) %>%
  rename(job_scar = v45) %>% # When jobs are scarce, men should have more right to a job than women 
  rename(pol_lead = v51) %>% # On the whole, men make better political leaders than women do
  rename(uni = v52) %>% # A university education is more important for a boy than for a girl
  rename(rel1 = v9) %>% # For each of the following, indicate how important it is in your life. Would you say it is ...
  rename(rel2 = v144g) %>%  # Do you belong to a religion or religious denomination? If yes, which one?
  rename(year = v262) %>%
  # replace missing values -1 (Don't know),-2 (No answer/refused),
  # -3 (Not applicable),-5 (Missing; Not applicable for other reasons)
  mutate(job_scar = replace(job_scar, job_scar %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(pol_lead = replace(pol_lead, pol_lead %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(uni = replace(uni, uni %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(rel1 = replace(rel1, rel1 %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel2 = replace(rel2, rel2 %in% c(-1,-2,-3,-5), NA)) 

# check missing values
sum(is.na(wvs6$job_scar)) # 1616 missing values
sum(is.na(wvs6$pol_lead)) # 4098 missing values
sum(is.na(wvs6$uni)) # 2605 missing values
sum(is.na(wvs6$rel1)) # 1348 missing values
sum(is.na(wvs6$rel2)) # 1273 missing values
# compared to the total number of observations number of missing values is small
# and thus is no problem for the analysis  

wvs6 <- wvs6 %>%
    # delete rows with missing values
    subset(!is.na(job_scar) & !is.na(pol_lead) & 
             !is.na(uni) & !is.na(rel1) & !is.na(rel2)) %>%
  # rescale the job_scar variable because its scale is from 1-3; agree - disagree
  # we rescale it to the scale the other two variables have: 1-4; strongly agree - strongly disagree
  mutate(job_scar = ((job_scar - 1)/2)*4) %>%
  # reverse code items to make interpretation later easier
  mutate(job_scar = 5 - job_scar) %>% # scale from 1-3; agree - disagree
  mutate(pol_lead = 5 - pol_lead) %>% # scale from 1-4; strongly agree - strongly disagree
  mutate(uni = 5 - uni) %>% # scale from 1-4; strongly agree - strongly disagree
  # create variable pat_cul as row_mean of the three variables above 
  mutate(pat_cul = (job_scar+pol_lead+uni)/3) 

# check how many observations per country are available
count_country_6 <- wvs6 %>%
    group_by(cow) %>%
    summarize(count = n())

# convert to ISO3 codes and check if all country codes will be converted correctly
wvs6$countrycode <- countrycode(wvs6$cow, "cown", "iso3c", nomatch = NA)
# 6, 201, 202, 348, 446, 714 are not matched

# check country that where not matched with code book for wave 6
wvs6[wvs6$cow==714,"v2"] # b_country = 344 which corresponds to Hong Kong
wvs6[wvs6$cow==667,"v2"] # b_country = 275 which corresponds to Palestine

# add non matched countries
# Palestine = PSE
# Hong Kong = HKG
wvs6$countrycode[wvs6$cow==714] <- "HKG"
wvs6$countrycode[wvs6$cow==667] <- "PSE"

#check results
unique(wvs6$countrycode)

# select only ISO3C country codes
wvs6 <- wvs6 %>%
  select(-c("cow","v2"))

######################
# Step 1.3 Clean Wave 5
######################

names(wvs5) <- tolower(names(wvs5))

wvs5 <- wvs5 %>%
  # select relevant columns
  select(v1,v2,cow,v44,v61,v62,v185,v9,v260) %>% # the lower the values the more do the participants disagree
  # rename columns
  rename(wave = v1) %>%
  rename(job_scar = v44) %>% # When jobs are scarce, men should have more right to a job than women 
  rename(pol_lead = v61) %>% # On the whole, men make better political leaders than women do
  rename(uni = v62) %>% # A university education is more important for a boy than for a girl
  rename(rel1 = v9) %>% # For each of the following, indicate how important it is in your life. Would you say it is ...
  rename(rel2 = v185) %>%  # Do you belong to a religion or religious denomination? If yes, which one?
  rename(year = v260) %>%
  # replace missing values -1 (Don't know),-2 (No answer/refused),
  # -3 (Not applicable),-5 (Missing; Not applicable for other reasons)
  mutate(job_scar = replace(job_scar, job_scar %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(pol_lead = replace(pol_lead, pol_lead %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(uni = replace(uni, uni %in% c(-1,-2,-3,-4,-5), NA)) %>%
  mutate(rel1 = replace(rel1, rel1 %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel2 = replace(rel2, rel2 %in% c(-1,-2,-3,-5), NA)) 

# check missing values
sum(is.na(wvs5$job_scar)) # 1535 missing values
sum(is.na(wvs5$pol_lead)) # 4040 missing values
sum(is.na(wvs5$uni)) # 2708 missing values
sum(is.na(wvs5$rel1)) # 1503 missing values
sum(is.na(wvs5$rel2)) # 993 missing values
# compared to the total number of observations number of missing values is small
# and thus is no problem for the analysis
unique(wvs6$pol_lead)
wvs5 <- wvs5 %>%
  # delete rows with missing values
  subset(!is.na(job_scar) & !is.na(pol_lead) & 
           !is.na(uni) & !is.na(rel1) & !is.na(rel2)) %>%
  # rescale the job_scar variable because its scale is from 1-3; agree - disagree
  # we rescale it to the scale the other two variables have: 1-4; strongly agree - strongly disagree
  mutate(job_scar = ((job_scar - 1)/2)*4) %>%
  # reverse code items to make interpretation later easier
  mutate(job_scar = 5 - job_scar) %>% # scale from 1-3; agree - disagree
  mutate(pol_lead = 5 - pol_lead) %>% # scale from 1-4; strongly agree - strongly disagree
  mutate(uni = 5 - uni) %>% # scale from 1-4; strongly agree - strongly disagree
  # create variable pat_cul as row_mean of the three variables above 
  mutate(pat_cul = (job_scar+pol_lead+uni)/3) 

# check how many observations per country are available
count_country_5 <- wvs5 %>%
  group_by(v2) %>%
  summarize(count = n()) # 58 countries

# check if all countrycodes will be converted correctly
wvs5$countrycode <- countrycode(wvs5$cow, "cown", "iso3c", nomatch = NA)
# 345, 714 are not matched

# check country that where not matched with codebook for wave 5
wvs5[wvs5$cow==714,"v2"] # b_country = 344 which corresponds to Hong Kong
wvs5[wvs5$cow==345,"v2"] # b_country = 688 which corresponds to Serbia

# add non matched countries
# Palestine = PSE
# Hong Kong = HKG
wvs5$countrycode[wvs5$cow==714] <- "HKG"
wvs5$countrycode[wvs5$cow==345] <- "SRB"

# drop unnecessary country code variables
wvs5 <- wvs5 %>%
  select(-c("v2","cow"))

######################
# Step 1.3 Check Countries
######################

# which countries are included in which data set 
coun_7 <- unique(wvs7$countrycode) # get all countries in wave 7
coun_6 <- unique(wvs6$countrycode) # get all countries in wave 6
coun_5 <- unique(wvs5$countrycode) # get all countries in wave 5

match <- intersect(coun_7,coun_6) # see matching countries
non_match <- c(setdiff(coun_7,coun_6),setdiff(coun_6,coun_7)) # see not matching countries

intersect(coun_7,non_match) # countries in wave 7 which are not in wave 6
intersect(coun_6,non_match) # countries in wave 6 which are not in wave 7

country <- data.frame(match) %>%
  rename(country = match)
non_match <- data.frame(non_match) %>%
  rename(country = non_match)
country <- rbind(country,non_match) # the data set includes 83 countries.

# include countries of wave 5
# take countries in wave 5 that are not in wave 6 and 7
non_match_2 <- setdiff(coun_5,country$country)
non_match_2 <- data.frame(non_match_2) %>%
  rename(country =  non_match_2)
country <- rbind(country,non_match_2) # 94 individual countries are included

######################
# Step 2.1 Create Variables for Wave 7
######################

# Which years are available in the WVS wave 7
unique(wvs7$year)  
# surveys have been conducted in: 2018 2017 2021 2020 2019 2022 
# in order to have a larger data set it might seem interesting to just take in all data.
# we can assume that cultural levels and religion are the same in 2019 as in 2020,2021,2022 for the respective countries

# Patriarchal culture variable
# because data on FLFP is only available until 2019, we will drop all countries that were added to the WVS 7 after 2019
wvs7_a <- wvs7 %>% 
# take row mean if country codes match
  group_by(countrycode,year) %>%
  summarize(across(pat_cul,mean)) 
# this yields the cultural variable per country and year

# Religiosity variable - we take share of people who answered: Religion is very important as proxy
# get sum of people who answered "very important" per country
rel1_7 <- wvs7 %>%
  group_by(countrycode, year) %>%
  summarize(count_total = n(),
            count_ver_imp = sum(rel1==1))

wvs7_a <- wvs7_a %>%
  inner_join(rel1_7, by = c("countrycode","year"), copy = FALSE) %>%
  mutate(rel1 = count_ver_imp/count_total*100) %>%
  select(-c("count_ver_imp","count_total"))

# Religious affiliation variable
# I will code a country as pertaining to one religion if more than 50% of the repsondants are of that religion
# No: do not belong to a denomination = 0
# Yes: Roman Catholic = 1
# Protestant = 2
# Orthodox (Russian/Greek/etc.) = 3
# Jew = 4
# Muslim = 5
# Hindu = 6
# Buddhist = 7
# Other = 8

# count how often certain religions are found in each country
rel2_7 <- wvs7 %>%
  #subset(year <= 2019) %>%
  group_by(countrycode, year) %>%
  summarize(total = n(),
            atheist = sum(rel2 == 0),
            catholic = sum(rel2 == 1),
            protestant = sum(rel2 == 2),
            orthodox = sum(rel2 == 3),
            jew = sum(rel2 == 4),
            muslim = sum(rel2 == 5),
            hindu = sum(rel2 == 6),
            buddhist = sum(rel2 == 7),
            other = sum(rel2 == 8)) %>%
  # add protestant and catholic together to create christian variable
  mutate(christian = catholic + protestant) %>%
  select(-c("catholic","protestant")) %>%
  # put affiliations into one column
pivot_longer(cols = c("atheist","christian","orthodox","jew",
                      "muslim","hindu","buddhist","other"),
             names_to = "religion", values_to = "count") %>%
  # create share of religious affiliates per country
  mutate(rel_share = count/total*100) %>%
  # subset for majority religion in country
  group_by(countrycode) %>%
  filter(rel_share == max(rel_share)) 

# create final data set containing cultural and religion variables
wvs7_b <- wvs7_a %>%
  inner_join(rel2_7, by = c("countrycode","year"), copy = FALSE) %>%
  mutate(wave = 7) %>%
  select(-c("count","total")) 

######################
# Step 2.2 Create Variables for Wave 6
######################

# check year
unique(wvs6$year)

# patriarchal culture variable
wvs6_a <- wvs6 %>% 
  # take row mean if country codes match
  group_by(countrycode,year) %>%
  summarize(across(pat_cul,mean)) 

# religiosity variable 
rel1_6 <- wvs6 %>%
  group_by(countrycode) %>%
  summarize(count_total = n(),
            count_ver_imp = sum(rel1 == 1)) %>%
  mutate(rel1 = count_ver_imp/count_total*100) %>%
  select(-c("count_ver_imp","count_total"))

# merge data with patriarchal culture and religiosity
wvs6_a <- wvs6_a %>%
  inner_join(rel1_6,by = "countrycode", copy = FALSE) 

# religion variable
rel2_6 <- wvs6 %>%
  group_by(countrycode) %>%
  summarize(total = n(),
            atheist = sum(rel2 == 0),
            catholic = sum(rel2 == 1),
            protestant = sum(rel2 == 2),
            orthodox = sum(rel2 == 3),
            jew = sum(rel2 == 4),
            muslim = sum(rel2 == 5),
            hindu = sum(rel2 == 6),
            buddhist = sum(rel2 == 7),
            other = sum(rel2 == 8)) %>%
  # add protestant and catholic together to create christian variable
  mutate(christian = catholic + protestant) %>%
  select(-c("catholic","protestant")) %>%
  # put affiliations into one column
  pivot_longer(cols = c("atheist","christian","orthodox","jew",
                        "muslim","hindu","buddhist","other"),
               names_to = "religion", values_to = "count") %>%
  # create share of religious affiliates per country
  mutate(rel_share = count/total*100) %>%
  # subset for majority religion in country
  group_by(countrycode) %>%
  filter(rel_share == max(rel_share)) 

# merge datasets to obtain patriarchal culture, religiosity, and religion
wvs6_b <- wvs6_a %>%
  inner_join(rel2_6,by = "countrycode", copy = FALSE) %>%
  mutate(wave = 6) %>%
  select(-c("count","total")) 

######################
# Step 2.3 Create Variables for Wave 5
######################

# determine at which year surveys were administered
unique(wvs5$year)

# patriarchal culture variable
wvs5_a <- wvs5 %>% 
  # take row mean 
  group_by(countrycode,year) %>%
  summarize(across(pat_cul,mean)) 

# religiosity variable 
rel1_5 <- wvs5 %>%
  group_by(countrycode) %>%
  summarize(count_total = n(),
            count_ver_imp = sum(rel1 == 1)) %>%
  mutate(rel1 = count_ver_imp/count_total*100) %>%
  select(-c("count_ver_imp","count_total"))

# merge data with patriarchal culture and religiosity
wvs5_a <- wvs5_a %>%
  inner_join(rel1_5,by = "countrycode", copy = FALSE) 

# religion variable
rel2_5 <- wvs5 %>%
  group_by(countrycode) %>%
  summarize(total = n(),
            atheist = sum(rel2 == 0),
            catholic = sum(rel2 == 64),
            protestant = sum(rel2 == 62),
            orthodox = sum(rel2 == 52),
            jew = sum(rel2 == 42),
            muslim = sum(rel2 == 49),
            hindu = sum(rel2 == 31),
            buddhist = sum(rel2 == 12),
            other = sum(!rel2 %in% c(0,64,62,52,42,49,31,12))) %>%
  # add protestant and catholic together to create christian variable
  mutate(christian = catholic + protestant) %>%
  select(-c("catholic","protestant")) %>%
  # put affiliations into one column
  pivot_longer(cols = c("atheist","christian","orthodox","jew",
                        "muslim","hindu","buddhist","other"),
               names_to = "religion", values_to = "count") %>%
  # create share of religious affiliates per country
  mutate(rel_share = count/total*100) %>%
  # subset for majority religion in country
  group_by(countrycode) %>%
  filter(rel_share == max(rel_share)) 

# merge data sets to obtain patriarchal culture, religiosity, and religion
wvs5_b <- wvs5_a %>%
  inner_join(rel2_5,by = "countrycode", copy = FALSE) %>%
  mutate(wave = 5) %>%
  select(-c("count","total"))

######################
# Step 3 Merge World Value Survey Waves 5,6,7
######################

wvs <- rbind(wvs6_b,wvs7_b,wvs5_b) %>%
  arrange(countrycode,year) %>%
  mutate(year = replace(year, year %in% c(2020,2021,2022),2019)) %>%
  rename(religiosity = rel1)

######################
# Step 4.1 Clean Pew Research Data
######################

names(pew) <- tolower(colnames(pew))

# select variables and year of interest
pew <- pew %>%
  select(-c("row_number","level","nation_fk","region","all religions")) %>%
  subset(year %in% c(2010,2020)) %>%
  rename(atheist = unaffiliated) %>%
  # convert to long format
  pivot_longer(cols = c("buddhists","christians","folk religions",
                        "hindus","jews","muslims","atheist","other religions"), 
               names_to = "religion",
               values_to = "share") %>%
  # subset for individual countries
  subset(country != "All Countries") %>%
  # replace values <1 and >99 with 1 and 99 to have numeric values
  mutate(share = str_replace(share,"< 1.0","1")) %>%
  mutate(share = as.numeric(str_replace(share,">99.0","99")))

# now we group by year and country and take the religion with the most followers
pew1 <- pew %>%
  group_by(country,year) %>%
  filter(share == max(share)) %>%
  arrange(country) %>%
  ungroup() %>%
  rename(max_rel = religion) %>%
  rename(max_sh = share)

# here we take the religion with the second most followers, which we will use later
pew1 <- pew %>%
  group_by(country,year) %>%
  filter(share == max(share[share!=max(share)])) %>%
  # in some countries the majority religion is so dominant that all other religions
  # have the second highest value 1.
  # In this case I drop the second highest value
  subset(share != 1) %>%
  arrange(country) %>%
  ungroup() %>%
  rename(sec_max_rel = religion) %>%
  rename(sec_max_sh = share) %>%
  full_join(pew1, by = c("country","year"), copy = FALSE) 

# If a country has many small religions and one religion with more than 50% affiliates,
# one can assume that the effect of the majority religion dominates the others given its relative size
# If, however, all religions are similarly big, we cannot expect to discern the dominating 
# effect of one of them. 
# Therefore, I only code countries pertaining to a certain religion if the difference between
# the largest and second largest religion are at least 10%.
# Otherwise it is not clear that one religion has a dominating effect.
# Thus, we avoid coding countries which are not dominated by one religion. 
# I code countries were two or more religions share a similar amount of followers as "mixed"

# lets check all countries where the difference between largest and second largest religion is
# smaller than 10%
rel_mix <- pew1 %>%
  mutate(rel_diff = max_sh-sec_max_sh) %>%
  subset(rel_diff < 10) 
# I code Bosnia-Herzegovina, Ivory Coast, Netherlands, Nigeria, Togo and Latvia as mixed countries
pew1 <- pew1 %>%
  mutate(max_rel = case_when(country %in% c("Bosnia-Herzegovina","Ivory Coast","Netherlands","Nigeria","Togo","Latvia")
                             ~ "mixed",
                             TRUE ~ as.character(max_rel))) %>%
  subset(year == 2010) %>%
  # select only country and majority religion
  select(country,max_rel,max_sh) %>%
  # delete duplicated rows
  distinct() %>%
  # turn country names into the iso3c country codes 
  # Kosovo and Channel Islands cannot be matched
  # This is not a problem as they are not part of the countries I investigate
  # Therefore, we delete these two countries here
  subset(!country %in% c("Kosovo","Channel Islands")) %>%
  mutate(countrycode = countrycode(country, "country.name", "iso3c", nomatch = NA))

# select relevant columns
pew1 <- pew1 %>%  
  select(countrycode,max_rel,max_sh) %>%
  arrange(countrycode)

######################
# Step 4.2 Check Religion Variable in WVS 
######################

# Some results on the religion variable need to be clarified because data was not available or countries
# are coded into different religious groups during the different years

# Armenia (ARM) 
# is coded as orthodox in wave 7 and other in wave 6
# the older data shows that in wave 6 this "other" refers to "Armenian Apostolic Church"
# in line with wave 7 I will count this as orthodox
wvs$religion[wvs$countrycode == "ARM"] <- "orthodox"

# Armenia (AUS) 
# is coded as christian in wave 5,6 and atheist in wave 7
# I code it as christian here
wvs$religion[wvs$countrycode == "AUS"] <- "christian"

# Canada (CAN)
# is coded as atheist in wave 7 and catholic in wave 5
# Given (https://www.pewresearch.org/short-reads/2019/07/01/5-facts-about-religion-in-canada/)
# I will code it as catholic
wvs$religion[wvs$countrycode == "CAN"] <- "christian"

# Columbia (COL)
# is coded as christian in wave 6 and atheist in wave 7
# Given the pew research data it is more plausible to code it as christian
wvs$religion[wvs$countrycode == "COL"] <- "christian"

# Cyprus (CYP)
# In Cyprus, the share of orthodox and Muslim adherents is almost the same in all waves
# Therefore, we will code it as mixed, because we cannot tell which culture should dominate
wvs$religion[wvs$countrycode == "CYP"] <- "other"

# Germany (DEU)
# Germany is coded as atheist in wave 5,6 and christian in wave 7
# Given the pew data, I will code Germany as christian
wvs$religion[wvs$countrycode == "DEU"] <- "christian"

# Egypt (EGY)
# here the question was not asked in wave 6
# Since its population is about 90% Muslim in wave 7, we can code Egypt as Muslim country
wvs$religion[wvs$countrycode == "EGY"] <- "muslim"

# Finland (FIN)
# is coded as other since the "Evangelical Lutheran Church of Finland" does not count as christian in the wvs
# given the PEW Data on Finland, we can code it as christian
wvs$religion[wvs$countrycode == "FIN"] <- "christian"

# Iran (IRN)
# Iran is not coded Muslim in 2007 probably because a distinction was made between Shiite and Sunni
wvs$religion[wvs$countrycode == "IRN"] <- "muslim"

# Kuwait (KWT)
# the question on religion was not asked in Kuwait.
# Given Ross (2008), we can code the country as Muslim
wvs$religion[wvs$countrycode == "KWT"] <- "muslim"

# South Korea (KOR)
# was coded as atheist in wave 7,6 and christian in wave 5
# Given the PEW Research data I will code it as atheist
wvs$religion[wvs$countrycode == "KOR"] <- "atheist"

# Libya (LBY)
# for all observations Libya has the value 9 that has no correspondance in the codebook.
# I therefore assume the question was not asked 
# Given that Libya was coded muslim in 2022, I assume it was also muslim in 2014.
wvs$religion[wvs$countrycode == "LBY"] <- "muslim"
# delete duplicates
wvs <- wvs[!duplicated(wvs), ]

# Qatar (KWT)
# the question on religion was not asked in Kuwait.
# Given Ross (2008), we can code the country as muslim
wvs$religion[wvs$countrycode == "QAT"] <- "muslim"

# New Zealand (NZL)
# is coded as atheist and other. 
# I will code it as atheist (https://figure.nz/chart/RfmHYb2IsMMrn9OC)
wvs$religion[wvs$countrycode == "NZL"] <- "atheist"

# Sweden (SWE)
# was coded as other in wave 5 (the church of Sweden) and atheist in wave 6
# PEW codes it as mainly christian 
# Therefore, I code it as protestant here, but given that I 
# combine protestant and catholic later, it is basically coded as christian
wvs$religion[wvs$countrycode == "SWE"] <- "christian"

# Taiwan (TWN)
# is coded differently each wave and the majority religion has always a pretty low share in general
# so I code it as other
wvs$religion[wvs$countrycode == "TWN"] <- "other"

# United States (USA)
# has been coded as christian in wave 5,6 and as atheist in waves 7
# Given that the PEW data codes it as christian I will also coded christian
wvs$religion[wvs$countrycode == "USA"] <- "christian"

# Vietnam (VNM)
# seems to have seen a change in the majority religion since 2006.
# In 2006 the majority religion was "traditional", whereas it is "atheist" in 2019. 
# Given the Pew data I will code it as other
wvs$religion[wvs$countrycode == "VNM"] <- "other"

# Zimbabwe (ZWE)
# should be coded as christian
wvs$religion[wvs$countrycode == "ZWE"] <- "christian"

######################
# Step 4.3 Merge Pew and WVS data
######################

# The Pew Data considers Catholics and Protestants as Christians

wvs <- inner_join(wvs,pew1, by = "countrycode", copy = FALSE) %>%
  # we code majority protestant or catholic countries as christian
  mutate(max_rel = str_replace(max_rel,"christians","christian")) %>%
  mutate(max_rel = str_replace(max_rel,"muslims","muslim")) %>%
  mutate(max_rel = str_replace(max_rel,"hindus","hindu")) %>%
  mutate(max_rel = str_replace(max_rel,"buddhists","buddhist")) %>%
  rename(religion_wvs = religion) %>%
  rename(religion_pew = max_rel)

# We see that in most cases the pew data attest way higher rates of religious affiliation than does the wvs data
# Nevertheless, most countries are grouped into the same religions
rel_diff <- wvs %>%
  select(countrycode,religion_wvs,religion_pew) %>%
  subset(religion_wvs != religion_pew)
# what is coded as "folk religions" in the pew data is coded as "other" in the wvs data
# Therefore I coded it as other in the pew data too
wvs$religion_pew[wvs$countrycode %in% c("MAC","TWN","VNM")] <- "other"

# We can see a trend between the countries that are coded differently
# Mostly the PEW data codes countries as christian which are coded as orthodox or atheist in the wvs data. 
# There are various possible reasons for this divergence:
# 1. The sample for the WVS was in terms of religion not an accurate representation of the population
# 2. Maybe people answer that their are atheist because they do not believe in god but still pertain officially to a religion
# 3. The Pew estimations are biased. 

# Because of the divergence I will run the final analysis with both variables for religion 

######################
# Step 5 Save Data
######################
write.csv(wvs,file.path("analysis_data","wvs.csv"), row.names = FALSE)



