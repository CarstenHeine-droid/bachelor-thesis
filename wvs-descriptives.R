####################
# Topic: Bachelorthesis
# Author: Carsten Heine
# Comment: VWS
# Date: 06.11.2023
####################

# Description of world value survey

################
# Step 0 load packages
################

# clean environment
rm(list=ls())

library("tidyverse") # for data cleaning
library("countrycode") # for adapting countrycodes

######################
# Step 1 Clean Datasets
######################

# read data in
wvs7 <- read_csv(file.path("raw_data","WVS_Cross-National_Wave_7_csv_v5_0.csv"), col_names = TRUE)
wvs6 <- read_delim(file.path("raw_data","WV6_Data_csv_v20201117.csv"), delim = ";",  col_names = TRUE)

######################
# Step 1.1 clean wvs 7
######################

# variables of interest (Q33, Q29, Q30,q6,q289)
names(wvs7) <- tolower(colnames(wvs7))
wvs7 <- wvs7 %>%
  select(a_wave,a_year,c_cow_num,q33,q29,q30,q6,q289,b_country) %>% # the lower the values the more do the participants disagree
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
  # -3 (Not applicable),-5 (Missing; Not applicable for other reasons)
  mutate(job_scar = replace(job_scar, job_scar %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(pol_lead = replace(pol_lead, pol_lead %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(uni = replace(uni, uni %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel1 = replace(rel1, rel1 %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel2 = replace(rel2, rel2 %in% c(-1,-2,-3,-5), NA)) 
  
sum(is.na(wvs7$job_scar)) # 690 missing values
sum(is.na(wvs7$pol_lead)) # 2500 missing values
sum(is.na(wvs7$uni)) # 1367 missing values
sum(is.na(wvs7$rel1)) # 889 missing values
sum(is.na(wvs7$rel2)) # 1349 missing values
# compared to the total number of observations number of missing values is small
# and thus is no problem for the analysis

wvs7 <- wvs7 %>%
  # delete rows with missing values
  subset(!is.na(job_scar) & !is.na(pol_lead) & 
           !is.na(uni) & !is.na(rel1) & !is.na(rel2)) %>%
  # create variable pat_cul as row_mean of the three variables above 
  mutate(pat_cul = (job_scar+pol_lead+uni)/3) 

# check how many observations per country are available
count_country_7 <- wvs7 %>%
  group_by(cow) %>%
  summarize(count = n())

# check if all countrycodes will be converted correctly
countrycode(wvs7$cow, "cown", "iso3c", nomatch = NA)
# 6, 201, 202, 348, 446, 714 are not matched

# check country that where not matched with codebook for wave 7
wvs7[wvs7$cow==714,"b_country"] # b_country = 344 which corresponds to Hong Kong
wvs7[wvs7$cow==446,"b_country"] # b_country = 446 which corresponds to Macao SAR
wvs7[wvs7$cow==6,"b_country"] # b_country = 630 which corresponds to Puerto Rico
wvs7[wvs7$cow==348,"b_country"] # b_country = 688 which corresponds to Serbia
wvs7[wvs7$cow==201,"b_country"] # b_country = 826 which corresponds to Great Britain
wvs7[wvs7$cow==202,"b_country"] # b_country = 909 which corresponds to Northern Ireland

# convert to iso codes
wvs7$countrycode <- countrycode(wvs7$cow, "cown", "iso3c", nomatch = NA)
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
# Step 1.2 clean wvs 6
######################
names(wvs6) <- tolower(colnames(wvs6))

# something seems to be coded wrongly with variable v144. There is another variable v144g, which contains the same values 7 magnitudes smaller. 
wvs6$v144 <- wvs6$v144/10000000
identical(wvs6$v144g, wvs6$v144g)

wvs6 <- wvs6 %>%
  select(v1,cow,v45,v51,v52,v144g,v9,v2) %>% # the lower the values the more do the participants disagree
  rename(job_scar = v45) %>% # When jobs are scarce, men should have more right to a job than women 
  rename(pol_lead = v51) %>% # On the whole, men make better political leaders than women do
  rename(uni = v52) %>% # A university education is more important for a boy than for a girl
  rename(rel1 = v9) %>% # For each of the following, indicate how important it is in your life. Would you say it is ...
  rename(rel2 = v144g) %>%  # Do you belong to a religion or religious denomination? If yes, which one?
  # replace missing values -1 (Don't know),-2 (No answer/refused),
  # -3 (Not applicable),-5 (Missing; Not applicable for other reasons)
  mutate(job_scar = replace(job_scar, job_scar %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(pol_lead = replace(pol_lead, pol_lead %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(uni = replace(uni, uni %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel1 = replace(rel1, rel1 %in% c(-1,-2,-3,-5), NA)) %>%
  mutate(rel2 = replace(rel2, rel2 %in% c(-1,-2,-3,-5), NA)) 

sum(is.na(wvs6$job_scar)) # 1616 missing values
sum(is.na(wvs6$pol_lead)) # 4098 missing values
sum(is.na(wvs6$uni)) # 2605 missing values
sum(is.na(wvs6$rel1)) # 1348 missing values
sum(is.na(wvs6$rel2)) # 1273 missing values
# clean ccode (country codes dataset)
  
wvs6 <- wvs6 %>%
    # delete rows with missing values
    subset(!is.na(job_scar) & !is.na(pol_lead) & 
             !is.na(uni) & !is.na(rel1) & !is.na(rel2)) %>%
    # create variable pat_cul as row_mean of the three variables above 
    mutate(pat_cul = (job_scar+pol_lead+uni)/3) 
  
# check how many observations per country are available
count_country_6 <- wvs6 %>%
    group_by(cow) %>%
    summarize(count = n())

# check if all countrycodes will be converted correctly
countrycode(wvs6$cow, "cown", "iso3c", nomatch = NA)
# 6, 201, 202, 348, 446, 714 are not matched

# check country that where not matched with codebook for wave 7
wvs6[wvs6$cow==714,"v2"] # b_country = 344 which corresponds to Hong Kong
wvs6[wvs6$cow==667,"v2"] # b_country = 275 which corresponds to Palestine

# convert to iso codes
wvs6$countrycode <- countrycode(wvs6$cow, "cown", "iso3c", nomatch = NA)
# add non matched countries
# Palestine = PSE
# Hong Kong = HKG
wvs6$countrycode[wvs6$cow==714] <- "HKG"
wvs6$countrycode[wvs6$cow==667] <- "PSE"

#check results
unique(wvs6$countrycode)

# select only ISO3C countrycodes
wvs6 <- wvs6 %>%
  select(-c("cow","v2"))
  
 
######################
# Step 1.3 Check how many countries are in the dataset
######################

# which countries are included in which dataset 
coun_7 <- unique(wvs7$countrycode) # get all countries in wave 7
coun_6 <- unique(wvs6$countrycode) # get all countries in wave 6

match <- intersect(coun_7,coun_6) # see matching countries
non_match <- c(setdiff(coun_7,coun_6),setdiff(coun_6,coun_7)) # see not matching countries

intersect(coun_7,non_match) # countries in wave 7 which are not in wave 6
intersect(coun_6,non_match) # countries in wave 6 which are not in wave 7

country <- data.frame(match) %>%
  rename(country = match)
non_match <- data.frame(non_match) %>%
  rename(country = non_match)
country <- rbind(country,non_match) # the dataset includes 83 countries.
# because we conduct a pooled OLS estimation countries that match will be included twice: with there value in 2014 and 2017

# Data on wave 6 VWS does not contain information on the year, therefore we assume all values are from year 2014. 
# This assumption seems plausible because values usually do not change quickly over time. 

######################
# Step 2.1 Create variables of interest for wave 7
######################

# Which years are available in the WVS wave 7
unique(wvs7$year)  
# surveys have been conducted in: 2018 2017 2021 2020 2019 2022 
# in order to have a larger data set it might seem interesting to just take in all data.
# we can assume that cultural levels and religion are the same in 2019 as in 2020,2021,2022 for the respective countries

# Patriarchal culture variable
# because data on FLFP is only available until 2019, we will drop all countries that were added to the WVS 7 after 2019
wvs7_a <- wvs7 %>% 
  #subset(year <= 2019) %>%
# take row mean if countrycodes match
  group_by(countrycode,year) %>%
  summarize(across(pat_cul,mean)) 
# this yields the cultural variable per country and year

# Religiosity variable - we take share of people who answered: Religion is very important as proxy
# get sum of people who answered "very important" per country
rel1_7 <- wvs7 %>%
  #subset(year <= 2019) %>%
  group_by(countrycode, year) %>%
  summarize(count_total = n(),
            count_ver_imp = sum(rel1==1))

# rel1_7b <- wvs7 %>%
#   subset(rel1 == 1 & year <= 2019) %>%
#   group_by(countrycode, year) %>%
#   summarize(count_ver_imp = n())

# # join the datasets with the sum for all people responding "very important" and the total respondants
# rel1_7 <- inner_join(rel1_7a,rel1_b, by = c("countrycode","year"), copy = FALSE) %>%
#   # calculate share of people who answer that religion is very important for them
#   mutate(rel1 = count_ver_imp/count_total*100) %>%
#   select(-c("count_ver_imp","count_total"))

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

# count how often certain religions are found in a country
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
  # put affiliations into one colum
pivot_longer(cols = c("atheist","catholic","protestant","orthodox","jew",
                      "muslim","hindu","buddhist","other"),
             names_to = "religion", values_to = "count") %>%
  # create share of religious affiliates per country
  mutate(rel_share = count/total*100) %>%
  # subset for majority religion in country
  group_by(countrycode) %>%
  filter(rel_share == max(rel_share)) 

# create final dataset containing cultural and religion variables
wvs7_b <- wvs7_a %>%
  inner_join(rel2_7, by = c("countrycode","year"), copy = FALSE) %>%
  select(-c("count","total")) %>%
  mutate(wave = 7)
  
# take the fifth smallest value
fif_min <- sort(wvs7_a$pat_cul, decreasing = FALSE)[5]
df <- wvs7_a %>%
  subset(pat_cul <= fif_min)
ggplot(df,aes(x=countrycode,y=pat_cul))+
  geom_bar(stat="identity")

######################
# Step 2.2 Create variables of interest for wave 6
######################

# wave 6 does not contain information on the years the information was acquired
# I add this information by hand, as it is available on the WVS website: https://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp (11.12.2023)

wvs6 <- wvs6 %>%
  mutate(year = case_when(countrycode %in% c("HTI") ~ 2015,
                           countrycode %in% c("DZA", "BRA", "JOR", "KWT", "LBY", "YEM","HKG","GEO") ~ 2014,
                           countrycode %in% c("ARG", "CHN", "ECU", "EGY", "DEU", "IRQ", "LBN", 
                                              "ZAF", "THA", "TUN","PSE") ~ 2013,
                           countrycode %in% c("AUS", "COL", "CHL", "GHA", "IND", "MYS", "MEX", 
                                              "NLD", "NGA", "NZL", "PAK", "PER", "PHL", "POL", 
                                              "ROU", "RWA", "SGP", "TWN", "ZWE") ~ 2012,
                           countrycode %in% c("ARM", "AZE", "BLR", "CYP", "EST", "KAZ", 
                                              "KGZ", "MAR", "RUS", "ESP", "SWE", 
                                              "TTO", "TUR", "UKR", "USA", "URY", "UZB","SVN") ~ 2011,
                           countrycode %in% c("JPN", "QAT", "KOR") ~ 2010,
                           .default = NA))

# check if all countries have been assigned
unique(wvs6$year)
# patriarchal culture variable
wvs6_a <- wvs6 %>% 
  # take row mean if countrycodes match
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
  # put affiliations into one columne
  pivot_longer(cols = c("atheist","catholic","protestant","orthodox","jew",
                        "muslim","hindu","buddhist","other"),
               names_to = "religion", values_to = "count") %>%
  # create share of religious affiliates per country
  mutate(rel_share = count/total*100) #%>%
  # subset for majority religion in country
  group_by(countrycode) %>%
  filter(rel_share == max(rel_share)) 

# the religion question was not asked in Egypt.
# Since its population is about 90% Muslim in wave7, we can code Egypt as Muslim country

# merge datasets to obtain patriarchal culture, religiosity, and religion
wvs6_b <- wvs6_a %>%
  inner_join(rel2_6,by = "countrycode", copy = FALSE) %>%
  mutate(wave = 6) %>%
  select(-c("count","total")) %>%
  mutate(wave = 6)

# we cannot compute a religion variable for this data, because the Variable V144 and V144G don't make sense.
# therefore, we will ascribe countries to a religion according to their religion value in wave 7

######################
# Step 3 join wave 6 and 7
######################

wvs <- rbind(wvs6_b,wvs7_b) %>%
  arrange(countrycode,year)

#
# there seem to be some not intuitive results in wave 6 in the religion variable:
# we will use an earlier release of the data to confirm the religions measured sofar

load(file.path("raw_data","WV6_Data_R_v_2016_01_01.rdata"))
df6 <- WV6_Data_R %>%
  select(V144,V2)

# Egypt (EGY)
# here the question was not asked
# Since its population is about 90% Muslim in wave7, we can code Egypt as Muslim country
wvs$religion[wvs$countrycode == "EGY"] <- "muslim"

# Armenia (ARM) 
# is coded as orthodox in wave 7 and other in wave 6
# the older data shows that in wave 6 this "other" refers to "Armenian Apostolic Church"
# in line with wave 7 I will count this as orthodox
wvs$religion[wvs$countrycode == "ARM"] <- "orthodox"

# Columbia (COL)
# is coded as mainly atheist in 2018 and catholic in 2011. 
# Given that in 2018 39% are Catholics and 10% others while only 46 are atheists,
# it might be the case that the 10% other are another type of Catholics and
# were therefore not counted in the overarching group of Catholics. 
# If this is the case, Catholicism was the majority religion. 
# Another explanation could be that a sample of 2500 interviewees was too small to gain accurate data on the
# religion. 
# In the end, it seems plausible to code Columbia as catholic country
wvs$religion[wvs$countrycode == "COL"] <- "catholic"

# Cyprus (CYP)
# In Cyprus, the share of orthodox and muslim adherents is almost the same in both datasets
# Therefore, we will leave it as it is or I might code it as other, because we cannot tell which culture should dominate

# Kuwait (KWT)
# the question on religion was not asked in Kuwait.
# Given Ross (2008), we can code the country as muslim
wvs$religion[wvs$countrycode == "KWT"] <- "muslim"

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

# Taiwan (TWN)
# has a pretty low share, so I code it as other

# Zimbabwe (ZWE)
# should be coded as protestant





