####################
# Topic: Bachelorthesis Analysis
# Author: Carsten Heine
# Comment: OLS/Fixed effect/GMM (Gaddis,Klasen)
# Date: 13.12.2023
####################

######################
# Step 0 Load Packages
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

# load data
# 1990-2010
flfp_a1 <- read_csv(file.path("analysis_data","flfp_a1.csv"), col_names = TRUE)
flfp_a2 <- read_csv(file.path("analysis_data","flfp_a2.csv"), col_names = TRUE)
flfp_a3 <- read_csv(file.path("analysis_data","flfp_a3.csv"), col_names = TRUE)

# 1990-2019
flfp_b1 <- read_csv(file.path("analysis_data","flfp_b1.csv"), col_names = TRUE)
flfp_b2 <- read_csv(file.path("analysis_data","flfp_b2.csv"), col_names = TRUE)
flfp_b3 <- read_csv(file.path("analysis_data","flfp_b3.csv"), col_names = TRUE)
flfp_b3_ay <- read_csv(file.path("analysis_data","flfp_b3_ay.csv"), col_names = TRUE) # contains data for all years

# oecd
oecd_a1 <- read_csv(file.path("analysis_data","oecd_a1.csv"), col_names = TRUE) 
oecd_a2 <- read_csv(file.path("analysis_data","oecd_a2.csv"), col_names = TRUE)
oecd_b1 <- read_csv(file.path("analysis_data","oecd_b1.csv"), col_names = TRUE)
oecd_b2 <- read_csv(file.path("analysis_data","oecd_b2.csv"), col_names = TRUE)

# non-oecd
n_oecd_a1 <- read_csv(file.path("analysis_data","n_oecd_a1.csv"), col_names = TRUE) 
n_oecd_a2 <- read_csv(file.path("analysis_data","n_oecd_a2.csv"), col_names = TRUE)
n_oecd_b1 <- read_csv(file.path("analysis_data","n_oecd_b1.csv"), col_names = TRUE)
n_oecd_b2 <- read_csv(file.path("analysis_data","n_oecd_b2.csv"), col_names = TRUE)

# world value survey
wvs <- read_csv(file.path("analysis_data","wvs.csv"), col_names = TRUE)

# word development indicators
wdi <- read_csv(file.path("analysis_data","wdi.csv"), col_names = TRUE)

######################
# Step 1 Panel Data 
######################

# create the panel data sets
p.data_a1 <- pdata.frame(flfp_a1, index = c("countrycode","year"))
p.data_a2 <- pdata.frame(flfp_a2, index = c("countrycode","year"))
p.data_a3 <- pdata.frame(flfp_a3, index = c("countrycode","year"))

p.data_b1 <- pdata.frame(flfp_b1, index = c("countrycode","year"))
p.data_b2 <- pdata.frame(flfp_b2, index = c("countrycode","year"))
p.data_b3 <- pdata.frame(flfp_b3, index = c("countrycode","year"))

p.data_oecd1 <- pdata.frame(oecd_a1, index = c("countrycode","year"))
p.data_oecd2 <- pdata.frame(oecd_a2, index = c("countrycode","year"))
p.data_oecd3 <- pdata.frame(oecd_b1, index = c("countrycode","year"))
p.data_oecd4 <- pdata.frame(oecd_b2, index = c("countrycode","year"))

p.data_noecd1 <- pdata.frame(n_oecd_a1, index = c("countrycode","year"))
p.data_noecd2 <- pdata.frame(n_oecd_a2, index = c("countrycode","year"))
p.data_noecd3 <- pdata.frame(n_oecd_b1, index = c("countrycode","year"))
p.data_noecd4 <- pdata.frame(n_oecd_b2, index = c("countrycode","year"))

#####################
# Step 2.1 OLS Analysis
#####################

# pooled analysis
ols_a1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_a1)
ols_a2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_a2)
ols_a3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_a3)
summary(ols_a1)
summary(ols_a2) # in the young group of people the effect is stronger,which might be due to an increase in education in the last decades
summary(ols_a3)

stargazer(ols_a1,ols_a2,ols_a3, type = "html", 
          title = "OLS Regressions, 1990-2010",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_1990-2010.html")

ols_b1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_b1) 
ols_b2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_b2) 
ols_b3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = flfp_b3) 
summary(ols_b1)
summary(ols_b2)
summary(ols_b3)

stargazer(ols_b1,ols_b2,ols_b3, type = "html", 
          title = "OLS Regressions, 1990-2019",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_1990-2019.htm")
# we see that, as the theory predicts, the U-shape hypothesis is supported in a pooled cross sectional model
# the estimators in the larger data set are larger, hinting at the fact that more recently collected data supports the hypothesis more

# visualize the idiosyncratic U-shape
flfp_b3  %>%
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

#####################
# Step 2.2 OLS Analysis, OECD Subset
#####################

# pooled analysis OECD countries
ols_c1 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = oecd_a1)
ols_c2 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = oecd_a2)
ols_c3 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = oecd_b1)
ols_c4 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = oecd_b2)

summary(ols_c1)
summary(ols_c2)
summary(ols_c3)
summary(ols_c4)

stargazer(ols_c1,ols_c2,ols_c3,ols_c4, type = "html", 
          title = "OLS Regressions, OECD Members",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_oecd.html")

# pooled analysis non-OECD countries
ols_c5 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = n_oecd_a1)
ols_c6 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = n_oecd_a2)
ols_c7 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = n_oecd_b1)
ols_c8 <-lm(flfpr ~ lg_gdppc + lg_gdppc_sq, data = n_oecd_b2)

summary(ols_c5)
summary(ols_c6)
summary(ols_c7)
summary(ols_c8)

stargazer(ols_c5,ols_c6,ols_c7,ols_c8, type = "html", 
          title = "OLS Regressions, Non-OECD Members",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/ols_noecd.html")

######################
# Step 3.1 Fixed Effect Analysis
######################

# fixed effect model (https://libguides.princeton.edu/R-Panel)
# (http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html)
# with time fixed effect(https://www.econometrics-with-r.org/10.4-regression-with-time-fixed-effects.html)
fixed_a1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_a1, method = "within", index = c("countrycode"))
fixed_a2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_a2, method = "within", index = c("countrycode"))
fixed_a3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_a3, method = "within", index = c("countrycode"))

summary(fixed_a1)
summary(fixed_a2)
summary(fixed_a3)

stargazer(fixed_a1,fixed_a2,fixed_a3, type = "html", 
          title = "Twoway fixed effect Regressions, 1990-2010",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/fixed_1990-2010.htm")

fixed_b1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_b1, method = "within", index = c("countrycode"))
fixed_b2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_b2, method = "within", index = c("countrycode"))
fixed_b3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_b3, method = "within", index = c("countrycode"))

summary(fixed_b1)
summary(fixed_b2)
summary(fixed_b3)

stargazer(fixed_b1,fixed_b2,fixed_b3, type = "html", 
          title = "Twoway fixed effect Regressions, 1990-2019",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/fixed_1990-2019.htm")

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
               data = flfp_b3)

summary(fixed_b4)
coef_b4 <- summary(fixed_b4)$coef
sig_coef <- coef_b4[coef_b4[,4] <= 0.05, 4]
sig_coef <- data.frame(sig_coef)
sig_coef %>%
  mutate(coef = row_number())

sig_coef$coefficients<- as.character(row.names(sig_coef))
sig_coef <- sig_coef %>%
  # filter for country code variables/coefficients
  filter(str_detect(coefficients,"countrycode")) %>%
  # create a column with only the countrycodes
  mutate(countrycode = substr(coefficients,12,14)) %>%
  select(sig_coef,countrycode)

write.csv(sig_coef, file.path("analysis_data","significant_country_coefficients.csv"),
          row.names = FALSE)

######################
# Step 3.2 Fixed Effect Analysis subsetted in OECD and non-OECD countries
######################

fixed_c1 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_oecd1, method = "within", index = c("countrycode"))
fixed_c2 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_oecd2, method = "within", index = c("countrycode"))
fixed_c3 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_oecd3, method = "within", index = c("countrycode"))
fixed_c4 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_oecd4, method = "within", index = c("countrycode"))

summary(fixed_c1)
summary(fixed_c2)
summary(fixed_c3)
summary(fixed_c4)

stargazer(fixed_c1,fixed_c2,fixed_c3,fixed_c4, type = "html", 
          title = "Twoway fixed effect Regressions, OECD Members",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/fixed_oecd.htm")

fixed_c5 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_noecd1, method = "within", index = c("countrycode"))
fixed_c6 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_noecd2, method = "within", index = c("countrycode"))
fixed_c7 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_noecd3, method = "within", index = c("countrycode"))
fixed_c8 <- plm(flfpr ~ lg_gdppc + lg_gdppc_sq , effect = "twoways", data = p.data_noecd4, method = "within", index = c("countrycode"))

summary(fixed_c5)
summary(fixed_c6)
summary(fixed_c7)
summary(fixed_c8)

stargazer(fixed_c5,fixed_c6,fixed_c7,fixed_c8, type = "html", 
          title = "Twoway fixed effect Regressions, Non-OECD Members",
          dep.var.caption = "Female Labor Force Participation Rate",
          covariate.labels = c("Log GDP per Capita","Log GDP per Capita squared"),
          notes.label = "Significance Level",
          out = "regression_tables/fixed_noecd.htm")


######################
# Step 4.1 GMM models
######################

# the lag function in the GMM models should work if we group the data sets according to the country code
p.data_a1 <- p.data_a1 %>% group_by(countrycode)
p.data_a2 <- p.data_a2 %>% group_by(countrycode)
p.data_a3 <- p.data_a3 %>% group_by(countrycode)

p.data_b1 <- p.data_b1 %>% group_by(countrycode)
p.data_b2 <- p.data_b2 %>% group_by(countrycode)
p.data_b3 <- p.data_b3 %>% group_by(countrycode)

# Gaddis (2014) accepts the model if there is first order autocorrelation (p<0.05) 
# but not second order autocorrelation (p>0.1) and
# we should not be able to reject the null hypothesis (joint validity of instruments) of the sargan hansen test(p > 0.1).

gmm_a1 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
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
               |lag(lg_gdppc, 3) + lag(lg_gdppc_sq, 3) + lag(flfpr, 4), # instruments
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

gmm_b3 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3:4) + lag(lg_gdppc_sq, 3:4) + lag(flfpr, 4), # instruments
               data = p.data_b3,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_b3)  # lag structure: gdp variables - 3:4 order lags; flfpr - 4 order lags


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

######################
# Step 4.2 GMM models OECD Subset
######################

# the lag function in the GMM models should work if we group the data sets according to the country code
p.data_oecd1 <- p.data_oecd1 %>% group_by(countrycode)
p.data_oecd2 <- p.data_oecd2 %>% group_by(countrycode)
p.data_oecd3 <- p.data_oecd3 %>% group_by(countrycode)
p.data_oecd4 <- p.data_oecd4 %>% group_by(countrycode)

p.data_noecd1 <- p.data_noecd1 %>% group_by(countrycode)
p.data_noecd2 <- p.data_noecd2 %>% group_by(countrycode)
p.data_noecd3 <- p.data_noecd3 %>% group_by(countrycode)
p.data_noecd4 <- p.data_noecd4 %>% group_by(countrycode)


# OECD Data sets 

gmm_c1 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3:4) + lag(lg_gdppc_sq, 3:4) + lag(flfpr, 3:4), # instruments
               data = p.data_oecd1,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c1)
# lag structure: no valid specification

gmm_c2 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3:4) + lag(lg_gdppc_sq, 3:4) + lag(flfpr, 3:5), # instruments
               data = p.data_oecd2,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c2)

# lag structure: no valid specification

gmm_c3 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3) + lag(lg_gdppc_sq, 3) + lag(flfpr, 3), # instruments
               data = p.data_oecd3,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c3)
# lag structure: gdp variables - 3 order lags; flfpr - 3 order lags 

gmm_c4 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3) + lag(lg_gdppc_sq, 3) + lag(flfpr, 3), # instruments
               data = p.data_oecd4,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c4)
# lag structure: gdp variables - 3 order lags; flfpr - 3 order lags


######################
# Step 4.3 GMM models Non-OECD Subset
######################

gmm_c5 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3) + lag(lg_gdppc_sq, 3) + lag(flfpr, 4), # instruments
               data = p.data_noecd1,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c5)
# lag structure: no valid specification 

gmm_c6 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3:4) + lag(lg_gdppc_sq, 3:4) + lag(flfpr, 4), # instruments
               data = p.data_noecd2,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c6)
# lag structure: no valid specification 

gmm_c7 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 3) + lag(lg_gdppc_sq, 3) + lag(flfpr, 4), # instruments
               data = p.data_noecd3,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c7)
# lag structure: gdp variables - 3 order lags; flfpr - 4 order lags

gmm_c8 <- pgmm(flfpr ~ lg_gdppc + lg_gdppc_sq + lag(flfpr,1) # normal covariates
               |lag(lg_gdppc, 2) + lag(lg_gdppc_sq, 2) + lag(flfpr, 2), # instruments
               data = p.data_noecd4,
               effect = "twoways",
               model = "twostep",
               collapse = FALSE,
               transformation = "d",
               fsm = "I")
summary(gmm_c8)
# lag structure: no valid specification























