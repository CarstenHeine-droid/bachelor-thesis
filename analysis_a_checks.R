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
flfp_a1 <- read_csv(file.path("analysis_data","flfp_a1.csv"), col_names = TRUE)
flfp_a2 <- read_csv(file.path("analysis_data","flfp_a2.csv"), col_names = TRUE)
flfp_a3 <- read_csv(file.path("analysis_data","flfp_a3.csv"), col_names = TRUE)

flfp_b1 <- read_csv(file.path("analysis_data","flfp_b1.csv"), col_names = TRUE)
flfp_b2 <- read_csv(file.path("analysis_data","flfp_b2.csv"), col_names = TRUE)
flfp_b3 <- read_csv(file.path("analysis_data","flfp_b3.csv"), col_names = TRUE)
flfp_b3_ay <- read_csv(file.path("analysis_data","flfp_b3_ay.csv"), col_names = TRUE) # contains data for all years

wvs <- read_csv(file.path("analysis_data","wvs.csv"), col_names = TRUE)

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
######################
# Step 2.2 Fixed Effect Analysis
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
  plm::pdata.frame(flfp_b3, index = "countrycode")$flfpr)

# Fit the OLS model on the demeaned dataset
m3.ols <- lm(flfpr ~ lg_gdppc + lg_gdppc_sq , data = design.matrix)

# Calculate VIF scores
car::vif(m3.ols)

# multicollinearity seems to be present - how to deal with it?

# Homoscedasticity 
# calculate the fitted values as the difference between the observed values and the model residuals for plm

fitted.values <- as.numeric(flfp_b3$flfpr - residuals(fixed_b3))
plot(fitted.values, residuals(fixed_b3),
     bty = 'n', xlab = 'Fitted Values', ylab = 'Residuals',
     main = 'Residuals vs. Fitted')
abline(h = 0, col = 'red', lty = 'dashed')

# some heteroscedasticity seems present - specifically in realtively large positive or negative residual values

# check for influential observations - which leverage do individual observations have?(http://karthur.org/2019/implementing-fixed-effects-panel-models-in-r.html)

# calculate the projections matrix
#X <- model.matrix(fixed_b1)
#P = X %*% solve(t(X) %*% X) %*% t(X)

# Create `labs` (labels) for 1 through 1169 observations
# halfnorm(diag(P), labs = 1:1169, ylab = 'Leverages', nlab = 1)
# flfp_b1[1134,] # value of venezuela in 2019 seems to have a great influence
# flfp_b1[flfp_b1$countrycode == "VEN",] # Probably because the GDPPC falls rapidly from 2015 to 2019

######################
# Step 5.3 GMM models
######################

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










































