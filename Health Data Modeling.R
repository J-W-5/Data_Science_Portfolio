#Cancer data gathered from American Community Survey (census.gov),
#clinicaltrials.gov, and cancer.gov.
#Data taken from data.world 
#https://data.world/exercises/linear-regression-exercise-1

dat <- read.csv("cancer-regression.csv")
options(scipen = 999)


#Scale demographics from percentage to per capita (100k)
poverty_per_cap <- (dat$povertypercent * .01) * 100000
white_per_cap <- (dat$pctwhite * .01) * 100000
black_per_cap <- (dat$pctblack * .01) * 100000
asian_per_cap <- (dat$pctasian * .01) * 100000
other_per_cap <- (dat$pctotherrace * .01) * 100000

private_coverage_per_cap <- (dat$pctprivatecoverage * .01) * 100000
priv_cov_alone_per_cap <- (dat$pctprivatecoveragealone * .01) * 100000
emp_coverage_per_cap <- (dat$pctempprivcoverage * .01) * 100000
public_coverage_per_cap <- (dat$pctpubliccoverage * .01) * 100000
pub_cov_alone_per_cap <- (dat$pctpubliccoveragealone * .01) * 100000

incidence_per_cap <- dat$incidencerate
death_rate_per_cap <- dat$target_deathrate
studies_per_cap <- dat$studypercap
location <- dat$geography

#create data frame of variables for regression analysis
dat_clean <- data.frame(poverty_per_cap, white_per_cap, black_per_cap,
                        asian_per_cap, other_per_cap, private_coverage_per_cap,
                        priv_cov_alone_per_cap, emp_coverage_per_cap,
                        public_coverage_per_cap, pub_cov_alone_per_cap,
                        incidence_per_cap, death_rate_per_cap, studies_per_cap, 
                        model_yhat, model2_yhat, location)
#omit NAs
dat_clean <- na.omit(dat_clean)

#Regression looking at cancer incidence rate
model <- lm(incidence_per_cap ~ poverty_per_cap + white_per_cap + black_per_cap +
            asian_per_cap + other_per_cap + studies_per_cap)

#Regression looking at cancer mortality rate
model2 <- lm(death_rate_per_cap ~ poverty_per_cap + white_per_cap 
             + black_per_cap + asian_per_cap + other_per_cap + studies_per_cap 
             + incidence_per_cap)

summary(model)
summary(model2)

#Model diagnostics

#Look at outliers 
model_hats <- hatvalues(model)
model2_hats <- hatvalues(model2)

summary(model2_hats)

#Cook's distance
summary(cooks.distance(model = model))
plot(model, which = 4)
plot(model2, which = 4)

#Residuals vs leverage
plot(model, which = 5)
plot(model2, which = 5)

#Check normality of residuals
hist( x = residuals(model2))
plot(model2, which = 2)
plot(model, which = 2)

shapiro.test(rstandard(model2))

#Check linearity of relationship
model_yhat <- fitted.values(object = model)
model2_yhat <- fitted.values(object = model2)

plot(x = model2_yhat,
     y = dat_clean$death_rate_per_cap,
     xlab = "Fitted Values",
     ylab = "Observed Values")

plot(x = model_yhat,
     y = dat_clean$incidence_per_cap,
     xlab = "Fitted Values",
     ylab = "Observed Values")

plot(model, which = 1)
plot(model2, which = 1)

residualPlots(model = model)
residualPlots(model = model2)

#Check homogeneity of variance
plot(model, which = 3)
plot(model2, which = 3)

ncvTest(model)
ncvTest(model2)

#Check collinearity
vif(model)
vif(model2)
cor(dat_clean)

