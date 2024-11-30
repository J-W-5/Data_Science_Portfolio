#Regression of poverty percentage and cancer death rate by county
dat <- read.csv("cancer-regression.csv")

library(dplyr)
library(ggplot2)
library(car)

#Filter out impossible median age outliers
clean <- dat %>%
  filter(medianage < 100)

#Create regression model
model <- lm(target_deathrate ~ povertypercent, clean)
summary(model)


#Check if assumptions were met

#normality of the model
plot(x = model, which = 2)
hist(x = residuals(model))

#linearity
plot(x = model, which = 1)

#homogeneity of variance
plot(x = model, which = 3)


#Visualize regression model
ggplot(clean, aes(x = povertypercent, y = target_deathrate)) +
  geom_point(alpha = .5) + 
  geom_smooth(method = lm) +
  labs(title = "Cancer Mortality and Poverty",
       subtitle = "The Association Between Cancer Mortality Rate and 
       Poverty Percantage by County",
       x = "Poverty Percentage",
       y = "Mean Per Capita (100k) Cancer Mortalities") +
  ggsave("Cancer Mortality and Poverty.png", type = "cairo", scale = 2)
