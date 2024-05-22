tuesdata <- tidytuesdayR::tt_load('2020-02-04') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 6)

attendance <- tuesdata$attendance
games <- tuesdata$games
standings <- tuesdata$standings

#Logistic Regression Model Predicting Superbowl Wins
standings$sb_winner <- as.factor(standings$sb_winner)

model <- glm(sb_winner ~ wins + loss + points_differential + 
               margin_of_victory + strength_of_schedule + simple_rating +
               offensive_ranking + defensive_ranking, 
             standings, family = "binomial")

summary(model)






