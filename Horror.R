#Read in data
tuesdata <- tidytuesdayR::tt_load('2022-11-01')
tuesdata <- tidytuesdayR::tt_load(2022, week = 44)
horror_movies <- tuesdata$horror_movies

library(ggplot2)
library(dplyr)
library(tidytuesdayR)
library(tidyverse)
library(tm)
library(wordcloud2)
library(scales)
library(showtext)
showtext_auto()

#Regression analysis looking at association between budget and revenue.
#data cleaning - cases with a 0 for either variable are removed
money_clean <- horror_movies %>%
  select(revenue, budget) %>%
  filter(revenue > 0, budget > 0)

options(scipen=999)

#Regression using budget to predict revenue
model <- lm(revenue~budget, money_clean)
summary(model)

#Data visualization
font_add_google(name = "Skranji", family = "Neapolitan")

ggplot(money_clean, aes(x = budget, y = revenue, color = "red")) +
  geom_point() +
  geom_smooth(method = lm, color = "red") +
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(0, 200000000, 25000000)) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6),
                     breaks = seq(0, 600000000, 100000000)) +
  labs(title = "Film Profits", 
       subtitle = "The Association Between Horror Film Budget and Revenue", 
       caption = "Data Source: The Movie Database
                  Tidy Tuesday 2022-11-01",
       x = "Budget",
       y = "Revenue")+
  theme(text=element_text(size = 35, family="Neapolitan", color = "red"),
        panel.background = element_rect( fill = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "red"),
        plot.caption = element_text(color = "grey", size = 16),
        plot.background = element_rect((fill = "black")))

#Text cleaning for wordcloud of horror movie taglines
tagline_clean <- horror_movies$tagline[!is.na(horror_movies$tagline)]

tagline_corpus <- iconv(tagline_clean, to = "utf-8")
tagline_corpus <- Corpus(VectorSource(tagline_corpus))
inspect(tagline_corpus[1:10])

tagline_corpus <- tm_map(tagline_corpus, tolower)
tagline_corpus <- tm_map(tagline_corpus, removePunctuation)
tagline_corpus <- tm_map(tagline_corpus, removeNumbers)

cleanset <- tm_map(tagline_corpus, removeWords, stopwords("english"))
inspect(cleanset[1:100])
cleanset <- tm_map(cleanset, removeWords, c("dont", "get", "just", 
                                            "one", "will", "cant", "youre",
                                            "hes", "theyre"))
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:100])

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)

words <- sort(rowSums(tdm), decreasing = T)
df <- data.frame(word = names(words),freq=words)

wordcloud2(df, size = 1, shape = "circle",
           color = "black", backgroundColor = "red", fontFamily = "chiller")
