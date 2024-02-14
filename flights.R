dat <- read.csv("flights_1.csv")

dat$arrival <- as.factor(ifelse(dat$arr_delay <= 0, "ontime", "delayed"))
dat$Destination <- dat$dest

dat_clean <- na.omit(dat)

NYC <- dat_clean %>%
  filter(origin == c("EWR", "JFK", "LGA"), dest == c("PHL", "RDU"))


ggplot(NYC, aes(x  = arrival, y = dep_delay, color = Destination)) +
  geom_boxplot() +
  facet_grid(dest~origin)+
  labs(title = "On time performance of NYC flights",
       subtitle = "December 2013",
       x = "Arrival", y = "Departure Delay") + 
  scale_y_continuous(breaks = seq(0,200, by=100), limits = c(0, 300)) + 
  ggsave("Flights.png", type = "cairo")
















