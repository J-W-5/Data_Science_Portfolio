tuesdata <- tidytuesdayR::tt_load('2020-02-04') 
tuesdata <- tidytuesdayR::tt_load(2020, week = 6)

attendance <- tuesdata$attendance
games <- tuesdata$games
standings <- tuesdata$standings

options(scipen = 999)

#Filter for NFC West
nfcw_attend <- attendance %>%
  filter(team_name == "Seahawks" | team_name == "49ers" |
           team_name == "Cardinals" | team_name == "Rams")

#Filter superbowl winners
sb_win <- standings %>%
  filter(sb_winner == "Won Superbowl")

#Create visualization of attendance for NFC West teams
ggplot(nfcw_attend, aes(x=year, y=home, group = team_name, color = team_name)) +
  geom_line(size = 1.1) +
  scale_color_manual(values = c("#B3995D", "#C41E3A", "#003594", "#002244")) + 
  labs(title = "NFC West Home Game Attendance") +
  xlab("Year") + 
  ylab("Attendance") + 
  scale_y_continuous(labels = scales::comma) +
  guides(color = guide_legend(title = "Team")) + 
  theme(
    panel.background = element_rect(fill = "#FAF9F6"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAF9F6"),
    legend.background = element_rect(fill = "#FAF9F6")) +
  theme(axis.line.x = element_line(color="black", size = .5),
        axis.line.y = element_line(color="black", size = .5)) +
  ggsave("NFC_W_2.png", type = "cairo")
