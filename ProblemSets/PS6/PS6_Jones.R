library(tidyverse)
library(ggplot2)

# Data Collected From: https://nflcombineresults.com/nflcombinedata.php?year=all&pos=QB&college=
Combine <- read.csv("Combine.csv", header = TRUE)
Combine <- Combine %>%
  rename(Player = Name) %>% rename(Hand = Hand.Size..in.)
head(Combine)
# First, going to mutate name of combine data from Firstname Lastname to F. Lastname to match other dataset to join them

# Load in Career NFL QBR
# Data Collected from https://www.pro-football-reference.com/leaders/pass_rating_career.htm
NFLQBR <- read.csv("NFLQBR.csv")
head(NFLQBR)

# Lets merge the two dataframes
combined <- left_join(NFLQBR, Combine, by = "Player")
summary(combined$Hand)
Combined <- combined[!is.na(combined$Hand), ]
head(Combined)

# now with combined datasets, going to create GGPlots
# Hand Size Scatter Plot
PS6a <- ggplot(data = Combined, aes(x = Hand, y = Rate)) +geom_jitter() + theme_minimal() +
  geom_label(data = subset(Combined, Player %in% 
                             c("Jalen Hurts", "Patrick Mahomes", 
                               "Matthew Stafford", "Joe Burrow", "Tom Brady", 
                               "Jimmy Garoppolo", "Jared Goff")), aes(label = Player)) +
  labs(title = "Comparing NFL Quarterback Hand Size with Career QBR", 
       subtitle = "Highlighted Quarterbacks Competed in Last Five Super Bowls") +
  xlab("Hand Size") + ylab("Career QBR")
ggsave("PS6a.png")

# Hand Size distribution
PS6b <- ggplot(data = Combined, aes(x = Hand)) + geom_histogram(binwidth = 0.15) + theme_minimal() + 
  ggtitle("Distribution of NFL Quarterback Hand Size") + xlab("Hand Size")
ggsave("PS6b.png")

# Wonderlic score and QBR Scatterplot
PS6c <- ggplot(data = Combined, aes(x = Wonderlic, y = Rate)) + geom_jitter() + theme_minimal() +
  geom_label(data = subset(Combined, Player %in% 
                             c("Aaron Rodgers", "Drew Brees", 
                               "Patrick Mahomes", "Tom Brady", "Eli Manning", 
                               "Matthew Stafford")), aes(label = Player)) +
  ggtitle("Comparing Quarterback Wonderlic Score with Career QBR")
ggsave("PS6c.png")
