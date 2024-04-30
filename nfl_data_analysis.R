#Ben Onderick
#12-7-23
#Final Project Analysis

rm(list=ls())

#Packages Needed
library(ggplot2)
library(corrplot)

#Read in the data from the new csv file
final_data <- read.csv("Project_Final_Data.csv")

#Clear the first column
final_data$X <- NULL

#Q1 Postseason Analysis
#Create dataframe with counts for each postseason category for each team 
postseason <- final_data %>% group_by(team_name,conference) %>% 
  summarise(
    afc_top_seed_total = sum(afc_top_seed == 1),
    nfc_top_seed_total = sum(nfc_top_seed == 1),
    afc_champion_total = sum(afc_champion == 1),
    nfc_champion_total = sum(nfc_champion == 1),
    super_bowl_champion_total = sum(super_bowl_champion == 1))

#Create a bar chart to show this 
ggplot(postseason %>%
         filter(super_bowl_champion_total > 0),
       aes(x = reorder(team_name, super_bowl_champion_total), 
           y = super_bowl_champion_total, 
           fill = conference)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = super_bowl_champion_total), 
            position = position_stack(vjust = 0.9), 
            color = "black", 
            size = 3) +
  labs(title = "Super Bowl Total by Team",
       x = "Team Name",
       y = "Super Bowl Total",
       fill = "Conference") +
  theme_minimal() +
  coord_flip() +
  scale_fill_manual(values = c("NFC" = "lightblue", "AFC" = "red"))

#Create the same chart except show the champion for the conferences. 

#AFC
ggplot(postseason %>%
         filter(afc_champion_total > 0),
       aes(x = reorder(team_name, -afc_champion_total), 
           y = afc_champion_total)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = afc_champion_total), 
            position = position_stack(vjust = 0.9), 
            color = "black", 
            size = 3) +
  labs(title = "AFC Champion Total by Team",
       x = "Team Name",
       y = "AFC Champion Total") +
  theme_minimal() 

#NFC
ggplot(postseason %>%
         filter(nfc_champion_total > 0),
       aes(x = reorder(team_name, -nfc_champion_total), 
           y = nfc_champion_total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = nfc_champion_total), 
            position = position_stack(vjust = 0.9), 
            color = "black", 
            size = 3) +
  labs(title = "NFC Champion Total by Team",
       x = "Team Name",
       y = "NFC Champion Total") +
  theme_minimal() 

#Q2 correlation between statistcs

#Get all of the statistics in their own dataframe
statistics <- data.frame(wins = final_data$wins, losses = final_data$losses, points = final_data$points_for, yards = final_data$yards, plays = final_data$plays, yards_per_play = final_data$yards_per_play,
                         turnovers = final_data$turnovers, passing_yards = final_data$passing_yards, passing_td = final_data$passing_td, rushing_yards = final_data$rushing_yards, rushing_td = final_data$rushing_td, 
                         penalties = final_data$penalties, penalty_yards = final_data$penalty_yards)

#Get a correlation matrix for all the variables
stat_correlation <- cor(statistics)
print(stat_correlation)
      
#Create a heatmap to better visualize the correlations
corrplot(stat_correlation, method = "color")

#Q3 Top 10 stat leaeders by team

#Stat total dataframe for passing yards, rushing yards and points
stats_totals <- final_data %>% group_by(team_name) %>% summarize(passing_yards = sum(passing_yards),
                                                                 passing_td = sum(passing_td),
                                                                 rushing_yards = sum(rushing_yards),
                                                                 rushing_td = sum(rushing_td),
                                                                 points_for = sum(points_for))
#Passing Yards Bar Chart
ggplot(stats_totals %>% top_n(10, passing_yards),
       aes(x = reorder(team_name, -passing_yards), y = passing_yards)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = passing_yards), 
            position = position_stack(vjust = 0.4), 
            color = "black", 
            size = 3) +
  labs(title = "Top 10 in Passing Yards",
       x = "Team Name",
       y = "Passing Yards") +
  theme_minimal()

#Passing TD's Bar Chart
ggplot(stats_totals %>% top_n(10, passing_td),
       aes(x = reorder(team_name, -passing_td), y = passing_td)) +
  geom_bar(stat = "identity", fill = "red") +
  geom_text(aes(label = passing_td), 
            position = position_stack(vjust = 0.4), 
            color = "black", 
            size = 3) +
  labs(title = "Top 10 in Passing Touchdowns",
       x = "Team Name",
       y = "Passing Touchdowns") +
  theme_minimal()

#Rushing Yards Bar Chart
ggplot(stats_totals %>% top_n(10, rushing_yards),
       aes(x = reorder(team_name, -rushing_yards), y = rushing_yards)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = rushing_yards), 
            position = position_stack(vjust = 0.9), 
            color = "black", 
            size = 3) +
  labs(title = "Top 10 in Rushing Yards",
       x = "Team Name",
       y = "Rushing Yards") +
  theme_minimal()

#Rushing Touchdowns Bar Chart
ggplot(stats_totals %>% top_n(10, rushing_td),
       aes(x = reorder(team_name, -rushing_td), y = rushing_td)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = rushing_td), 
            position = position_stack(vjust = 0.4), 
            color = "black", 
            size = 3) +
  labs(title = "Top 10 in Rushing Touchdowns",
       x = "Team Name",
       y = "Rushing Touchdowns") +
  theme_minimal()

