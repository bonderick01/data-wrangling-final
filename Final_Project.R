#Ben Onderick
#11-13-23
#Final Project 

rm(list=ls())

#Reading in excel file
library(readxl)
team_stats <- read_excel("nfl_team_data.xlsx")

#Scraping playoff data for the years 
library(rvest)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/List_of_National_Football_League_seasons"
page <- read_html(url)

playoff_data <- page %>% html_nodes("table") %>% .[4] %>% html_table() %>% .[[1]]

#Cleaning Season Summary Table

#Rename the columns
colnames(playoff_data) <- c("Year","Teams","Games","AFC_Top_Seed","NFC_Top_Seed","Postseason","AFC_Champion","NFC_Champion","SB","Super_Bowl_Champion","Ref")

#Drop 1st row
playoff_data <- playoff_data[-1, ]

#Trim data down to desired seasons (2009-2019)
playoff_data <- playoff_data[40:50, ]

#Remove unwanted columns
playoff_data <- subset(playoff_data, select = -Teams)
playoff_data <- subset(playoff_data, select = -Games)
playoff_data <- subset(playoff_data, select = -Postseason)
playoff_data <- subset(playoff_data, select = -SB)
playoff_data <- subset(playoff_data, select = -Ref)

#Remove index characters in team names
playoff_data$AFC_Top_Seed <- sub("\\[[a-zA-Z]+\\]", "", playoff_data$AFC_Top_Seed)
playoff_data$NFC_Top_Seed <- sub("\\[[a-zA-Z]+\\]", "", playoff_data$NFC_Top_Seed)

#Merge the two datasets into one final dataframe
final_data <- merge(team_stats,playoff_data)

#Clean all column names and make them the same format
library(janitor)
final_data <- clean_names(final_data,"snake")

#Combine location and team_name
final_data$team <- paste(final_data$location,final_data$team_name,sep = " ")

#Reorder the columns 
final_data <- final_data[, c(1,2,25,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]

#Make the data more useable as a team based application, if seeding matchs name, 1, if not, 0
final_data$afc_top_seed <- ifelse(final_data$afc_top_seed == final_data$team,1,0)
final_data$nfc_top_seed <- ifelse(final_data$nfc_top_seed == final_data$team,1,0)
final_data$afc_champion <- ifelse(final_data$afc_champion == final_data$team,1,0)
final_data$nfc_champion <- ifelse(final_data$nfc_champion == final_data$team,1,0)
final_data$super_bowl_champion <- ifelse(final_data$super_bowl_champion == final_data$team,1,0)

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
library(ggplot2)
library(ggplot2)

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
library(ggplot2)

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
install.packages("corrplot")
statistics <- data.frame(wins = final_data$wins, losses = final_data$losses, points = final_data$points_for, yards = final_data$yards, plays = final_data$plays, yards_per_play = final_data$yards_per_play,
                           turnovers = final_data$turnovers, passing_yards = final_data$passing_yards, passing_td = final_data$passing_td, rushing_yards = final_data$rushing_yards, rushing_td = final_data$rushing_td, 
                           penalties = final_data$penalties, penalty_yards = final_data$penalty_yards)

stat_correlation <- cor(statistics)
print(stat_correlation)

library(corrplot)
corrplot(stat_correlation, method = "color")

#Q3 Top 10 stat leaeders by team 
#Stat total dataframe for charts
stats_totals <- final_data %>% group_by(team_name) %>% summarize(passing_yards = sum(passing_yards),
                                                                 passing_td = sum(passing_td),
                                                                 rushing_yards = sum(rushing_yards),
                                                                 rushing_td = sum(rushing_td),
                                                                 points_for = sum(points_for))
#Passing Yards
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

#Passing TD's
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

#Rushing Yards
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
    
#Rushing Touchdowns
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
