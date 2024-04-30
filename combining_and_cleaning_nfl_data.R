#Ben Onderick
#11-13-23
#Final Project Merging and Cleaning

rm(list=ls())

#All packages needed
library(readxl)
library(rvest)
library(dplyr)
library(janitor)

#Reading in excel file
team_stats <- read_excel("nfl_team_data.xlsx")

#Scraping playoff data for the years 
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

#Write final dataframe to csv file
write.csv(final_data, "Project_Final_Data.csv")
