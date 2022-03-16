#Baseball Script

#Packages
library(rvest)
library(tidyverse)
library(naniar)
library(ggthemes)
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)

#Scrape data
season_data <- read_html("https://ecupirates.com/sports/baseball/stats/2022")

tables <- html_nodes(season_data, css = "table")
tables <- html_table(tables, fill = TRUE)

hitting <- tables[[1]]
colnames(hitting) <- make.names(colnames(hitting))

pitching <- tables[[2]]
colnames(hitting) <- make.names(colnames(hitting))

fielding <- tables[[3]]
colnames(hitting) <- make.names(colnames(hitting))

hitting$X. <- as.numeric(hitting$X.)

#Add variables

#Calculation of individual, total, and opponents 'Batting Average on Balls in Play'.
#This is calculated by using the formula: (H - HR)/(AB - K - HR + SF).
hitting$BABIP <- length(hitting$Player)
hitting$BABIP <- (hitting$H - hitting$HR) / (hitting$AB - hitting$HR - hitting$SO + hitting$SF)


#Calculation of individual, total, and opponents 'Isolated Power'. This is calculated by using the
#formula: (1x2B + 2x3B + 3xHR) / AB
hitting$ISO <- length(hitting$Player)
hitting$ISO <- (((hitting$X2B) + (hitting$X3B * 2) + (hitting$HR * 3)) / (hitting$AB))


#Calculation of individual, total, and opponents 'Runs Created'. This is calculated by using the
#formula: TB x (H + BB) / (AB + BB).
hitting$RC <- length(hitting$Player)
hitting$RC <- (((hitting$TB)*((hitting$H)+(hitting$BB))/((hitting$AB)+(hitting$BB))))


#Calculation of individual, total, and opponents 'Offensive Strikeout Rate'. 
#This is calculated by using the formula: (SO)/(AB + BB + HBP + SF + SH). 
hitting$SOR <- length(hitting$Player)
hitting$SOR <- (((hitting$SO)/((hitting$AB)+(hitting$BB)+(hitting$HBP)+(hitting$SF)+
                   (hitting$SH))))

#Calculation of Total Extra Basehits (XBH). This is calculated by using the
#formula: X2B + X3B + HR.
hitting$XBH <- length(hitting$Player)
hitting$XBH <- (((hitting$X2B)+(hitting$X3B)+(hitting$HR)))


#Calculation of individual, total, and opponents 'Offensive Walk Rate'. This is 
#calculated by using the formula: (BB)/(AB + BB + HBP + SF + SH).
hitting$BBR <- length(hitting$Player)
hitting$BBR <- (((hitting$BB)/((hitting$AB)+(hitting$BB)+(hitting$HBP)+(hitting$SF)+
                   (hitting$SH))))

#Initializing variables in new data frame and cleaning raw data.
activeHitting <- hitting %>%
  filter((hitting$AB) >= 10, (hitting$H) <= 20)
colnames(activeHitting)[1] <- "Number"
activeHitting <- activeHitting %>%
  separate(Player,c('Name'),',')
activeHitting
#Visualization of BABIP vs AVG vs H for each player with more than 10 ABs on the season.
viz1 <- ggplot(data = activeHitting, mapping = aes(x=BABIP, y=AVG, colour=H)) +
  geom_point() +
  ylim(0.000, 0.500) +
  xlim(0.000, 0.500) +
  labs(title="2022 ECU Baseball AVG vs BABIP", x="Batting Average against Balls In Play (BABIP)",
       y="Batting Average (AVG)", color="Hits") +
  geom_text_repel(aes(label = Number)) +
  scale_color_gradient2(mid = "black", high = "purple", )
viz1

#Visualization of SLG vs ISO vs XBH for each player with more than 10 ABs on the season.
viz2 <- ggplot(data = activeHitting, mapping = aes(x=ISO, y=SLG., colour=XBH)) +
  geom_point() +
  ylim(0.000, 1.000) +
  xlim(0.000, 0.400) +
  labs(title="2022 ECU Baseball SLG vs ISO", x="Isolated Power (ISO)",
       y="Slugging Percentage (SLG)", color="XBH") +
  geom_text_repel(aes(label = Number)) +
  scale_color_gradient2(mid = "black", high = "purple", )
viz2

#Visualization of RC+ vs RBI vs R for each player with more than 10 ABs on the season.
viz3 <- ggplot(data = activeHitting, mapping = aes(x=RC, y=RBI, colour=R)) +
  geom_point() +
  ylim(0, 20) +
  xlim(0, 20) +
  labs(title="2022 ECU Baseball RC vs RBI", x="Runs Created (RC)",
       y="Runs Batted In (RBI)", color="R") +
  geom_text_repel(aes(label = Number)) +
  scale_color_gradient2(mid = "black", high = "purple", )
viz3

#Visualization of SO% vs K vs AB for each player with more than 10 ABs on the season.
viz4 <- ggplot(data = activeHitting, mapping = aes(x=SOR, y=SO, colour=AB)) +
  geom_point() +
  ylim(0, 50) +
  xlim(0, 0.400) +
  labs(title="2022 ECU Baseball SO% vs K", x="Strikeout Rate (SO%)",
       y="Strikeouts (K)", color="AB") +
  geom_text_repel(aes(label = Number)) +
  scale_color_gradient2(mid = "black", high = "purple", )
viz4

#Visualization of BB% vs BB vs AB for each player with more than 10 ABs on the season.
viz5 <- ggplot(data = activeHitting, mapping = aes(x=BBR, y=BB, colour=AB)) +
  geom_point() +
  ylim(0, 30) +
  xlim(0, 0.400) +
  labs(title="2022 ECU Baseball BB% vs BB", x="Walk Rate (BB%)",
       y="Walks (BB)", color="AB") +
  geom_text_repel(aes(label = Number)) +
  scale_color_gradient2(mid = "black", high = "purple", )
viz5