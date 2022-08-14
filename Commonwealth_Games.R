# Exploratory Data Analysis
# Source of the dataset: https://www.kaggle.com/datasets/ramjasmaurya/commonwealth-games-2022 

# Load necessary packages
library("tidyverse")

# Load the data in R
players <- read.csv("commonwealth games 2022 - players participated.csv")
winners <- read.csv("commonwealth games 2022 - players won medals in cwg games 2022.csv")

# Check the structure of players
str(players)
# Check the structure of winners
str(winners)
# Get summary of players
summary(players)
# Get summary of winners
summary(winners)

# Data Cleaning
# Column names are all caps
# Change 'winners$medal' to ordinal variable
# In 'players$age', a value is missing
# In 'players$age', minimum value is zero
# In some cases athlete.name have spaces between the names, and in come cases not

# column names to lower case
names(players) <- tolower(names(players)) 
names(winners) <- tolower(names(winners))

# Setting orders for medal
winners$medal <- factor(winners$medal, order = TRUE, levels = c("G", "S", "B"))

# Find the player with NA value in age column
players %>% 
  filter(is.na(players$age))

# The age is missing for Felicity Cradick. Google search gives the age to be 20.
# Add the age for Felicity Cradick
players$age[players$athlete.name == "FelicityCradick"] <- 20

# Find the player with '0' in age column
players %>% 
  filter(players$age == 0)

# Actual age of Anthony Pesela is 20. Add the correct age
players$age[players$athlete.name == "AnthonyPesela"] <- 20

# Remove white space from athlete.name
players$athlete.name <- gsub(" ", "", players$athlete.name)
winners$athlete.name <- gsub(" ", "", winners$athlete.name)

# Arrange by athlete.name
players <- players %>% 
  arrange(athlete.name)

winners <- winners %>% 
  arrange(athlete.name)

# Some names have '.' at the start. Correct the names 
players$athlete.name[players$athlete.name == ".Sudhir"] <- "Sudhir"
players$athlete.name[players$athlete.name == ".ParmjeetKumar"] <- "ParmjeetKumar"
players$athlete.name[players$athlete.name == ".ManpreetKaur"] <- "ManpreetKaur"
winners$athlete.name[winners$athlete.name == ".Sudhir"] <- "Sudhir" 

# Some names in 'winners' data frame have the word 'null' at the start. Remove the word null
winners$athlete.name <- gsub("null", "", winners$athlete.name)

# arrange by athlete.name
winners <- winners %>% 
  arrange(athlete.name)

### Data Preparation

# Create a new data frame by joining players and winners
players_02 <- merge(x = players, y = winners , by = c("athlete.name", "team"), all.x = TRUE)

# Remove unwanted columns
players_02 <- subset(players_02, select = -c(continent, event, sport.x, sport.y))

# Note: The original 'winners' data frame contained 1558 entries for winners.
# After merging, 8 entries are missing. These entries are neglected in further analysis.

# Add a column for winners
players_02 <- players_02 %>% 
  mutate(winner = ifelse(is.na(medal), 0, 1))

# Convert players_02$winner to factors
players_02$winner <- as.factor(players_02$winner)

# Create a data frame with only winners
players_ow <- players_02 %>%
  select(athlete.name, team, gender, age, winner) %>% 
  filter(!duplicated(athlete.name) & winner == 1)

# players_02 contains multiple entries for players with more than one medal 
# Create a data frame with single entry for all players

players_ap <- players_02 %>%
  select(athlete.name, team, gender, age, winner) %>% 
  filter(!duplicated(athlete.name))

# Data Vizualization

# Boxplot for age by gender
ggplot(data = players_ap, aes(x = gender, y = age, color = gender))+
  theme_bw() +
  geom_boxplot() +
  labs(x = "Gender",
       y = "Age",
       title = "Boxplot for age by gender",
       subtitle = "Commonwealth Games 2022")

# Histogram for winners by age
ggplot(data = players_ap, aes(x = age, fill = winner))+
  theme_bw() +
  geom_histogram(binwidth = 2, color= "black", alpha = 0.5) +
  facet_wrap(~gender) +
  labs(x = "Age",
       y = "No of players",
       title = "Winners by age",
       subtitle = "Commonwealth Games 2022")

# Bar graph for total players by gender
ggplot(data = players_ap, aes(x = gender, fill = winner))+
  theme_bw() +
  geom_bar(alpha = 0.5) +
  geom_text(stat='count',position = position_stack(vjust = 0.5), aes(label=..count..)) +
  labs(x = "Gender",
       y = "No of players",
       title = "Total players by gender",
       subtitle = "Commonwealth Games 2022")

# Pie chart for total number of medals
plot_03 <- players_02 %>% 
  select(medal, winner) %>% 
  filter(winner == 1) %>% 
  group_by(medal) %>% 
  summarise(total_medals = n())

plot_03 %>% 
  ggplot(aes(x = "", y = total_medals, fill = medal, label= total_medals))+
  geom_bar(stat = "identity", alpha = 0.5)+
  geom_text(position = position_stack(vjust = 0.7)) +
  theme_void() +
  labs(title = "Total number of medals",
       subtitle = "Commonwealth Games 2022") +
  coord_polar("y", start=0)

# Bar graph for top 10 teams by size
top_10_by_size <- players %>% 
  group_by(team) %>% 
  summarise(team_size = n()) %>% 
  arrange(-team_size)

top_10_by_size <- head(top_10_by_size, 10)

plot_02 <- players_ap %>%                                 
  select(team, gender, winner) %>%                                 
  group_by(team) %>%                                      
  mutate(total = n()) %>%                                 
  filter(total >= (top_10_by_size$team_size[10]))

plot_02 %>% 
  ggplot(aes(y= reorder(team, total), label = total))+    
  geom_bar(position = position_stack(reverse = TRUE), alpha = 0.5) +
  theme_bw() +
  geom_text(stat='count',position = position_stack(reverse = TRUE, vjust = 0.5), aes(label=..count..)) +
  labs(y = "Team",
       x = "No of players",
       title = "Top 10 teams by size",
       subtitle = "Commonwealth Games 2022")

# Bar graph for top 10 teams by size distributed by gender
plot_02 %>% 
  ggplot(aes(y= reorder(team, total), fill = gender, label = total))+    # reorder the graph total
  geom_bar(position = position_stack(reverse = TRUE), alpha = 0.5) +
  theme_bw() +
  geom_text(stat='count',position = position_stack(reverse = TRUE, vjust = 0.5), aes(label=..count..)) +
  labs(y = "Team",
       x = "No of players",
       title = "Top 10 teams by size distributed by gender",
       subtitle = "Commonwealth Games 2022")

# Bar graph for top 10 teams by size distributed by medal
plot_02 %>% 
  ggplot(aes(y= reorder(team, total), fill = winner, label = total))+    # reorder the graph total
  geom_bar(position = position_stack(reverse = TRUE), alpha = 0.5) +
  theme_bw() +
  geom_text(stat='count',position = position_stack(reverse = TRUE, vjust = 0.5), aes(label=..count..)) +
  labs(y = "Team",
       x = "No of players",
       title = "Top 10 teams by size ditributed by medal",
       subtitle = "Commonwealth Games 2022")

# Bar graph for top 10 teams by size distributed by gender and medal
plot_02 %>% 
  ggplot(aes(y= reorder(team, total), fill = winner, label = total))+    # reorder the graph total
  geom_bar(position = position_stack(reverse = TRUE), alpha = 0.5) +
  theme_bw() +
  geom_text(stat='count',position = position_stack(reverse = TRUE, vjust = 0.5), aes(label=..count..)) +
  labs(y = "Team",
       x = "No of players",
       title = "Top 10 teams by size distributed by gender and medal",
       subtitle = "Commonwealth Games 2022") +
  facet_wrap(~gender)

# Bar graph for top 5 teams by number of medals
top_5_by_medals <- winners %>% 
  group_by(team) %>% 
  summarise(total_medals = n()) %>%
  arrange(-total_medals)

top_5_by_medals<- head(top_5_by_medals, 5)

plot_01 <- players_02 %>% 
  filter(winner == 1) %>%                                 # filter only winners
  select(team, medal) %>%                                 # select team and medal column
  group_by(team) %>%                                      # group by team
  mutate(total = n()) %>%                                 # calculate total medals by team
  filter(total >= (top_5_by_medals$total_medals[5]))      # filter top 5 countries
plot_01 %>% 
  ggplot(aes(y= reorder(team, total), fill = medal, label = total))+    # reorder the graph total
  geom_bar(position = position_stack(reverse = TRUE), alpha = 0.5) +
  theme_bw() +
  geom_text(stat='count',position = position_stack(reverse = TRUE, vjust = 0.5), aes(label=..count..)) +
  labs(y = "Team",
       x = "No of medals",
       title = "Top 5 Teams",
       subtitle = "Commonwealth Games 2022")
