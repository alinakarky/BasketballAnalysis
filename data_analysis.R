# load required libraries
library(tidyverse)
library(broom)
library(dplyr)
library(ggplot2)
library(scales)

#https://unicanberra.instructure.com/courses/11769/pages/data-description-reproducible-data-analysis-project

# load all the datasets available and have a look
player_statistics <- read_csv("data/raw/2018-19_nba_player-statistics.csv")
player_salaries <- read.csv("data/raw/2018-19_nba_player-salaries.csv")

team_statistics_1 <- read_csv("data/raw/2018-19_nba_team-statistics_1.csv")
team_statistics_2 <- read_csv("data/raw/2018-19_nba_team-statistics_2.csv")

#team_statistic_2 has data that makes sense without missing values so lets take that data
team_statistics <- team_statistics_2
team_payroll <- read.csv("data/raw/2019-20_nba_team-payroll.csv")

# check the basic structure of each of the dataset
str(player_statistics)
head(player_statistics)
tail(player_statistics)

str(player_salaries)
head(player_salaries)
tail(player_salaries)

str(team_statistics)
head(team_statistics)
tail(team_statistics)

str(team_payroll)
head(team_payroll)
tail(team_payroll)

# check missing values
sum(is.na(player_statistics))
which(is.na(player_statistics), arr.ind = TRUE)

#plot missing values
naniar::vis_miss(player_statistics)

sum(is.na(player_salaries))
sum(is.na(team_statistics))
sum(is.na(team_payroll))


# can't be converted to wide to long or vice versa as each variable seems to be different
#  since 0.6% have missing data lets delete them

player_statistics <- drop_na(player_statistics)
team_statistics <- drop_na(team_statistics)

#check for duplicate player name and use only distinct players 
player_statistics <- distinct(player_statistics,player_name,.keep_all = TRUE)

# combine player statistics and salary
player_statistics <- merge(player_statistics, player_salaries, by.x = "player_name", by.y = "player_name")


# Analysis of distribution of age
player_statistics %>%
  ggplot(aes(x=Age))+
  geom_bar(color="white", fill="blue")


# analysis of salary of players based on points
ggplot(player_statistics, 
  aes(x = PTS, y = salary)) +
  geom_point(alpha = .4, size = 3) +
  xlab("Points Scored")+
  ylab("Salary (In Millions)")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ # millions
  geom_smooth(method = "lm") 

#just playing with the data to view top players and their salaries
top_10_salary <- player_statistics %>% 
  arrange(desc(salary)) %>% 
  top_n(10)


ggplot(data = top_10_salary)+
  geom_bar(mapping = aes(x = player_name, y=salary), fill = "dodgerblue", stat = "identity")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6))+ # millions
  theme(axis.text.x = element_text(angle = 30, vjust = 0.8))


# the key objective is to find best player in each position but based on what?
# lets use Performance Index Rating(PER) for each player to decide which player is best

# Based on the wikipedia, PER refers to the substraction of sum of all positive measures and
# sum of all negative measures 
# So lets use the following formula based on what data we have in our dataframe:
# PER = (Points + Rebounds + Assists + Steals + Blocks)
# - (Missed Field Goals + Missed Free Throws + Turnovers + Fouls Committed).

#lets create new column PER based on the formula
player_statistics <- player_statistics %>% 
  mutate(PER = (PTS+TRB+AST+STL+BLK)-(FGA+FTA+TOV+PF))

# now PER is our predictive variable so lets compare PER with other response variables

# now the final dataset can be considered as tidy one so lets save that dataset in processed directory
write.csv(player_statistics,'data/processed/player_statistics.csv')


# lets check the distribution or PER

ggplot(data = player_statistics, aes(x = PER)) +
  geom_histogram(binwidth = 25, colour = "black", fill = "blue")

# the distribution is right skewed
  
# Lets check the relationship of PER with different variables we used in equations
# PTS vs PER
ggplot(player_statistics, aes(x = PTS, y = PER)) +
  geom_point(alpha = .4, size = 3) +
  geom_smooth(method = "lm") 
# moderate linear relation

# salary vs PER
ggplot(player_statistics, 
  aes(x = PER, y = salary)) +
  geom_point(alpha = .4, size = 3) +
  xlab("Performance Indicator Tating (PER)")+
  ylab("Salary (In Millions)")+
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) # millions

# age vs salary??
ggplot(player_statistics, 
       aes(x = Age, y = salary)) +
  geom_point(alpha = .4, size = 3) +
  scale_y_continuous(labels = label_number(suffix = " M", scale = 1e-6)) # millions

# no clear relation

#linear regression

fit <- lm(PER ~ PTS+TRB+AST+STL+BLK+FGA+FTA+TOV+PF, player_statistics)


tidy(fit, conf.int = TRUE)
# the estimates now prove that whatever variables we used as positive are actually positive
# and the negative ones imply negative relation with PER values. Now lets just use
# positive values for our linear regression.

fit <- lm(PER ~ PTS+TRB+AST+STL+BLK, player_statistics)

car::vif(fit)

# Values of all other variables except "Kicks" and "Marks" are almost close to 1 
# so, these two variables have multicollinearity as they have values around 2.5.

# visualize
pairs(formula = ~ PTS+TRB+AST+STL+BLK+FGA+FTA+TOV+PF, data = player_statistics)

# we cab see that some values have almost same distance and patters indicating multicollinearlity

## Question 14 code goes here
car::avPlots(fit)

# testing heteroscedasticity
res <- residuals(fit)
fitted <- predict(fit)

ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "black") + 
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# The points are about same distance from line so there us homoscedasticity

# create exp_Goals variable
new_df <- mutate(player_statistics, exp_per=predict(fit, newdata = player_statistics))

# plot data
ggplot(data = new_df, aes(x = PER, y = exp_per)) +
  geom_point(colour = "black") + 
  geom_abline(colour = "red", linetype = "dashed")

#selecting top player based on position
top_5_player <- player_statistics %>%
  group_by(Pos) %>%
  arrange(desc(PER)) %>%
  select(Pos,player_name,Age,PER, salary) %>% 
  filter(row_number()==1)

View(top_5_player)
