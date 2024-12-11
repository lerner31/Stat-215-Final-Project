# set working directory 
setwd("~/Desktop/STA215")
raw_data <- read.csv("raw_data.csv")
data <- na.omit(raw_data)

# load packages
library(readr)
library(dplyr)
library(haven)
library(psych)
library(ggplot2)

#Descriptive Statistics table 1
summary(data$music)
sd(data$music)

table(data$romance)

table(data$fourth_wall)

summary(data$rating)
sd(data$rating)
# Chi-Squared Test (Podcast & Creatures)
table(data$romance , data$fourth_wall)
chisq.test(data$romance , data$fourth_wall)



# box plot
boxplot(data$music ~ data$romance)
#ANOVA
anova_results <- aov(music ~ romance, data = data)
summary(anova_results)

#Scatter Plot
#Calculate mean lines
plot(data$music, data$rating)
mean_x <- mean(data$music) 
mean_y <- mean(data$rating) 

abline(v = mean_x)
abline(h = mean_y)

#Linear Regression
linear_relationship <- lm(data$rating ~ music, data = data)
summary(linear_relationship)
abline(linear_relationship)


# residuals plot
plot(data$music, residuals(linear_relationship))
abline(h = 0)
