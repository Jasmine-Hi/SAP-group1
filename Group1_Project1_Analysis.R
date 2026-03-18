library(tidyverse)

data <- read.csv("project1-growth-wt.csv")
data$Sex <- factor(data$Sex)

# Basic checks
str(data)
summary(data)
colSums(is.na(data))
table(data$Sex)
prop.table(table(data$Sex))

# Exploratory plots
ggplot(data, aes(x = Wt1)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  labs(title = "Distribution of Weight at 1 Month",
       x = "Wt1 (kg)", y = "Count")

ggplot(data, aes(x = Wt24)) +
  geom_histogram(bins = 15, color = "black", fill = "lightgreen") +
  labs(title = "Distribution of Weight at 24 Months",
       x = "Wt24 (kg)", y = "Count")

ggplot(data, aes(x = Solids)) +
  geom_histogram(bins = 15, color = "black", fill = "orange") +
  labs(title = "Distribution of Age at Introduction to Solids",
       x = "Solids (months)", y = "Count")

ggplot(data, aes(x = Wt1, y = Wt24)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Relationship between Weight at 1 Month and Weight at 24 Months",
       x = "Wt1 (kg)", y = "Wt24 (kg)")

# Q1
model1 <- lm(Wt24 ~ Wt1, data = data)
summary(model1)
plot(model1)

# Q2
weight_sex_model  <- lm(Wt24 ~ Wt1 + Sex, data = data)
weight_age_model  <- lm(Wt24 ~ Wt1 + Solids, data = data)
weight_comb_model <- lm(Wt24 ~ Wt1 + Sex + Solids, data = data)

summary(weight_sex_model)
summary(weight_age_model)
summary(weight_comb_model)

AIC(model1, weight_sex_model, weight_age_model, weight_comb_model)

anova(model1, weight_sex_model)
anova(model1, weight_age_model)
anova(model1, weight_comb_model)

plot(weight_comb_model)

# Q3
data$Wt_Diff <- data$Wt24 - data$Wt1

ggplot(data, aes(x = Sex, y = Wt_Diff, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Weight Gain from 1 to 24 Months by Sex",
       x = "Sex", y = "Weight Gain (kg)")

t_test_results <- t.test(Wt_Diff ~ Sex, data = data)
t_test_results

aggregate(Wt_Diff ~ Sex, data = data, mean)
