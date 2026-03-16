install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)
library(ggplot2)

#data <- read.csv("C:\Users\irene\OneDrive\Documents\Irene UofG\SAP-group1\project1-growth-wt.csv")
data <- read.csv("C:/Users/irene/OneDrive/Documents/Irene UofG/SAP-group1/project1-growth-wt.csv")

### base stracture
str(data)
summary(data)
head(data)
colSums(is.na(data))

data$Sex <- factor(data$Sex)

str(data)
table(data$Sex)
prop.table(table(data$Sex))


### Wt1 分布
ggplot(data, aes(x = Wt1)) +
  geom_histogram(bins = 15, color = "black", fill = "lightblue") +
  labs(
    title = "Distribution of Weight at 1 Month",
    x = "Wt1 (kg)",
    y = "Count"
  )

### Wt24 分布
ggplot(data, aes(x = Wt24)) +
  geom_histogram(bins = 15, color = "black", fill = "lightgreen") +
  labs(
    title = "Distribution of Weight at 24 Months",
    x = "Wt24 (kg)",
    y = "Count"
  )

### Solids 分布
ggplot(data, aes(x = Solids)) +
  geom_histogram(bins = 15, color = "black", fill = "orange") +
  labs(
    title = "Distribution of Age at Introduction of Solids",
    x = "Solids (months)",
    y = "Count"
  )

### 散点图
ggplot(data, aes(x = Wt1, y = Wt24)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship between Weight at 1 Month and Weight at 24 Months",
    x = "Wt1 (kg)",
    y = "Wt24 (kg)"
  )


model1 <- lm(Wt24 ~ Wt1, data = data)


summary(model1)
plot(model1)
plot(resid(model1))

# Secondary objectives
#a)
#i)
weight_sex_model <- lm(Wt24 ~ Wt1+Sex, data= data)
summary(weight_sex_model)
#ii)
weight_age_model <- lm(Wt24 ~ Wt1+Solids, data= data)
summary(weight_age_model)
#iii)
weight_comb_model <- lm(Wt24 ~ Wt1 + Sex + Solids, data = data)
summary(weight_comb_model)

### Question 3

Question 3: Does the average weight gain differ between Boys and Girls?

To answer this question, we first calculated the total weight gain for each baby ($Weight\ at\ 24\ months\ minus\ Weight\ at\ 1\ month$). We then used a Two-Sample T-test to compare the average growth of boys and girls.

```{r echo=FALSE,warning=FALSE,message=FALSE}
data$Wt_Diff <- data$Wt24 - data$Wt1
t_test_results <- t.test(Wt_Diff ~ Sex, data = data)
t_test_results
```

Results and Interpretation:

-   Average gain for Boys is 8.608

-   Average gain for Girls is 7.823

-   The p-value is 0.001

  The p-value is 0.001, which is much smaller than 0.05. In statistics, this means the result is highly significant.

  There is a very strong evidence that weight gain during the first two years of life is not the same for both sexes because the p-value is 0.001\<0.05 and we have the 95 CI [-1.2534,-0.3160] does not include zero this mean the  mean for boys does not equal the mean of girls.
