---
  title: "Project"
author: "Jennifer Sung"
date: "`r Sys.Date()`"
output:
  html_document: default
pdf_document: default
---
  
  ```{r}

knitr::opts_chunk$set(echo = TRUE)
setwd("/Users/jennifersung/Desktop/Happiness Project/")
library(stargazer)
WHR <- read.csv('WHR.csv')


```


***Summary and Subsets***
  
  ```{r}

dim(WHR)
summary(WHR)

```

There are 2089 observations and 10 variables in this dataset.


***Subset of Data***
  
  
  ```{r}


S_Africa.whr <- subset(WHR, subset = (WHR$Country == "South Africa"))
China.whr <- subset(WHR, subset = (WHR$Country == "China"))
India.whr <- subset(WHR, subset = (WHR$Country == "India"))
Russia.whr <- subset(WHR, subset = (WHR$Country == "Russia"))
Brazil.whr <- subset(WHR, subset = (WHR$Country == "Brazil"))
BRIC.whr <- subset(WHR, subset = (WHR$CODE == "BRIC"))
Advanced.whr <- subset(WHR, subset = (WHR$CODE == "MajorDeveloped"))
bric_mean <- tapply(BRIC.whr$Ladder, BRIC.whr$year, mean)
adv_mean <- tapply(Advanced.whr$Ladder, Advanced.whr$year, mean)




```


## Basic Descriptive statistic and Correlation (BRICS, Advanced, Global)

```{r}

mean(WHR$Ladder)
median(WHR$Ladder)
min(WHR$Ladder)
max(WHR$Ladder)
sd(WHR$Ladder)
IQR(WHR$Ladder)


mean(BRIC.whr$Ladder)
median(BRIC.whr$Ladder)
min(BRIC.whr$Ladder)
max(BRIC.whr$Ladder)
sd(BRIC.whr$Ladder)
IQR(BRIC.whr$Ladder)

mean(Advanced.whr$Ladder)
median(Advanced.whr$Ladder)
min(Advanced.whr$Ladder)
max(Advanced.whr$Ladder)
sd(Advanced.whr$Ladder)
IQR(Advanced.whr$Ladder)


cor(BRIC.whr$Ladder, BRIC.whr$LogGDP, use = "complete.obs")
cor(BRIC.whr$Ladder, BRIC.whr$Social, use = "complete.obs")
cor(BRIC.whr$Ladder, BRIC.whr$Health, use = "complete.obs")
cor(BRIC.whr$Ladder, BRIC.whr$Freedom, use = "complete.obs")
cor(BRIC.whr$Ladder, BRIC.whr$Generosity, use = "complete.obs")
cor(BRIC.whr$Ladder, BRIC.whr$Corruption, use = "complete.obs")

cor(Advanced.whr$Ladder, Advanced.whr$LogGDP, use = "complete.obs")
cor(Advanced.whr$Ladder, Advanced.whr$Social, use = "complete.obs")
cor(Advanced.whr$Ladder, Advanced.whr$Health, use = "complete.obs")
cor(Advanced.whr$Ladder, Advanced.whr$Freedom, use = "complete.obs")
cor(Advanced.whr$Ladder, Advanced.whr$Generosity, use = "complete.obs")
cor(Advanced.whr$Ladder, Advanced.whr$Corruption, use = "complete.obs")



```


***Histograms of Ladder Score and GDP***
  
  ```{r}

hist(BRIC.whr$Ladder, xlab = "Happiness Ladder Score", main = "Ladder Scores")


hist(BRIC.whr$LogGDP, xlab = "Happiness Ladder Score", main = "GDP of BRICS")


```


##Figure 1: Boxplot of BRICS ladder score and Log GDP 

```{r}

boxplot(WHR$Ladder ~ WHR$CODE, las = 2, xlab = "Economy Type", names = c("Other", "BRIC", "Advanced"), ylab = "Global Scores", main = "Happiness of BRIC countries")

boxplot(BRIC.whr$Ladder ~ BRIC.whr$Country, las = 2, names = c("Brazil", "China", "India", "Russia", "S. Africa"), xlab = "Country", ylab = "Ladder score", main = "Scores of BRIC countries")

boxplot(BRIC.whr$LogGDP ~ BRIC.whr$Country, names = c("Brazil", "China", "India", "Russia", "S. Africa"), las = 2, xlab = "Country", ylab = "GDP", main = "Log GDP of BRIC countries")


```

***mean and median for box plot***
  
  ```{r}

median(Brazil.whr$Ladder)
median(India.whr$Ladder)
median(S_Africa.whr$Ladder)
median(China.whr$Ladder)
median(Russia.whr$Ladder)

median(Brazil.whr$LogGDP)
median(India.whr$LogGDP)
median(S_Africa.whr$LogGDP)
median(China.whr$LogGDP)
median(Russia.whr$LogGDP)

```

***Figure 2: Life Ladder and Log GDP of BRICS countries over the years***
  
  ```{r}


bric_mean <- tapply(BRIC.whr$Ladder, BRIC.whr$year, mean)
adv_mean <- tapply(Advanced.whr$Ladder, Advanced.whr$year, mean)
bric_mean_gdp <- tapply(BRIC.whr$LogGDP, BRIC.whr$year, mean)
adv_mean_gdp <- tapply(Advanced.whr$LogGDP, Advanced.whr$year, mean)

plot(names(bric_mean), bric_mean, type = "l", col = "red", main = "Average Ladder Score", ylim = c(4.7, 7), xlab = "Years", ylab = "Life Ladder")
lines(names(adv_mean), adv_mean, type = "l", col = "blue")
text(2019, 5.5, "BRIC", col = "red")
text(2019, 6.5, "Advanced", col = "blue")

plot(names(bric_mean_gdp), bric_mean_gdp, type = "l", col = "red", main = "Average Ladder Score", ylim = c(9, 11), xlab = "Years", ylab = "Life Ladder")
lines(names(adv_mean_gdp), adv_mean_gdp, type = "l", col = "blue")
text(2019, 9.4, "BRIC", col = "red")
text(2019, 10.6, "Advanced", col = "blue")

plot(S_Africa.whr$year, S_Africa.whr$Ladder, type = "l", col = "red", main = "Life Ladder of BRIC", ylim = c(3, 7.5), xlab = "years", ylab = "Life Ladder")
lines(China.whr$year, China.whr$Ladder, type = "l", col = "blue")
lines(India.whr$year, India.whr$Ladder, type = "l", col = "purple")
lines(Russia.whr$year, Russia.whr$Ladder, type = "l", col = "black")
lines(Brazil.whr$year, Brazil.whr$Ladder, type = "l", col = "Green")
text(2017, 4.4, "South Africa", col ="red")
text(2018, 5.3, "China", col = "blue")
text(2018, 3.2, "India", col = "purple")
text(2014, 6.2, "Russia", col ="black")
text(2014, 7.2, "Brazil", col ="Green")

plot(S_Africa.whr$year, S_Africa.whr$LogGDP, type = "l", col = "red", main = "Log GDP of BRIC", ylim = c(8,10.3), xlab = "years", ylab = "GDP")
lines(China.whr$year, China.whr$LogGDP, type = "l", col = "blue")
lines(India.whr$year, India.whr$LogGDP, type = "l", col = "purple")
lines(Russia.whr$year, Russia.whr$LogGDP, type = "l", col = "black")
lines(Brazil.whr$year, Brazil.whr$LogGDP, type = "l", col = "Green")
text(2019, 9.4, "South Africa", col ="red")
text(2013, 9.2, "China", col = "blue")
text(2013, 8.4, "India", col = "purple")
text(2014, 10.25, "Russia", col ="black")
text(2014, 9.8, "Brazil", col ="Green")

```

***Figure 3: Regression Model between BRICS and Advanced countries***
  
  ```{r}

fit1 <- lm(Ladder ~ LogGDP, data = BRIC.whr)

fit2 <- lm(Ladder ~ LogGDP + Social + Health + Freedom + Generosity + Corruption, data = BRIC.whr)

fit3 <- lm(Ladder ~ LogGDP + Social + Health + + Freedom + Generosity + Corruption + factor(year) + factor(Country), data = BRIC.whr)

stargazer(fit1, fit2, fit3, type = "text", keep = c("LogGDP", "Social", "Health", "Freedom", "Generosity", "Corruption"), omit = "factor")

fit4 <- lm(Ladder ~ LogGDP, data = Advanced.whr)

fit5 <- lm(Ladder ~ LogGDP + Social + Health + Freedom + Generosity + Corruption, data = Advanced.whr)

fit6 <- lm(Ladder ~ LogGDP + Social + Health + + Freedom + Generosity + Corruption + factor(year) + factor(Country), data = Advanced.whr)


stargazer(fit4, fit5, fit6, type = "text", keep = c("LogGDP", "Social", "Health", "Freedom", "Generosity", "Corruption"), omit = "factor")


