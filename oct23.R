setwd("/Users/jennifersung/Desktop/Research Design/")
unvoting <- read.csv("unvoting.csv")
congvote <- read.csv("congvote.csv")
dim(unvoting)

head(unvoting)

dim(congvote)

head(congvote)

# Question 1. Voting in the United Nations General Assembly (2 pts)
# Like legislators in the US Congress, the member states of the United Nations (UN) are politically divided on many
# issues such as trade, nuclear disarmament, and human rights. During the Cold War, countries in the UN General Ass
# embly tended to split into two factions: one led by the capitalist United States and the other by the communist S
# oviet Union. In this exercise, we will analyze how states' ideological positions, as captured by their votes on U
# N resolutions, have changed since the fall of communism.(This exercise is based on Michael A. Bailey, Anton Strez
# hnev, and Erik Voeten. "Estimating Dynamic State Preferences from United Nations Voting Data." Journal of Conflic
# t Resolution, August 2015.)
# The data is called "unvoting.csv".
# In the analysis that follows, we measure state preferences in terms of numerical ideal points. These ideal point
# s capture what international relations scholars have called countries’ liberalism on issues such as political fre
# edom, democratization, and financial liberalization. The two measures are highly correlated, with larger (more li
# beral) ideal points corresponding to a higher percentage of votes that agree with the US.
# ## a) We begin by examining how the distribution of state ideal points has changed since the end of communism.
# Create a histogram of the ideal points in 1980, which is before the fall of the Berlin Wall, and histogram of ide
# al points in 2000, which is after the fall of the Berlin Wall. Add median to each plot as a vertical line. How
# do the two distributions differ? Has there been more ideological convergence or divergence after the fall of comm
# unism?

unvoting1980 = subset(unvoting, subset = (Year == "1980"))
unvoting2000 = subset(unvoting, subset = (Year == "2000"))
hist(unvoting1980$idealpoint, freq = FALSE, main = "Before Berlin wall 1980", xlim = c(-3, 3), ylim = c(0, 1),
xlab = "Ideal point")
abline(v = median(unvoting1980$idealpoint))


hist(unvoting2000$idealpoint, freq = FALSE, main = "After Berlin wall 2000", xlim = c(-3, 3), ylim = c(0, 1), x
lab = "Ideal point")
abline(v = median(unvoting2000$idealpoint))

# b) Create a scatterplot of the evolution of the Russian/Soviet Union
# and the United States ideal points over time. You will want to use the
# unique( ) function for the year variable on the x-axis (e.g.,
# unique(un$Year)). Add the yearly median ideal point of all countries.
# [You can use type = “l” in the plot( ) function to connect the points.]
# Make the plots for Russia/Soviet Union, the United States, and the
# median different colors, and include labels using text( ). Also add
# labels to the x-axis and y-axis. What does this graph tell us about
# the change in the ideological positions of Russia / Soviet Union and
# the United States over time?


years = unique(unvoting$Year)
plot(years, unvoting$idealpoint[unvoting$CountryAbb == "USA"], ylim = c(-4,4), type = "l", xlab = "Year", ylab =
"Ideal Point", main = "Ideal point over time", col = "blue")
 lines(years, unvoting$idealpoint[unvoting$CountryAbb == "RUS"],
 col = "red")
 lines(years, tapply(unvoting$idealpoint, unvoting$Year, median), col = "purple")
 text(1970, -0.9, "Russia/Soviet Union")
 text(2000, 2, "USA")
 text(1980, 0.2, "All Countries")

#  As observed from the graph, it appears Russia has become alot more liberal between 1980 to 1990. The U.S. had a relatively slow increase in
# liberal ideologies over the years but is still more liberal compared to Russia and other countries. The graph might be telling us that the gap in liberal
# ideologies difference is shrinking between Russia and United States.

# a) (1 pt) Answer the following questions about the data
# i. How many observations are in the dataset? How many variables are in the dataset?
dim(congvote)

table(congvote$inc)

# iii. Provide the basic descriptive statistics for the central tendency of the d.cong.vs variable (i.e., mean and
# median).
mean(congvote$d.cong.vs)


median(congvote$d.cong.vs)

sd(congvote$d.cong.vs)

min(congvote$d.cong.vs)

max(congvote$d.cong.vs)

IQR(congvote$d.cong.vs)

# b) (1 pt) Visualizing the distribution of Democratic Vote share across districts
# i. Create a boxplot of d.cong.vs variable that compares the following types of elections: i) those with a Democratic
# incumbent; ii) those with no Democratic or Republican incumbent; and iii) those with a Republican incumbent.
# Make sure you provide appropriate labels to the figure. Briefly explain how the boxplot is or is not consistent with
# the existence of an incumbency advantage?


rep <- subset(congvote, subset = (inc == "-1"))
none <- subset(congvote, subset = (inc == "0"))
dem <- subset(congvote, subset = (inc == "1"))
boxplot(dem$d.cong.vs, rep$d.cong.vs, none$d.cong.vs, names = c("Democratic inc.", "Republicans inc.", "Without D
em/Rep inc."), ylab = "vote share", main = "Incumbency Advantage", col = c("blue", "red", "purple"))

# ii. Create a histogram of the d.cong.vs variable using only the observations for the year 1956. Now create a
# histogram using only the observations for the year 1988. Make sure you provide appropriate labels to the figures.
# How do these histograms differ and what might this difference indicate about the change in the incumbency
# advantage between 1956 and 1988?
observ1956 <- subset(congvote, congvote$year == 1956)
observ1988 <- subset(congvote, congvote$year == 1988)
hist(observ1956$d.cong.vs, xlab = "Vote Share", main = "Observation for 1956")
abline(v = median(observ1956$d.cong.vs))

hist(observ1988$d.cong.vs, xlab = "Vote Share", main = "Observation for 1988")
abline(v = median(observ1988$d.cong.vs))

# c) (1 pt) Democratic congressional vote and Democratic presidential vote
# i. What is the correlation between district Democratic partisanship (i.e., d.partisan) and Democratic congressional
# vote (i.e., d.cong.vs)? What does this correlation tell us about the relationship between district Democratic
# partisanship and Democratic congressional vote across congressional districts?
cor(congvote$d.cong.vs, congvote$d.partisan)


# ii. Create a scatterplot using only the 1956 observations and a separate scatterplot using only the 1988
# observations where the x-axis is partisanship in the district (i.e., d.partisan) and the y-axis is the vote share of the
# Democratic congressional candidate in the district (d.cong.vs). Make sure both graphs have the appropriate labels
# and titles. Make the categories of party.inc different colors (i.e., the races where party.inc is “D” would be a
# different color from those races where party.inc is “R”). Discuss whether the scatterplots provide visual evidence
# regarding how the incumbency advantage differs between 1956 and 1988? [Hint: To make the categories of
# party.inc different colors you can add something like
#  in
# the plot() function for the scatterplot using the 1988 observations.]


observation56 <- subset(congvote, subset = (year == 1956))
observation88 <- subset(congvote, subset = (year == 1988))
plot(x = observation56$d.partisan, y = observation56$d.cong.vs, pch = 16, main = "1956 Observation",
 col = c("blue", "black", "red")[as.factor(congvote$party.inc[congvote$year == 1956])], ylim = c(0, 1), xlab
 = "Partisanship in district", ylab = "vote share")

 plot(x = observation88$d.partisan, y = observation88$d.cong.vs, pch = 16, main = "1988 Observation",
 col = c("blue", "black", "red")[as.factor(congvote$party.inc[congvote$year == 1988])], ylim = c(0, 1), xlab
 = "Partisanship in district", ylab = "vote share")

 fit <- lm(d.cong.vs ~ inc, data = congvote)
fit

summary(fit)

coef(fit)

summary(fit)$r.squared

fit_56 <- lm(d.cong.vs ~ inc, data = congvote, subset = year == 1956)
fit_56

fit_88 <- lm(d.cong.vs ~ inc, data = congvote, subset = year == 1988)
fit_88

fit3 <- lm(d.cong.vs ~ inc + d.partisan, data = congvote)
fit3

 cor(congvote$inc,congvote$d.partisan)

dpartsan_56 <- lm(d.cong.vs ~ inc + d.partisan, data = congvote, subset = year == 1956)
dpartsan_56

dpartisan_88 <- lm(d.cong.vs ~ inc + d.partisan, data = congvote, subset = year == 1988)
dpartisan_88