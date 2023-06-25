## Jennifer Sung Assignment_2

getwd()
setwd("/Users/jennifer/Desktop/Research Design/")
boston <- read.csv("boston.csv")
class(boston)
head(boston)
summary(boston)

###Part 1: Effect of Demographic Change on Exclusionary Attitudes.

## Part1.1 
#Treatment and Control
control_boston <- subset(boston, treatment == 0, na.rm = T)
treatment_boston <- subset(boston, treatment == 1, na.rm = T)

no_college <- mean(control_boston$college == 0, na.rm = T)
yes_college <- mean(control_boston$college == 1, na.rm = T)  

#Mean for those who did not go to college and did go.
mean(control_boston$college)
mean(treatment_boston$college)

# Median for those who did not go to college and did go.
median(control_boston$college)
median(treatment_boston$college)

#Mean on ideology
mean(control_boston$ideology == 1, na.rm = T)
mean(control_boston$ideology == 3, na.rm = T)
mean(control_boston$ideology == 5, na.rm = T)

#Median on ideology
median(control_boston$ideology == 1, na.rm = T)
median(control_boston$ideology == 3, na.rm = T)
median(control_boston$ideology == 5, na.rm = T)

#Question:The the median in both the control and treatment group was the same, while the mean for the control and treatment group had some difference.



## Part 1.2
# Difference in numberim.post and numberin.pre variable
boston$change_att_imm <- boston$numberim.post - boston$numberim.pre

# Mean in Treatment and control
control_imm <- mean(boston$change_att_imm[boston$treatment == 0], na.rm = T)

treatment_imm <- mean(boston$change_att_imm[boston$treatment == 1], na.rm = T)

# Change between treatment and control
treatment_imm - control_imm

# Question: Looking at the results and comparing it to the scale, the attitudes in the treatment group (0.11) were moreagainst increase in immigration, than the control group (-0.18).





## Part 1.3

mean(boston$numberim.post[boston$treatment == 0 & boston$college == 0])
mean(boston$numberim.post[boston$treatment == 1 & boston$college == 1], na.rm = TRUE)

#Question: There seems to a slight decrease in negative attitudes towards increase immigration to those who went to college.





getwd()
setwd("/Users/jennifer/Desktop/Research Design/")
chechen <- read.csv("chechen.csv")
class(chechen)
head(chechen)
summary(chechen)

###Part 2:Indiscriminate Violence and Insurgency

##Part 2.1

# Village struck will equal to 1. Villages not struck will equal to 0
table(chechen$fire)  

sum(chechen$fire == 1)   # 159 villagers were not struck by Russian artillery
sum(chechen$fire == 0)





##Part 2.2
# Mean of deaths outside groznny
mean(chechen$deaths[chechen$groznyy == 0], na.rm = T) 

# Mean of deaths in groznny
mean(chechen$deaths[chechen$groznyy == 1], na.rm = T) 

# Mediam of deaths outside groznny
median(chechen$deaths[chechen$groznyy == 0], na.rm = T) 

# Mediam of deaths in groznny
median(chechen$deaths[chechen$groznyy == 1], na.rm = T) 

# Using the mean, on avg. 3.7 people were killed in groznny, and on avg. 1.57 people were killed outside of groznny
# Using the median, on avg. 3 people were killed in groznny, and on avg. 0 people were killed outside of groznny




#Part 2.3
# Mean of those not hit and postattack
postattack_nonfire <- mean(chechen$postattack[chechen$fire == 0])

# Mean of those hit and postattack
postattack_fired <- mean(chechen$postattack[chechen$fire == 1])

# Difference in postattack
postattack_difference <- mean(chechen$postattack[chechen$fire == 1]) - mean(chechen$postattack[chechen$fire == 0]) 

# Question: If it is assumed that indiscriminate violence violence is effective in suppressing insurgent activity.Then according to the data,the villages that were struck with artillery fire had less insurgent attacks than villages not struck with artillery fire.
# Those that struck with artillery fire had an avg. of 1.5 insurgent attacks compared to 2.05 insurgent attacks for villages not struck with artillery fire. While indiscriminate has decreased, assuming this was because of artillery fire hitting or not hitting a village is not enough evidence. You need to consider other factors.




## Part 2.4
# Mean of those not hit and postattack
postattack_nonfire 

# Mean of those hit and postattack
postattack_fired 

# Mean of those not hit and preattack
preattack_nonfire <- mean(chechen$preattack[chechen$fire == 0])

# Mean of those hit and preattack
preattack_fired <- mean(chechen$preattack[chechen$fire == 1])

# Difference in preattacks 
preattack_difference <- mean(chechen$preattack[chechen$fire == 1]) - mean(chechen$preattack[chechen$fire == 0])

#Question: The average number of insurgency attacks in the preattack, are similar between villages that were not struck with artillery fire (2.15) and those struck (2.11). Since it is almost similar, this increases the validity of comparison in understanding whether insurgency attacks increase/decrease postattack.




## Part 2.5
# calculating diffattack variable
diffattack <- chechen$postattack - chechen$preattack

# Mean of diffattack for non-shelled
mean(diffattack[chechen$fire == 0])

# Mean of diffattack for shelled
mean(diffattack[chechen$fire == 1])

#Difference between diffattack
mean(diffattack[chechen$fire == 1]) - mean(diffattack[chechen$fire == 0])

#Question: In this comparison, the number of insurgent attacks has significantly decreased in villages that were struck with artillery fire than villages not hit with artillery fire. This supports the belief that indiscriminate violence is effective in suppressing insurgent activity. And the validity of this analysis has improved because it is taking into account pre and post insurgent attacks.


##




