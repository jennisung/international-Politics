## Jennifer Sung Assignment_1

getwd()
setwd("/Users/jennifersung/Desktop/Research Design/")
turnout_asgmt1.2 <- read.csv("turnout_asgmt1-2.csv")
class(turnout_asgmt1.2)


## Question 1 
# Dimensions:
dim(turnout_asgmt1.2) 

# How many observations are there? 17 observations. 

#Summary:
summary(turnout_asgmt1.2)

# What is the Range of years covered in this data set? 1980 to 2020
(turnout_asgmt1.2$year)
range(turnout_asgmt1.2$year)

# Which presidential election years are included in the data?
presidentelects <- turnout_asgmt1.2$year[c(1,3,5,7,9,11,13,14,15,16,17)]

# Which midterm election years are included in the data?
midtermelects <- turnout_asgmt1.2$year[c(2,4,6,8,10,12)]




## Question 2
# Step 1: VAP turnout rate with ADDED eligible overseas voters:
VAPtr <- turnout_asgmt1.2$total/(turnout_asgmt1.2$VAP + turnout_asgmt1.2$overseas) * 100
names(VAPtr) <- turnout_asgmt1.2$year

VAPtr

# Step 2: Calculate turnout rate for VEP
VEPtr <- (turnout_asgmt1.2$total / turnout_asgmt1.2$VEP) * 100
names(VEPtr) <- turnout_asgmt1.2$year

VEPtr

# Comparison and Difference:

difference <- VEPtr -VAPtr
summary(difference)

mean(VAPtr) 
mean (VEPtr)


# What difference do you observe? The mean of the VEP turnout was greater than the mean of the VAP turnout.





## Question 3
DifferenceVAP <- turnout_asgmt1.2$ANES - VAPtr
DifferenceVEP <- turnout_asgmt1.2$ANES - VEPtr

#Difference on average:
mean(DifferenceVAP) 
mean(DifferenceVEP)
mean(DifferenceVAP) - mean(DifferenceVEP)

#Range of difference
range (DifferenceVAP)
range(DifferenceVEP)
range (DifferenceVAP) - range(DifferenceVEP)

#Comment on results:The difference of average for "VAP and ANES" were more than "VEP and ANES" difference. 
#In addition, the "VAP and ANES" has a bigger range of difference than "VEP and ANES".





## Question 4
#Step 1: Divide presidential election into 2 periods:
#Period 1980 to 2000
presidentialelection <- turnout_asgmt1.2[c(1,3,5,7,9,11), ]

#Period 2004  to 2020
presidentialelection2 <- turnout_asgmt1.2[c(13,14,15,16,17), ]


#Step 2: Calculate difference between VEP and ANES turnout rate:
presidentialelectionvep1 <- (presidentialelection$total / presidentialelection$VEP) * 100


presidentialelection2vep2 <- (presidentialelection2$total / presidentialelection2$VEP) * 100


ANES1 <- presidentialelection$ANES

ANES2 <- presidentialelection2$ANES

#Step 3 calculate difference between the the VEP and ANES turnout rate between periods:
#Period 1980 to 2000
mean(ANES1) - mean(presidentialelectionvep1)

#Period 2004 to 2020
mean(ANES2) - mean(presidentialelection2vep2)

#Question: Yes, the ANES bias has increased in the "2004 to 2020" period, when compared to the "1980 to 2000" period.



## Question 5
#Step 1:
#Presidential election data from 1980 to 2000 (from ques 4)
presidentialelection <- turnout_asgmt1.2[c(1,3,5,7,9,11), ]

#Presidential VEP turnout rate:
VEPPE <- (presidentialelection$total / presidentialelection$VEP) * 100

#Presidential ANES turnout rate
ANESPresident <- presidentialelection$ANES

# Midterm election data from 1982 to 2002:
Midtermelection <- turnout_asgmt1.2[c(2,4,6,8,10,12), ]

# Midterm VEP turnout rate:
VEPME <- (Midtermelection$total / Midtermelection$VEP) * 100

#Midterm ANES turnout rate:
ANESMidterm <- Midtermelection$ANES


#Step 2: Compare the difference:
mean(presidentialelection$ANES) - mean(VEPPE)
mean(Midtermelection$ANES) - mean(VEPME)

(mean(presidentialelection$ANES) - mean(VEPPE)) - (mean(Midtermelection$ANES) - mean(VEPME)) 

#Question: Since, voter turnout is higher in the presidential election, it should not be excluded in the comparison




## Question 6
#Step 1 Begin by subtracting felons & noncitizens from adjusted VAP:
AdjustedVAP <- turnout_asgmt1.2$VAP - turnout_asgmt1.2$felons - turnout_asgmt1.2$noncit
AdjustedVAP
AdjustedVAP[14]

#Step 2: Calculate adjusted VAP from total ballots in 2008
turnout_asgmt1.2[14,]
AdjustedVAP[14]
adjustedVAPTurnouterate <- (turnout_asgmt1.2$total[14] - turnout_asgmt1.2$osvoters[14])/ (AdjustedVAP[14] - turnout_asgmt1.2$overseas[14] ) * 100


#Compare adjused VAP with Unadjusted VAP, and VEP, ANES:
#Adjusted VAP Turnout Rate:64.30767
# Unadjusted VAP: 
mean(VAPtr[14])

# Unadjusted VEP:
mean(VEPtr[14])

# Unadjusted ANES:
mean(turnout_asgmt1.2$ANES[14])

#Comment: In 2008, the "adjusted VAP turnout rate" was greater than "unadjusted VAP turnout rate".
# In addition, the "adjusted VAP turnout rate" was greater than "unadjusted VEP turnout rate".


