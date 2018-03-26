#Assignment: (Andrew Wong) Fixed and Random Effects ANOVA
#1For the unemployment data test for differences in unemployment rates in 2016 by Rural_urban continum code (fixed effects)

#2. several ovens in a metal work shop are used to heat metal specimens. All ovens are supposed to operate at the same temp. 3 ovens are
#selected at random and their temps on sucessive heats are recorded.
#Oven Temp.
#1    491.5, 498.3,498.1, 493.5, 493.6
#2    488.5, 484.65, 479.9, 477.35
#3    490.1, 484.8, 488.25, 473, 471.85, 478.65


#Are the ovens operating at the same temperature? Justify.


#1: For the unemployment data test for differences in unemployment rates in 2016 by Rural_urban continum code

#Import data
library(readr)
Unemployment <- read_csv("Unemployment.csv")
View(Unemployment)
data <- Unemployment
attach(data)

#Look at structure and summary of data
str(data)
dim(data)
summary(data) #Some of the data has NA's. Since we were advised to remove them in this dataset. I will do that. (Usually wouldn't do this cause you might eliminate important data)

#Remove rows with NA's (Not complete)
data <- data[complete.cases(data),]
summary(data) #No NA's
dim(data) #3136

#Look at Rural_urban_continuum_code_2013 as my grouping variable
colnames(data)
code <- as.factor(data$Rural_urban_continuum_code_2013)


#Response: Rate of unemployment in 2016
rate <- data$Unemployment_rate_2016

#Check if design is balanced
tapply(rate,code, length) #No balanced. Really badly not balanced

#Check mean
tapply(rate,code, mean) #means look similar, but need to check variances to see scale


#Check variances
tapply(rate,code,var) #Some of the variances look pretty  close to one another


#Look at boxplot to get more information
boxplot(rate~code) #All boxes overlap, which is good for the assumption for equal medians


#Modeling: Fixed effects

#Check for normality:
factor<- c("1","2","3","4","5","6","7","8","9")
par(mfrow = c(3,3))
for(i in 1:9) {
  qqnorm(rate[code ==factor[i]])
}

#Since the dataset is large these departures indicate non-normality
shapiro.test(rate[code == "1"]) #See the pvalue is very low. Test confirms for group 1

tapply(rate,code, shapiro.test) #p value is very low for all groups

#Could be in trouble with ANOVA since there is non-normality
##approximate analysis using oneway test, since it assumes unequal variances
oneway.test(rate~code) #p value is less than .001. 

#Exact analysis:
aov.out <- aov(rate~code, data = data)
summary(aov.out) #p- value is less than .001

#Reject H0, that the groups have the same mean.


#Let's see which plots differ
par(mfrow = c(1,1))
plot(TukeyHSD(aov.out))
pairwise.t.test(rate,code) #So we can see which groups are diff. The plot didn't show all tabs in it's y axis. EX: group 1 and 2 differ

#Check our model fit with plot(aov.out)

par(mfrow = c(2,2))
plot(aov.out) #residual vs fitted not really centered at zero. Also qqnorm is not normal


#Use regression to predict unemployment rate by code
##overall normality (Need to check)
par(mfrow = c(1,1))
qqnorm(rate) #again doesn't look like a normal distribution

##regression on untransformed data
reg <- lm(rate~code)
summary(reg)
##baseline is code 1 and all other estimates are related to baseline
names(reg) #tells me what output is avaliable

#look at residuals
qqnorm(reg$residuals) #Again doesnt' look normal. We have a problem with the residuals
plot(reg$residuals) #Not centered about zero. We have some weird things like maybe outliers. Same as the plot of ANOVA


#Try a transformation
lograte <- log(rate)
qqnorm(lograte) #Much better in apearance of normal
logreg <- lm(lograte~code)
summary(logreg) #Model is better since R squared is better, and some variables are not sig

#Look at residuals
qqnorm(logreg$residuals) #Looks normal
plot(logreg$residuals) #Looks a lot more centered about zero

#Compare side by side the untransformed and the transformed data
par(mfrow = c(2,2))
plot(reg$residuals)
plot(logreg$residuals)
qqnorm(reg$residuals)
qqnorm(logreg$residuals)
par(mfrow = c(1,1))
#Since unbalanced we watned to do the regression anyways

#grab p value while releveling the factors one at a time as the baseline
list <- list()
for(i in 1:9) {
  codeloop <- relevel(code, ref = i)
  list[[i]] <- summary(lm(lograte ~ codeloop))$coefficients[,4]
}
#This makes it easier for me to see the p values to check for difference of means much easier than running the lm 9 times individually. 
list

#Make it easier to see since before it was in scientifc notation. Now it rounds to the 4th digit place
myData<- list()
myData <- lapply(list,round,4)
myData
#at alpha = .05
#When 1 is baseline: Sig Diff are all except group 9
#When 2 is baseline: Sig Diff are  for sure are 1,6,9.
#When 3 is baseline: Sig Diff 1,4,6,7,8,9
#When 4 is baseline: Sig Diff 1,2,3,9
#When 5 is baseline: Sig Diff 9
#When 6 is baseline: Sig Diff 1, 2, 3, 5, 9
#When 7 is baseline: Sig Diff 1, 2, 3, 9
#When 8 is baseline: Sig Diff 1, 3, 9
#When 9 is baseline: Sig Diff 2, 3, 4, 5, 6, 7, 8


#2. several ovens in a metal work shop are used to heat metal specimens. All ovens are supposed to operate at the same temp. 3 ovens are
#selected at random and their temps on sucessive heats are recorded.
#Oven Temp.
#1    491.5, 498.3,498.1, 493.5, 493.6
#2    488.5, 484.65, 479.9, 477.35
#3    490.1, 484.8, 488.25, 473, 471.85, 478.65


#Are the ovens operating at the same temperature? Justify.


ovens <- as.factor(c(rep(1,5), rep(2,4), rep(3,6)))
ovens

y <- c(491.5, 498.3,498.1, 493.5, 493.6, 488.5, 484.65, 479.9, 477.35,490.1, 484.8, 488.25, 473, 471.85, 478.65)


#Look at mean of data
tapply(y,ovens, mean) #Look pretty close to another let's check variances

#Look at variances of data
tapply(y,ovens,var) #Variances are really different from one another. So assume unequal variances

#Let's look at a boxplot of the data
boxplot(y~ovens) #confirms that the medians are different from oven 1 to the other 2. Interquartiles don't overlap

#Check for normality:
factor<- c("1","2","3")
par(mfrow = c(1,3))
for(i in 1:3) {
  qqnorm(y[ovens ==factor[i]])
}
#All except for graph 1 looks normal, but it probably was pointless to check for normatlity since only a few point. Not enough
##approximate analysis using oneway test, since it assumes unequal variances
oneway.test(y~ovens) #p value is less than .001. So reject H0 that group means are the same

#Let's try exact analysis
oven.out<- aov(y~ovens)
summary(oven.out) ##difference between groups. P-value is less than .001 so reject H0 that group means are the same


#Let's see which plots differ
par(mfrow = c(1,1))
plot(TukeyHSD(oven.out))
pairwise.t.test(y,ovens) #1 and 2 differ, and 1 and 3 differ

#Check our model fit with plot(aov.out)
par(mfrow = c(2,2))
plot(oven.out) # Residual vs fitted looks okay since centred around zero, and qqnorm looks good too. 


#Conclusion: Group 1 ovens differs from group 2 and 3's. 
