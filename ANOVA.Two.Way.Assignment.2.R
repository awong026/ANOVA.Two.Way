#Assignment: Two-Way ANOVA (Andrew Wong)

#Homework:

#1. Use dataset Adler in package car to perform a two-way fixed effects ANOVA. To find information about the dataset ?Adler

library(car)
head(Adler)
summary(Adler)
str(Adler)
attach(Adler)
#Factors: instruction and expectation.
#Response: rating

#EDA

#Check means
tapply(rating, instruction:expectation, mean) #Means look different for Good and low from the rest of means

#Check variances
tapply(rating, instruction:expectation, var) #Variances look different too. Not safe to assume equal variances

#Check length for balanced design
tapply(rating, instruction: expectation, length) #Close but not a balanced design


#Plots
library(Rmisc)
library(tidyverse)
pd = position_dodge(.2)
sum <- summarySE(Adler,measurevar = "rating", groupvars = c("instruction", "expectation"))
sum

ggplot(sum, aes(instruction, rating, color =expectation)) + geom_errorbar(aes(ymin = rating-se, ymax = rating+se), width = .2, size = .7, position = pd) + geom_point(shape = 15, size = 4, position = pd) + theme_bw()
#Since some bars don't overlap. Some means are probably different from each other.
#Square points is group means
#Error bars are 1 se above and below the square point


#can do a boxplot too
boxplot(rating~instruction, data = Adler) #ONly for the instruction. Means don't seem differ
boxplot(rating~expectation, data = Adler) #Only for expectation. Means don't seem to differ
boxplot(rating~instruction:expectation, data = Adler) #Looks like there is one that has a difference of means

#Modeling
model = lm(rating~instruction*expectation, data = Adler)
summary(model) #Several categories and their interactions are significantly different

#Also look at ANoVA table too
aov.out <- aov(rating~instruction*expectation, data = Adler)
summary(aov.out) #See again, the interaction of instruction and expectation is significant

##Check assumptions:

#Check normal assumptions by looking at residuals
hist(residuals(model), col = "gray") #Looks like nice normal shape, nothing that's a deal breaker
qqnorm(residuals(model)) #Looks reasonable good. Doesn't look like a departure from normality

plot(fitted(model), residuals(model)) #No pattern, so good. 

#The interaction effect since significant
interaction.plot(instruction, expectation, rating) #When the lines cross there is an interaction. There is an interaction effect.
# rating is low for good instruction + low expectation, and rating is high when expectation is high + instruction is good. 
# rating is high when expecation is high and when instruction is scientific vs. rating is medium when expetation is high and scientific instruction. 

#Looks like people who have low expectations do better (high ratings) with scientific instruction and people with high expectations have high ratings when given good instruction.
#Good instruction has low ratings when people have low expectations, and scientific instruction does not help with ratings as much for high expectation people as when given good instruction. 

