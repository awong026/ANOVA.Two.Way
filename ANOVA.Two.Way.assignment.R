1.	#Perform a two way ANOVA on the data in twan03.xls available on ecourseware.  
#This data is the number of spruce moths found in traps classified by Factor 1: location of trap in tree - Top, Middle, lower, and ground and Factor 2: type of lure in trap - scent, sugar, chemical. 
#You will need to rearrange the data so that each row is one observation with three variables - Factor 1, Factor 2 and Count

library(readxl)
twan03 <- read_excel("C:/Users/awong/Downloads/twan03.xls", 
                     sheet = "Sheet2")
View(twan03)
head(twan03)

##Need to change both Location and Trap variables to factors to use anova
twan03$Location <- as.factor(twan03$Location)
twan03$Trap <- as.factor(twan03$Trap)
is.factor(twan03$Location)
is.factor(twan03$Trap)
head(twan03)


#EDA
library(Rmisc)
#Checking for equal sd
sum <- summarySE(twan03, measurevar = "Count", groupvars = c("Location","Trap"))
sum ##Look at sd and see that all are in the same range (Nothing is 10x another)

##Looking for variance  
aggregate((Count ~ Location * Trap), FUN = var) ##Can assume equal variances  (not an order mag apart) , but it is close. 

##plot both factors together 
library(ggplot2)
pd <- position_dodge(.2) ##Define position
ggplot(sum, aes( x = Location, y = Count, color = Trap)) + 
  geom_errorbar(aes(ymin = Count - se, ymax = Count +se), width = .2, size = .7, position = pd) +
  geom_point(shape = 15, size = 4, position = pd) + 
  theme_bw() + 
  theme(axis.title.y = element_text(vjust = 1.8), axis.text.x = element_text(vjust = -.5), axis.title = element_text(face = "bold"))+
  scale_color_manual(values = c("black","blue", "red"))

#Looks like there may be different means at different locations, but there is also a possbility there isn't since errorbars for all 4 locations intersect

#Simple Boxplots

boxplot(Count~Location, data = sum)
#potential difference between Locations
boxplot(Count~Trap, data = sum)
#Looks like means could be the same for types of traps

fit <-aov(Count~Location*Trap) #gives those and their interaction effect
##fit <- aov(Count ~ Location + Trap + Location:Trap)
summary(fit) ##Or anova(fit)

#Interaction effect and Trap are not significant since their pvalues are above .05
#Location is significant since its pvalue is below .001

#explore more
dataTop <- twan03[which(Location == "Top"),] ##Pulls those rows
dataMid <- twan03[which(Location == "Middle"),]
dataLow <- twan03[which(Location == "Lower"),]
dataG <- twan03[which(Location == "Ground"),]

##Separates Count by Location
anova(aov(Count~Trap, dataTop)) ## Trap isn't signifacnt within Location top group
anova(aov(Count~Trap, dataMid)) ##Trap isn't signifacnt within Location Mid group
anova(aov(Count~Trap, dataLow)) ##Trap isn't signifacnt within Location low group
anova(aov(Count~Trap, dataG)) ##Trap isn't signifacnt within Location ground group

#Check normaility
qqnorm(dataTop$Count) #Looks okay
qqnorm(dataMid$Count) #Looks okay
qqnorm(dataLow$Count) #Looks okay
qqnorm(dataG$Count) #Looks okay


##Do the same thing by Trap instead
dataScent <- twan03[which(Trap == "Scent"),]
dataSugar <- twan03[which(Trap == "Sugar"),]
dataChemical <- twan03[which(Trap == "Chemical"),]

#Separates Count by Trap
anova(aov(Count~Location, dataScent)) #REally close to sig since pvalue is .06 # will count it as significant since it's close
anova(aov(Count~Location, dataSugar)) #Kind of close to sig since pvalue is .11
anova(aov(Count~Location, dataChemical)) #sig since pvalue is below .01


#Check normality
qqnorm(dataScent$Count) #okay
qqnorm(dataSugar$Count) #okay
qqnorm(dataChemical$Count) #okay

##The Location effect on month count depends on the Trap. If trap is Scent or Chemical  there is a significant effect of Location on Count
#but at trap sugar there is no significant location effect. There is significant effect on count, other than these. 

##ORRRRR

anova(aov(Count~as.factor(Location)*as.factor(Trap), twan03))
#Interaction is not sig and Trap is not sig. But Location is sig. 
##since there is a difference in mean count on Location, use multiple comparison, to figure out which one. 
##to see which pairs differ
pairwise.t.test(twan03$Count, twan03$Location)

##Or...

model <- aov(Count~as.factor(Location), twan03)
summary(model) #pvalue below .05
#We reject H0. There is at least one count mean different per Location

#Since reject H0. Let's do mutliple comparison to check which pair of means are different
plot(TukeyHSD(model))
##Lower-Ground, Middle-Ground, Top_Lower, Top-Middle


#2.	Perform a three-way ANOVA on the data in the dataset diamonds in R package ggplot2.  
#Model depth in terms of the three factors, cut, color and clarity
library(ggplot2)
attach(diamonds)
head(diamonds)
summary(diamonds)

#Change cut, color, and clarity to factors to use anova
diamonds$cut <- as.factor(diamonds$cut)
diamonds$color <- as.factor(diamonds$color)
diamonds$clarity <- as.factor(diamonds$clarity)

#EDA
library(Rmisc)
#Checking for equal sd
sum <- summarySE(diamonds, measurevar = "depth", groupvars = c("cut", "color", "clarity"))
sum ##Look at sd and see that all are in the same range

##Looking for variance  
aggregate((depth ~ cut* color * clarity), FUN = var) ##Can't assume equal variances

#boxplots
boxplot(depth ~ cut, sum) #means of depth looks different for cut
boxplot(depth ~ color, sum) #means of depth looks the same for color
boxplot(depth ~ clarity, sum) #Most of the depth looks the same for clarity, but some ends look a bit different


fit <- aov(depth ~ cut* color*clarity) ##Only use if our EDA showed normal and constant variances
summary(fit)
##All significant and their interactions are significant so there is a interaction effect
##We need to do the rest of the analysis knowing that there is an interaction effect. 


#levels
levels(diamonds$cut)
levels(diamonds$color)
levels(diamonds$clarity)


#explore more
dataFair <- diamonds[which(cut == "Fair"),] ##Pulls those rows
dataGood <- diamonds[which(cut == "Good"),]
dataVG <- diamonds[which(cut == "Very Good"),]
dataPre <- diamonds[which(cut == "Premium"),]
dataIdeal <- diamonds[which(cut == "Ideal"),]

##Separates depth by cut type
anova(aov(depth ~ color*clarity, dataFair)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color*clarity, dataGood)) ##Dose is hight signifacnt within VC group
anova(aov(depth ~ color*clarity, dataVG)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color*clarity, dataPre)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color*clarity, dataIdeal)) ##Dose is hight signifacnt within OJ group

##Separate depth  by cut type, but only when keeping one of the other 2 in test
anova(aov(depth ~ color, dataFair)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color, dataGood)) ##Dose is hight signifacnt within VC group
anova(aov(depth ~ color, dataVG)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color, dataPre)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ color, dataIdeal)) ##Dose is hight signifacnt within OJ group

##Separate depth  by cut type, but only when keeping one of the other 2 in test
anova(aov(depth ~ clarity, dataFair)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ clarity, dataGood)) ##Dose is hight signifacnt within VC group
anova(aov(depth ~ clarity, dataVG)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ clarity, dataPre)) ##Dose is hight signifacnt within OJ group
anova(aov(depth ~ clarity, dataIdeal)) ##Dose is hight signifacnt within OJ group

#Check normaility
qqnorm(dataFair$depth) #Looks okay
qqnorm(dataGood$depth) #Looks okay
qqnorm(dataVG$depth) #Looks okay
qqnorm(dataPre$depth) #Looks okay
qqnorm(dataIdeal$depth) #Looks okay #Doesn't look normal

##Do the same thing by dose instead
data05 <- ToothGrowth[which(dose == "0.5"),]
data10 <- ToothGrowth[which(dose == "1"),]
data20 <- ToothGrowth[which(dose == "2"),]

anova(aov(len~supp, data05)) #Sig
anova(aov(len~supp, data10)) #Sig
anova(aov(len~supp, data20)) #Not Sig

#Check normality
qqnorm(data05$len) #okay
qqnorm(data10$len) #okay
qqnorm(data20$len) #okay

##The supplment effec on toothgrowth depends on the dose. At dose .5 and 1.0 there is a significant effect of supplement on length
#but at does 2.0 there is no significant supplement effect



#################################################################################################################
#Can havee higher way ANOVAS
##EXAMPLE with highway
fit <- aov(hwy ~ as.factor(cyl)*as.factor(drv)*as.factor(fl), mpg)
anova(fit)
summary(fit)


#Look at interact plot
interaction.plot(mpg$drv, mpg$fl, mpg$hwy) #Interaction effect is the line that is different (stright line)
#hwy is the same for all except for one of them. 
interaction.plot(mpg$cyl, mpg$fl, mpg$hwy) #Doesn't look like all data is balanced. Then don't do anova

##EDA
install.packages("Rmisc")
library(Rmisc)
#Checking for equal sd
sum <- summarySE(ToothGrowth, measurevar = "len", groupvars = c("supp", "dose"))
sum ##Look at sd and see that all are in the same range

attach(ToothGrowth)
##Looking for variance  
aggregate((len ~ supp * dose), FUN = var) ##Can assume equal variances  (not an order mag apart) 

##plot both factors together 
library(ggplot2)
pd <- position_dodge(.2) ##Define position
ggplot(sum, aes( x = dose, y = len, color = supp)) + 
  geom_errorbar(aes(ymin = len - se, ymax = len +se), width = .2, size = .7, position = pd) +
  geom_point(shape = 15, size = 4, position = pd) + 
  theme_bw() + 
  theme(axis.title.y = element_text(vjust = 1.8), axis.text.x = element_text(vjust = -.5), axis.title = element_text(face = "bold"))+
  scale_color_manual(values = c("black","blue"))
