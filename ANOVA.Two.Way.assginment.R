#Assignment: Two and three way ANOVA 

#####################################################################################################

#Question 1: Perform a two way ANOVA on the data in twan03.xls available on ecourseware.  
#This data is the number of spruce moths found in traps classified by Factor 1: location of trap in tree - Top, Middle, lower, and ground and Factor 2: type of lure in trap - scent, sugar, chemical. 
#You will need to rearrange the data so that each row is one observation with three variables - Factor 1, Factor 2 and Count

######################################################################################################


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


#Check normality
qqnorm(dataScent$Count) #okay
qqnorm(dataSugar$Count) #okay
qqnorm(dataChemical$Count) #okay


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

#Conclusion
##Lower-Ground, Middle-Ground, Top_Lower, Top-Middle are the pairs that are different

############################################################################################

#2.	Perform a three-way ANOVA on the data in the dataset diamonds in R package ggplot2.  
#Model depth in terms of the three factors, cut, color and clarity

###########################################################################################

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
anova(aov(depth ~ color*clarity, dataFair)) ##Fair is significant for color and fair with clarity, but not the interaction effect of the two for depth
anova(aov(depth ~ color*clarity, dataGood)) ##Good is significant for color, clarity, and its interaction effect for depth
anova(aov(depth ~ color*clarity, dataVG)) ##Very good is significant for color, clarity, and its interactoin effect for depth
anova(aov(depth ~ color*clarity, dataPre)) ##Premium is significant for color and very good is significant for clarity, but not the interaction effect of clarity and color.
anova(aov(depth ~ color*clarity, dataIdeal)) ##Ideal is significant for color, clarity, and its interaciton for depth

##Separate depth  by cut type, but only when keeping one of the other 2 in test
anova(aov(depth ~ color, dataFair)) ##Color is significant within Fair group
anova(aov(depth ~ color, dataGood)) ##Color is significant within Very good group
anova(aov(depth ~ color, dataVG)) ##Color is significant within very good group
anova(aov(depth ~ color, dataPre)) ##Color is significant within premium group
anova(aov(depth ~ color, dataIdeal)) ##color is signfiicant within Ieal group

##Separate depth  by cut type, but only when keeping one of the other 2 in test
anova(aov(depth ~ clarity, dataFair)) ##Clarity is significant within Fair group
anova(aov(depth ~ clarity, dataGood)) ##Clarity is significant within Good group
anova(aov(depth ~ clarity, dataVG)) ##Clarity is significant within Very good group
anova(aov(depth ~ clarity, dataPre)) ##Clarity is significant within Preium  group
anova(aov(depth ~ clarity, dataIdeal)) ##Clarity is significant within Ideal group

#Check normaility
qqnorm(dataFair$depth) #Looks okay
qqnorm(dataGood$depth) #Looks okay, but there is weird hitch in the middle of the model
qqnorm(dataVG$depth) #Looks okay
qqnorm(dataPre$depth) #Looks okay
qqnorm(dataIdeal$depth) #Looks okay #Doesn't look normal

##Do the same thing by color instead
dataD <- diamonds[which(color == "D"),] ##Pulls those rows
dataE <- diamonds[which(color == "E"),]
dataF <- diamonds[which(color == "F"),]
dataG <- diamonds[which(color == "G"),]
dataH <- diamonds[which(color == "H"),]
dataI <- diamonds[which(color == "I"),]
dataJ <- diamonds[which(color == "J"),]


anova(aov(depth ~ clarity*cut, dataD)) #All sig
anova(aov(depth ~ clarity*cut, dataE)) #All sig
anova(aov(depth ~ clarity*cut, dataF)) #All sig
anova(aov(depth ~ clarity*cut, dataG)) #All sig
anova(aov(depth ~ clarity*cut, dataH)) #All sig
anova(aov(depth ~ clarity*cut, dataI)) #All sig
anova(aov(depth ~ clarity*cut, dataJ)) #All sig

#Only clarity
anova(aov(depth ~ clarity, dataD)) #sig
anova(aov(depth ~ clarity, dataE)) #sig
anova(aov(depth ~ clarity, dataF)) #sig
anova(aov(depth ~ clarity, dataG)) #sig
anova(aov(depth ~ clarity, dataH)) #sig
anova(aov(depth ~ clarity, dataI)) #sig
anova(aov(depth ~ clarity, dataJ)) #sig

#Only Cut
anova(aov(depth ~ cut, dataD)) #sig
anova(aov(depth ~ cut, dataE)) #sig
anova(aov(depth ~ cut, dataF)) #sig
anova(aov(depth ~ cut, dataG)) #sig
anova(aov(depth ~ cut, dataH)) #sig
anova(aov(depth ~ cut, dataI)) #sig
anova(aov(depth ~ cut, dataJ)) #sig
#Check normality
qqnorm(dataD$depth) #okay, some outliers messing 
qqnorm(dataE$depth) #okay, some upper outliers messing
qqnorm(dataF$depth) #okay
qqnorm(dataG$depth) #okay, some lower outliers messing
qqnorm(dataH$depth) #okay
qqnorm(dataI$depth) #okay
qqnorm(dataJ$depth) #okay, a lower outlier messing




##Do the same thing by clarity instead
dataI1 <- diamonds[which(clarity == "I1"),] ##Pulls those rows
dataSI2 <- diamonds[which(clarity == "SI2"),]
dataSI1 <- diamonds[which(clarity == "SI1"),]
dataVS2 <- diamonds[which(clarity == "VS2"),]
dataVS1 <- diamonds[which(clarity == "VS1"),]
dataVVS2 <- diamonds[which(clarity == "VVS2"),]
dataVVS1 <- diamonds[which(clarity == "VVS1"),]
dataIF <- diamonds[which(clarity == "IF"),]


anova(aov(depth ~ color*cut, dataI1)) #All sig except interaction
anova(aov(depth ~ color*cut, dataSI2)) #All sig
anova(aov(depth ~ color*cut, dataSI1)) #All sig except color
anova(aov(depth ~ color*cut, dataVS2)) #All sig
anova(aov(depth ~ color*cut, dataVS1)) #All sig
anova(aov(depth ~ color*cut, dataVVS2)) #All sig
anova(aov(depth ~ color*cut, dataVVS1)) #All sig
anova(aov(depth ~ color*cut, dataIF)) #All sig

#Only color
anova(aov(depth ~ color, dataI1)) #sig
anova(aov(depth ~ color, dataSI2)) #sig
anova(aov(depth ~ color, dataSI1)) #Not sig
anova(aov(depth ~ color, dataVS2)) #sig
anova(aov(depth ~ color, dataVS1)) #sig
anova(aov(depth ~ color, dataVVS2)) #sig
anova(aov(depth ~ color, dataVVS1)) #sig
anova(aov(depth ~ color, dataIF)) #not sig, but close

#Only cut
anova(aov(depth ~ cut, dataI1)) #sig
anova(aov(depth ~ cut, dataSI2)) #sig
anova(aov(depth ~ cut, dataSI1)) #sig
anova(aov(depth ~ cut, dataVS2)) #sig
anova(aov(depth ~ cut, dataVS1)) #sig
anova(aov(depth ~ cut, dataVVS2)) #sig
anova(aov(depth ~ cut, dataVVS1)) #sig
anova(aov(depth ~ cut, dataIF)) #sig
#Check normality
qqnorm(dataI1$depth) #okay, some outliers messing 
qqnorm(dataSI2$depth) #okay
qqnorm(dataSI1$depth) #okay, a lower outlier messing
qqnorm(dataVS2$depth) #okay, some  outliers messing
qqnorm(dataVS1$depth) #okay, a lower outlier messing
qqnorm(dataVVS2$depth) #okay, some lower outliers messing
qqnorm(dataVVS1$depth) #okay
qqnorm(dataVVS1$depth) #okay

#Conclusion
#	At Fair cut,  color and clarity are significant 
#	At Good cut, color, clarity, and their interaction effect is significant
#	At Very good cut, color, clarity, and their interaction effect is significant
#	At Premium cut, color and clarity are significant
#	At Ideal cut, color, clarity, and their interaction effect is significant
#	At all colors, cut, clarity, and their interaction effect is significant
#	At all clarities except I1 and SI1, cut, color, and their interaction effect are significant
#	At clarity I1, color and cut are significant
#	At clarity SI1, cut and their interaction effect is significant

#Problem with the results:
  #ANOVA is not the best fit for this data because the data has non constant variances. 
  #That means the results are possibly incorrect


