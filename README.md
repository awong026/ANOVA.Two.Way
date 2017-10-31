# ANOVA.Two.Way

## Objective
Question 1: Perform a two way ANOVA on the data in twan03.xls. This data is the number of spruce moths found in traps classified by Factor 1: location of trap in tree - Top, Middle, lower, and ground and Factor 2: type of lure in trap - scent, sugar, chemical. You will need to rearrange the data so that each row is one observation with three variables - Factor 1, Factor 2 and Count. 

Question 2: Perform a three-way ANOVA on the data in the dataset diamonds in R package ggplot2. Model depth in terms of the three factors, cut, color and clarity

## Summary
Question 1: EDA- Checked equal variances and they were since no variance was 10x larger than another. Also looked at boxplots. Count vs location boxplots looked like there could be a difference. Count vs Trap boxplots looked like means could be the same for all types of traps. Checked normality later in the analysis and things were okay here. 

Created and performed ANOVA- Interaction effect and Trap are not significant since their pvalues are above .05. Location is significant since its pvalue is below .001

Question 2: EDA- Couldn’t assume constant variances since some of the variances had 10x larger numbers than others, which means a nonparametric method might be better than ANOVA. Made boxplots for each predictor variable to the response variable depth. Depth vs Cut boxplots looked like different means, Depth vs Colors boxplots look like equal means, and Death vs clarity boxplots looked the same except for one. Checked normality later and things were not good. There were many plots that had outliers. 

Created and performed ANOVA: All predictors were significant and their interaction effects were significant, which means I had to study the analysis with the interaction effect.

Problem with the end results:
ANOVA is not the best fit for this data because the data has non constant variances. That means the results are possibly incorrect. 

## Conclusion
Question 1: To see which pairs differ- Used Tukey and plotted it. Got that Count mean were different for Location pairs Lower-Ground, Middle-Ground, Top-Lower, and Top-Middle. 

Question 2: 
Interaction effect results for determining depth:                                                                        
o	At Fair cut,  color and clarity are significant                                          
o	At Good cut, color, clarity, and their interaction effect is significant                                        
o	At Very good cut, color, clarity, and their interaction effect is significant                             
o	At Premium cut, color and clarity are significant                                           
o	At Ideal cut, color, clarity, and their interaction effect is significant                                 
o	At all colors, cut, clarity, and their interaction effect is significant                                    
o	At all clarities except I1 and SI1, cut, color, and their interaction effect are significant                                      
o	At clarity I1, color and cut are significant                                          
o	At clarity SI1, cut and their interaction effect is significant                               
