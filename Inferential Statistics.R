#####################################
## Inferential Statistics Example ##
####################################
# Alexander Helbach 
# 12/15/2021
# PURPOSE: 
# This script demonstrates how to conduct a T-Test, ANOVA, Correlation, Chi-Square,
# and regression in research context. 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Loading packages 
pacman::p_load(tidyverse, ggpubr, rstatix, datarium)
#-------------------------------------------------------------------------------
## T-Test: Is tooth length equal across supplement gruops (OJ vs VC)
#-------------------------------------------------------------------------------
# Hypotheses #
# H1: Toothgrowth variance between OJ and VC supplments are equal. 
# H2: Toothgrowth variance between OJ and VC supplments are not equal. 
#-------------------------------------------------------------------------------
# Loading in data 
df <- datasets::ToothGrowth
# Display data 
head(df)
#-------------------------------------------------------------------------------
# Checking Assumptions of a T-Test 
#------------------------------------------------------------------------------_
# Summary Statistics
df %>%
  select(len, supp) %>% 
  group_by(supp) %>% 
  get_summary_stats(type = c("mean_sd"))
# Boxplot of supplement mean tooth length
ggboxplot(df, "supp", "len")
#-------------------------------------------------------------------------------
# Assumptions of a T-Test
#-------------------------------------------------------------------------------
# Outliers 
df %>% identify_outliers(len) # no extreme outliers found 
# Normality Assumptions 
df %>%  shapiro_test(len) # p = .109 Assumption met
# Visual Normality Check 
ggqqplot(df, x = "len") # Also looks good
#-------------------------------------------------------------------------------
# Conducting T-Test
#-------------------------------------------------------------------------------
# T-Test 
df %>% t_test(len~supp, detailed = T) # p = .0606 Results not signifcant 
# Cohens D: Effect Size 
df %>% cohens_d(len~supp) # Small magnitude of effect
####################################################################
# Results indicate tooth growth varaince is equal so we confirm H1 #
####################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Correlation: What is the correlation between MPG and Horsepower? 
# Hypotheses # 
# H1: MPG and Horsepower are not correlated 
# H2: MPG and Horsepower are correlated
#-------------------------------------------------------------------------------
df <- datasets::mtcars
# Display data 
head(df)
#-------------------------------------------------------------------------------
# Rough plot for digonostic purposes
plot(df$mpg,df$hp) # appears to demonstrate a negative correlation 
#-------------------------------------------------------------------------------
cor(df$mpg,df$hp, method = c("pearson")) # r = -.77 yup its negative and strong 
##########################################################################
# Results indicate MPG and HP are negatively correlated so we confirm H2 #
##########################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ANOVA: Is the weight each chicken signigicantly different across the four groups
#-------------------------------------------------------------------------------
# H1: Chicken weights between each diet are the same betwee each group 
# H2: Chicken weights between each diet are not the same betwee each group 
#-------------------------------------------------------------------------------
df <- datasets::ChickWeight
head(df)
#-------------------------------------------------------------------------------
# Summary Statistics
df %>%
  select(Diet, weight) %>% 
  group_by(Diet) %>% 
  get_summary_stats(type = c("mean_sd"))
# Boxplot of weight by chicken diet 
ggboxplot(df, "Diet", "weight", fill = "Diet")
#-------------------------------------------------------------------------------
# Assumptions of an ANOVA
#-------------------------------------------------------------------------------
# Outliers 
df %>% 
  select(Diet, weight) %>% 
  group_by(Diet) %>% 
  identify_outliers(weight) # all have outliers but no extreme outliers found.
# Normality Assumptions: qqplot of residuals than shaperio test 
# Qqplot
D.model <- lm(weight~Diet, data = df)
ggqqplot(residuals(D.model)) # concerning, lots of variance on the extremes
# Shapiro Test
df %>% 
  select(Diet, weight) %>% 
  group_by(Diet) %>% 
  shapiro_test(weight) # p < .05 assumption failed. onto non parametric testing
#-------------------------------------------------------------------------------
# Nonparametric ANOVA
kruskal.test(weight~Diet, data = df) # Results are significant. Time for post hoc 
df %>% tukey_hsd(weight~Diet) # noice we have some pairs who are significant
#-------------------------------------------------------------------------------
##########################################################################
# Results indicate signifacnt difference between diets so we confirm H2. 
# Post analysis deomstrates Diet 1 compared to Diet 3 are signifcantly different 
# and Diet 1 compared to Diet 4 are significantly different.
#########################################################################
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Chi-Square: Is there a difference in surval rate between ticket class?
#-------------------------------------------------------------------------------
df <- read.csv("C:/Users/HelbachAlexander/OneDrive - University of Wisconsin-Stout/Desktop/CleanedTitanic.csv")
head(df)
#-------------------------------------------------------------------------------
# Percent table 
PropTable <- (table(df$pclass, df$survived)/length(df$survived)*100)
PropTable <- round(PropTable, digits = 2)
PropTable
#-------------------------------------------------------------------------------
df_chi <- chisq.test(df$pclass, df$survived)
df_chi # extremely significant 
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
# Linear Regression: how much of the variance in sales can we explain with Facebook
#--------------------------------------------------------------------------------
# Load data 
data("marketing", package = "datarium")
df <- marketing
# View data 
head(df)
# Quick and dirty assumption testing 
par(mfrow = c(2, 2))
plot(model) # look alright to me lets send it :) 
# Model build 
model <- lm(sales~facebook, data = df)
summary(model) # 33% of variance can be explained by facebook for sales
# cool that was fun 
#-------------------------------------------------------------------------------

