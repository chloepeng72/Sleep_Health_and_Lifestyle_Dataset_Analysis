# Term Project
# CS 555
# Yihe Peng
# December 6 2023


library(asbio)
library(tidyverse)
library(rgl)
library(dplyr)
library(car)
library(emmeans)


data <- read_csv("MET CS 555 Fnd of Machine Learning/Term Project/Sleep_health_and_lifestyle_dataset.csv")
data <- read_csv("Sleep_health_and_lifestyle_dataset.csv")

# basic exploratory analysis
summary(data)
colnames(data)
unique(data$Occupation)
unique(data$`Sleep Disorder`)
unique(data$`BMI Category`)
# this is very weird, there are "Normal" and "Normal Weight" in this case. I decided to put them in one category.
# convert "Normal Weight" to "Normal"
data$`BMI Category` <- replace(data$`BMI Category`,data$`BMI Category`== 'Normal Weight', 'Normal')


## Graphic demonstration
# histogram of Age distribution
hist(data$Age,
     breaks = 10, 
     main = "Distribution of the Age of the participants",
     xlab = "Age",
     ylab = "Number of participants")

pie(table(data$Gender), main = "Gender of the Participants")
pie(table(data$Occupation), main = "Oppcupations of the Participants")
pie(table(data$`BMI Category`), main = "BMI Category of the Participants")
pie(table(data$`Sleep Disorder`), main = "Sleep Disorder of the Participants")


hist(data$`Quality of Sleep`,
     breaks = 10, 
     main = "Distribution of Quality of Sleep of the Participants",
     xlab = "Sleep Score",
     ylab = "Number of participants")

hist(data$`Stress Level`,
     breaks = 5, 
     main = "Distribution of Stress Level of the Participants",
     xlab = "Stress Level",
     ylab = "Number of participants")

hist(data$`Physical Activity Level`,
     breaks = 10, 
     main = "Distribution of Physical Activity of the Participants",
     xlab = "Physical Activity",
     ylab = "Number of participants")

hist(data$`Heart Rate`,
     breaks = 10, 
     main = "Distribution of Heart Rate of the Participants",
     xlab = "Heart Rate",
     ylab = "Number of participants")

hist(data$`Daily Steps`,
     breaks = 10, 
     main = "Distribution of Daily Steps of the Participants",
     xlab = "Daily Steps",
     ylab = "Number of participants")

hist(data$`Sleep Duration`,
     breaks = 10, 
     main = "Distribution of Slepp Duration of the Participants",
     xlab = "Sleep Duration",
     ylab = "Number of participants")


#######################################################
# First I am interested in exploring the relationship between Sleep Duration and Stress Level
cor(data$`Sleep Duration`,data$`Stress Level`)
cor(data$`Stress Level`,data$`Sleep Duration`)
# The correlation coefficient value is -0.811


# simple linear regression on Sleep Duration and Stress Level
# so there isn't strong evidence suggests which variable happened first
# I will be stress level as explanatory variable I am interested in how does stress affect sleep.

model <- lm(data$`Sleep Duration`~ data$`Stress Level`)
summary(model)
# Sleep_duration = 9.0903 -0.3636*Stress_level

# Scatter plot and correlation coefficient
plot(data$`Sleep Duration`, data$`Stress Level`,
     main = 'Stress Level vs Sleep Duration', xlab = 'Sleep Duration', 
     ylab = 'Stress Level', xlim=c(4,10), ylim=c(2,10), pch = 1)
abline(model)


#################################################################################
# Formally test out whether all the following variables are valid predictors for Stress Level
# Step 1: Set up the hypotheses and select the alpha level = 0.05 level.
#H_0:  β_1=β_2=β_3=β_4= 0  The multiple linear regression line has no slope.
#H_1: β_1≠β_2≠β_3≠β_4≠0   The multiple linear regression line has slope.

# Step 2: Select the appropriate test statistic
predict_model <- lm(data$`Stress Level`~ data$Age + data$`Sleep Duration`+data$`Quality of Sleep`
                    +data$`Physical Activity Level`+data$`Heart Rate`+ data$`Daily Steps`)
summary(predict_model)
# We know the f statistical value is 425.3
# Stress Level=3.315−0.00543*Age−0.06022*Sleep Duration−0.9599*Quality of Sleep−0.02002*Physical Activity +0.1145*Heart Rate+0.0004254*Daily Steps


# With 95% confident interval 
k <- 6
n <- nrow(data)
alpha_F <- qf(0.95, k, (n-k-1)) 
alpha_F

#Step 3: State the decision rule
#The Decision Rule is Reject H_0 if F statistic value >= F critical value, otherwise, do not reject H_0.

#Step 4: Compute the test statistic.
#From R, we know that the F statistic value is 425.3 and the F critical value is 2.123.

#Step 5: State your conclusion
#Reject H_0 since 425.3 is greater than 2.123. 
#We have significant evidence at the α=0.05 level that the co-efficient of the multiple linear regression line is not equal to 0. 
#Therefore, we reject the null hypothesis.  

#From the global test above, I know the overall model is significant
# but now I want to see which variable is not within the model
# Now using T test to see.
# Age_t = -1.221    
# Sleep Duration_t = -0.634
# Quality of Sleep_t =  -12.633
# Physical Activity Level_t =  -5.940
# Heart Rate_t = 8.532
# Daily Steps_t = 10.648


# Calculating the t critical value
n <- nrow(data)
# since there are 6 variables are affecting the Stress Level
k <- 6
# 95% confident interval 
1-0.05/2 # Since this is one tail test
t_val_alpha <- qt(0.975,(n-k-1))
# t_val_alpha = 1.966414
# Since the t value of Age, Sleep Duration and Quality of Sleep, Physical Activity are less than the critical t value.
# Therefore, these variables are not a significant predictor for Stress Level. 

#####################################################################################

data1 <- data.frame(data$`Heart Rate`, data$`Stress Level`, data$`Daily Steps`)
pairs(data1)
cor(data1)

######################################################################################################
# Box plot for each groups
boxplot(data$`Stress Level`~ data$`Sleep Disorder`, data=data, main="Stress Level by groups", 
        xlab="Group", ylab="Stress Level", ylim=c(0, 10))

# H_0: there isn't significant different between groups regarding to stress level
# H_1: There is a significant different between groups regarding to stress level
# Perform one-way ANOVA 
m <- aov(data$`Stress Level`~ data$`Sleep Disorder`)
summary(m)
# F statistic is 6.604

# calculate f critical value
qf(0.95, df1 = 2, df2 = 371)
# F critical value is 3.02

# Reject H_0 since 6.604 is greater than 3.02.
# We have significant evidence at the α=0.05 level that at least one mean of stress level of one group is different from another. 
# Therefore, we reject the null hypothesis.  

# Now use Tukeys Test of Honest Significance Test
TukeyHSD(m, conf.level = 0.95)


# pairwise t-test 
pairwise.t.test(data$`Stress Level`, data$`Sleep Disorder`, p.adj="none")

# pairwise t-test with bonferroni adjustment 
pairwise.t.test(data$`Stress Level`, data$`Sleep Disorder`, p.adj="bonferroni")

#P value adjustment method: bonferroni

# Interpretation::

# Without Adjustment (p.adj="none"):
# There is a significant difference in stress levels between the "None" and "Insomnia" groups (p-value = 0.0012).
# There is also a significant difference in stress levels between the "None" and "Sleep Apnea" groups (p-value = 0.0170).
# However, there is no significant difference in stress levels between the "Insomnia" and "Sleep Apnea" groups (p-value = 0.4693).

# With Bonferroni Adjustment (p.adj="bonferroni"):
# After Bonferroni adjustment, the significance level is more stringent. Only the difference between "None" and "Insomnia" remains significant (adjusted p-value = 0.0036).
# The difference between "None" and "Sleep Apnea" is no longer statistically significant after Bonferroni adjustment (adjusted p-value = 0.0511).
# The difference between "Insomnia" and "Sleep Apnea" was not significant initially and remains non-significant after adjustment (adjusted p-value = 1.0000).

# In conclusion, the level of statistical significance depends on whether or not using p-value adjustment!!! 
# Without adjustment, there seems to be significant differences between "None" and "Insomnia," as well as between "None" and "Sleep Apnea." 
# After Bonferroni adjustment, only the "None" vs. "Insomnia" comparison remains significant.


########################################################## Least square mean analysis 
## and one-way ANOVA adjusting for Heart Rate (ANCOVA)
# Fit the linear model
m2 <- lm(`Stress Level` ~ `Sleep Disorder` + `Heart Rate`, data = data)
summary(m2)

# Anova for the linear model
m3 <- Anova(m2, type = 3)
summary(m3)

# Set the options for lsmeans
options(contrasts = c("contr.treatment", "contr.poly"))

# Calculate the lsmeans 
lsmeans_model <- lsmeans(m2, pairwise ~ `Sleep Disorder`, adjust = "none")

# Display lsmeans results
print(lsmeans_model)

# Obtain emmeans
emmeans_model <- emmeans(m2, specs = ~ `Sleep Disorder`)

# Display emmeans results
print(emmeans_model)


# Interpretation:

# With the result above, the analysis ANCONA including “Heart Rate ” as a co-founding variable, 
#not only confirm the effect of “Heart Rate” has on “Stress Level”, but also, 
#indicate that after controlling for “Heart Rate” there is no significant difference between “Insomnia” and “None”. (p-value = 0.0774 greater than α=0.05)



# In summary, the inclusion of "Heart Rate" as a co-variate in the analysis has not only provided insights into 
# its own significant association with "Stress Level" but has also helped in clarifying the relationship between "Sleep Disorder" and "Stress Level" 
# by accounting for the potential confounding effect of "Heart Rate."