#diabetes <- NVJ7Cw98Eem6Pgo4_YwqLg_35bb00c00f7c11e9903947c521ebe81a_final_diabetes_data_for_R_csv_2_
#save(diabetes, file="diabetes.RData")

rm(list=ls())
setwd("~/Documents/Public health")
library(haven)
library(gtsummary)
library(dplyr)

# Load Diabetes data file
load("~/Documents/Public health/diabetes.RData")

# checking the number of rows and columns
dim(diabetes)

# checking names of the rows
dimnames(diabetes)[[2]]

#insurance: 0=none, 1=government, 2=private
#fh = family history of diabetes (yes/no, where 1=yes, 0=no)
#smoking: 1=current, 2=never and 3=ex


table(diabetes$insurance)  # Insurance frequency
table(diabetes$fh)         # Family History frequency
table(diabetes$smoking)   # smokig frequency

# Which columns have missing values.
colSums(is.na(diabetes))

t <- table(diabetes$gender) # store the tabulation for further manipulation
addmargins(t) # this will sum up the gender totals to give an overall total and print the results
round(prop.table(t),digits=3) # get proportions rounded to 3dp

# Alternative
tbl_summary(data.frame(gender = diabetes$gender))
diabetes %>% 
  select(gender) %>% 
  tbl_summary()

# Calculating BMI
#.........................
# As this is a US data set, height is in inches and weight is in pounds. So we need to convert the the weight 
#from pounds (lbs) to kilograms (kg) and the height from inches to meters, which are the standard units used in the International System of Units (SI)
# # 1 inch = 0.0254 meters and 1 pound (lb) = 0.453592 kilograms (kg) and BMI= Weight(kg) / Height(m)^2

diabetes <- diabetes %>% 
  mutate(height.si = height*0.0254) %>% 
  mutate(weight.si = weight*0.453592) %>% 
  mutate(bmi = weight.si/height.si^2)

summary(diabetes$bmi)

# making a categorical variable (bmi_categorised) from a continuous one (bmi)
diabetes <- diabetes %>% 
  mutate(bmi_categorised = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi <= 25 ~ "normal",
    bmi > 25 & bmi <= 30 ~ "overweight",
    bmi > 30 ~ "obese",
    TRUE ~ NA_character_
  ))

# check that the bmi_categorised variable has worked  
table(diabetes$bmi_categorised, exclude = NULL) 
# Note: “table” excludes missing values by default. To see these – and we ALWAYS want to see these – use an “exclude=NULL” option 
                                                                                                 #when making the variable
## frequencies of diabetes by BMI category 
#.............................................
# dm = diabetes status
diabetes %>%
  select(bmi_categorised, dm) %>%
  mutate(dm = factor(ifelse(is.na(dm), "Missing", as.character(dm)))) %>%  # Convert NA to "Missing" and ensure dm is a factor
  mutate(bmi_categorised = factor(ifelse(is.na(bmi_categorised), "Missing", as.character(bmi_categorised)))) %>%
  tbl_summary(by = bmi_categorised, 
              missing = "always")

# Alternative (of frequencies of diabetes by BMI category)
dm_by_bmi_category <- table(diabetes$bmi_categorised, diabetes$dm, exclude = NULL) 
dm_by_bmi_category #check

# with the row percentages 
round(100 * prop.table(dm_by_bmi_category, margin = 1), digits = 1) 
# Note: margin = 1 for row percentage calculation, If we want to do column percentage, use margin = 2.


#.............................................................
#Analyzing Age and Gender Distribution in Diabetes Dataset
#.............................................................

# Step 1: Create Age Groups, allowing for missing values
diabetes <- diabetes %>%
  mutate(age_group = case_when(
    age < 45 ~ "Under 45",
    age >= 45 & age <= 64 ~ "45-64",
    age >= 65 & age <= 74 ~ "65-74",
    age >= 75 ~ "75 or over",
    TRUE ~ NA_character_  # This will handle missing values
  ))

# Step 2: Tabulate age group by itself
age_group_table <- table(diabetes$age_group)
print(age_group_table)

# Step 3: Cross-tabulate age group by gender
age_gender_table <- table(diabetes$age_group, diabetes$gender)
print(age_gender_table)

# Step 4: Add overall percentages to the cross-tabulation
age_gender_percentage <- prop.table(age_gender_table, margin = 1) * 100
age_gender_percentage_rounded <- round(age_gender_percentage, 1)
print(age_gender_percentage_rounded)

#Were there any missing values for age and/or gender?   
sum(is.na(diabetes$gender))
sum(is.na(diabetes$age))



#.........................................................................
#Simple Logistic Regression
#...................................................................

#Factoring the dm variable
diabetes$dm <- as.factor(diabetes$dm)

glm(dm ~ 1, family = binomial(link = "logit"), data = diabetes)
# Note the link = logit is not mandatory for logistic regression in R, but it is the default link function for the binomial family in the glm() function.
# “1” is just R’s way of saying that there’s only an intercept term in the model

#Factoring the gender variable
diabetes$gender <- as.factor(diabetes$gender)

model <- glm(dm ~ gender, family=binomial, data = diabetes)
summary(model)
# This means we are saying that the log odds of having diabetes differs by gender alone.

#Now we are adding age as continuous variable in the model instead of gender.
model1 <- glm(dm ~ age, family=binomial, data = diabetes)
summary(model1)

# We assume that age has linear relation with the outcome so we include this in the model. More precisely, this assumes that the relation between 
# age and the log odds of having diabetes is linear.
# Is that reasonable? The easiest way is just to plot one against the other.

# create a cross tabulation of age and diabetes status  
dm_by_age <- table(diabetes$age, diabetes$dm) 

# output the frequencies of diabetes status by age 
freq_table <- prop.table(dm_by_age, margin = 1) 

# calculate the odds of having diabetes 
odds <- freq_table[, "yes"]/freq_table[, "no"] 

# calculate the log odds 
logodds <- log(odds) 

# plot the ages found in the sample against the log odds of having diabetes 
plot(rownames(freq_table), logodds) 



# Model explanation
#........................................................
glm(dm ~ 1, family = binomial, data = diabetes)
exp(-1.705)
# the odds of having diabetes is 18%. Now if we want to explain it in probabilities, 
# just divide the odds by 1 plus the odds, to give 0.182/1.182 = 0.15, or 15%.

table(diabetes$dm)
#Using these numbers, the odds of having diabetes is 60/330 = 0.182 and the probability is 60/(330+60) = 0.15, 
#both exactly the same as from the model, which is entirely as we had expected (and hoped!).


glm(dm ~ age, family=binomial, data = diabetes)
#Log odds of having diabetes= intercept + (coefficient for age) * age in years =  -4.4045 + 0.0525 * age in years
# the log odds if you’re 25 is 0.0525 higher than if you’re 24 and that the log odds if you’re 75 is 0.0525 higher than if you’re 74
# The result is not statistically significant as p value is greater than 0.5

r <- glm(dm ~ gender, family=binomial, data = diabetes)
summary(r)
#genderfemale” is the coefficient, which represents the log odds of diabetes of females compared with males. 
exp(r$coefficients)
# This means the odds of males having diabetes are about 9.1% higher than those of females.



#...................................
# Questions
#........................................
#what percentage of people from Buckingham have diabetes?
diabetes %>% 
  filter(location == "Buckingham") %>%  
  select(dm) %>%                        
  mutate(dm = as.factor(dm)) %>%        
  pull(dm) %>%                          # Extract "dm" as a vector
  table() %>%                           # Create a frequency table
  prop.table()                          # Convert to proportions 
# 16.3% of people from Buckingham have diabetes.

# Now fit a logistic regression with “location” as the predictor variable. 
# What are the log odds of having diabetes being from Louisa compared with Buckingham?  Give the answer (the log odds ratio) to two decimal places.

m <- glm(dm ~ location, family=binomial, data = diabetes)
summary(m)
# The log-odds ratio of having diabetes for individuals from Louisa compared with Buckingham is: -0.1395 
exp(coefficients(m))



# histogram of age
hist(diabetes$age)

# density plot of age
d <- density(diabetes$age) 
plot(d,main = "") # gives warnings but the “main” argument suppresses the ugly default title. If you want to add title, put name within colon.

#BMI, HDL and cholesterol
hist(diabetes$bmi)
# Remove missing values and calculate density
d <- density(na.omit(diabetes$bmi))
# Plot the density
plot(d, main = "Density Plot of BMI", xlab = "BMI")

d <- density(na.omit(diabetes$hdl))
# Plot the density
plot(d, main = "")

d <- density(na.omit(diabetes$chol))
# Plot the density
plot(d, main = "")

# Gender distribution of the table
library(ggplot2)
ggplot(diabetes, aes(x = gender)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count")

# Fit a logistic regression model
logistic_model <- glm(dm ~ age + gender + chol + bmi + hdl, 
                      data = diabetes, 
                      family = binomial)

summary(logistic_model)




#...................................
# Multiple logistic regression
#..........................
m <- glm(dm ~ age + gender + bmi, family = binomial, data = diabetes)
summary(m)

# Exponentiate the coefficients of the model
exp(coef(m))
round(exp(coef(m)),2) # There are a lot of unnecessary decimal places. So rounding the coefficients.


# Age Variable
#.....................................................................
# The coefficient (exponent) of age variable indicates a one-year increase in age is associated with six percent higher odds of being diagnosed with diabetes.
# The p value less than 0.05 indicates that the age variable is statistically significant.


# Confidence interval (alternative of P value)
#....................................................................
# If we want to know the confidence interval for the coefficients. 
# By default, the confint() function in R calculates the 95% confidence intervals (CI) for the model parameters.
exp(confint(m))
round(exp(confint(m)), 2) #rounded the CI.


# Gender Variable
#.....................................................................
# The gender variable isn’t statistically significant. Here too the p value is large at 0.448, well above the standard 0.05 threshold. 
# Its 95% CI is wide, from 0.68 to 2.41, so this data set doesn’t tell you a whole lot about the relation between gender and diabetes risk.


# BMI variable
#.......................................
# The odds ratio for a unit increase in BMI is exp(0.073879) = 1.08, with 95% CI 1.03 to 1.13, p=0.00153 (or 0.002 to three decimal places, 
# as is usual reporting practice). That’s a pretty low p value, so you can conclude that people with higher BMIs are more at risk of diabetes.




#.....................................................
# Running A New Logistic Regression Model with predictor variables: age, cholesterol and insurance type.
#........................................................
diabetes$insurance <- as.factor(diabetes$insurance)
m2 <- glm(dm ~ age + chol + insurance, family = binomial, data = diabetes)
summary(m2)
round(exp(coef(m2)),2)




#.....................................................
# Model Fit in Logistic Regression
#........................................................
# Check the readme file for understanding the way of model fit in logistic regression.

# design logistic regression 
full_model <- glm(dm ~ age + chol + insurance, family = binomial, data = diabetes) 
summary(full_model) 

# run a null model 
null_model <- glm(dm ~ 1, family = binomial, data = diabetes) 
summary(null_model) 


# calculate McFadden's R-square 
R2 <- 1-logLik(full_model)/logLik(null_model) 
# print it 
R2 
# This R-squared of about 14% is typical of logistic regression models and is actually not too bad (but not great).


# c-statistic
#...............................................
# The easiest way to generate the c-statistic in R is to download the package “DescTools” and use the function Cstat(). 

# require a package 
#install.packages("DescTools")
library(DescTools)

# generate the c-statistic 
Cstat(full_model)
# A C-statistic (also known as the area under the ROC curve, or AUC) of 0.7644 suggests a fair to good model fit, 
#   as values closer to 1 indicate better discrimination between outcome categories


# Hosmer-Lemeshow statistic and test: 
#install.packages("ResourceSelection") 
library(ResourceSelection)
full_model$y # full_model$y  is the outcome variable we specified (dm); fitted(full_model) generates fitted values from the model. 

# run Hosmer-Lemeshow test 
HL <- hoslem.test(x = full_model$y, y = fitted(full_model), g = 10) 
HL 

# plot the observed vs expected number of cases for each of the 10 groups 
plot(HL$observed[,"y1"], HL$expected[,"yhat1"]) 

# plot the observed vs expected number of noncases for each of the 10 groups 
plot(HL$observed[,"y0"], HL$expected[,"yhat0"]) 

# plot observed vs. expected prevalence for each of the 10 groups 
plot(x = HL$observed[,"y1"]/(HL$observed[,"y1"]+HL$observed[,"y0"]), 
     y = HL$expected[,"yhat1"]/(HL$expected[,"yhat1"]+HL$expected[,"yhat0"])) 


# As you can see, there are different ways of plotting the information from a Hosmer-Lemeshow test. 
#      Another way is to plot the ten ratios of observed:predicted cases, where a well-calibrated model would show ten points very near 1. 
#install.packages("generalhoslem") 
library(generalhoslem)

# run Hosmer-Lemeshow test 
logitgof(obs = full_model$y, exp = fitted(full_model), g = 10) #The standard practice is to divide the predicted probabilities into 10 equal-sized groups (deciles)
# So according to the Hosmer-Lemeshow test, the p-value is 0.1879 from both packages, suggesting good calibration. 

# Null Deviance and Residual Deviance
#.................................
# analyse table of deviance 
anova(full_model, test = "Chisq") 
# The fourth column shows the deviances of the models compared with the saturated model. The first (334.54) is the null deviance and each subsequent number is the deviance of the model with each new variable. 
#    The final value (289.28) is the deviance of the proposed model (with all three of our variables). This is the residual deviance. As expected, adding each new variable to our model explains the data better, thus reducing the deviance.
# To test whether each added parameter increases the deviance by a significant amount, we asked R to compare it with a chi-square value for the number of degrees of freedom lost. If the p-value is low, 
#.   it indicates that the corresponding added variable causes a significant change in deviance, and thus is a better fitting model.

# In our case, adding the variables age and cholesterol significantly reduce the deviance and improve the model fit, as indicated by their low p-values, 
#.  but including the insurance variable does not improve the model fit enough to justify the loss in degrees of freedom, as indicated by its high p-value of 0.2896.



#.................................
# Backwards Elimination
#.................................
# Make the variables and run the models
# Convert variables to appropriate types
dm <- as.factor(diabetes$dm) 
insurance <- as.factor(diabetes$insurance) # let's say 0=none, 1=gov, 2=private 
fh <- as.factor(diabetes$fh) # 1=FH, 0=no FH 
smoking <- as.factor(diabetes$smoking) # 1,2,3 
chol <- diabetes$chol 
hdl <- diabetes$hdl 

# Additional variables for the dataset
ratio <- diabetes$ratio 
location <- as.factor(diabetes$location) 
age <- diabetes$age 
gender <- as.factor(diabetes$gender) 
frame <- as.factor(diabetes$frame) 
systolic <- diabetes$bp.1s 
diastolic <- diabetes$bp.1d 

# Fit the logistic regression model
model <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic, family = binomial, data = diabetes)

summary(model)
#It’s clear that neither of the BP variables is significantly associated with the odds of being diagnosed with diabetes in this data set, 
#    but the other four variables were. 

# If we drop the BP variables
model2 <- glm(dm ~ age + bmi + chol + hdl, family = binomial, data = diabetes)
summary(model2)
# Have any of the coefficients for the four remaining variables changed? Not much, which is good. 
#   But why is blood pressure not significant here despite what the literature says? 
#       One way to find out is to see if it correlates with other variables. Here's the code to do that and the output.


# Correlation test
cor.test(systolic, hdl) # strange that systolic and diastolic are not significant 
cor.test(systolic, diabetes$bmi) # significant 
cor.test(systolic, chol) # very significant
cor.test(systolic, age) # extremely significant 
# So systolic BP correlates weakly (but statistically significantly) with cholesterol and moderately (and also statistically significantly) with age. 
#.   Both of these results are entirely expected from what we know about physiology


# adding gender + location + frame + insurance + smoking 
model3 <- glm(dm ~ age + bmi + chol + hdl + systolic + diastolic + gender + location + frame + insurance + smoking, family = binomial, data = diabetes) 
summary(model3)

# Analysis of Deviance Table 
anova(model, test = "Chisq") 

# Most variables in the model were not significantly associated with diabetes, but it's important to report both significant and non-significant findings to avoid publication bias. 
# Removing non-significant variables should be done carefully to ensure it doesn’t distort the results, as p-values alone are not always the best measure of scientific relevance.

