library(tidyr)
library(dplyr)
library(boot)
rm(list=ls())

# Reading the dataset 
healthcare <- read.csv("../Data/Raw/healthcare-dataset-stroke-data.csv",sep=",")

str(healthcare) # Missing values in BMI
#====
#===== Data Description
# Stroke is a binary response variable (1 = the patient had a stroke and 0 = the patient did not have a stroke). I may use binary logistic regression using the glm function. 
# ID is a unique identifier. I may remove this variable because it may not be useful.
# BMI is a numeric variable. it is the body mass index of each patient.
# Avg_glucose_level: is the average glucose level in blood of each patient, and it is a numeric variable. 
# Hypertension and heart disease are binary explanatory variables. 0 means the patient does not have __. 1 means the patient does have __.
# Age is the age of the patient
# Ever_married and Residence type are a character variable with two levels
# Gender is the gender of the patient, and it consists of three levels. However, I will drop one of the levels
# Smoking status is a character variable that has four levels. These four levels describes the smoking status of each patient.
# Work_type has 5 levels, and it describes what kind of work the patient does in life. 
#====


# Converting character variables into binary integer variables

table(healthcare$Residence_type) # There are only two residence type.
# Since there are only two choices, I will convert Residence_type from character to integer (binary)
table(healthcare$ever_married) 

#===== 
# Making gender a factor and dropping one of the levels 

table(healthcare$gender) # There are three levels, but there is only one subject in the "other" category. I will merge it with male category. 

healthcare$gender <- as.factor(healthcare$gender)

str(healthcare)

healthcare$gender[(healthcare$gender=="Male")|healthcare$gender=="Other"] <- "Male"

healthcare$gender <- droplevels(healthcare$gender)

#=====

summary(healthcare, use ="complete.obs") # Basic summary statistics 

#==== 
#===== Checking to see if there are missing values in the dataset.
apply(healthcare,2,function(x) sum(is.na(x))) # There are 201 missing values in BMI

cor(healthcare[sapply(healthcare,is.numeric)],use = "pairwise.complete.obs") # Since BMI is not highly correlated with the other numeric variables, it may provide additional information. I will still include the variable in my predictive models.
#====

# Removing all missing values in the dataset. We lose 201 variables, but we still have a large amount of observations. I dropped ID since it's not very useful. 
new_healthcare <- healthcare %>%
                  drop_na()  %>% 
                  select(!(id)) %>%
                  mutate(Residence_type = recode(Residence_type,"Urban"= "1","Rural" ="0"))%>% 
                  mutate(Residence_type = as.factor(Residence_type)) %>%
                  mutate(ever_married = recode(ever_married, "Yes"="1","No"="0"))%>%
                  mutate(ever_married = as.factor(ever_married))%>%
                  mutate(work_type = as.factor(work_type)) %>%
                  mutate(smoking_status = as.factor(smoking_status))%>%
                  mutate(heart_disease = as.factor(heart_disease))%>%
                  mutate(hypertension = as.factor(hypertension))%>%
                  mutate(stroke = as.factor(stroke))
                  

# Looking at the new dataset

str(new_healthcare)

cor(new_healthcare[sapply(new_healthcare,is.numeric)],use = "pairwise.complete.obs") # correlation matrix of new dataset

# We should be able to do the logistic regression using glm(). 

stroke_glm <- glm(stroke ~ . ,family = binomial(link =logit), data = new_healthcare)

summary(stroke_glm)
