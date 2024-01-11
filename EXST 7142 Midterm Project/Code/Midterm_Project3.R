library(tidyr)
library(dplyr)
library(boot)
library(ROSE)
library(randomForest)
library(caret)
library(e1071)
library(pROC)
library(MLmetrics)
library(ROCR)
library(ROSE)
library(ggplot2)
rm(list=ls())

healthcare <- read.csv("../Data/Raw/healthcare-dataset-stroke-data.csv")

#===== 
# Making gender a factor and dropping one of the levels

table(healthcare$gender) # There are three levels, but there is only one subject in the "other" category. I will merge it with male category. 

healthcare$gender <- as.factor(healthcare$gender)

str(healthcare)

healthcare$gender[(healthcare$gender=="Male")|healthcare$gender=="Other"] <- "Male"

healthcare$gender <- droplevels(healthcare$gender)

# Removing all missing values in the dataset. We lose 201 variables, but we still have a large amount of observations. I dropped ID since it's not very useful. 
new_healthcare <- healthcare %>%
    drop_na()  %>% 
    select(!(id)) %>%
    mutate(Residence_type = as.factor(Residence_type)) %>%
    mutate(ever_married = as.factor(ever_married))%>%
    mutate(work_type = as.factor(work_type)) %>%
    mutate(smoking_status = as.factor(smoking_status))%>%
    mutate(heart_disease = as.factor(heart_disease))%>%
    mutate(hypertension = as.factor(hypertension))%>%
    mutate(stroke = as.factor(stroke))

# Looking at the new dataset

str(new_healthcare)


table(new_healthcare$stroke) # there is imbalance 

barplot(prop.table(table(new_healthcare$stroke)),
        col = rainbow(2),
        ylim = c(0, 0.9),
        main = "Class Distribution")

# We should be able to do the logistic regression using glm().

set.seed(1) #Makes the results reproducible.
indx <- sample(1:4909,size=4909*.7,replace=F)
x1   <- new_healthcare[indx,1:10] #Training X
x2   <- new_healthcare[-indx,1:10] #Test X
y1   <- new_healthcare[indx,11] #Training Y
y2   <- new_healthcare[-indx,11] #Test Y
NH_1 <- as.data.frame(x1); NH_1$Y <- y1
NH_2 <- as.data.frame(x2); NH_2$Y <- y2


stroke_glm <- glm(Y ~ . ,family = binomial(link =logit), data = NH_1)

summary(stroke_glm)

#####
### Confusion Matrix and AUC 
stroke.pred <- predict(stroke_glm,  NH_2, type = "response")
stroke_class <- ifelse(stroke.pred > 0.5, 1,0)
stroke_class[stroke_class == "1"]
y2[y2 == "1"]
confusionMatrix(factor(stroke_class),NH_2$Y, mode = "everything", positive ="1")

AUC_lg <- auc(NH_2$Y,stroke.pred)

## ROC Curve of Logistic Regression
roc_stroke <- roc(NH_2$Y,stroke.pred)
ggroc(roc_stroke)
ggroc(roc_stroke, colour = 'steelblue', size = 2)+
    ggtitle(paste0('ROC Curve','(AUC =',AUC_lg,')'))+
    theme_minimal()
#####
## Random Forest 
set.seed(1)

index <- sample(1:4909, size = 4909, replace = F)

NH.train <- new_healthcare[index[1:3436],]
NH.test <- new_healthcare[index[3437:4909],]

table(NH.test$stroke)

## Set seed for Random forest 
set.seed(1)

fit1 <- randomForest(stroke~.,data = NH.train, xtest = NH.test[,-11],
                     ytest=NH.test$stroke,ntree=200, keep.forest = TRUE)
fit1


#####
### Confusion Matrix and AUC 
fit.pred <- predict(fit1,NH.test,type = "prob")
AUC_rf <- auc(NH.test$stroke,fit.pred[,2])


confusionMatrix(predict(fit1, NH.test), NH.test$stroke, mode = "everything", positive = '1') # F1 score is 0.0286 but sometimes it will give NAN 

## ROC curve 
roc_stroke1 <- roc(NH.test$stroke,fit.pred[,2])
ggroc(roc_stroke, colour = 'steelblue', size = 2)+
    ggtitle(paste0('ROC Curve','(AUC =',AUC_rf,')'))+
    theme_minimal()

library(MASS)
sub_log <- stepAIC(stroke_glm, direction = "both")
stroke.pred
sub_stroke_pred <- predict(sub_log, newdata = NH.test)
stroke_class2 <- ifelse(sub_stroke.pred > 0.5, 1,0)
length(stroke_class2[stroke_class2 == "1"])
library(car)
vif(stroke_glm)
vif(sub_log)
library(AUC)
auc(roc(stroke_class2,y2))
boxplot(new_healthcare, names = names(new_healthcare))
