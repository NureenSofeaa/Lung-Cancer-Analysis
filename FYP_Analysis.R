data1 <- read.csv(file.choose(), header=T)
dim(data1)

# Independent variables
# to convert 1:NO 2:YES to 0:NO 1:YES
library(dplyr)
data <- data1 %>% mutate(across(3:15, ~ ifelse(. == 1, 0, 1)))
# Dependent variable
# to convert NO = 0 and YES = 1
data$LUNG_CANCER <- ifelse(data$LUNG_CANCER == "NO", 0, 1)

## Preprocessing Data

# check if there is unused variable
str(data)

# check if there is missing data
summary(data)

# check if there is duplicate data 
duplicate <- sum(duplicated(data))
cat("Total Duplicate Rows :" , duplicate, "\n")
data2 <- data[!duplicated(data),]
duplicate2 <- sum(duplicated(data2))
cat("Total Duplicate Rows :" , duplicate2, "\n")

## Objective 1 : Menentukan faktor faktor risiko yang berpotensi mempengaruhi
#                 kanser paru-paru

## Backward Stepwise Regression
data2 <- data2 %>% mutate(across(-AGE, as.factor))
dataL <- glm(LUNG_CANCER~. , data=data2, family=binomial)
summary(dataL)
dataL2 <- step(dataL, direction='backward')
summary(dataL2)

# Multicollinearity
library(car)
vif(dataL2)
## VIF < 5, no multicollinearity

## Objektif 2 : Melihat ketepatan Analisis Regresi Logistik dalam meramalkan 
#                kehadiran kanser paru-paru

# Logistic Regression
dataLR <- sample(1:nrow(data2), 0.7*nrow(data2))
train <- data2[dataLR,]
test <- data2[-dataLR,]

logit <- glm(LUNG_CANCER ~., family=binomial, data=train)
summary(logit)
logit2 <- glm(LUNG_CANCER~.-AGE-CHEST.PAIN-GENDER-ANXIETY-WHEEZING-SHORTNESS.OF.BREATH,family=binomial, data=train)
summary(logit2)

# Model Performance Evaluation
logit$aic
logit2$aic

# Confusion Matrix 
pred <- predict(logit2,newdata=test, type="response")
pred2 <- ifelse(pred>0.5,"Yes","No")
ConM <- table(Predicted=pred2, Actual=test$LUNG_CANCER)
print(ConM)
efficiency <- sum(diag(ConM))/sum(ConM)
efficiency

TP <- ConM["Yes", "1"]
TN <- ConM["No", "0"]
FP <- ConM["Yes", "0"]
FN <- ConM["No", "1"]

accuracy <- (TP+TN)/sum(ConM)
print(accuracy)
sensitivity <- TP/(TP+FN)
print(sensitivity)
precision <- TP/(TP+FP)
print(precision)

# Odds Ratio
exp(cbind(OR=coef(logit2),confint(logit2)))

## Model Adequacy Analysis 

# ROC Curve and AUC: Check the model’s discriminative power.
# Shows the trade-off between correctly identifying positives (true positives) and 
#   incorrectly labeling negatives as positives (false positives) at various thresholds.

library(pROC)
roc_curve <- roc(test$LUNG_CANCER, pred)
plot(roc_curve, main = "ROC Curve with AUC")
auc <- auc(roc_curve)
text(0.09, 0.8, labels = paste("AUC =", round(auc, 3)), col = "red", cex = 0.9)
# Higher AUC (closer to 1) indicates better model performance.

## Hosmer-Lemeshow Goodness of Fit Test: Assess how well the model fits the data.

library(ResourceSelection)
test$LUNG_CANCER <- as.numeric(as.character(test$LUNG_CANCER))
HL <- hoslem.test(test$LUNG_CANCER, pred)
print(HL)
# A high p-value suggests that the model’s predictions are not significantly different from the observed values.
# The model fits the data reasonably well in terms of predicting the dependent variable
# No evidence of poor fit.
if (HL$"p.value" < 0.05) 
{
  cat("Reject the null hypothesis. The model is not a good fit.\n")
} else {
  cat("Fail to reject the null hypothesis. The model is a good fit.\n")
}

# Determine if its a good model (Likelihood Ratio Test)
library(lmtest)
null <- glm(LUNG_CANCER~1,data=test, family=binomial)
full <- glm(LUNG_CANCER~.-AGE-CHEST.PAIN-GENDER-ANXIETY-WHEEZING-SHORTNESS.OF.BREATH, data=test, family=binomial)
LRT < lrtest(null,full)
print(LRT)
# P-value < 0.05 indicate that it is a good model
if (LRT$"Pr(>Chisq)"[2] < 0.05) 
{
  cat("Reject the null hypothesis. The full model is significantly better than 
        the null model.\n")
} else {
  cat("Fail to reject the null hypothesis. The null model is sufficient.\n")
}

