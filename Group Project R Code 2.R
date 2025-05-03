# Loading in needed packages
library(caret)
library(moments)
library(corrplot)
library(AppliedPredictiveModeling)
library(ggplot2)
library(e1071)
library(glmnet)
library(MASS)
library(pamr)
library(pROC)
library(sparseLDA)
library(dplyr)


# Reading data into dataframe, data is seperated by semicolon so using that as delimeter
setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA5790/group_project/R_code")

studentSuccess <- read.csv('data.csv', sep = ';', header = TRUE)

# Getting a view of what the data looks like
str(studentSuccess)
studentSuccess$Target

# check distribution of response
par(mfrow = c(1,1))
hist(as.numeric(factor(studentSuccess$Target)), col = "steelblue",
     main = "Distribution of Student Success",
     xlab = "(1 = Dropout, 2 = Enrolled, 3 = Graduate)")

# Histogram shows very different frequencies for the various levels, a stratified sampling approach should be taken


#### pre-processing ###

# check for missing values
nas_bycol <- colSums(is.na(studentSuccess))
sum(is.na(studentSuccess))

# no missing values

# separate response from predictors
success_ouctome <- studentSuccess$Target # saves outcome separately
success_predictors <- studentSuccess
success_predictors$Target <- NULL # removes outcome from predictor set
str(success_predictors)
str(success_ouctome)

success_ouctome <- as.factor(success_ouctome)


## check skewness


# histograms of predictors
par(mfrow = c(3,4))
#hist(Previous.qualification, col = "lightblue")
hist(studentSuccess$Admission.grade, col = "darkblue")
hist(studentSuccess$Curricular.units.1st.sem..credited., col = "cornflowerblue")
hist(studentSuccess$Curricular.units.1st.sem..evaluations., col = "darkorange")
hist(studentSuccess$Curricular.units.1st.sem..grade., col = "darkred")
hist(studentSuccess$Curricular.units.2nd.sem..credited., col = "yellow")
# hist(Curricular.units.2nd.sem..evaluations.) # not skewed trying to lower margins
hist(studentSuccess$Curricular.units.2nd.sem..grade., col = "darkslateblue")
#hist(Unemployment.rate)
#hist(GDP)
#hist(Previous.qualification..grade.)
hist(studentSuccess$Age.at.enrollment, col = "darkmagenta")
hist(studentSuccess$Curricular.units.1st.sem..enrolled., col = "purple")

par(mfrow = c(2,2))
hist(studentSuccess$Curricular.units.1st.sem..approved., col = "darkgreen")
hist(studentSuccess$Curricular.units.1st.sem..without.evaluations., col = "darkseagreen")
hist(studentSuccess$Curricular.units.2nd.sem..enrolled., col = "darkolivegreen")
#hist(Curricular.units.2nd.sem..approved.)
hist(studentSuccess$Curricular.units.2nd.sem..without.evaluations., col = "darkslategrey")
#hist(Inflation.rate)


# Getting indices of columns that do contain continuous variables so we can find the skewness of these variables
continuous_cols <- sapply(studentSuccess, is.numeric)
skew.values <- apply(studentSuccess[ ,continuous_cols], 2, skewness)
print(skew.values)

# will need transformation

# make categorical dummy vars

dummies <- dummyVars(~ ., data = success_predictors)
success_new_pred <- predict(dummies, success_predictors)

# only numeric values
success_pred_numeric <- success_predictors %>% select_if(is.numeric) # select only numeric predictors
corrplot(cor(success_pred_numeric))


# check for multicollinearity
corr_pred <- cor(success_new_pred)
corrplot(corr_pred)

corr_pred2 <- cor(continuous_cols)

# check how many are highly correlated

high_cor <- findCorrelation(corr_pred, cutoff = 0.7)
length(high_cor)
str(high_cor)

without_high_cor <- success_new_pred[, - high_cor]
length(without_high_cor)
length(high_cor)

str(without_high_cor)

# 8 highly correlated predictors
# will need remedied - PCA for all besides PLSDA

# check for near-zero variance


degenerate_predictors <- nearZeroVar(success_new_pred)
degenerate_predictors

seg_data <- without_high_cor[, - degenerate_predictors] # new object without near-zero variance predictors
length(seg_data)

str(seg_data)
head(seg_data)
head(degenerate_predictors)

# 7 near-zero variance predictors removed


#### split data ####


training_rows <- createDataPartition(success_ouctome, p = 0.8, list = FALSE)

# training set
train_predictors <- seg_data[training_rows,]
train_response <- success_ouctome[training_rows]

# test set
test_predictors <- seg_data[-training_rows,]
test_response <- success_ouctome[-training_rows]



# set 10-fold CV
ctrl <- trainControl(method = "cv", number = 10)


### tune models ###

# logistic model
set.seed(123)
log_tune <- train(train_predictors, train_response,
                  method = "multinom",
                  preProc = c("BoxCox", "center", "scale", "pca"),
                  metric = "Kappa",
                  trControl = ctrl)

# lda model
lda_tune <- train(train_predictors, train_response,
                  method = "lda2",
                  preProc = c("BoxCox", "center", "scale", "pca"),
                  metric = "Kappa",
                  trControl = ctrl)


# plsda model
plsda_tune <- train(train_predictors, train_response,
                  method = "pls",
                  preProc = c("center", "scale"),
                  metric = "Kappa",
                  trControl = ctrl)

# penalized model
glmnGrid <- expand.grid(.alpha = c(.1, .2, .5, 1),
                        .lambda = seq(.01, .2, length = 20))

penalized_tune <- train(train_predictors, train_response,
                    method = "glmnet",
                    preProc = c("center", "scale"),
                    metric = "Kappa",
                    tuneGrid = glmnGrid,
                    trControl = ctrl)

# KNN model
knn_tune <- train(train_predictors, train_response,
                  method = "knn",
                  preProc = c("BoxCox", "center", "scale", "pca"),
                  metric = "Kappa",
                  trControl = ctrl)


# model tuning results
log_tune
lda_tune
plsda_tune
penalized_tune
knn_tune


# fit models to test data
log_model_test <- predict(log_tune, newdata = test_predictors)
lda_model_test <- predict(lda_tune, newdata = test_predictors)
plsda_model_test <- predict(plsda_tune, newdata = test_predictors)
glm_model_test <- predict(penalized_tune, newdata = test_predictors)
knn_model_test <- predict(knn_tune, newdata = test_predictors)

## plot tuning parameters ##

# log plot
ggplot(data = log_tune$results, aes(x = decay, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs Decay in Logistic Regression") +
  theme(plot.title = element_text(hjust = 0.5))

# LDA plot
ggplot(data = lda_tune$results, aes(x = dimen, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs Dimen in LDA") +
  theme(plot.title = element_text(hjust = 0.5))

# PLSDA plot
ggplot(data = plsda_tune$results, aes(x = ncomp, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs Number of Retained Components in PLSDA",
       x = "Number of Components") +
  theme(plot.title = element_text(hjust = 0.5))

# penalized glm plot
ggplot(data = penalized_tune$results, aes(x = alpha, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs Alpha in Penalized GLM") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = penalized_tune$results, aes(x = lambda, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs Lambda in Penalized GLM") +
  theme(plot.title = element_text(hjust = 0.5))

# knn plot
ggplot(data = knn_tune$results, aes(x = k, y = Kappa)) +
  geom_line(colour = "darkblue") +
  geom_point(colour = "darkblue") +
  labs(title = "Kappa vs k in kNN") +
  theme(plot.title = element_text(hjust = 0.5))


# confusion matrix for test set
log_confusion <- confusionMatrix(log_model_test, test_response)
lda_confusion <- confusionMatrix(lda_model_test, test_response)
plsda_confusion <- confusionMatrix(plsda_model_test, test_response)
glm_confusion <- confusionMatrix(glm_model_test, test_response)
knn_confusion <- confusionMatrix(knn_model_test, test_response)

log_confusion
lda_confusion
plsda_confusion
glm_confusion
knn_confusion
