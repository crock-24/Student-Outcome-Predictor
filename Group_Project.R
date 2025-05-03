# Loading in needed packages
library(caret)
library(moments)
library(corrplot)

# Set the working directory to where the data is located
setwd("/Users/crorick/Documents/MS\ Applied\ Stats\ Fall\ 2023/MA5790/group_project/R_code")

# Reading data into dataframe, data is seperated by semicolon so using that as delimeter
studentSuccess <- read.csv('data.csv', sep = ';', header = TRUE)

# Getting a view of what the data looks like
str(studentSuccess)

# Turning integer variables into factor variables because most are categorical
integer_cols <- sapply(studentSuccess, is.integer)
studentSuccess[integer_cols] <- lapply(studentSuccess[integer_cols], as.factor)

# Turning some of the integer variables back into numeric that are not categorical
studentSuccess$Admission.grade <- as.numeric(as.character(studentSuccess$Admission.grade))
studentSuccess$Age.at.enrollment <- as.numeric(as.character(studentSuccess$Age.at.enrollment))
studentSuccess$Previous.qualification <- as.numeric(as.character(studentSuccess$Previous.qualification))
studentSuccess$Curricular.units.1st.sem..credited. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..credited.))
studentSuccess$Curricular.units.1st.sem..enrolled. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..enrolled.))
studentSuccess$Curricular.units.1st.sem..evaluations. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..evaluations.))
studentSuccess$Curricular.units.1st.sem..approved. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..approved.))        
studentSuccess$Curricular.units.1st.sem..grade. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..grade.))
studentSuccess$Curricular.units.1st.sem..without.evaluations. <- as.numeric(as.character(studentSuccess$Curricular.units.1st.sem..without.evaluations.))
studentSuccess$Curricular.units.2nd.sem..credited. <- as.numeric(as.character(studentSuccess$Curricular.units.2nd.sem..credited.))
studentSuccess$Curricular.units.2nd.sem..enrolled. <- as.numeric(as.character(studentSuccess$Curricular.units.2nd.sem..enrolled.))
studentSuccess$Curricular.units.2nd.sem..evaluations. <- as.numeric(as.character(studentSuccess$Curricular.units.2nd.sem..evaluations.))
studentSuccess$Curricular.units.2nd.sem..approved. <- as.numeric(as.character(studentSuccess$Curricular.units.2nd.sem..approved.))
studentSuccess$Curricular.units.2nd.sem..without.evaluations. <- as.numeric(as.character(studentSuccess$Curricular.units.2nd.sem..without.evaluations.))

# Getting indices of columns that do contain continuous variables so we can find the skewness of these variables
continuous_cols <- sapply(studentSuccess, is.numeric)
skew.values <- apply(studentSuccess[ ,continuous_cols], 2, skewness)
print(skew.values)

# Applying boxcox transformation on Previous.qualification skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Previous.qualification)
studentSuccess$Previous.qualification <- predict(BC.transformation, studentSuccess$Previous.qualification)

# Applying boxcox transformation on Age.at.enrollment skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Age.at.enrollment)
studentSuccess$Age.at.enrollment  <- predict(BC.transformation, studentSuccess$Age.at.enrollment)

# Applying boxcox transformation on Curricular.units.1st.sem..credited. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.1st.sem..credited.)
studentSuccess$Curricular.units.1st.sem..credited.  <- predict(BC.transformation, studentSuccess$Curricular.units.1st.sem..credited.)

# Applying boxcox transformation on Curricular.units.1st.sem..enrolled. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.1st.sem..enrolled.)
studentSuccess$Curricular.units.1st.sem..enrolled.  <- predict(BC.transformation, studentSuccess$Curricular.units.1st.sem..enrolled.)

# Applying boxcox transformation on Curricular.units.1st.sem..grade. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.1st.sem..grade.)
studentSuccess$Curricular.units.1st.sem..grade.  <- predict(BC.transformation, studentSuccess$Curricular.units.1st.sem..grade.)

# Applying boxcox transformation on Curricular.units.1st.sem..without.evaluations. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.1st.sem..without.evaluations.)
studentSuccess$Curricular.units.1st.sem..without.evaluations.  <- predict(BC.transformation, studentSuccess$Curricular.units.1st.sem..without.evaluations.)

# Applying boxcox transformation on Curricular.units.2nd.sem..credited. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.2nd.sem..credited.)
studentSuccess$Curricular.units.2nd.sem..credited.  <- predict(BC.transformation, studentSuccess$Curricular.units.2nd.sem..credited.)

# Applying boxcox transformation on Curricular.units.2nd.sem..grade. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.2nd.sem..grade.)
studentSuccess$Curricular.units.2nd.sem..grade.  <- predict(BC.transformation, studentSuccess$Curricular.units.2nd.sem..grade.)

# Applying boxcox transformation on Curricular.units.2nd.sem..without.evaluations. skewed predictor (skew > 1)
BC.transformation <- BoxCoxTrans(studentSuccess$Curricular.units.2nd.sem..without.evaluations.)
studentSuccess$Curricular.units.2nd.sem..without.evaluations.  <- predict(BC.transformation, studentSuccess$Curricular.units.2nd.sem..without.evaluations.)

# Centering and scaling all continuous predictors
studentSuccess[ ,continuous_cols] <- sapply(studentSuccess[ ,continuous_cols], scale)

# Plotting correlation plot to see if any continuous predictor variables are highly correlated with each other
correlations <- cor(studentSuccess[ ,continuous_cols])
corrplot(correlations, order = "hclust")

# Looking for highly correlated continuous predictors and then removing them from data
highlyCorrelated <- findCorrelation(correlations, cutoff = 0.9)
studentSuccess <- studentSuccess[ , -highlyCorrelated]

# Turning the marital status categorical variable into dummy variables, appending it to data, then removing marital status
dummy_vars <- model.matrix(~ Marital.status - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Marital.status)

# Turning the Application.mode categorical variable into dummy variables, appending it to data, then removing Application.mode
dummy_vars <- model.matrix(~ Application.mode - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Application.mode)

# Turning the Application.order categorical variable into dummy variables, appending it to data, then removing Application.order
dummy_vars <- model.matrix(~ Application.order - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Application.order)

# Turning the Course categorical variable into dummy variables, appending it to data, then removing Course
dummy_vars <- model.matrix(~ Course - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Course)

# Turning the Mother.s.qualification categorical variable into dummy variables, appending it to data, then removing Mother.s.qualification
dummy_vars <- model.matrix(~ Mother.s.qualification - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Mother.s.qualification)

# Turning the Father.s.qualification categorical variable into dummy variables, appending it to data, then removing Father.s.qualification
dummy_vars <- model.matrix(~ Father.s.qualification - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Father.s.qualification)

# Turning the Mother.s.occupation categorical variable into dummy variables, appending it to data, then removing Mother.s.occupation
dummy_vars <- model.matrix(~ Mother.s.occupation - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Mother.s.occupation)

# Turning the Father.s.occupation categorical variable into dummy variables, appending it to data, then removing Father.s.occupation
dummy_vars <- model.matrix(~ Father.s.occupation - 1, data = studentSuccess)
studentSuccess <- cbind(dummy_vars, studentSuccess)
studentSuccess <- subset(studentSuccess, select = -Father.s.occupation)

# Looking for near zero variance predictors and removing them from the data
zero.var.indices <- nearZeroVar(studentSuccess)
studentSuccess <- studentSuccess[,-zero.var.indices]

# Converting response to a numeric variable to visualize frequency of each response level
Target <- as.numeric(factor(studentSuccess$Target))

# Histogram shows very different frequencies for the various levels, a stratified sampling approach should be taken
hist(Target)

# Use stratified sampling to create a training and testing set. 80% of data will be spent on training model
Response <- as.factor(studentSuccess$Target)
Pred <- studentSuccess[,-length(studentSuccess)]
trainingRows <- createDataPartition(studentSuccess$Target, p = 0.8, list = FALSE)
trainPred <- Pred[trainingRows, ]
trainResp <- Response[trainingRows]
testPred <- Pred[-trainingRows, ]
testResp <- Response[-trainingRows]

# Use K fold cross validation for training. 75% training data and 25% testing data. 15 different resamples
ctrl <- trainControl(method = "LGOCV", number = 15, classProbs = TRUE, savePredictions = TRUE)

### K Nearest Neighbors (KNN) model
set.seed(100)
knnFit <- train(x = trainPred, y = trainResp, method = "knn",metric = "Kappa", preProc = c("center", "scale"), tuneGrid = data.frame(.k = 1:30),trControl = ctrl)
knnFit
plot(knnFit)
confusionMatrix(data = predict(knnFit, testPred),reference = testResp)

### Naive Bayes Model
library(klaR)
set.seed(100)
nbFit <- train(x = trainPred, y = trainResp, method = "nb", metric = "Kappa", preProc = c("center", "scale"),tuneGrid = data.frame(.fL = 2,.usekernel = TRUE,.adjust = TRUE),trControl = ctrl)
nbFit
confusionMatrix(data = predict(nbFit, testPred),reference = testResp) 

### Mixed Discriminant Analysis
library(mda)
set.seed(100)
mdaFit <- train(x = trainPred, y = trainResp,method = "mda",metric = "Kappa", tuneGrid = expand.grid(.subclasses = 1:4),trControl = ctrl)
mdaFit
plot(mdaFit)
confusionMatrix(data = predict(mdaFit, testPred), reference = testResp)

### Penalized Model
glmnGrid <- expand.grid(.alpha = c(0, .1, .2, .4, .6),.lambda = seq(.01, .2, length = 5))
set.seed(100)
glmnTuned <- train(trainPred, y = trainResp, method = "glmnet", tuneGrid = glmnGrid, preProc = c("center", "scale"), metric = "Kappa", trControl = ctrl)
glmnTuned
plot(glmnTuned)
confusionMatrix(data = predict(glmnTuned, testPred), reference = testResp)

# Finding which variables are the most important to the model
varImp(mdaFit)


