# Predicting Student Outcomes

---

## Project Overview

This project aims to identify key characteristics that predict whether a university student will graduate, drop out, or remain enrolled by the end of their normal course duration. Early identification of at-risk students allows timely intervention to improve academic success and reduce dropout rates.

Data was collected from Capacitação da Administração Pública in Portugal, containing demographic, academic, and socio-economic information on 4,424 students. Multiple classification models were tested to determine the best predictive approach.

---

## Table of Contents

- [Background](#background)  
- [Data Description](#data-description)  
- [Data Preprocessing](#data-preprocessing)  
- [Modeling Approach](#modeling-approach)  
- [Results](#results)  
- [Conclusion](#conclusion)  
- [Appendix](#appendix)  
- [R Code](#r-code)

---

## Background

Student dropout is a critical issue, affecting both individuals and educational institutions. Understanding factors associated with dropout risk supports targeted academic assistance. This study leverages data from a Portuguese program aimed at reducing dropouts by early identification through machine learning.

---

## Data Description

- **Sample Size:** 4,424 students  
- **Response Variable:** Student status at course end (`dropout`, `enrolled`, `graduate`)  
- **Predictors:** 35 variables including demographic data (e.g., marital status, nationality), academic records (e.g., admission grades, curricular units), socio-economic indicators (e.g., parental occupation, unemployment rate), and more.

---

## Data Preprocessing

- Categorical variables with multiple levels were converted into dummy variables.  
- Near-zero variance predictors (7 variables) were removed to avoid noise.  
- No missing values were present; no imputation required.  
- Skewed continuous variables (13 out of 19) were transformed using Box-Cox where appropriate.  
- Predictors were centered and scaled to ensure comparable weighting across variables.  
- High correlations among some predictors were handled via PCA for applicable models.

---

## Modeling Approach

- The data was split into training (80%) and testing (20%) sets using stratified sampling to maintain class proportions.  
- Ten different classification algorithms were trained and tuned with 10-fold cross-validation, optimizing for Kappa statistic due to class imbalance.  
- Models tested include: Logistic Regression, Linear Discriminant Analysis (LDA), Partial Least Squares Discriminant Analysis (PLSDA), Penalized GLM, k-Nearest Neighbors (kNN), Nonlinear Discriminant Analysis (NDA), Neural Networks, Flexible Discriminant Analysis (FDA), Support Vector Machine (SVM), and Naive Bayes.

---

## Results

- The Penalized Generalized Linear Model (GLM) showed the best performance on both training and test sets with a Kappa of ~0.43 and accuracy ~70%.  
- Neural Network was the second-best model with a slightly lower Kappa and accuracy.  
- Confusion matrices revealed good sensitivity for dropout and graduate classes, but low sensitivity for the enrolled class, indicating challenges distinguishing between enrolled and graduated students.

---

## Conclusion

- The Penalized GLM is recommended for predicting student outcomes and identifying at-risk students for dropout.  
- Expanding the dataset to include diverse geographical regions could improve generalizability.  
- Further feature engineering or alternative modeling techniques may enhance predictive accuracy.

---

## Appendix

- Detailed figures, parameter tuning plots, and additional model diagnostics are available in the appendix section of the full report.

---

## R Code

The R code used for preprocessing, modeling, and evaluation is provided in the accompanying files. It includes data import, variable transformations, model tuning, and performance assessments.
