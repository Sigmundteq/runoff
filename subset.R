# Load required packages

library("mlr3")
library("mlr3learners")
library("mlr3viz")
library("paradox")
library("mlr3tuning")
library("mlr3verse")
library("rsample")

setwd("D:/thesis")

# import data
discharge <- read.csv("discharge.csv")
tas <- read.csv("tas.csv")
pr <- read.csv("pr.csv")

# choose data ranging from 1979-2015
discharge_subset <- subset(discharge, year >= 1979 & year <= 2015)
tas_subset <- subset(tas, year >= 1979 & year <= 2015)
pr_subset <- subset(pr, year >= 1979 & year <= 2015)

# combine temperature and pr
climate_subset <- cbind(tas_subset, pr_subset)

# Principal Component Analysis
climate_pca <- prcomp(climate_subset[, 3:ncol(climate_subset)])
climate_pcs <- predict(climate_pca, newdata = climate_subset[, 3:ncol(climate_subset)])[, 1:3]

# combine discharge and principal component
data_subset <- cbind(discharge_subset, climate_pcs)


# Convert data to mlr3 data format
task <- as_task_regr(data_subset, target = "runoff")

# Split data into train and test sets
set.seed(123)
# split <- rsample::initial_split(task$data, prop = 0.7)
train_set <- sample(task$row_ids, 0.7* task$nrow)
test_set <- setdiff(task$row_ids, train_set)

# Define learner and train model using default hyper-parameters
svm_learner <- lrn("regr.svm")
svm_learner$train(task, row_ids = train_set)
svm_learner$model

# Predict test set
svm_pred <- svm_learner$predict(task, row_ids =  test_set)
svm_pred

# Calculate the correlation coefficient between truths and predictions
correlation <- cor(task$truth(test_set), svm_pred$data$response)
