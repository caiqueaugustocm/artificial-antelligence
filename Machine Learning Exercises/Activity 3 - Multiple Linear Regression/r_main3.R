###############################
#     Data Preprocessing      #
###############################

#Importing the Dataset
dataset = read.csv('50_Startups.csv')

# Encoding Categorical Data
dataset$State = factor(dataset$State,
                         levels = c('New York','California','Florida'),
                         labels = c(1,2,3))

#Splitting the Dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

###################################################################
#     Fitting Multiple Linear Regression to the Training set      #
###################################################################
regressor = lm(formula = Profit ~ .,
               data = training_set)

# Predicting the Test set results
Y_pred = predict(regressor, newdata = test_set)

##################################################################
#     Building the optimal model using Backward Elimination      #
##################################################################
#Moment 1
regressorbe = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressorbe)

#Moment 2
regressorbe = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
                 data = dataset)
summary(regressorbe)

#Moment 3
regressorbe = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
                 data = dataset)
summary(regressorbe)

#Moment 4
regressorbe = lm(formula = Profit ~ R.D.Spend,
                 data = dataset)
summary(regressorbe)

###########################################################
#     Ideal Model Function Using Reverse Elimination      #
###########################################################

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)

###########################################################