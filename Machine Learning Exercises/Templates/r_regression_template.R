#__________________________#
############################
#     Regression Model     #
############################
#--------------------------#


#################################
#     Importing the dataset     #
#################################
dataset = read.csv('dataset.csv')
dataset = dataset[2:3]

####################################################################
#     Splitting the dataset into the Training set and Test set     #
####################################################################
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
# split = sample.split(dataset$Salary, SplitRatio = 2/3)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

###########################
#     Feature Scaling     #
###########################
# training_set = scale(training_set)
# test_set = scale(test_set)

###################################################
#     Fitting Regression Model to the dataset     #
###################################################
# CREATE 
# YOUR 
# REGRESSOR 
# HEREEEEE!!!!

###################################
#     Predicting a new result     #
###################################
y_pred = predict(regressor, data.frame(Level = 6.5))

####################################################
#     Visualising the Regression Model results     #
####################################################
library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$X, y = dataset$Y),
             colour = 'red') +
  geom_line(aes(x = dataset$X, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('X') +
  ylab('Y')

########################################################################
#     Visualising the Regression Model results (higher resolution)     #
########################################################################
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$X), max(dataset$X), 0.01)
ggplot() +
  geom_point(aes(x = dataset$X, y = dataset$Y),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Regression Model)') +
  xlab('X') +
  ylab('Y')