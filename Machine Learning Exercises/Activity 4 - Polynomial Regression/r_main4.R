###############################
#     Data Preprocessing      #
###############################

#Importing the Dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#####################################################
#     Fitting Linear Regression to the dataset      #
#####################################################

lin_reg = lm(formula = Salary ~ .,
             data = dataset)

#########################################################
#     Fitting Polynomial Regression to the dataset      #
#########################################################

dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
               data = dataset)

########################
#     Visualising      #
########################
library(ggplot2)
## the Linear Regression results ##
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)), 
            colour = 'blue') +
  ggtitle('Truth of Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')
  
## the Polynomial Regression results ##
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary), 
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)), 
            colour = 'blue') +
  ggtitle('Truth of Bluff (Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#######################
#     Predicting      #
#######################

# a new result with Linear Regression
y_pred = predict(lin_reg, data.frame(Level = 6.5))

# a new result with Polynomial Regression
y_pred = predict(poly_reg, data.frame(Level = 6.5,
                                      Level2 = 6.5^2,
                                      Level3 = 6.5^3,
                                      Level4 = 6.5^4))
