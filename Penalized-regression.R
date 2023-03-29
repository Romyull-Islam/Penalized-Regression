library(caret)  # classification and regression training
library(glmnet) # fits generalized linear and similar models via penalized maximum likelihood.

########################################################
############### Load the data from MASS package #####
data("Boston", package = "MASS")
head(Boston)
#####################################################
####### Define the predictors and response ########
# Predictor variables
x <- model.matrix(medv~., data= Boston)[,-1]
# Outcome variable
y <- Boston$medv

######################### fit the full model ###
data1 <- Boston[, ]
model1 <- lm(medv~., data=data1)
summary(model1)
#####################################################
################  Fitting the ELastic net model ######
# When alpha=0, Ridge Model is fit and 
# if alpha=1, a lasso model is fit.
# if alpha = (0, 1), a ELastic Net is fit
# Find the best lambda using cross-validation
set.seed(123)  # reproducibility
crossvalid <- cv.glmnet(x, y, alpha = 0.3) #cross-validation based on alpha

plot(crossvalid)   # plot cross-validation error
plot(crossvalid$glmnet.fit,"lambda", label=FALSE) # plot the model(lasso/ridge/net) path 


# fit the penalized model with best lambda
blambda = crossvalid$lambda.min #print the lambda that gives minimum error
(clambda = crossvalid$lambda.1se) #print the lambda that gives you the most highly regularized model that is within 1sd of minimum error

# Fit the final Penalized model based on your lambda
lasso.model <- glmnet(x=x, y=y,
                      alpha  = 0.3, 
                      lambda = clambda)

lasso.model$beta  # weights of the fitted model

# Make predictions
predictions <- predict(lasso.model, x)

# Evaluation metrics
data.frame(
  RMSE = RMSE(predictions, y),
  Rsquare = R2(predictions, y)
)





