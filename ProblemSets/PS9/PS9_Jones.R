# Problem Set 9 Code

# Install and load in packages
#install.packages("tidymodels")
#install.packages("glmnet")
library(tidymodels)
library(glmnet)
library(tidyverse)

# Load in Housing Data
housing <- read_table(
  "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", 
  col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")
# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town
#    13. LSTAT    lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's
head(housing)

# set seed for data
set.seed(123456)

# Create two data sets called housing_train and housing_test using the initial_split()
# Creating split with .8 in training set and .2 in test set
housing_split <- initial_split(housing, prop = .8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

# Create recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)


###### 8 ######
# Estimate LASSO model to predict log median house value
tune_specL <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define grid to find lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Create Workflow
rec_wfL <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_specL)  # %>% add_recipe(housing_recipe)
# tune results
rec_resL <- rec_wfL %>% 
  tune_grid(resamples = rec_folds, 
            grid = lambda_grid)

# find optimal lambda
top_rmseL <- show_best(rec_resL, metric = "rmse")
best_rmseL <- select_best(rec_resL, metric = "rmse")

# train with tuned lambda
final_lasso <- finalize_workflow(rec_wfL, best_rmseL)
# Print out results in test set
last_fit(final_lasso, split = housing_split)%>%
  collect_metrics() %>% print
  # rmse in sample 0.170 
  # rsq 0.809

# show best RMSE
top_rmseL %>% print(n = 1)
  # rmse out of sample 0.0632



### 9 ### 
# Repeat 8, but estimate a ridge regression
tune_specR <- linear_reg(
  penalty = tune(), 
  mixture = 0       # 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")
# define grid to find lambda
lambda_grid <- grid_regular(penalty(), levels = 50)
# 6-fold cross-validation
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Create Workflow
rec_wfR <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_specL) # %>% add_recipe(housing_recipe)
# Tune restults
rec_resR <- rec_wfR %>% 
  tune_grid(resamples = rec_folds, 
            grid = lambda_grid)

# find optimal lambda
top_rmseR <- show_best(rec_resR, metric = "rmse")
best_rmseR <- select_best(rec_resR, metric = "rmse")

# train with tuned lambda
final_ridge <- finalize_workflow(rec_wfR, best_rmseR)
# Print out results in test set
last_fit(final_ridge, split = housing_split)%>%
  collect_metrics() %>% print
# rmse in sample 0.175
# rsq 0.799

# show best RMSE
top_rmseR %>% print(n = 1)
# rmse out of sample 0.0695
