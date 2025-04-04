#Load in the penguins data set from R using the data 
#function within the tidymodels package
library(tidymodels)
??penguins
data(penguins)
#Set a seed for the data set and create a 70/30 split in the penguins data
set.seed(422442)
(resample_split <- initial_split(penguins, prop = 0.7))
#From this split create unique values for the testing and training data
penguins_train<-training(resample_split)
glimpse(penguins_train)

penguins_test<-testing(resample_split)
glimpse(penguins_test)

#Create a 10-fold validation for the training data
nrow(penguins_train)*1/10

vfold_cv(penguins_train,v=10)
#End of part 1: Work continues after Wednesday lecture.

#Start of daily exercise 16
# Add a new section for the model fitting and workflow

# Define a logistic regression model and a rand_forest model
logistic_model <- 
  logistic_reg(mode='classification')%>%
  set_engine('glm')

rand_forest_model <- 
  rand_forest(mode='classification') %>% 
  set_engine("ranger")

# Set up a workflow_set() to compare the logistic regression model 
# (the winner of lecture here) to the rand_forest model you create. 
# Use accuracy as your primary metric to rank the models.
penguin_recipe <- recipe(species ~ ., data = penguins_train) %>%
  step_naomit(all_predictors())

Models_workflow <- workflow_set(
  preproc = list(penguin_recipe),
  models=list(
    logical=logistic_model,
    ran_forest=rand_forest_model
  ),
  cross=TRUE)
workflow_set<-workflow_set(
  preproc = list(penguin_recipe),
  models = list(logistic_model,rand_forest_model),
  cross=TRUE,
  case_weights=NULL)

cv_folds<-vfold_cv(penguins_train,v=5)

results <- Models_workflow %>%
  workflow_map(
    resamples = cv_folds,
    metrics = metric_set(accuracy),
    verbose = TRUE)

# As a comment, write a sentence about what model you think is best!
# Random forest performed better than logistic regression, especially since the relationship is non-linear.
