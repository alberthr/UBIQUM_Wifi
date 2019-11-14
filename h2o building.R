library(h2o)
library(tidyverse)
library(rpart)
library(caret)
library(Metrics)

df <- readRDS("cleandf.rds")
vd <- read.csv("validationData.csv")
dim(df)

# base de datos ad-hoc para lanzar modelos (building)
building_train <- df[c(1:465,469)]
building_test <- vd[, c(1:520,524)]





h2o.init()
h2o.removeAll()
building_train_h2o <- as.h2o(x = building_train, destination_frame = "building_train_h2o")
building_test_h2o <- as.h2o(x = building_test, destination_frame = "building_test_h2o")


bdrf1 <- h2o.randomForest(training_frame = building_train_h2o,
                        validation_frame = building_test_h2o,
                        x=1:465,
                        y=466,
                        model_id = "bd_rf_covType_v1",
                        ntrees = 1000,                          
                        stopping_rounds = 2,
                        score_each_iteration = T,
                        seed = 1000000)
summary(rf1)
rf1@model$validation_metrics


bdgbm1 <- h2o.gbm(training_frame = building_train_h2o,
                validation_frame = building_test_h2o,
                x=1:465,   
                y=466,   
                model_id = "bd_gbm_covType1",   
                seed = 2000000) 
      

bdgbm2 <- h2o.gbm(training_frame = building_train_h2o,  
                validation_frame = building_test_h2o, 
                x=1:465,  
                y=466,     
                ntrees = 20,     
                learn_rate = 0.2,   
                max_depth = 10,   
                stopping_rounds = 2,  
                stopping_tolerance = 0.01, 
                score_each_iteration = T, 
                model_id = "bd_gbm_covType2", 
                seed = 2000000)  


bdgbm3 <- h2o.gbm(training_frame = building_train_h2o, 
                validation_frame = building_test_h2o, 
                x=1:465,  
                y=466,    
                ntrees = 30,    
                learn_rate = 0.3,  
                max_depth = 10,    
                sample_rate = 0.7, 
                col_sample_rate = 0.7,  
                stopping_rounds = 2,    
                stopping_tolerance = 0.01, 
                score_each_iteration = T, 
                model_id = "bd_gbm_covType3",
                seed = 2000000)  


###############################################################################

h2o.hit_ratio_table(bdrf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(bdgbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(bdgbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(bdgbm3,valid = T)[1,2]    ## review the newest model's accuracy

###############################################################################


bdrf2 <- h2o.randomForest(training_frame = building_train_h2o,     
                        validation_frame = building_test_h2o, 
                        x=1:465,  
                        y=466, 
                        model_id = "bd_rf_covType2", 
                        ntrees = 200,   
                        max_depth = 30,     
                        stopping_rounds = 2, 
                        stopping_tolerance = 1e-2, 
                        score_each_iteration = T, 
                        seed=3000000)


###############################################################################

h2o.hit_ratio_table(bdgbm3,valid = T)[1,2]    ## review the newest GBM accuracy
h2o.hit_ratio_table(bdrf1,valid = T)[1,2]     ## original random forest accuracy
h2o.hit_ratio_table(bdrf2,valid = T)[1,2]     ## newest random forest accuracy

###############################################################################





h2o.saveModel(bdgbm3, path = "building_h2o_gbm") 
building_model <- h2o.loadModel(path = "./building_h2o_gbm/bd_gbm_covType3")

# Print the models and compare
print(bdgbm3)
print(building_model)






