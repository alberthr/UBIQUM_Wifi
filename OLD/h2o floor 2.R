library(h2o)
library(tidyverse)
library(rpart)
library(caret)
library(Metrics)

#------------------------------------------------------------------------------------------------#

# abro ficheros estandarizados
df <- readRDS("df_mobile_std.rds")
vd <- readRDS("vd_mobile_std.rds")

# o abro los ficheros enteros
df <- readRDS("cleandf.rds")
vd <- read.csv("validationData.csv")
df[df==100] <- NA
vd[vd==100] <- NA

#------------------------------------------------------------------------------------------------#

# selecciono los individuos del edificio y borro waps inservibles
df2 <- df[df$BUILDINGID==2,]
colwap <- which(startsWith(names(df), "WAP"))
#waps100 <- sapply(df2[,colwap], function(x) nrow(df2) - sum(is.na(x))) %>% as.data.frame()
waps100 <- sapply(df2[,colwap], function(x) nrow(df2) - sum(x==100)) %>% as.data.frame()

delcol <- which(waps100$.==0)
df2 <- df2[,-delcol]

# selecciono individus de l'edifici
vd2 <- vd[vd$BUILDINGID==2,]

#------------------------------------------------------------------------------------------------#

# H2O PROCESS

# inicializo entorno
h2o.init()
h2o.removeAll()
floor_train_h2o <- as.h2o(x = df2, destination_frame = "floor_train_h2o")
floor_test_h2o <- as.h2o(x = vd2, destination_frame = "floor_test_h2o")



flrf1 <- h2o.randomForest(training_frame = floor_train_h2o,
                        validation_frame = floor_test_h2o, 
                        x=c(1:203),
                        y=206,
                        model_id = "fl_rf_covType_v1", 
                        ntrees = 1000,
                        stopping_rounds = 2,
                        score_each_iteration = T,
                        seed = 1000000)


flgbm1 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o,   
                x=c(1:203),
                y=206, 
                model_id = "fl_gbm_covType1", 
                seed = 2000000)  


flgbm2 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o, 
                x=c(1:203),
                y=206,
                ntrees = 20,
                learn_rate = 0.2,
                max_depth = 10,  
                stopping_rounds = 2, 
                stopping_tolerance = 0.01, 
                score_each_iteration = T, 
                model_id = "fl_gbm_covType2", 
                seed = 2000000)


flgbm3 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o, 
                x=c(1:203),
                y=206,
                ntrees = 30, 
                learn_rate = 0.3, 
                max_depth = 10, 
                sample_rate = 0.7, 
                col_sample_rate = 0.7, 
                stopping_rounds = 2, 
                stopping_tolerance = 0.01, 
                score_each_iteration = T,
                model_id = "fl_gbm_covType3",
                seed = 2000000) 


###############################################################################

h2o.hit_ratio_table(flrf1,valid = T)[1,2]     ## review the random forest accuracy
h2o.hit_ratio_table(flgbm1,valid = T)[1,2]    ## review the first model's accuracy
h2o.hit_ratio_table(flgbm2,valid = T)[1,2]    ## review the second model's accuracy
h2o.hit_ratio_table(flgbm3,valid = T)[1,2]    ## review the newest model's accuracy

###############################################################################


flrf2 <- h2o.randomForest(training_frame = floor_train_h2o,
                        validation_frame = floor_test_h2o,
                        x=c(1:203),
                        y=206,
                        model_id = "fl_rf_covType2", 
                        ntrees = 1200,   
                        max_depth = 70, 
                        stopping_rounds = 2, 
                        stopping_tolerance = 1e-2,
                        score_each_iteration = T, 
                        seed=3000000) 

###############################################################################

h2o.hit_ratio_table(flgbm1,valid = T)[1,2]    ## review the newest GBM accuracy
h2o.hit_ratio_table(flrf1,valid = T)[1,2]     ## original random forest accuracy
h2o.hit_ratio_table(flrf2,valid = T)[1,2]     ## newest random forest accuracy

###############################################################################

# gbm1 = 88.06 <- no std no NA
# gbm1 = 91.79 <- no std y NA
# gbm1 = 92.91 <- std y NA

