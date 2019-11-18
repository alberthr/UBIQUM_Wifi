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
df1 <- df[df$BUILDINGID==1,]
colwap <- which(startsWith(names(df), "WAP"))
waps100 <- sapply(df1[,colwap], function(x) nrow(df1) - sum(is.na(x))) %>% as.data.frame()
#waps100 <- sapply(df1[,colwap], function(x) nrow(df1) - sum(x==100)) %>% as.data.frame()

delcol <- which(waps100$.==0)
df1 <- df1[,-delcol]

# selecciono individus de l'edifici
vd1 <- vd[vd$BUILDINGID==1,]

#------------------------------------------------------------------------------------------------#

# H2O PROCESS

# inicializo entorno
h2o.init()
h2o.removeAll()
floor_train_h2o <- as.h2o(x = df1, destination_frame = "floor_train_h2o")
floor_test_h2o <- as.h2o(x = vd1, destination_frame = "floor_test_h2o")



flrf1 <- h2o.randomForest(training_frame = floor_train_h2o,
                          validation_frame = floor_test_h2o, 
                          x=c(1:207),
                          y=209,
                          model_id = "fl_rf_covType_v1", 
                          ntrees = 1000,
                          stopping_rounds = 2,
                          score_each_iteration = T,
                          seed = 1000000)


flrf2 <- h2o.randomForest(training_frame = floor_train_h2o,
                          validation_frame = floor_test_h2o,
                          x=c(1:207),
                          y=209, 
                          model_id = "fl_rf_covType2", 
                          ntrees = 1200,   
                          max_depth = 70, 
                          stopping_rounds = 2, 
                          #stopping_tolerance = 1e-2,
                          score_each_iteration = T, 
                          seed=3000000) 


flgbm1 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o,   
                x=c(1:207),
                y=209, 
                model_id = "fl_gbm_covType1", 
                seed = 2000000)  


flgbm2 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o, 
                x=c(1:207),
                y=209,
                ntrees = 100,
                learn_rate = 0.2,
                max_depth = 10,  
                stopping_rounds = 2, 
                score_each_iteration = T, 
                model_id = "fl_gbm_covType2", 
                seed = 2000000)


flgbm3 <- h2o.gbm(training_frame = floor_train_h2o,
                validation_frame = floor_test_h2o, 
                x=c(1:207),
                y=209,
                ntrees = 30, 
                learn_rate = 0.2, 
                max_depth = 10, 
                sample_rate = 0.7, 
                col_sample_rate = 0.7, 
                stopping_rounds = 2, 
                #stopping_tolerance = 0.01, 
                score_each_iteration = T,
                model_id = "fl_gbm_covType3",
                seed = 2000000) 


h2o.performance(flgbm1, floor_test_h2o)
h2o.performance(flgbm2, floor_test_h2o)
h2o.performance(flgbm3, floor_test_h2o)
h2o.performance(flrf1, floor_test_h2o)
h2o.performance(flrf2, floor_test_h2o)

# gbm2  RMSE = 12.13 / R2 = 0.8802 <- std y NA

pred <- as.data.frame(h2o.predict(flgbm2, floor_test_h2o))
vd1$LAT_PRED <- pred$predict

library(plotly)
plot_ly(vd1, x=~LATITUDE, y=~LAT_PRED, color=~as.factor(FLOOR),
        type = 'scatter', mode = 'markers')


for (i in unique(vd1$FLOOR)) {
    tmp <- filter(vd1, FLOOR==i)
    res <- rmse(tmp$LAT_PRED, tmp$LATITUDE)
    cat(paste(i, "-",res, "\n"))
}





