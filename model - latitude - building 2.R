library(h2o)
library(tidyverse)
library(Metrics)
library(plotly)

#------------------------------------------------------------------------------------------------#

nas <- T

std <- T
log <- T
cnt <- T
building <- 2
explico <- "LATITUDE"

#------------------------------------------------------------------------------------------------#

# abro ficheros 

if (std==F & log==T) {df <- readRDS("df_nostd_log.rds"); vd <- readRDS("vd_nostd_log.rds");}
if (std==F & log==F) {df <- readRDS("df_nostd_nolog.rds"); vd <- readRDS("vd_nostd_nolog.rds");}

if (std==T & log==T & cnt==T) {df <- readRDS("df_std_log_center.rds"); vd <- readRDS("vd_std_log_center.rds")}
if (std==T & log==T & cnt==F) {df <- readRDS("df_std_log_nocenter.rds"); vd <- readRDS("vd_std_log_nocenter.rds")}
if (std==T & log==F & cnt==T) {df <- readRDS("df_std_nolog_center.rds"); vd <- readRDS("vd_std_nolog_center.rds")}
if (std==T & log==F & cnt==F) {df <- readRDS("df_std_nolog_nocenter.rds"); vd <- readRDS("vd_std_nolog_nocenter.rds")}

if (nas==TRUE) {df[df==100] <- NA; vd[vd==100] <- NA}

#------------------------------------------------------------------------------------------------#

# selecciono los individuos del edificio y borro waps inservibles

df_building <- df[df$BUILDINGID==building,]
colwap <- which(startsWith(names(df_building), "WAP"))
if (nas==T) {waps100 <- sapply(df_building[,colwap], function(x) nrow(df_building) - sum(is.na(x))) %>% as.data.frame()}
if (nas==F) {waps100 <- sapply(df_building[,colwap], function(x) nrow(df_building) - sum(x==100)) %>% as.data.frame()}
delcol <- which(waps100$.==0)
df_building <- df_building[,-delcol]


# selecciono individus de l'edifici
vd$id <- rownames(vd)
vd_building <- vd[vd$BUILDINGID==building,]

df_building$FLOOR <- factor(df_building$FLOOR)
#vd_building$FLOOR <- factor(vd_building$FLOOR)


#------------------------------------------------------------------------------------------------#

# H2O PROCESS

# inicializo entorno
h2o.init()
h2o.removeAll()

trainname <- paste0("train_b",building,"_",explico)
testname <- paste0("test_b",building,"_",explico)
train <- as.h2o(x = df_building, destination_frame = trainname)
test <- as.h2o(x = vd_building, destination_frame = testname)

x <- which(startsWith(names(df_building), "WAP"))
y <- which(names(df_building) %in% explico)


nfolds <- 5


flrf1 <- h2o.randomForest(training_frame = train,
                          validation_frame = test, 
                          x=x,
                          y=y,
                          model_id = "fl_rf_covType_v1", 
                          ntrees = 1000,
                          stopping_rounds = 2,
                          score_each_iteration = T,
                          seed = 1)


flrf2 <- h2o.randomForest(training_frame = train,
                          validation_frame = test, 
                          x=x,
                          y=y,
                          model_id = "bd_rf_covType2", 
                          ntrees = 200,   
                          max_depth = 30,     
                          stopping_rounds = 2, 
                          stopping_tolerance = 1e-2, 
                          score_each_iteration = T, 
                          seed=1)


flgbm1 <- h2o.gbm(training_frame = train,
                  validation_frame = test, 
                  x=x,
                  y=y,
                  model_id = "fl_gbm_covType1", 
                  seed = 1)  


flgbm2 <- h2o.gbm(training_frame = train,
                  validation_frame = test, 
                  x=x,
                  y=y,
                  ntrees = 20,
                  learn_rate = 0.2,
                  max_depth = 10,  
                  stopping_rounds = 2, 
                  stopping_tolerance = 0.01, 
                  score_each_iteration = T, 
                  model_id = "fl_gbm_covType2", 
                  seed = 1)


flgbm3 <- h2o.gbm(training_frame = train,
                  validation_frame = test, 
                  x=x,
                  y=y,
                  ntrees = 30, 
                  learn_rate = 0.3, 
                  max_depth = 10, 
                  sample_rate = 0.7, 
                  col_sample_rate = 0.7, 
                  stopping_rounds = 2, 
                  stopping_tolerance = 0.01, 
                  score_each_iteration = T,
                  model_id = "fl_gbm_covType3",
                  seed = 1) 



#------------------------------------------------------------------------------------------------#

h2o.performance(flrf1, test)
h2o.performance(flrf2, test)
h2o.performance(flgbm1, test)
h2o.performance(flgbm2, test)
h2o.performance(flgbm3, test)



#base_models <- list(my_gbm@model_id, my_rf@model_id)

#ensemble <- h2o.stackedEnsemble(x = x,
#                                y = y,
#                                training_frame = train,
#                                base_models = base_models)

#(perf <- h2o.performance(ensemble, newdata = test))

#------------------------------------------------------------------------------------------------#

pred <- as.data.frame(h2o.predict(flgbm2, test))
vd_building$LAT_PRED <- pred$predict

#library(plotly)
#plot_ly(vd_building, x=~LATITUDE, y=~LAT_PRED, color=~as.factor(FLOOR),
#        type = 'scatter', mode = 'markers')


for (i in unique(vd_building$FLOOR)) {
    tmp <- filter(vd_building, FLOOR==i)
    res <- mae(tmp$LAT_PRED, tmp$LATITUDE)
    cat(paste(i, "-",res, "-", nrow(tmp),"\n"))
}






