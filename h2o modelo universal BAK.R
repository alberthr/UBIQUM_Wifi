library(h2o)
library(tidyverse)
library(Metrics)
library(plotly)

#------------------------------------------------------------------------------------------------#

nas <- TRUE
logdf <- TRUE
phonestd <- TRUE
building <- 1
explico <- "FLOOR"

#------------------------------------------------------------------------------------------------#

# abro ficheros 

if (logdf==T & phonestd==F) {df <- readRDS("df_log.rds"); vd <- readRDS("vd_log.rds");}
if (logdf==T & phonestd==T) {df <- readRDS("df_mobile_log_std.rds"); vd <- readRDS("vd_mobile_log_std.rds")}

if (logdf==F & phonestd==F) {df <- readRDS("cleandf.rds"); vd <- read.csv("validationData.csv")}
if (logdf==F & phonestd==T) {df <- readRDS("df_mobile_std.rds"); vd <- readRDS("vd_mobile_std.rds")}
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



rf1 <- h2o.randomForest(training_frame = train,
                        validation_frame = test, 
                        x=x,
                        y=y,
                        model_id = paste0("rf1_b",building,"_",explico), 
                        ntrees = 1000,
                        nfolds = 5,
                        stopping_rounds = 2,
                        score_each_iteration = T,
                        seed = 1)


rf2 <- h2o.randomForest(training_frame = train,
                        validation_frame = test, 
                        x=x,
                        y=y,
                        model_id = paste0("rf2_b",building,"_",explico),
                        ntrees = 1200,   
                        max_depth = 70, 
                        stopping_rounds = 2, 
                        nfolds = 5,
                        #stopping_tolerance = 1e-2,
                        score_each_iteration = T, 
                        seed=1) 


gb1 <- h2o.gbm(training_frame = train,
               validation_frame = test, 
               x=x,
               y=y,
               nfolds = 5,
               model_id = paste0("gb1_b",building,"_",explico),
               seed = 1)  


gb2 <- h2o.gbm(training_frame = train,
               validation_frame = test, 
               x=x,
               y=y,
               ntrees = 100,
               learn_rate = 0.2,
               max_depth = 10,  
               nfolds = 5,
               stopping_rounds = 2, 
               score_each_iteration = T, 
               model_id = paste0("gb2_b",building,"_",explico),
               seed = 3000000)



gb3 <- h2o.gbm(training_frame = train,
               validation_frame = test, 
               x=x,
               y=y,
               ntrees = 30, 
               learn_rate = 0.2, 
               max_depth = 10, 
               sample_rate = 0.7, 
               col_sample_rate = 0.7, 
               stopping_rounds = 2, 
               nfolds = 5,
               #stopping_tolerance = 0.01, 
               nfolds = 5,
               score_each_iteration = T,
               model_id = paste0("gb3_b",building,"_",explico),
               seed = 1) 


xg1 <- h2o.xgboost(x = x,
                   y = y,
                   training_frame = train,
                   validation_frame = test,
                   distribution = "multinomial",
                   ntrees = 50,
                   max_depth = 5,
                   min_rows = 5,
                   learn_rate = 0.1,
                   nfolds = 5,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)
                 

xg2 <- h2o.xgboost(x = x,
                   y = y,
                   training_frame = train,
                   validation_frame = test,
                   distribution = "multinomial",
                   ntrees = 50,
                   max_depth = 8,
                   min_rows = 1,
                   learn_rate = 0.1,
                   sample_rate = 0.7,
                   col_sample_rate = 0.9,
                   nfolds = 5,
                   fold_assignment = "Modulo",
                   keep_cross_validation_predictions = TRUE,
                   seed = 1)


#------------------------------------------------------------------------------------------------#

h2o.performance(rf1, test)
h2o.performance(rf2, test)
h2o.performance(gb1, test)
h2o.performance(gb2, test)
h2o.performance(gb3, test)
h2o.performance(xg1, test)
h2o.performance(xg2, test)

base_models <- list(rf1, gb1, xg1)

ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = base_models)


#------------------------------------------------------------------------------------------------#

pred <- as.data.frame(h2o.predict(bestmodel, test))
vd_building$LAT_PRED <- pred$predict

library(plotly)
plot_ly(vd_building, x=~LATITUDE, y=~LAT_PRED, color=~as.factor(FLOOR),
        type = 'scatter', mode = 'markers')


for (i in unique(vd_building$FLOOR)) {
    tmp <- filter(vd_building, FLOOR==i)
    res <- mae(tmp$LAT_PRED, tmp$LATITUDE)
    cat(paste(i, "-",res, "\n"))
}






