library(h2o)
library(tidyverse)
library(Metrics)
library(plotly)

#------------------------------------------------------------------------------------------------#

nas <- TRUE
logdf <- FALSE
phonestd <- FALSE
building <- 1
explico <- "LATITUDE"

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


nfolds <- 5

my_gbm <- h2o.gbm(x = x,
                  y = y,
                  training_frame = train,
                  validation_frame = test,
                  #distribution = "multinomial",
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = nfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = x,
                          y = y,
                          training_frame = train,
                          validation_frame = test,
                          #ntrees = 1200,   
                          max_depth = 70,
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train & Cross-validate a DNN
my_dl <- h2o.deeplearning(x = x,
                          y = y,
                          training_frame = train,
                          l1 = 0.001,
                          l2 = 0.001,
                          hidden = c(200, 200, 200),
                          nfolds = nfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)


### XGBoost base models
# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1 <- h2o.xgboost(x = x,
                       y = y,
                       training_frame = train,
                       #distribution = "multinomial",
                       ntrees = 50,
                       max_depth = 3,
                       min_rows = 2,
                       learn_rate = 0.2,
                       nfolds = nfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2 <- h2o.xgboost(x = x,
                       y = y,
                       training_frame = train,
                       #distribution = "multinomial",
                       ntrees = 50,
                       max_depth = 8,
                       min_rows = 1,
                       learn_rate = 0.1,
                       sample_rate = 0.7,
                       col_sample_rate = 0.9,
                       nfolds = nfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)


#------------------------------------------------------------------------------------------------#

h2o.performance(my_gbm, test)
h2o.performance(my_rf, test)
h2o.performance(my_dl, test)
h2o.performance(my_xgb1, test)
h2o.performance(my_xgb2, test)


base_models <- list(my_gbm@model_id, my_dl@model_id)

ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = base_models)

(perf <- h2o.performance(ensemble, newdata = test))

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






