library(h2o)
library(tidyverse)
library(Metrics)

#------------------------------------------------------------------------------------------------#

nas <- T

std <- T
log <- T
cnt <- T
building <- 0
explico <- "FLOOR"

#------------------------------------------------------------------------------------------------#

# abro ficheros 


if (std==F & log==T) {df <- readRDS("./data_frames/df_nostd_log.rds"); vd <- readRDS("./data_frames/vd_nostd_log.rds");}
if (std==F & log==F) {df <- readRDS("./data_frames/df_nostd_nolog.rds"); vd <- readRDS("./data_frames/vd_nostd_nolog.rds");}

if (std==T & log==T & cnt==T) {df <- readRDS("./data_frames/df_std_log_center.rds"); vd <- readRDS("./data_frames/vd_std_log_center.rds")}
if (std==T & log==T & cnt==F) {df <- readRDS("./data_frames/df_std_log_nocenter.rds"); vd <- readRDS("./data_frames/vd_std_log_nocenter.rds")}
if (std==T & log==F & cnt==T) {df <- readRDS("./data_frames/df_std_nolog_center.rds"); vd <- readRDS("./data_frames/vd_std_nolog_center.rds")}
if (std==T & log==F & cnt==F) {df <- readRDS("./data_frames/df_std_nolog_nocenter.rds"); vd <- readRDS("./data_frames/vd_std_nolog_nocenter.rds")}

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


modelo <- h2o.gbm(training_frame = train,
                  validation_frame = test, 
                  x=x,
                  y=y,
                  ntrees = 30, 
                  learn_rate = 0.3, 
                  max_depth = 10, 
                  sample_rate = 0.7, 
                  col_sample_rate = 0.7, 
                  score_each_iteration = T,
                  model_id = "modelo_floor_building0",
                  nfolds = nfolds,
                  seed = 1) 


h2o.performance(modelo, test)


#------------------------------------------------------------------------------------------------#

h2o.saveModel(modelo, path = "modelos_h2o") 
model <- h2o.loadModel(path = "./modelos_h2o/modelo_floor_building0")




pred <- as.data.frame(h2o.predict(modelo, test))
vd_building$FLOOR_PRED <- pred$predict
table(vd_building$FLOOR_PRED, vd_building$FLOOR)


for (i in unique(vd_building$FLOOR)) {
    tmp <- filter(vd_building, FLOOR==i)
    res <- accuracy(tmp$FLOOR_PRED, tmp$FLOOR)
    cat(paste(i, "-",res, "-", nrow(tmp),"\n"))
}






