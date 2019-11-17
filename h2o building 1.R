library(h2o)
library(tidyverse)
library(Metrics)
library(plotly)

#------------------------------------------------------------------------------------------------#

nas <- TRUE
logdf <- F
phonestd <- T
center <- T
building <- 1
explico <- "FLOOR1"

#------------------------------------------------------------------------------------------------#

# abro ficheros estandarizados

if (logdf==T & phonestd==F) {df <- readRDS("df_log.rds"); vd <- readRDS("vd_log.rds");}
if (logdf==T & phonestd==T & center==F) {df <- readRDS("df_mobile_log_std.rds"); vd <- readRDS("vd_mobile_log_std.rds")}
if (logdf==T & phonestd==T & center==T) {df <- readRDS("df_mobile_log_std_cent.rds"); vd <- readRDS("vd_mobile_log_std_cent.rds")}

if (logdf==F & phonestd==F) {df <- readRDS("cleandf.rds"); vd <- read.csv("validationData.csv")}
if (logdf==F & phonestd==T & center==F) {df <- readRDS("df_mobile_std.rds"); vd <- readRDS("vd_mobile_std.rds")}
if (logdf==F & phonestd==T & center==T) {df <- readRDS("df_mobile_std_cent.rds"); vd <- readRDS("vd_mobile_std_cent.rds")}

if (nas==TRUE) {df[df==100] <- NA; vd[vd==100] <- NA}

#------------------------------------------------------------------------------------------------#

df$FLOOR1 <- df$FLOOR
vd$FLOOR1 <- vd$FLOOR

df$FLOOR1[df$FLOOR1!=1] <- 0
vd$FLOOR1[vd$FLOOR1!=1] <- 0

df$FLOOR1 <- factor(df$FLOOR1, levels = c(0,1))
vd$FLOOR1 <- factor(vd$FLOOR1, levels = c(0,1))

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


model <- h2o.gbm(training_frame = train,
                 validation_frame = test, 
                 x=x,
                 y=y,
                 ntrees = 100,
                 learn_rate = 0.2,
                 max_depth = 10,  
                 stopping_rounds = 2, 
                 score_each_iteration = T, 
                 model_id = paste0("gb2_b",building,"_",explico),
                 seed = 2000000)


pred <- as.data.frame(h2o.predict(model, test))
vd_building$B1_F1 <- pred$predict


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
y <- which(names(df_building) %in% "FLOOR")


model <- h2o.gbm(training_frame = train,
                 validation_frame = test, 
                 x=x,
                 y=y,
                 ntrees = 100,
                 learn_rate = 0.2,
                 max_depth = 10,  
                 stopping_rounds = 2, 
                 score_each_iteration = T, 
                 model_id = paste0("gb2_b",building,"_",explico),
                 seed = 2000000)


pred <- as.data.frame(h2o.predict(model, test))
vd_building$B1_FOther <- pred$predict


vd_building$FINALFLOOR <- ifelse(vd_building$B1_F1==1, vd_building$B1_F1, vd_building$B1_FOther)


