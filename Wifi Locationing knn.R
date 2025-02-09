library(h2o)
library(tidyverse)
library(reshape2)
library(Metrics)



new <- read.csv("./submit.csv")

if (exists("transformo_fichero")) {
    
    transformo_fichero(new, T, T)
    transformo_fichero(new, F, T)
    transformo_fichero(new, F, F)
    
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE BUILDING
    
    h2o.init()
    h2o.removeAll()
    
    test <- as.h2o(x = new, destination_frame = "test")
    model <- h2o.loadModel(path = "./modelos_h2o/modelo_building_gbm")
    
    pred <- as.data.frame(h2o.predict(model, test))
    new$BUILDING_PRED <- as.numeric(pred$predict)-1
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LONGITUDE BUILDING 0
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/longitude_b0_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LONGITUDE_B0 <- pred
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LONGITUDE BUILDING 1
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/longitude_b1_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LONGITUDE_B1 <- pred
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LONGITUDE BUILDING 2
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/longitude_b2_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LONGITUDE_B2 <- pred
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LATITUDE BUILDING 0
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/latitude_b0_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LATITUDE_B0 <- pred
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LATITUDE BUILDING 1
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/latitude_b1_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LATITUDE_B1 <- pred
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE LATITUDE BUILDING 2
    
    vd <- readRDS("vd_std_nolog_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    test <- vd
    model <- readRDS("./modelos_caret/latitude_b2_merged.rds")
    test[is.na(test)] <- 0
    
    pred <- predict(model, test)
    new$LATITUDE_B2 <- pred
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE FLOOR BUILDING 0
    
    vd <- readRDS("vd_std_log_center.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    h2o.init()
    h2o.removeAll()
    
    test <- as.h2o(x = vd, destination_frame = "test")
    model <- h2o.loadModel(path = "./modelos_h2o/modelo_floor_building0")
    
    pred <- as.data.frame(h2o.predict(model, test))
    new$FLOOR_B0 <- pred$predict
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE FLOOR BUILDING 2
    
    vd <- readRDS("vd_std_log_center.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    h2o.init()
    h2o.removeAll()
    
    test <- as.h2o(x = vd, destination_frame = "test")
    model <- h2o.loadModel(path = "./modelos_h2o/modelo_floor_building2")
    
    pred <- as.data.frame(h2o.predict(model, test))
    new$FLOOR_B2 <- pred$predict
    
    #----------------------------------------------------------------------------------------------#
    
    # ESTIMACION DE FLOOR BUILDING 1
    
    vd <- readRDS("vd_std_log_nocenter.rds")
    elimino <- which(names(vd) %in% c("LATITUDE", "LONGITUDE","FLOOR", "BUILDINGID"))
    if (length(elimino)>0) vd <- vd[,-elimino]
    
    h2o.init()
    h2o.removeAll()
    
    test <- as.h2o(x = vd, destination_frame = "test")
    model1 <- h2o.loadModel(path = "./modelos_h2o/modelo_floor_building1_1")
    model2 <- h2o.loadModel(path = "./modelos_h2o/modelo_floor_building1_A")
    
    
    pred1 <- as.data.frame(h2o.predict(model1, test))
    pred2 <- as.data.frame(h2o.predict(model2, test))
    
    new$FLOOR_B1_1 <- pred1$predict
    new$FLOOR_B1_A <- pred2$predict
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    new$LONGITUDE_PRED <- ifelse(new$BUILDING_PRED==0, new$LONGITUDE_B0, ifelse(new$BUILDING_PRED==1, new$LONGITUDE_B1, new$LONGITUDE_B2))
    mae(new$LONGITUDE, new$LONGITUDE_PRED)
    (cor(new$LONGITUDE, new$LONGITUDE_PRED))^2
    
    
    new$LATITUDE_PRED <- ifelse(new$BUILDING_PRED==0, new$LATITUDE_B0, ifelse(new$BUILDING_PRED==1, new$LATITUDE_B1, new$LATITUDE_B2))
    mae(new$LATITUDE, new$LATITUDE_PRED)
    (cor(new$LATITUDE, new$LATITUDE_PRED))^2
    
    new$FLOOR_B1 <- (ifelse(new$FLOOR_B1_1==1, new$FLOOR_B1_1, new$FLOOR_B1_A))-1
    new$FLOOR_PRED <- ifelse(new$BUILDING_PRED==0, as.numeric(new$FLOOR_B0)-1, ifelse(new$BUILDING_PRED==1, new$FLOOR_B1, as.numeric(new$FLOOR_B2)-1))
    accuracy(new$FLOOR, new$FLOOR_PRED)
    
    #----------------------------------------------------------------------------------------------#
    #----------------------------------------------------------------------------------------------#
    
    #new$distance75 <- 
    #    sqrt(
    #        (abs(new$LONGITUDE_PRED-new$LONGITUDE))^2 +
    #        (abs(new$LATITUDE_PRED - new$LATITUDE))^2
    #    ) + 
    #    4 * abs(new$FLOOR_PRED - new$FLOOR) + 
    #    50 * abs(new$BUILDING_PRED - new$BUILDINGID)
    #
    #distance75 <- new$distance75
    #distance75 <- distance75[order(distance75)]
    #print(quantile(distance75, 0.75))
    
    finalsubmit <- new[,c("LATITUDE_PRED", "LONGITUDE_PRED", "FLOOR_PRED")]
    
    write.csv(finalsubmit, "knn_soft.csv", row.names = F)

}
