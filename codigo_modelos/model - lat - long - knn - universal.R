# TFF

library(caret)
library(beepr)
library(h2o)
library(tidyverse)
library(Metrics)

#------------------------------------------------------------------------------------------------#

nas <- T

std <- T
log <- F
cnt <- F
building <- 0
explico <- "LONGITUDE"

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
vd_building$FLOOR <- factor(vd_building$FLOOR)


vd_building$BUILDINGID <- factor(vd_building$BUILDINGID)

df_building[is.na(df_building)] <- -0
vd_building[is.na(vd_building)] <- -0


#------------------------------------------------------------------------------------------------#

vd_building$FLOOR <- factor(vd_building$FLOOR)
vd_building$BUILDINGID <- factor(vd_building$BUILDINGID)


df_building <- group_by(df_building, BUILDINGID, LATITUDE, LONGITUDE, FLOOR) %>%
    summarise_at(vars(names(df_building)[which(startsWith(names(df_building), "WAP"))]), funs(mean))

vd_building <- group_by(vd_building, BUILDINGID, LATITUDE, LONGITUDE, FLOOR) %>%
    summarise_at(vars(names(vd_building)[which(startsWith(names(vd_building), "WAP"))]), funs(mean))

vd_building <- vd_building[, names(df_building)]


#------------------------------------------------------------------------------------------------#

# KNN
train <- df_building[,which(startsWith(names(df_building), "WAP"))]
cl <- as.data.frame(df_building[,explico])
cl <- cl[,1]

finaltrain <- cbind(cl, train)
ctrl <- trainControl(method="cv",number = 5) 
for (n in 1:5) {
    resultats <- data.frame()
    for (i in 1:20) {
        knnFit <- train(cl ~ ., 
                        data = finaltrain, 
                        
                        method = "knn", 
                        trControl = ctrl, 
                        metric = 'MAE',
                        tuneGrid = expand.grid(k = c(n)))
        resultats <- rbind(resultats, knnFit$results)
    }
    print(round(sapply(resultats, mean),2))
}




knnFit <- train(cl ~ ., 
                data = finaltrain, 
                method = "knn", 
                trControl = ctrl, 
                metric = 'MAE',
                tuneGrid = expand.grid(k = c(3)))
knnFit


saveRDS(knnFit, "./modelos_caret/longitude_b0_merged.rds")

