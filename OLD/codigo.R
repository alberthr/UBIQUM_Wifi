library(tidyverse)
library(rpart)
library(caret)
library(Metrics)


#------------------------------------------------------------------------------------------------#

# ABRO Y MIRO UN POCO

df <- read.csv("trainingData.csv")
df$BUILDINGID <- factor(df$BUILDINGID)
df$FLOOR <- factor(df$FLOOR)

#------------------------------------------------------------------------------------------------#

# LIMPIO FICHERO

# miro cuantos wap no me dan información (todos sus datos = 100)
waps100 <- sapply(df[,1:520], function(x) nrow(df) - sum(x==100)) %>% as.data.frame()
sum(waps100$.==0)

# miro cuanta gente no tiene información de ningun hotspot (todos los hotspots = 100)
row100 <- apply(df[,1:520], MARGIN = 1, function(x) sum(x==100))
sum(row100==520)

# borro filas y columnas inutiles
delrow <- which(row100==520)
delcol <- which(waps100$.==0)
df2 <- df[-delrow, -delcol]

# guardo trabajo
saveRDS(object = df2, file = "cleandf.rds")








