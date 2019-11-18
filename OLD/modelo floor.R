library(tidyverse)
library(rpart)
library(caret)
library(Metrics)


df <- readRDS("cleandf.rds")
vd <- read.csv("validationData.csv")
names(df)
dim(df)

# base de datos ad-hoc para lanzar modelos (building)

nf <- 500  # max: 19861
nc <- 15 # max: 465
hp <- 465

filas <- sample(nrow(df), size = nf,replace = F)
columnas <- sample(hp, size = nc,replace = F)
columnas <- c(columnas, 468)
floor <- df[filas, columnas]




# rpart con caret para sacar cp optimo
# control <- trainControl(method = "cv", number = 5)
# setup <- train (BUILDINGID ~ ., data = building,
#                 method = "rpart",
#                 tuneLength = 50,
#                 trControl = control)



# rpart con caret
tuneGrid <- expand.grid(cp = c(0))
rpartfloor <- train (FLOOR ~ ., data = floor,
                        method = "rpart",
                        tuneGrid = tuneGrid)

pred <- predict(rpartfloor, vd)
accuracy(vd$FLOOR, pred)
table(vd$FLOOR, pred)
saveRDS(rpartfloor, "rpartfloor.rds")



# creo un nuevo dataset con solo las variables que explican en rpart para aplicar en RF
imp <- varImp(rpartfloor)$importance
imp$HP <- rownames(imp)
impvar <- rownames(imp[imp$Overall>0,])
floortrim <- floor[,impvar]
floortrim$FLOOR <- floor$FLOOR







model <- train(FLOOR ~ ., data = floortrim,
               method = "ranger")



# random forest
tuneGrid <- expand.grid(.mtry = c(10, 15, 20, 25, 30, 50, 100, 150, 250))
control <- trainControl(method = "cv", number = 2)
rffloor <- train (FLOOR ~ ., data = floor,
                     method = "rf",
                     tuneGrid = tuneGrid,
                     trControl = control)
pred <- predict(rffloor, vd)
accuracy(vd$FLOOR, pred)
table(vd$FLOOR, pred)
saveRDS(rffloor, "rffloor.rds")

