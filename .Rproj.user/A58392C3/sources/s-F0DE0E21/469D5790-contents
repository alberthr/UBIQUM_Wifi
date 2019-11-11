library(tidyverse)
library(rpart)
library(caret)
library(Metrics)


df <- readRDS("cleandf.rds")
names(df)

# base de datos ad-hoc para lanzar modelos (building)

nf <- 19861  # max: 19861
nc <- 465 # max: 465
hp <- 465

filas <- sample(nrow(df), size = nf,replace = F)
columnas <- sample(hp, size = nc,replace = F)
columnas <- c(columnas, 469)
building <- df[filas, columnas]




# rpart con caret para sacar cp optimo
# control <- trainControl(method = "cv", number = 5)
# setup <- train (BUILDINGID ~ ., data = building,
#                 method = "rpart",
#                 tuneLength = 50,
#                 trControl = control)



# rpart con caret
tuneGrid <- expand.grid(cp = c(0))
rpartbuilding <- train (BUILDINGID ~ ., data = building,
                method = "rpart",
                tuneGrid = tuneGrid)

pred <- predict(rpart, vd)
accuracy(vd$BUILDINGID, pred)
table(vd$BUILDINGID, pred)
saveRDS(rpart, "rpartbuilding.rds")





# random forest
tuneGrid <- expand.grid(.mtry = c(10, 15, 20, 25, 30, 50, 100, 150, 250))
control <- trainControl(method = "cv", number = 5)
rfbuilding <- train (BUILDINGID ~ ., data = building,
                method = "rf",
                tuneGrid = tuneGrid,
                trControl = control)
pred <- predict(setup, vd)
accuracy(vd$BUILDINGID, pred)
table(vd$BUILDINGID, pred)
saveRDS(rpart, "rfbuilding.rds")



