library(tidyverse)

vd <- read.csv("./data_frames/validationData.csv")
df <- read.csv("./data_frames/trainingData.csv")

total <- rbind(vd, df)
total <- group_by(total, BUILDINGID, LATITUDE, LONGITUDE, FLOOR, PHONEID) %>%
    summarise_all(funs(median))

write.csv(total, "./data_frames/mergedData.csv")
dim(total)


saveRDS(total, "./data_frames/cleandf.rds")
#write.csv(validation, "./data_frames/validationData.csv")
