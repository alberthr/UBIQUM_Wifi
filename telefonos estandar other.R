library(tidyverse)
library(rpart)
library(caret)
library(Metrics)


#------------------------------------------------------------------------------------------------#

# ESTRUCTURA DE  WAPS

library(reshape)

df <- read.csv("validationData.csv")
mediatotal <- readRDS("mediatotal.rds")
    
colwap <- which(startsWith(names(df), "WAP"))
othercol <- which(names(df) %in% c("BUILDINGID","FLOOR", "PHONEID"))
hotspots <- df[,c(colwap, othercol)]

mdata <- melt(hotspots, id.vars = c("BUILDINGID","FLOOR", "PHONEID"))
mdata$value[mdata$value==100] <- NA
mdata <- mdata[complete.cases(mdata),]

# sapply(unique(df$PHONEID), function(x) hist(mdata2$value[mdata$PHONEID==x], main=x, breaks = 25))
# tiene sentido estandarizar las señales por telefono

#------------------------------------------------------------------------------------------------#

# ESTANDARIZO SEÑALES POR TELEFONO 0-1

library(tidyverse)

# creo matriz con informacion para conversiones
estandar <- data.frame(phone = numeric(0), mean = numeric(0), sd = numeric(0), 
                       min = numeric(0), max = numeric(0))
for (i in unique(mdata$PHONEID)) {
    tmp <- filter(mdata, PHONEID==i)
    media <- mean(tmp$value)
    desviacion <- sd(tmp$value)
    max <- max(tmp$value)
    min <- min(tmp$value)
    n <- nrow(tmp)
    tmpdf <- data.frame(phone=i, mean=media, sd=desviacion, min=min, max=max, n=n)
    estandar <- rbind(estandar, tmpdf)
}
estandar$meanstd <- (estandar$mean-estandar$min)/(estandar$max-estandar$min)

# fusiono df principal con esta información y añado un id para orderar siempre
df$id <- as.numeric(rownames(df))
dftmp <- merge(x=df, y=estandar, by.x = "PHONEID", by.y = "phone", all = T)
dftmp <- dftmp[order(dftmp$id),]

# creo df solo con waps para trabajar
colwap <- which(startsWith(names(dftmp), "WAP"))
dfwap <- dftmp[,colwap]
dfwap[dfwap==100] <- NA

# aplico transformaciones
# 1) paso a escala 0-1
# 2) calculo el promedio del total muestra 
# 3) transformo cada telefono al mismo promedio
dftrans <- (dfwap-dftmp$min)/(dftmp$max-dftmp$min)
dfconvert <- log(mediatotal, dftmp$meanstd)
dftrans_std <- dftrans^dfconvert
dftrans_std$PHONEID <- dftmp$PHONEID

#------------------------------------------------------------------------------------------------#

# VALIDATION DEL ULTIMO PUNTO

mdata2 <- melt(dftrans_std, id.vars = c("PHONEID"))
mdata2 <- mdata2[complete.cases(mdata2),]

estandar_validation <- data.frame(phone = numeric(0), mean = numeric(0), sd = numeric(0), 
                                  min = numeric(0), max = numeric(0))
for (i in unique(mdata$PHONEID)) {
    tmp <- filter(mdata2, PHONEID==i)
    media <- mean(tmp$value)
    desviacion <- sd(tmp$value)
    max <- max(tmp$value)
    min <- min(tmp$value)
    tmpdf <- data.frame(phone=i, mean=media, sd=desviacion, min=min, max=max)
    estandar_validation <- rbind(estandar_validation, tmpdf)
}
# lo doy por bueno

#------------------------------------------------------------------------------------------------#

# añado columnas que faltan

colwap <- which(startsWith(names(df), "WAP"))
addcols <- df[,-colwap]
df_standarized <- cbind(dftrans, addcols)

# guardo trabajo
saveRDS(object = df_standarized, file = "vd_mobile_std.rds")





