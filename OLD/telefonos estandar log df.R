library(tidyverse)
library(rpart)
library(caret)
library(Metrics)
library(reshape)


#------------------------------------------------------------------------------------------------#

# ESTRUCTURA DE  WAPS

df <- readRDS("cleandf.rds")

colwap <- which(startsWith(names(df), "WAP"))
othercol <- which(names(df) %in% c("BUILDINGID","FLOOR", "PHONEID"))
df[df==100] <- NA
df[,colwap] <- log(df[,colwap]+105)
saveRDS(df, "df_log.rds")

mdata <- melt(df[,c(colwap,othercol)], id.vars = c("BUILDINGID","FLOOR", "PHONEID"))
mdata <- mdata[complete.cases(mdata),]

sapply(unique(df$PHONEID), function(x) boxplot(mdata$value[mdata$PHONEID==x], main=x))
# tiene sentido estandarizar las señales por telefono

#------------------------------------------------------------------------------------------------#

# ESTANDARIZO SEÑALES POR TELEFONO 0-1

# creo matriz con informacion para conversiones
estandar <- data.frame(phone = numeric(0), mean = numeric(0), sd = numeric(0), 
                       min = numeric(0), max = numeric(0))
for (i in unique(mdata$PHONEID)) {
    tmp <- filter(mdata, PHONEID==i)
    boxstats <- boxplot(tmp$value)$stats
    median <- boxstats[3,1]
    max <- boxstats[5,1]
    min <- boxstats[1,1]
    tmpdf <- data.frame(phone=i, median=median, min=min, max=max)
    estandar <- rbind(estandar, tmpdf)
}
estandar$meanstd <- (estandar$median-estandar$min)/(estandar$max-estandar$min)


# fusiono df principal con esta información y añado un id para orderar siempre
df$id <- as.numeric(rownames(df))
dftmp <- merge(x=df, y=estandar, by.x = "PHONEID", by.y = "phone", all = T)
dftmp <- dftmp[order(dftmp$id),]


# creo df solo con waps para trabajar
colwap <- which(startsWith(names(dftmp), "WAP"))
dfwap <- dftmp[,colwap]

# 1) aplico transformaciones para hacerlo 0-1
# 2) pongo outliers en el limite
# 3) calculo el promedio del total muestra 
# 4) transformo cada telefono al mismo promedio
# 5) ordeno el df
dftrans <- (dfwap-dftmp$min)/(dftmp$max-dftmp$min)
dftrans[dftrans<0] <- 0
dftrans[dftrans>1] <- 1
mediatotal <- sum(dftrans, na.rm = T)/sum(!is.na(dftrans))
dfconvert <- log(mediatotal, dftmp$meanstd)
#dftrans_std <- dftrans^dfconvert
dftrans_std <- dftrans
dftrans_std$PHONEID <- dftmp$PHONEID
#dftrans_std$id <- as.numeric(rownames(dftrans_std))
#dftrans_std <- dftrans_std[order(dftrans_std$id),]


#------------------------------------------------------------------------------------------------#

# VALIDATION DEL ULTIMO PUNTO

borro <- which(names(dftrans_std) %in% "id")
mdata2 <- melt(dftrans_std[,-borro], id.vars = c("PHONEID"))
mdata2 <- mdata2[complete.cases(mdata2),]

estandar_validation <- data.frame(phone = numeric(0), mean = numeric(0), 
                                  min = numeric(0), max = numeric(0))
for (i in unique(mdata2$PHONEID)) {
    tmp <- filter(mdata2, PHONEID==i)
    media <- mean(tmp$value)
    desviacion <- sd(tmp$value)
    max <- max(tmp$value)
    min <- min(tmp$value)
    tmpdf <- data.frame(phone=i, mean=media, sd=desviacion, min=min, max=max)
    estandar_validation <- rbind(estandar_validation, tmpdf)
}

sapply(unique(mdata2$PHONEID), function(x) boxplot(mdata2$value[mdata2$PHONEID==x], main=x))
# lo doy por bueno

#------------------------------------------------------------------------------------------------#

# añado columnas que faltan

colwap <- which(startsWith(names(df), "WAP"))
addcols <- df[,-colwap]

colwap <- which(startsWith(names(dftrans_std), "WAP"))
df_standarized <- cbind(dftrans_std[,colwap], addcols)

# guardo trabajo
saveRDS(object = df_standarized, file = "df_mobile_log_std.rds")
saveRDS(object = mediatotal, file = "mediatotal_log.rds")




