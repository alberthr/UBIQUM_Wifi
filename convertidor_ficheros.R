transformo_fichero <- function (df, center, log) {
    
    #---------------------------------------------------------------------------------#
    
    df <- df
    center <- center
    log <- log
    
    namelog <- ifelse (log==T, "log", "nolog")
    namecenter <- ifelse (center==T, "center", "nocenter")
    name_std <- paste0("vd_std_",namelog,"_",namecenter,".rds")
    
    #---------------------------------------------------------------------------------#
    
    # CONVIERTO A LOGARITMICO O NO SEGUN SEA NECESARIO
    
    colwap <- which(startsWith(names(df), "WAP"))
    othercol <- which(names(df) %in% c("PHONEID"))
    df[df==100] <- NA
    if (log==T) df[,colwap] <- log(df[,colwap]+105)
    
    #---------------------------------------------------------------------------------#
    
    # MELT PARA CALCULOS BASICOS
    
    mdata <- melt(df[,c(colwap,othercol)], id.vars = c("PHONEID"))
    mdata <- mdata[complete.cases(mdata),]
    
    #---------------------------------------------------------------------------------#
    
    # CALCULO MAXIMOS Y MINIMOS PARA CADA MODELO DE TELEFONO Y LO ANADO AL DF
    
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
    
    # fusiono df principal con esta informacion y anado un id para orderar siempre
    df$id <- as.numeric(rownames(df))
    dftmp <- merge(x=df, y=estandar, by.x = "PHONEID", by.y = "phone", all = T)
    dftmp <- dftmp[order(dftmp$id),]
    
    # creo df solo con waps para trabajar
    colwap <- which(startsWith(names(dftmp), "WAP"))
    dfwap <- dftmp[,colwap]
    
    #---------------------------------------------------------------------------------#
    
    # ESTANDARIZO SENALES POR TELEFONO 0-1 Y CENTRO O NO SEGUN SEA NECESARIO
    
    # 1) aplico transformaciones para hacerlo 0-1
    dftrans <- (dfwap-dftmp$min)/(dftmp$max-dftmp$min)
    
    # 2) pongo outliers en el limite
    dftrans[dftrans<0] <- 0
    dftrans[dftrans>1] <- 1
    
    # 3) transformo cada telefono al mismo promedio
    if (center==T) {
        if (log==T) {mediatotal <- readRDS("media_log.rds")}
        if (log==F) {mediatotal <- readRDS("media_nolog.rds")}
        dfconvert <- log(mediatotal, dftmp$meanstd)
        dftrans_std <- dftrans^dfconvert
    } else {dftrans_std <- dftrans}
    dftrans_std$PHONEID <- dftmp$PHONEID
    
    #---------------------------------------------------------------------------------#
    
    # CREO DATA FRAME FINAL
    
    # anado columnas que faltan
    
    colwap <- which(startsWith(names(df), "WAP"))
    addcols <- df[,-colwap]
    
    colwap <- which(startsWith(names(dftrans_std), "WAP"))
    df_standarized <- cbind(dftrans_std[,colwap], addcols)
    
    
    # guardo trabajo
    saveRDS(object = df_standarized, file = name_std)
    
    #---------------------------------------------------------------------------------#
}


