# require(gdata)
# df = read.xls ("data/original/IndicadoresDistritos2017.xls", sheet = 1, header = TRUE)

require(xlsx)
df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetName="CENTRO")
df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetIndex=18)
df = ""
# for (i in 2:22) 

for (i in 1:19) {
  df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetIndex=i)
  print(i)
  print(names(df)[1])
}
final <- ""
for (i in 2:22) {
  df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetIndex=i)
  print(paste("i:", i))
  print(names(df)[1])
  
  if ( i ==  19|i == 20 ) { # 2 barrios: vallecas, vicalvaro
    print("2 barrios")
    dfclean <- df[,-c(2:5,10:21)]
    for (j in 2:ncol(dfclean)) {
      dfclean[,j] <- as.character(dfclean[,j])
    }
    for (j in c(3,5)) {
      dfclean[1,j] <- dfclean[1,j-1]
    }
    for (j in c(2,4)) {
      dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
    }
    
    names(dfclean) <- dfclean[1,]
    
    tdfclean2 <- data.frame(t(dfclean[1:368,]))
    if ( i == 2 ) {
      final  <- tdfclean2
    } else if ( !i == 2 ){
      final  <- rbind(final,tdfclean2)
    }
  } else if ( i ==  17|i == 22) { # 5 barrios: hortaleza, barajas
    print("5 barrios")
    dfclean <- df[,-c(2:5,16:22)]
    for (j in 2:ncol(dfclean)) {
      dfclean[,j] <- as.character(dfclean[,j])
    }
    for (j in c(3,5,7,9,11)) {
      dfclean[1,j] <- dfclean[1,j-1]
    }
    for (j in c(2,4,6,8,10)) {
      dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
    }
    
    names(dfclean) <- dfclean[1,]
    
    tdfclean5 <- data.frame(t(dfclean[1:368,]))
    if ( i == 2 ) {
      final  <- tdfclean5
    } else if ( !i == 2 ){
      final  <- rbind(final,tdfclean5)
    }
  } else if ( i == 2 | i == 4 | i == 5 | i == 6  | i == 7| i == 8 | i == 14| i == 15| i == 16  ) { # 6 barrios Centro, Retiro
    print("6 barrios")
    dfclean <- df[,-c(2:5,18:24)]
    for (j in 2:ncol(dfclean)) {
      dfclean[,j] <- as.character(dfclean[,j])
    }
    for (j in c(3,5,7,9,11,13)) {
      dfclean[1,j] <- dfclean[1,j-1]
    }
    for (j in c(2,4,6,8,10,12)) {
      dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
    }

    names(dfclean) <- dfclean[1,]

    tdfclean6 <- data.frame(t(dfclean[1:368,]))
    if ( i == 2 ) {
      final  <- tdfclean6
    } else if ( !i == 2 ){
      final  <- rbind(final,tdfclean6)
    }
  } else if (i == 3 | i == 10| i == 11| i == 12| i == 13| i == 17) { 
    # Arganzuela, moncloa, latina, carabanchel,usersa
      print("7 barrios")
      dfclean <- df[,-c(2:5,20:23)]
      for (j in 2:ncol(dfclean)) {
        dfclean[,j] <- as.character(dfclean[,j])
      }
      for (j in c(3,5,7,9,11,13,15)) {
        dfclean[1,j] <- dfclean[1,j-1]
      }
      for (j in c(2,4,6,8,10,12,14)) {
        dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
      }

      names(dfclean) <- dfclean[1,]

      tdfclean7 <- data.frame(t(dfclean[1:368,]))
      final <- rbind(final,tdfclean7)
  } else if (i == 9|i == 21) { # 8 barrios. fuencarral el pardo, san blas
    print("8 barrios")
    dfclean <- df[,-c(2:5,22:25)]
    for (j in 2:ncol(dfclean)) {
      dfclean[,j] <- as.character(dfclean[,j])
    }
    for (j in c(3,5,7,9,11,13,15,17)) {
      dfclean[1,j] <- dfclean[1,j-1]
    }
    for (j in c(2,4,6,8,10,12,14,16)) {
      dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
    }
    
    names(dfclean) <- dfclean[1,]
    
    tdfclean8 <- data.frame(t(dfclean[1:368,]))
    final <- rbind(final,tdfclean8)
  } else if (i == 16) { # ciudad lineal
    print("9 barrios")
    # df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetIndex=i)
    dfclean <- df[,-c(2:5,24:27)]
    for (j in 2:ncol(dfclean)) {
      dfclean[,j] <- as.character(dfclean[,j])
    }
    for (j in c(3,5,7,9,11,13,15,17,19)) {
      dfclean[1,j] <- dfclean[1,j-1]
    }
    for (j in c(2,4,6,8,10,12,14,16,18,19)) {
      dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
    }
    
    names(dfclean) <- dfclean[1,]
    
    tdfclean10 <- data.frame(t(dfclean[1:368,]))
    final <- rbind(final,tdfclean10)
  }
}

finaltable <- as.matrix(final)

finalmatrix <- as.data.frame(finaltable)

finalselect <- finalmatrix[,c(33,34,233:257)]

write.csv(finalselect, file = "data/output/madrid-variables-barrios.csv")
# names(df[1])
# row.names(df) <- df$X01..CENTRO..INFORMACIÓN.DE.DISTRITO.Y.BARRIOS

a <- 1:nrow(finalselect)
b <- a[seq(1, length(a), 2)]

finalselect[- grep("VIVIENDA", finalselect$X233),3]$X233[1:20]

finalselect[b,]
  # ------------------------
df = read.xlsx("data/original/IndicadoresDistritos2017.xls", sheetIndex=19)
dfclean <- df[,-c(2:5,24:27)]
for (j in 2:ncol(dfclean)) {
  dfclean[,j] <- as.character(dfclean[,j])
}
for (j in c(3,5,7,9,11,13,15,17,19)) {
  dfclean[1,j] <- dfclean[1,j-1]
}
for (j in c(2,4,6,8,10,12,14,16,18,19)) {
  dfclean[1,j] <- paste(dfclean[1,j],"_perc",sep="")
}

names(dfclean) <- dfclean[1,]

tdfclean10 <- data.frame(t(dfclean[1:368,]))
final <- rbind(final,tdfclean10)  
  
dfclean <- df[,-c(2:5,18:24)]
# dftemp <- as.data.frame(t(dfclean))
# row.names(dftemp ) <-
for (i in 2:ncol(dfclean)) {
  dfclean[,i] <- as.character(dfclean[,i])
}
for (i in c(3,5,7,9,11,13)) {
  dfclean[1,i] <- dfclean[1,i-1]
}
for (i in c(2,4,6,8,10,12)) {
  dfclean[1,i] <- paste(dfclean[1,i],"_perc",sep="")
}

names(dfclean) <- dfclean[1,]

tdfclean <- data.frame(t(dfclean))
# colnames(tdfclean) <- as.list(tdfclean[1,])
# 
# madrid <- tdfclean[2:13,]
# names(madrid) <- tdfclean[2,3]


colnames(tdfclean)
row.names(tdfclean)
colnames(tdfclean)<- tdfclean[1,]
dfclean <- dfclean[c(3,4),]
