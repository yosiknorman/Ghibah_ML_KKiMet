library(openxlsx)
rm(list = ls())

# fullData = fullDatax[-Non_NA, 1:8]

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
# maxmindf <- as.data.frame(sapply(fullData, normalize))
# 
# minvec <- sapply(fullData,min)
# maxvec <- sapply(fullData,max)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}

SYNOP = read.xlsx("data/synop_priok.xlsx")
TS = as.character(as.matrix(SYNOP$DATA.TIMESTAMP))
TS_UTC = as.POSIXct(paste0(substr(TS,1,18), "1"), tz = "UTC")
TS_UTC_ALL  = seq(TS_UTC[1], TS_UTC[length(TS_UTC)],by = "hour")
# "3HOURLY PRECIPITATION"
DF = data.frame(TS_UTC = TS_UTC,SYNOP[,3:ncol(SYNOP)], stringsAsFactors = F)

ada = which(!TS_UTC_ALL %in% TS_UTC)
TS_UTC_ALL[ada]
length(TS_UTC)
length(TS_UTC_ALL)

HH3 = c(0, 3, 6, 9,12, 15, 18, 21)
JAMS = substr(TS_UTC, 12, 13)
iJAMS3 = list()

PER3JAM = list()
for(i in 1:length(HH3)){
  iJAMS3[[i]] =  which(as.integer(JAMS) == HH3[i])
  PER3JAM[[i]] = DF[iJAMS3[[i]],]
}
PER3JAM = do.call(rbind, PER3JAM)
PER3JAM = PER3JAM[order(PER3JAM$TS_UTC),]
PER3JAM$RAINFALL.LAST.MM[is.na(PER3JAM$RAINFALL.LAST.MM)] = 9999
PER3JAM$RAINFALL.LAST.MM[PER3JAM$RAINFALL.LAST.MM == 8888] = 0
PER3JAMs = PER3JAM[PER3JAM$RAINFALL.LAST.MM != 9999,] 
PER3JAMs = PER3JAMs[-which(is.na(PER3JAMs), arr.ind = T)[,1],]

fullData = PER3JAMs[1:100,2:ncol(PER3JAMs)]
maxmindf <- as.data.frame(sapply(fullData, normalize))

minvec <- sapply(fullData,min)
maxvec <- sapply(fullData,max)
denormalize <- function(x,minval,maxval) {
  x*(maxval-minval) + minval
}


PANJANG = 1:as.integer(quantile(1:nrow(maxmindf), 0.8)  )
TRAIN = maxmindf[PANJANG,]
TEST = maxmindf[-PANJANG,]

library(neuralnet)
NN = neuralnet(RAINFALL.LAST.MM~PRESSURE.QFE.MB.DERIVED+PRESSURE.QFF.MB.DERIVED+RELATIVE.HUMIDITY.PC+TEMP.DRYBULB.C.TTTTTT, 
               data = TRAIN, hidden = c(2, 5, 3), linear.output = TRUE)

softplus <- function(x){
  1/(log(1 + exp(x)))
} 
NN <- neuralnet(RAINFALL.LAST.MM~PRESSURE.QFE.MB.DERIVED+PRESSURE.QFF.MB.DERIVED+RELATIVE.HUMIDITY.PC+TEMP.DRYBULB.C.TTTTTT, 
                data = TRAIN, 
                linear.output = T, hidden = c(3, 2), act.fct = "logistic")
#
# dim(NN$covariate)

HA = compute(NN, TEST[,-1])


fullDataTRAIN = fullData[PANJANG,]
fullDataTEST = fullData[-PANJANG,]
minvec <- sapply(fullDataTRAIN,min)
maxvec <- sapply(fullDataTRAIN,max)
BALIK = denormalize(HA$net.result, minvec[1], maxvec[1])
# BALIK = data.frame(do.call(cbind, hasi), stringsAsFactors = F)
# colnames(BALIK) = colnames(fullData)

BALIK[BALIK < 0] = 0
BAIK = data.frame(OBS = TEST[,1], PRED = HA$net.result, ASLIOBS = fullDataTEST$RAINFALL.LAST.MM, 
                  ASLIPRED = BALIK)

