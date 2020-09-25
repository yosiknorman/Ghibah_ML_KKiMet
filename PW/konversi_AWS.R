#!/usr/local/bin/Rscript
library(openxlsx)

rm(list = ls())
AWS = read.xlsx("data/awsfull.xlsx", sheet = 1)
TIME = as.character(as.POSIXct(convertToDateTime(AWS$waktu+AWS$X2) )+1)

Rain = AWS$rain
TIME_DAY = (substr(TIME, 1, 10))
UTIME_DAY = unique(TIME_DAY)

PER_HARI = function(x){
  Dnow = which(TIME_DAY == UTIME_DAY[x])
  if(substr(TIME[Dnow[1]], 12,16) == "00:00"){
    Dnow = Dnow[2:length(Dnow)]
  }
  if(any(TIME == as.character(as.POSIXct(paste0(UTIME_DAY[x]," 00:00:01" ))+(3600*24)))){
    if(as.POSIXct(TIME[Dnow[length(Dnow)]])+(10*60) == as.POSIXct(paste0(UTIME_DAY[x]," 00:00:01" ))+(3600*24)){
      contd = which(TIME == as.character(as.POSIXct(paste0(UTIME_DAY[x]," 00:00:01" ))+(3600*24)))
      IHARIAN = c(Dnow,contd) 
    }else{
      IHARIAN = Dnow
    }
  }else{
    IHARIAN = Dnow
  }
  AWSRAIN = as.numeric(AWS$rain[IHARIAN])
  # length(AWSRAIN) #140
  AWSRAINREV = rev(AWSRAIN)
  
  raindif = c()
  for(i in 1:(length(AWSRAINREV)-1)){
    raindif[i] = AWSRAINREV[i] - AWSRAINREV[i+1]
  }
  raindif = rev(raindif)
  if(raindif[1] == 0){
    raindif = c(0, raindif)
  }else{
    raindif = c((raindif[1]/2), raindif)
  }
  TIMERAIN = TIME[IHARIAN]
  return(data.frame(IHARIAN = IHARIAN, 
                    TIMERAIN = TIMERAIN,
                    AWSRAIN = AWSRAIN,
                    RAINDIF = raindif, 
                    stringsAsFactors = F
                    )
         )
}

PreProc = list()
for(i in 1:length(UTIME_DAY)){
  PreProc[[i]] = PER_HARI(i)
}
PreProcRR = do.call(rbind,PreProc)
RH = as.numeric(AWS$rh[PreProcRR$IHARIAN])
Temp = as.numeric(AWS$temp[PreProcRR$IHARIAN])
Press = as.numeric(AWS$pressure[PreProcRR$IHARIAN])
AWSQC = data.frame(PreProcRR, RH = RH, Temp = Temp, Press = Press, stringsAsFactors = F)
TIME_hour = substr(AWSQC$TIMERAIN, 1, 13)
U_TIME_hour = unique(TIME_hour)

How_toTrainDragon = function(PARAM, TIMETIME){
  if(any(AWSQC$TIMERAIN == paste0(TIMETIME, ":00:01"))){
    Temps = AWSQC[AWSQC$TIMERAIN == paste0(TIMETIME, ":00:01"), PARAM]
  }else{
    RENTANG = as.character(seq(as.POSIXct(paste0(TIMETIME, ":00:01"))-(10*60),
                               as.POSIXct(paste0(TIMETIME, ":00:01"))+(10*60), 
                               by = "10 min"))
    
    if(any(AWSQC$TIMERAIN %in% RENTANG)){
      ADA_RENTANG = which(AWSQC$TIMERAIN %in% RENTANG)
      Temps = AWSQC[ADA_RENTANG[1], PARAM]
    }else{
      Temps = 9999
    }
  }
  return(Temps)
}

iTH = list()
RAINHOUR = c()
Temp_hour = c()
Press_hour = c()
RH_hour = c()
for(i in 1:length(U_TIME_hour)){
  iTH[[i]] = which(TIME_hour == U_TIME_hour[i])
  RAINHOUR[i] = sum(AWSQC$RAINDIF[iTH[[i]]])
  RH_hour[i] = How_toTrainDragon(5, TIMETIME = U_TIME_hour[i])
  Temp_hour[i] = How_toTrainDragon(6, TIMETIME = U_TIME_hour[i])
  Press_hour[i] = How_toTrainDragon(7, TIMETIME = U_TIME_hour[i])
}
AWS_HOURLY = data.frame(TIME = U_TIME_hour, RAINHOUR = RAINHOUR, 
                        RH_hour = RH_hour, Temp_hour = Temp_hour, 
                        Press_hour = Press_hour,stringsAsFactors = F)

PER3 = c(0, 3, 6,9,12,15,18,21)
as.POSIXct(paste0(AWS_HOURLY$TIME[1], ":00:01"), 
           paste0(AWS_HOURLY$TIME[nrow(AWS_HOURLY)], ":00:01"),
           by = ""
           )

jam3 = function(x){
  res = AWS_HOURLY[which(as.integer(substr(AWS_HOURLY$TIME, 12, 13)) == PER3[x]),]
  return(res)
}

DG_WKT = as.POSIXct(paste0(AWS_HOURLY$TIME, ":00:01"), tz = "UTC")
RAINPER3JAM = function(DG_WKTx){
  lastT = which(substr(AWS_HOURLY$TIME, 12, 13) == DG_WKTx)
  fromT = lastT-2
  # cx = 2
  R3JAM = c()
  WAKTU3JAM = c()
  for(cx in 1:length(lastT)){
    NOTASI = fromT[cx]:lastT[cx]
    if(lastT[cx] == 1){
      R3JAM[cx] = AWS_HOURLY[1,2]
      WAKTU3JAM[cx] = AWS_HOURLY[1,1]
    }else{
      TIMM = paste0(AWS_HOURLY$TIME[NOTASI], ":00:01")
      SEQ_TIM = as.character(seq(as.POSIXct(paste0(AWS_HOURLY$TIME[lastT[cx]], ":00:01"))-(3600*2), 
                                 as.POSIXct(paste0(AWS_HOURLY$TIME[lastT[cx]], ":00:01")), 
                                 by = "hour"))
      if(any(TIMM == SEQ_TIM)){
        ID_TRUE = NOTASI[TIMM %in% SEQ_TIM]
        R3JAM[cx] = sum(AWS_HOURLY[ID_TRUE,2]) # TAMBAH BULET
        WAKTU3JAM[cx] = AWS_HOURLY[lastT[cx],1]
      }else{
        R3JAM[cx] = 9999
        WAKTU3JAM[cx] = AWS_HOURLY[lastT[cx],1]
      }
    }
  }
  return(data.frame(WAKTU3JAM = WAKTU3JAM, 
                    R3JAM = R3JAM,stringsAsFactors = F))
}
RATIME = c("00", "03", "06", "09","12", "15", "18", "21") # GANTI
RARA3 = list()
for(i in 1:length(RATIME)){
  RARA3[[i]] = RAINPER3JAM( DG_WKTx = RATIME[i])
}
RARA3 = do.call(rbind, RARA3)

PER3jam = list()
for(i in 1:length(PER3)){
  PER3jam[[i]] = jam3(i)
}
PER3jamx = do.call(rbind,PER3jam)

Time_nonR = PER3jamx$TIME
for(i in 1:length(Time_nonR)){
  PER3jamx$RAINHOUR[i] = RARA3$R3JAM[RARA3$WAKTU3JAM == Time_nonR[i]]
}
PER3jamx$TIME = as.POSIXct(paste0(PER3jamx$TIME,":00:01"), tz = "UTC")
FINAL <- PER3jamx[order(PER3jamx$TIME),]

write.csv2(FINAL, file = "data/AWS3Hourly.csv", row.names = F)
