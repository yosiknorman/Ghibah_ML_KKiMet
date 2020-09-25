#!/usr/bin/python
import os
import pandas as pd
import numpy as np
from datetime import date, time, datetime, timedelta
from array import *
from itertools import chain
import csv
print("jalan")
prf_data = "data/synop_priok.xlsx"
#os.chdir("/media/yosik/My Passport/YSKNRMN_inc/Prada/")
#os.getcwd()
#os.listdir("data/")

df = pd.read_excel(prf_data, sheet_name=1) 
NAMA = np.array(df.columns)
j  = list(np.r_[1:len(NAMA)])
NAMA = list(NAMA[j])

MM = np.asmatrix(df)
DTTGL = MM[:,1]

# MAKE EPOCH
HA = []
TAHUN = [] 
BULAN = []
TANGGAL = []
JAM = []
MENIT = []
EP_TIME = []
for i in range(len(DTTGL)):
    HA.append(str(DTTGL[i])[4:(len(str(DTTGL[i]))-11)])
    #    HA.array
    
HA = np.array(HA)
for i in range(len(DTTGL)):
    TAHUN.append(int(HA[i][0:4]))
    BULAN.append(int(HA[i][5:7]))
    TANGGAL.append(int(HA[i][8:11]))
    JAM.append(int(HA[i][11:13]))
    MENIT.append(int(HA[i][14:16]))
    EP_TIME.append(pd.datetime(TAHUN[i],BULAN[i],TANGGAL[i], JAM[i], MENIT[i]))
    #EP_TIME.append(datetime.isoformat(EP_TIME[i]))
    
EP_TIME1 = range(len(EP_TIME))
for i in range(len(EP_TIME)):
    EP_TIME1[i] = (datetime.isoformat(EP_TIME[i]))
    
START_TAHUN = str(TAHUN[0])
FINISH_TAHUN = str(TAHUN[len(TAHUN)-1])
START_BULAN = str(BULAN[0])
FINISH_BULAN = str(BULAN[len(BULAN)-1])
START_TANGGAL = str(TANGGAL[0])
FINISH_TANGGAL = str(TANGGAL[len(TANGGAL)-1])

HH_Freq = pd.date_range(start=START_TAHUN+"-"+START_BULAN+"-"+START_TANGGAL,
                   end=FINISH_TAHUN+"-"+FINISH_BULAN+"-"+FINISH_TANGGAL, freq = "H")
HH_Freq1 = range(len(HH_Freq))
for i in range(len(HH_Freq)):
    HH_Freq1[i] = (datetime.isoformat(HH_Freq[i]))

HASIL = range(len(HH_Freq1))
MOMO = MM[:,2:(MM.shape[1])]
ALL = np.zeros( (len(HH_Freq1), MOMO.shape[1] ))


#for i in range(10):
for i in range(len(HH_Freq1)):
    HASIL[i] = (HH_Freq1[i] in EP_TIME1)     
    if HASIL[i]:
        id = EP_TIME1.index(HH_Freq1[i])
        ALL[i,:] = MOMO[id,:]
    else:
        ALL[i,:] = np.repeat("nan",MOMO.shape[1])
        
ALL_WITHEPOCH = np.column_stack((HH_Freq1, ALL))
H= list(ALL_WITHEPOCH [:, 2].astype(float)*10)
YANG_NAN = list(np.argwhere(np.isnan(H)).flatten())
PDDF = pd.DataFrame(ALL_WITHEPOCH ,columns = NAMA)

df.to_csv('PDDF.csv', index=False, header=True, sep=';')