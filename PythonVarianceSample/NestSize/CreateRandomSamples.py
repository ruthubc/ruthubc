'''
Created on Aug 14, 2016

@author: Ruth check
'''


import pandas as pd
from random import uniform
#from scipy.stats import cumfreq
import numpy as np
import os


#os.chdir('C:/Work/Dropbox/')
#os.chdir('G:/Dropbox/RuthEcuador2013/NestSize/')
os.chdir('C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/')

fileOutputName = "bootSamplesDolph_9June.csv"


#ln leg length max and min adults  0.88 and 1.33

# sample size 1-25

minValuePos = 0.88

maxValuePos = 1.33

numSampleSets = 100

maxSmpSz = 26

sampleSizes = range(2, maxSmpSz + 1)

sampleSzLst = sampleSizes * numSampleSets

print sampleSzLst

COUNT = 0

columns = ['SampleID','SampleSize', 'Value']
df = pd.DataFrame(columns = columns)

def createRanSmpFun(smpSize, minVal, maxVal, df, columns):    
    global COUNT
    COUNT = COUNT+1
    sampID = [COUNT] * smpSize
    sampSizeRep = [smpSize] * smpSize
    ranList =[uniform(minVal, maxVal) for p in range(0,(smpSize))] 
       
    dfNew  = pd.DataFrame(np.column_stack((sampID, sampSizeRep, ranList)), columns = columns) 
    
    df = df.append(dfNew)
    return df

    
while (COUNT < ((maxSmpSz -1) * numSampleSets)):
    sampleSize = sampleSzLst[COUNT]
    print 
    print COUNT
    df = createRanSmpFun(sampleSize, minValuePos, maxValuePos, df, columns)
    
df.to_csv(fileOutputName, index=False, encoding='utf-8')

print ("file exported")