'''
Created on Aug 14, 2016

@author: Ruth
'''

import pandas as pd
from random import uniform
from scipy.stats import cumfreq
import numpy as np

pd.options.mode.chained_assignment = None  # default='warn'


numBoots = 1000
numBins = 100

spiders = pd.read_csv('G:/Dropbox/spidersAverage.csv')
spiders['bootSD_cond'] = 1000.00000

numRows = len(spiders)

def randomSDFun (ranListSum, maxRanSumPos,  min_Cond , max_Cond, N_input):
    
    while  ranListSum > maxRanSumPos:
        ranList =[uniform(min_Cond,max_Cond) for p in range(0,N_input)]
        ranListSum = sum(ranList)
    
    diff = sum_data - ranListSum
    ranList.append(diff)
    ranSd = np.std(ranList, ddof=1)

    return ranSd

for rowNum in range(numRows):
    print "\nRun Number", rowNum+1, "/", numRows
    print spiders.NestID[rowNum], spiders.Instar[rowNum]
    data_mean =  spiders.mean_data[rowNum]
    N = spiders.N[rowNum]
    sum_data =  spiders.sum_data[rowNum]
    sd_data = spiders.sd_data[rowNum]
    N_input = N - 1
    min_Cond = spiders.minCond[rowNum]
    max_Cond = spiders.maxCond[rowNum]
    maxRanSumPos = sum_data - min_Cond
    
    # getting the random sample
    ranListSum = 100 # ensures it isabove maxRanSumPoss
    sd_list = []
    
    while len(sd_list) < numBoots:
        sdOutput = randomSDFun (ranListSum, maxRanSumPos,  min_Cond , max_Cond, N_input)
        sd_list.append(sdOutput)
        
    minList=  min(sd_list)
    maxList=  max(sd_list)
    cumFreq = cumfreq(sd_list, numBins, defaultreallimits=(minList, maxList))
    countValues =  cumFreq[0]
    freq_interval = cumFreq[2]
    result = (sd_data -minList) / freq_interval
    myIndex = int(round(result)) -1

    if myIndex < 0:
        output = 0.00
    else:
        myLength = len(sd_list)
        output = countValues[myIndex] / myLength 

    print "sample variance:", output
    spiders.bootSD_cond[rowNum] = output
    

spiders.to_csv("G:/Dropbox/bootSampCondPython.csv")

print "Finished - file exported"


