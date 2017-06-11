'''
Created on Aug 14, 2016

@author: Ruth check
'''



import pandas as pd
from random import uniform

import numpy as np
import os


os.chdir('C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/')

inputFileName = "Summary_bootSamplesDolph_9June.csv"

fileOutputName = "Dolph_bootVars.csv"

numBoots = 10000
numBins = 1000


columns = ['SampleID','SampleSize', 'boot_SD']
df = pd.DataFrame(columns)

df = df.transpose()
print df
df.to_csv(fileOutputName, mode='a', header=False)

randSamps = pd.read_csv(inputFileName)



print randSamps[:5]

time_lim = 600

numRows = len(randSamps)

minValPos = 0.88

maxValPos = 1.33

columns = ['SampleID','SampleSize', 'bootSD']
dfColNames = pd.DataFrame(columns = columns)

dfColNames.to_csv(fileOutputName, mode='a', header=False)


def valueCheck (total_aim, sample_list, maxPos, minPos):
    samp_tot = sum(sample_list)
    numMax = sum(x > maxPos for x in sample_list)
    numMin = sum(x < minPos for x in sample_list)
    tot_diff = abs(samp_tot - total_aim)
    totWrong = numMax + numMin
    if tot_diff > 0.0001:
        print "total difference:", tot_diff
        raise Exception('sample total is different from data total')
    elif totWrong > 0:
        print "max value possible = ", maxPos
        print "min value possible ", minPos
        print sample_list
        raise Exception('wrong values in sample')
    


def randomSDFun (totalAim, maxValuePos, minValuePos, sampleN): # returns standard dev of random sample
    
    diff = 1000 # setting diff negative to start the while loop
    
    while (diff > maxValuePos or diff < minValuePos):
        ranList =[uniform(minValuePos, maxValuePos) for p in range(0,(sampleN-1))] # getting the random sample correct size -1
        ranListSum = sum(ranList)
        diff = sum_data - ranListSum # working out what the last remaining number should be
        #print diff
        
    ranList.append(diff) # if diff takes on an allowable vaule it is added to the list        
    valueCheck(totalAim, ranList, maxValuePos, minValuePos)     # checking values are correct  
    
    boot_sd = np.std(ranList)
    
    outputList = [sampleN, boot_sd]

    return outputList



for rowNum in range(numRows): # getting the necessary varibles from the data file
    print "\nRun Number", rowNum+1, "/", numRows
    print "SampleID", randSamps.SampleID[rowNum]
    
    data_mean =  randSamps.data_mean[rowNum]
    N = randSamps.N[rowNum]
    sum_data =  randSamps.data_sum[rowNum]
    sd_data = randSamps.data_sd[rowNum]
    sampleID = randSamps.SampleID[rowNum]

    
    print "SampleSize:", N
    print "sum:", sum_data
    
    runCt = 0
    while (runCt < numBoots):
        sdOutput = randomSDFun(sum_data, maxValPos, minValPos, N)
        sdOutput = [sampleID] + sdOutput
        dfOutput = pd.DataFrame(sdOutput)
        dfOutput = dfOutput.transpose()
        #print dfOutput
        dfOutput.to_csv(fileOutputName, mode='a', header=False)
        runCt = runCt + 1

    
   
print "Finished - file exported"


