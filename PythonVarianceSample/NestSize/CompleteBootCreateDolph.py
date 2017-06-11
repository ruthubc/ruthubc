'''
Created on Aug 14, 2016

@author: Ruth check
'''



import pandas as pd
from random import uniform
import time
import numpy as np
import os
from scipy.stats import cumfreq


#os.chdir('C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/')

os.chdir('G:/Dropbox/RuthEcuador2013/NestSize/')

inputFileName = "Summary_bootSamplesDolph_9June.csv"


fileOutputName = "Dolph_bootVars.csv"


numBoots = 10000
numBins = 1000

time_lim = 600


randSamps = pd.read_csv(inputFileName)



numRows = len(randSamps)

minValPos = 0.88

maxValPos = 1.33



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
    


def randomSDFun (totalAim, maxValuePos, minValuePos, sampleN, time_lim_input): # returns standard dev of random sample
    
    diff = 1000 # setting diff negative to start the while loop
    
    while_start_time = time.time()
    while_time = 0
    i = 0
    
    while (diff > maxValuePos or diff < minValuePos) and while_time < time_lim_input:
        ranList =[uniform(minValuePos, maxValuePos) for p in range(0,(sampleN-1))] # getting the random sample correct size -1
        ranListSum = sum(ranList)
        diff = sum_data - ranListSum # working out what the last remaining number should be
        while_time = loop_time = time.time() - while_start_time
        i = i+1

    
    if while_time < time_lim_input: 
        ranList.append(diff) # if diff takes on an allowable vaule it is added to the list        
        valueCheck(totalAim, ranList, maxValuePos, minValuePos)     # checking values are correct  
        ranSd = np.std(ranList, ddof=1)
    else:
        #print "while loop did not run in time"
        print "num while loops =", i, "while_time:", while_time
        ranSd = "NA"

    return ranSd

for rowNum in range(numRows): # getting the necessary varibles from the data file

    print "\nRun Number", rowNum+1, "/", numRows
    print "SampleID", randSamps.SampleID[rowNum]
    
    data_mean =  randSamps.data_mean[rowNum]
    N = randSamps.N[rowNum]
    sum_data =  randSamps.data_sum[rowNum]
    sd_data = randSamps.data_sd[rowNum]
    sampleID = randSamps.SampleID[rowNum]
    

    
    # getting the random sample
    sd_list = []    
    
    start_time = time.time()
    loop_time = 0
    while len(sd_list) < numBoots and loop_time < time_lim:
         
        loop_time = time.time() - start_time       
        sdOutput = randomSDFun(sum_data, maxValPos, minValPos, N, time_lim)
        
        if sdOutput == "NA":
            print "randomSDFun while loop did not run in time"            
        else:
            sd_list.append(sdOutput)
                   
        
    print("--- %s seconds ---" % (time.time() - start_time))
    
    if len(sd_list) == numBoots:
  
                
        minList=  min(sd_list)
        maxList=  max(sd_list)

        cumFreq = cumfreq(sd_list, numBins, defaultreallimits=(minList, maxList))
        lowerLimit = cumFreq[1]
        countValues =  cumFreq[0]
        freq_interval = cumFreq[2]
        upperLimit = lowerLimit + (freq_interval*numBins)
        xaxis = np.arange(lowerLimit, (upperLimit), freq_interval)
        
        if len(xaxis) > numBins:
            print "xaxis too long, length:", len(xaxis)
            del_index = numBins
            xaxis = np.delete(xaxis, del_index)
        
        

        result = (sd_data -minList) / freq_interval
        myIndex = int(round(result)) -1
        print "myIndex:", myIndex
        
        print countValues
        
        if myIndex < 0:
            output = 0.00
        elif myIndex > numBins -1:
            output = numBins -1       
        else:            
            myLength = len(sd_list)
            output = countValues[myIndex] / myLength 

        print "sample variance:", output
        randSamps.bootSD_var[rowNum] = output
        randSamps.num_boots[rowNum] = numBoots
        randSamps.myIndex[rowNum] = myIndex
    

            
    else:
        print "did not run in time"
        
    

randSamps.to_csv(fileOutputName)

print "Finished - file exported"


