'''
Created on Aug 14, 2016

@author: Ruth
'''

import pandas as pd
from random import uniform
from scipy.stats import cumfreq
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import savefig

pd.options.mode.chained_assignment = None  # default='warn'


numBoots = 500
numBins = 10

spiders = pd.read_csv('G:/Dropbox/BootAve.csv')
spiders['bootSD_cond'] = 1000.00000 # puts this as the default boot sd in the file
numRows = len(spiders)
numRows = 1

def randomSDFun (totalAim, maxValuePos, minValuePos, sampleN): # returns standard dev of random sample
    
    diff = -100 # setting diff negative to start the while loop
    
    while diff < minValuePos or diff > maxValuePos:
        ranList =[uniform(minValuePos, maxValuePos) for p in range(0,(sampleN-1))] # getting the random sample correct size -1
        ranListSum = sum(ranList)
        diff = sum_data - ranListSum # working out what the last remaining number should be
    
    
    ranList.append(diff) # if diff takes on an allowable vaule it is added to the list
    ranSd = np.std(ranList, ddof=1)

    return ranSd

for rowNum in range(numRows): # getting the necessary varibles from the data file
    print "\nRun Number", rowNum+1, "/", numRows
    #http://www.pythonforbeginners.com/concatenation/string-concatenation-and-formatting-in-python
    print spiders.NestID[rowNum], spiders.Instar[rowNum]
    
    data_mean =  spiders.mean_data[rowNum]
    N = spiders.N[rowNum]
    sum_data =  spiders.sum_data[rowNum]
    sd_data = spiders.sd_data[rowNum]
    min_Cond = spiders.minCond[rowNum]
    max_Cond = spiders.maxCond[rowNum]
    
    # getting the random sample
    sd_list = []
    
    while len(sd_list) < numBoots:
        sdOutput = randomSDFun(sum_data, max_Cond, min_Cond, N)
        sd_list.append(sdOutput)
        
    minList=  min(sd_list)
    maxList=  max(sd_list)

    cumFreq = cumfreq(sd_list, numBins, defaultreallimits=(minList, maxList))
    lowerLimit = cumFreq[1]
    countValues =  cumFreq[0]
    freq_interval = cumFreq[2]
    upperLimit = lowerLimit + (freq_interval*numBins)
    xaxis = np.arange(lowerLimit, upperLimit, freq_interval)
    
    print "cumfreq length", len(countValues)
    print "length xaxis", len(xaxis)
    
    plot_title = 
    
    plt.plot(xaxis, cumFreq[0])
   # plt.annotate("some text")
    plt.title("title")
    #plt.show()
    plt.savefig("G:/Dropbox/fig.png")
    
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


