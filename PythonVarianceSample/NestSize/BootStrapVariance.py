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
import os
import time

os.chdir('C:/Work/Dropbox/')
#os.chdir('G:/Dropbox/')


fileOutputName = "bootSampCondPython.csv"

numBoots = 1000
numBins = 100
spiders = pd.read_csv('spidersAverageMul.csv')
spiders['bootSD_var'] = 1000.00000 # puts this as the default boot sd in the file
spiders['num_boots'] = 0.00
numRows = len(spiders)
time_lim = 200

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

    graphTitle = str(spiders.NestID[rowNum]) + str(spiders.Instar[rowNum])
    print spiders.NestID[rowNum], spiders.Instar[rowNum]
    
    data_mean =  spiders.mean_data[rowNum]
    N = spiders.N[rowNum]
    sum_data =  spiders.sum_data[rowNum]
    sd_data = spiders.sd_data[rowNum]
    min_var = spiders.minVar[rowNum]
    max_var = spiders.maxVar[rowNum]
    
    # getting the random sample
    sd_list = []
    
    
    start_time = time.time()
    loop_time = 0
    while len(sd_list) < numBoots and loop_time < time_lim:
        sdOutput = randomSDFun(sum_data, max_var, min_var, N)
        sd_list.append(sdOutput)
        loop_time = time.time() - start_time
        
        
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

        if myIndex < 0:
            output = 0.00
        else:
            myLength = len(sd_list)
            output = countValues[myIndex] / myLength 

            print "sample variance:", output
            spiders.bootSD_var[rowNum] = output
            spiders.num_boots[rowNum] = numBoots
    
        
            plt.figure() 
    
            plt.plot(xaxis, cumFreq[0]) 
            plt.plot((sd_data, sd_data), (0, numBoots), 'r-') # vertical line of the measured SD from the data
 
            plot_title = graphTitle + " N=" + str(N) + " Boot Var calc =" + str(output)
    
            plt.title(plot_title)   
            graphFileName = graphTitle + ".png"
    
            plt.savefig(graphFileName)
            plt.close()
            
    else:
        print "did not run in time"
        
    

spiders.to_csv(fileOutputName)

print "Finished - file exported"


