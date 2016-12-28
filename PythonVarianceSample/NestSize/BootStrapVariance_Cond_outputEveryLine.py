'''
Created on Aug 14, 2016

@author: Ruth check
'''


import pandas as pd
from random import uniform
from scipy.stats import cumfreq
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.pyplot import savefig
import os


#os.chdir('C:/Work/Dropbox/')
#os.chdir('G:/Dropbox/RuthEcuador2013/NestSize/')


#fileOutputName = "bootSampCondPython_cond_12Sept.csv"


numBoots = 10000


spiders = pd.read_csv('spidersAverageMul_leg_1.csv')
spiders['bootSD_var'] = 1000.00000 # puts this as the default boot sd in the file
spiders['num_boots'] = 0.00
spiders['myIndex'] = 1000

numRows = len(spiders)




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

    print "outputing one random sample"
    return ranList



for rowNum in range(numRows): # getting the necessary varibles from the data file
    print "\nRun Number", rowNum+1, "/", numRows
    print spiders.NestID[rowNum], spiders.Instar[rowNum]
    
    #reading the values from the input file
    data_mean =  spiders.mean_data[rowNum]
    N = spiders.N[rowNum]
    sum_data =  spiders.sum_data[rowNum]
    min_var = spiders.minVar[rowNum]
    max_var = spiders.maxVar[rowNum]
    
    print "N:", N
    print "sum:", sum_data
    
    runCt = 0
    while (runCt < numBoots):
        sdOutput = randomSDFun(sum_data, max_var, min_var, N)
        dfOutput = pd.DataFrame(sdOutput)
        dfOutput = dfOutput.transpose()
        #print dfOutput
        dfOutput.to_csv('legVariance1.csv', mode='a', header=False)
        runCt = runCt + 1
        print "row number:", runCt

 
        


print "Finished"


