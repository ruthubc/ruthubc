'''
Created on Aug 14, 2016

@author: Ruth check
'''


import pandas as pd
from random import uniform
from scipy.stats import cumfreq
import numpy as np
import os

#ln leg length max and min adults  0.88 and 1.33

# sample size 1-25

minValuePos = 0.88

maxValuePos = 1.33

sampleSizes = range(1, 26)





i = 0

columns = ['SampleID','SampleSize', 'Value']
df = pd.DataFrame(columns = columns)

def createRanSmpFun(smpSize, minVal, maxVal, df):    
    i = i + 1
    sampID = [i] * smpSize
    sampSize = [smpSize] * smpSize
    ranList =[uniform(minVal, maxVal) for p in range(0,(smpSize))]