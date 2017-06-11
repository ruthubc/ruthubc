'''
Created on Aug 14, 2016

@author: Ruth check
'''


import pandas as pd
from random import uniform
#from scipy.stats import cumfreq
import numpy as np
import os

os.chdir('C:/Users/Ruth/Dropbox/RuthEcuador2013/NestSize/')

bootInputFileName = "Dolph_bootVars.csv"

fileWithMean = "Summary_bootSamplesDolph_9June.csv"

fileOutputName = "Dolph_bootVarsWithMeans.csv"

boot_df = pd.read_csv(bootInputFileName)

mean_df = pd.read_csv(fileWithMean)

merged = pd.merge(boot_df, mean_df, on='SampleID', how = 'left')

merged.to_csv(fileOutputName)