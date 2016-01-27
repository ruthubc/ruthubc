'''
Created on Jan 25, 2016

@author: Ruth
'''

import pandas as pd
import UpdateColSpace
import numpy as np

data = pd.read_csv("AnimationData.csv", sep = ',') # import data

print data.head(5)

data = data[['pop_age', 'colony_ID', 'num_adsB4_dispersal', 'dispersers']]

UpdateColSpace.main(data)  ## updates the colony space

print data.head(5)

data.to_csv('AnimationData.csv', sep = ',', index = False)
