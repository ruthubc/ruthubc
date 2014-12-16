'''
Created on Dec 15, 2014
Testing the new individual competition equation discussed with Leticia after meeting Dec9th
The adults capture the food and all food is split between juvs and ads in the colony
@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt

Ranks = np.array([6, 1, 0, 4, 3, 5, 2]) # has to be an array

food_cap = 0.41 # food per capita i.e the number of adults
num_ads = 3 # have to get this to actually count the number of adults
num_spi = len(Ranks) # when having comp only between instars this is the total number of instars

col_fd = food_cap * num_ads # getting the total amt of food as only ads engage in prey capture
xbr = col_fd/num_spi # the average amount of food per individual

slp = 0.5 # the slope of the equation

CompEqn = xbr * (1+ (slp * med_rnk) * (xbr - (xbr * rnk/med_rnk))/xbr^2)