'''
Created on Jul 10, 2014

@author: user
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from core.SpiderClass import Spider
from core.ColonyClass import Colony

col1= Colony([Spider(disperse = 1), Spider(disperse = 1)])

col1.print_dets()

disList = []
disList = col1.spis_to_dis_lst(disList)

print "item 1"
print disList

col2 = Colony(disList)

print "col2"
col2.print_dets()

print [Colony(i) for i in disList]



