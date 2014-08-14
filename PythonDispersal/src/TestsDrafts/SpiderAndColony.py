'''
Created on May 20, 2014

@author: Ruth
'''
from core.SpiderClass import Spider
from core.ColonyClass import Colony
from core.PopulationClass import Poplt
import numpy as np
import scipy.stats as ss

########### having a test of spider class ####
spi1 = Spider(0.1, 1, 0.2, 4)

spi2 = Spider(0.2, 1, 0.2, 5)

spi3 = Spider(0.3, 2, 0.2, 5)

spi4 = Spider(0.4, 2, 0.2, 5)

spi5 = Spider(0.7, 2, 0.2, 5)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4, spi5]
col= Colony(colList, 1)

print col.size_list(3)

listg = [2,5,2,6,9,7,8, 1, 9 ,7 , 1]

setg = list(set(listg))

print listg

col.update_rank()

col.print_spiders()

print [j for j in col.colony_list if j.instar == 1]

#print [f for f in listg if f ==7]


