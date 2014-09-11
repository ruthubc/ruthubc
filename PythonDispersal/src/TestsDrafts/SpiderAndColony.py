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
spi1 = Spider(0.1, 1, 0.2, age = 4)

spi2 = Spider(0.2,  5, 0.2, age = 3)

spi3 = Spider(0.3, 2, 0.2, age = 5)

spi4 = Spider(0.4,  2, 0.2, age = 5)

spi5 = Spider(0.7, 2, 0.2, age =2)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4, spi5]
col= Colony(colList, 1)

col.print_spiders()

col.cal_colony_food(0.01, 1)
