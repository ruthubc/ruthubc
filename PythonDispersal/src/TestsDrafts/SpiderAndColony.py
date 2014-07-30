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
spi1 = Spider(0.75, 0.2, 0.2, 4)

spi2 = Spider(0.89, 0.3, 0.2, 5)

spi3 = Spider(0.02, 0.3, 0.2, 5)

spi4 = Spider(0.4, 0.3, 0.2, 5)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4]

colony = Colony(colList, 4)

#[i.spi_age_add1() for i in colony.colony_list]

#colony.colony_list[1].spi_age_add1()

#spi1.spi_age_add1()

colony.col_age_increase()
colony.col_age_increase()


MyPop = Poplt([colony])

print MyPop
#colony.spider_age_increase()



