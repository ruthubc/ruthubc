'''
Created on Jul 25, 2014

@author: user
'''

from core.SpiderClass import Spider
from core.ColonyClass import Colony
import numpy as np
import scipy.stats as ss

########### having a test of spider class ####
spi1 = Spider(0.75, 0.2, 0.2, 4)

spi2= Spider(0.89, 0.3, 0.2, 5)

spi3= Spider(0.02, 0.3, 0.2, 5)

spi4= Spider(0.4, 0.3, 0.2, 5)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4]

colony = Colony(colList, 4)


