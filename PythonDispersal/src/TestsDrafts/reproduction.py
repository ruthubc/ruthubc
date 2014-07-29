'''
Created on Jul 28, 2014

@author: Ruth
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


col = Colony([spi1, spi2, spi3, spi4], 3)

col.die_or_ctphe(1, 0.5, 0.1, 0.5)
#TODO: continue testing!


