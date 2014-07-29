'''
Created on Jul 28, 2014

@author: Ruth
'''

from core.SpiderClass import Spider
from core.ColonyClass import Colony
from core.PopulationClass import Poplt
import numpy as np
import scipy.stats as ss

spi1 = Spider(0.75, 0.2, 0.2, 4, disperse = 1)

spi2= Spider(0.89, 0.3, 0.2, 5, disperse = 1)

spi3= Spider(0.02, 0.3, 0.2, 5, disperse = 1)

spi4= Spider(0.02, 0.3, 0.2, 5, disperse = 1)

col = Colony([spi1, spi2, spi3, spi4], 1)

col.print_spiders()

disExt = col.spis_to_dis_lst()


dislist = []

dislist.extend(disExt)

print len(dislist)

pop = Poplt([col], dispersal_list= dislist)

print pop.dispersal_list

pop.add_new_cols()

print "number of colonies"

print len(pop.poplt_list)

print pop.poplt_dict()


