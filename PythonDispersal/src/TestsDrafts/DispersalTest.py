'''
Created on Jul 28, 2014

@author: Ruth
'''

from core.SpiderClass import Spider
from core.ColonyClass import Colony
from core.PopulationClass import Poplt
import numpy as np
import scipy.stats as ss

spi1 = Spider(0.75, 1, ind_food = 1)

spi2= Spider(0.89, 1, ind_food = 0.5)

spi3= Spider(0.02, 1, ind_food = 0.5)

spi4= Spider(0.02, 1, ind_food = 0.5)

col = Colony([spi1, spi2, spi3, spi4], 1)

col.print_spiders()

col.rep_or_disp(0.7, 0.6)

print "after fun:",

col.print_spiders()

'''

pop = Poplt([col], dispersal_list= [])

pop.update_dispersal_list(0)

print pop.dispersal_list[1]

pop.add_new_cols()




print pop.poplt_dict()

print col

col.removing_spiders()

print col




disExt = col.spis_to_dis_lst()
dislist.extend(disExt)

print len(dislist)
print pop.dispersal_list

pop.add_new_cols()

print "number of colonies:", 

print len(pop.poplt_list)

dislist = col.spis_to_dis_lst()

pop = Poplt([col], dispersal_list= dislist)

'''
