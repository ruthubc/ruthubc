'''
Created on 2012-12-23

@author: ruth
'''
from SpiderClass import Spider
from ColonyClass import Colony
import numpy as np


pete = Spider(0.8)

instarList = np.arange(0.1, 1, 0.1)

col = Colony([pete, Spider(0.2), Spider(0.4), Spider(0.11), Spider(0.89), Spider(0.11), Spider(0.11)])

col.age_increment()
col.age_increment()
col.update_instar(instarList)
col.reproduction(5)

#list1 = [Spider(0.8), Spider(0.4)]

#list2 = list([Spider() for i in range(7)])





#newCol = Colony(list1 + list2)
#newCol.print_spiders()
#newCol.rel_size()
#print "now rel size"

#newCol.print_spiders()


#newCol.print_spiders()

print "now rel size"

col.rel_size()


col.print_spiders()

print len([spi for spi in col.colony_list if spi.instar >= 5])

#print col.colony_list
