'''
Created on Jul 28, 2014

@author: Ruth
'''
from core.SpiderClass import Spider
from core.ColonyClass import Colony
import numpy as np
import scipy.stats as ss

adultSize = 0.6
min_food = 0.2

########### having a test of spider class ####
spi1 = Spider(0.75, 0.2, 4)

spi2= Spider(0.89, 0.3, ind_food = 0.5)

spi3= Spider(0.02, 0.3, 5)

spi4= Spider(0.4, 0.3, 5)


col = Colony([spi1, spi2], 3)

spi1.dispORrep(adultSize, min_food)

print "spider disperse:",
print spi1.disperse
print "spider reproduce:",
print spi1.reproduce



col.rep_or_disp(adultSize, min_food)

print "spider disperse:",
print col.colony_list[1].disperse
print "spider reproduce:",
print col.colony_list[1].reproduce



col.reproduction(no_off = 5)
col.reproduction(no_off = 5)
col.reproduction(no_off = 5)
col.reproduction(no_off = 5)

print col.colony_dict()



print "spider disperse:",
print col.colony_list[1].disperse
print "spider reproduce:",
print col.colony_list[1].reproduce
