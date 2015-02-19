'''
Created on Jul 28, 2014

@author: Ruth
'''
from core.SpiderClass import Spider
from core.AdultClass import Adult
from core.ColonyClass import Colony
from core.JuvClass import Juv
import numpy as np
import scipy.stats as ss

Off_var_lst= [4.0, 8.0, 0.1, 1.0]

########### having a test of spider class ####
spi1 = Adult([0, 0, 0.1], 0, 2)

spi2= Adult([0, 0, 0.5], 0, 3)


spi3= Adult([0, 0, 0.7], 0, 2)

spi4= Adult([0, 0, 0.9], 0, 10)

spi1.noOffspring(Off_var_lst)
spi2.noOffspring(Off_var_lst)

ad_list = [spi1, spi2, spi3, spi4]
col = Colony(3, [spi1, spi2, spi3, spi4])

col.col_num_off(Off_var_lst)

col.reproduction()

ad_list[1].food = 0.126

print [i.food for i in ad_list]


juv1 = Juv()
juv2 = Juv()
juv3 = Juv()
juv4 = Juv()
juv5 = Juv()

juvlist = [Juv()] * 4


col2 =col = Colony(3, [spi1, spi2, spi3, spi4], juvlist, colony_food = 0.5)

print col.juv_list

col.juv_rnk_assign()


print col.juv_list[3].rank

col.distr_food()

print [j.food for j in col.juv_list]

''''

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
'''
