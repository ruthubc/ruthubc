'''
Created on Jun 17, 2015

@author: Ruth
'''

import numpy as np
from core.Competition import Comp
from core.AdultClass import Adult
from core.JuvClass import Juv
from core.ColonyClass import Colony
import matplotlib.pyplot as plt


adList = []

ad1 = Adult([0,0,1], 0, 0)
ad2 = Adult([0,0,1], 0, 0)
ad3 = Adult([0,0,1], 1, 0)
ad4 = Adult([0,0,1], 1, 0)
ad5 = Adult([0,0,1], 1, 0)
ad6 = Adult([0,0,1], 1, 0)

adList= [ad1, ad2, ad3, ad4, ad5, ad6]

col = Colony(1, adList, slope = 10)

print col

col.col_num_off(5, -2)

col.reproduction()


col.juv_rnk_assign()

x = [i.rank for i in col.juv_list]

print x

col.colony_food = 0.2 * len(col.ad_list)

print col.colony_food

col.assign_food()

y = [i.food for i in col.juv_list]

print y

plt.plot(x, y, 'ro')
plt.title("juv food")
plt.show()

col.moult(0.4)

print len(col.ad_list)

adsz = [i.food for i in col.ad_list]

print adsz







