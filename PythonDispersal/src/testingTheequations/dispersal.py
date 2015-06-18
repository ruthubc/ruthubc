'''
Created on Mar 23, 2015

@author: Ruth
'''
import numpy as np
from heapq import nsmallest
from core.Competition import Comp
import itertools
from core.AdultClass import Adult
from core.JuvClass import Juv
from core.ColonyClass import Colony
import matplotlib.pyplot as plt

adList = []

ad1 = Adult([0,0,0], 0, 0)
ad2 = Adult([0,0,0.4], 0, 0)
ad3 = Adult([0,0,1], 0, 0)
ad4 = Adult([0,0,0.2], 0, 0)
ad5 = Adult([0,0,0.3], 0, 0)
ad6 = Adult([0,0,1], 0, 0)
ad7 = Adult([0,0,0.001], 0, 0)
ad8 = Adult([0,0,0.4], 0, 0)
ad9 = Adult([0,0,1], 0, 0)
ad10 = Adult([0,0,0.8], 0, 0)
ad11 = Adult([0,0,0.4], 0, 0)
ad12 = Adult([0,0,1], 0, 0)


adList= [ad1, ad2, ad3,ad4, ad5, ad6, ad7, ad8, ad9, ad10, ad11, ad12 ]

col = Colony(1, adList)

[i.disperseChoice(0.4) for i in col.ad_list]

print [i.food for i in col.ad_list]

print [i.disperse for i in col.ad_list]

col.spis_to_dis_lst()

print "colony",  [i.food for i in col.ad_list]
print "dispersers",  [i.food for i in col.dispersers]
