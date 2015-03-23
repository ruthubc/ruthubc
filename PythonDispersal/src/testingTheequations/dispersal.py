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
ad2 = Adult([0,1,0], 0, 0)
ad3 = Adult([0,0,0], 1, 0)

adList= [ad1, ad2, ad3]

col = Colony(1, adList)

col.spis_to_dis_lst()

print col.ad_list