import numpy as np
import scipy.stats as ss
import random as rndm
from core.SpiderClass import Spider
from core.JuvClass import Juv
from core.AdultClass import Adult

ad1 = Adult(ad_fd = 51, no_off = 1)
ad2 = Adult(ad_fd = 91,no_off = 1)
ad3 = Adult(ad_fd = 57,no_off = 1)
ad4 = Adult(ad_fd = 5, no_off = 1)

print ad1.no_off
print ad2.no_off

list = [ad1, ad2, ad3, ad4]

no_new_off = [i.no_off for i in list]

print no_new_off

print sum(no_new_off)


print ad1.SpiderList()

for index in enumerate(no_new_off):
    i = index[0]
    list[i].rank = i

newList =  [Adult(i.SpiderList(), tot_fd = i.ad_fd) for i in list]

print [i.tot_fd for i in newList]