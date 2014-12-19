'''
Created on Dec 18, 2014

@author: Ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider

class Adult(Spider):
    def __init__(self, SpiderList, ad_fd, disperse):
        Spider.__init__(self, SpiderList[0], SpiderList[1])
        self.ad_fd = ad_fd
        self.disperse = disperse

    def ad_death(self): # kills the adults
            self.die = 1

    def disperseChoice (self, ad_min_fd, ad_max_fd): # disperses if gets less than min food, min_food population variable
        if self.ad_fd >= ad_min_fd and self.ad_fd <= ad_max_fd:
                self.disperse = 1