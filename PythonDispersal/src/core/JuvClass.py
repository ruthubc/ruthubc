'''
Created on Dec 18, 2014

@author: Ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider

class Juv(Spider):

    def __init__(self, SpiderList = [0, 0, 0], juv_fd =0): # spider list = rank , die, food
        Spider.__init__(self, SpiderList[0], SpiderList[1], SpiderList[2])
        self.juv_fd = juv_fd

    def juv_mltORDth(self, jv_min_fd):  # if juv doesn't get enough food she dies before turning into an adult
        if self.food < jv_min_fd:
            self.die = 1