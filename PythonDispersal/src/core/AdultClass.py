'''
Created on Dec 18, 2014

@author: Ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider

class Adult(Spider):
    def __init__(self, SpiderList = [0, 0, 0], disperse = 0, no_off = 0): # make sure juv_fd updates to tot_fd
        Spider.__init__(self, SpiderList[0], SpiderList[1], SpiderList[2])
        self.disperse = disperse
        self.no_off = no_off

    def disperseChoice (self, dis_fd): # disperses if adult above a specific size
        if self.food >= dis_fd:
                self.disperse = 1


    def noOffspring(self, OMn, OMx, SMx, SMn): # the number of offspring an adult have depends on food while juv and adult
        #OMn = min number of offspring
        #OMx = max number of offspring
        #SMn = min adult size that can reproduce
        #SMx = max adult size (or food)
        C = -(-OMn * SMx + OMx * SMn)/ (SMx -SMn)
        M = (-OMx +OMn) / (SMx -SMn)
        NumOff = M * self.tot_fd + C
        self.no_off = NumOff
