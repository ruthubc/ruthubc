'''
Created on Dec 18, 2014

@author: Ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider

class Adult(Spider):
    def __init__(self, SpiderList, ad_fd, tot_fd, disperse, no_off): # make sure juv_fd updates to tot_fd
        Spider.__init__(self, SpiderList[0], SpiderList[1])
        self.ad_fd = ad_fd
        self.tot_fd = tot_fd
        self.disperse = disperse
        self.no_off = no_off

    def ad_death(self): # kills the adults
            self.die = 1

    def disperseChoice (self, ad_min_fd, ad_max_fd): # disperses if gets less than min food, min_food population variable
        if self.ad_fd >= ad_min_fd and self.ad_fd <= ad_max_fd:
                self.disperse = 1

    def updateTot_fd (self, ad_fd, tot_fod): # gets total food 
        self.tot_fd += ad_fd

    def noOffspring(self, OMn, OMx, SMx, SMn): # the number of offspring an adult have depends on food while juv and adult
        C = -(-OMn * SMx + OMx * SMn)/ (SMx -SMn)
        M = (-OMx +OMn) / (SMx -SMn)
        NumOff = M * self.tot_fd
        return NumOff
        
    
       