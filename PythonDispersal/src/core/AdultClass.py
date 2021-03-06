'''
Created on Dec 18, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long, C

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider


class Adult(Spider):
    def __init__(self, SpiderList = [0, 0, 0], disperse = 0, no_off = 0):
        Spider.__init__(self, SpiderList[0], SpiderList[1], SpiderList[2]) # [rank, die, food]
        self.disperse = disperse
        self.no_off = no_off

    def disperseChoice(self, dis_fd):  # disperses if adult above a specific size
        if self.food >= dis_fd:
            self.disperse = 1
        else: self.disperse = 0

    #TODO: check this works
    def noOffspring(self, M, C):  # updates the number of offspring an adult have depends on food while juv and adult
        if self.food > 0:
            NumOff = np.exp((-M * np.log(self.food)) - C)
            self.no_off = int(round(NumOff))
            #self.no_off = NumOff # for testing
        else:
            raise Exception("Adult with zero food")

