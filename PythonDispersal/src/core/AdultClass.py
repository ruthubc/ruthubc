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
    def __init__(self, SpiderList = [0, 0, 0], disperse = 0, no_off = 0):  # make sure juv_fd updates to tot_fd
        Spider.__init__(self, SpiderList[0], SpiderList[1], SpiderList[2]) # [rank, die, food]
        self.disperse = disperse
        self.no_off = no_off

    def disperseChoice(self, dis_fd, disp_rsk):  # disperses if adult above a specific size
        rndNum = rndm.random()
        if self.food >= dis_fd:
            if rndNum <= disp_rsk:
                self.die = 1
            else:
                self.disperse = 1

    #TODO: check this works
    def noOffspring(self, M, C):  # updates the number of offspring an adult have depends on food while juv and adult
        if self.food > 0:
            NumOff = np.exp((-M * np.log(self.food)) - C)
            self.no_off = int(round(NumOff))
        else:
            raise Exception("Adult with zero food")

