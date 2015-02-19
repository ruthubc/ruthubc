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

    def disperseChoice(self, dis_fd):  # disperses if adult above a specific size
        if self.food >= dis_fd:
                self.disperse = 1

    def noOffspring(self, off_nmbr_list):  # updates the number of offspring an adult have depends on food while juv and adult
        OMn = float(off_nmbr_list[0])  # min number of offspring
        OMx = float(off_nmbr_list[1])  # max number of offspring
        SMn = float(off_nmbr_list[2])  # min adult size that can reproduce
        SMx = float(off_nmbr_list[3])  # max adult size (or food)
        #TODO: stop C and M being calculated for each 
        C = ((-OMn * SMx) + (OMx * SMn)) / (SMx - SMn)
        #print 'c:', C
        M = (OMn - OMx) / (SMx - SMn)
        #print 'M:', M
        NumOff = (-M * self.food) - C
        self.no_off = int(round(NumOff))
        #print 'food', self.food
        #print 'numoff:', NumOff
        #print round(NumOff)
