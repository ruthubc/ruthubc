'''*
Created on Dec 27, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long

import numpy as np
from heapq import nsmallest
from fractions import Fraction

# slope already made into slp/num juvs in the colony class, so the input slope is already slope/juv


#### TO DO, NEXT JOB: decide what to do with too much food i.e. change slope or intercept??


class CompInt(object):
    '''making colony class'''

    def __init__(self, col_fd, num_juv, slp):
        self.col_fd = float(col_fd)
        self.num_juv = float(num_juv)
        self.slp = slp
        self.intercept = 0
        self.maxRank = self.num_juv - 1  # as the highest rank is 0 i.e. the intercept
        self.thisSlope = 0
        self.thisMaxRank = 0
        self.OneMaxRank = 0

    def getIntercept(self): # assigning intercept
        # create intercept dict, gonna have it so the numbers are rounded to one decimal place and made into strings
        IntcptDict = {
                '0.2': 0.6,
                '0.4': 0.7,
                '0.6': 0.8,
                '0.8': 0.9,
                '1.0': 1,
                '1.3': 1.125,
                '1.7': Fraction(4, 3),
                '2.5': 1.75,
                '5.0': 3
                }
        LookUp = str(round(self.slp, 1))
        self.intercept = IntcptDict[LookUp]

    def calSlope(self): # calculating the slope based on the tot food and the intercept
        if self.slp > 1:  # i.e. if the intercept is above one
            print "slope greater than one"
            self.thisSlope = (self.intercept - 2 * (self.intercept ** 2)) / (1 + self.intercept - self.col_fd)
            print "calculated slope", self.thisSlope
            self.OneMaxRank = (1- self.intercept)/self.thisSlope
            self.thisMaxRank = self.intercept/self.thisSlope
        else:
            self.thisSlope = -(3 * (self.intercept ** 2)) / (3 * self.intercept - self.col_fd) # only true if intercept below 1, 
            #includes the max rank as the point where the line crosses x-axis
            print "calSlope", self.thisSlope
            self.thisMaxRank = self.intercept/self.thisSlope
            print "max Rank = ", self.thisMaxRank

    def maxRankCheck(self): # checking if the x-axis intercept is greater than the number of juvs -1
        if self.thisMaxRank > self.maxRank:
            print "max rank larger than number of juvs"
            self.exceedMaxRank()
            
    def exceedMaxRank(self):
        # increase intercept, if that is not enough then increase slope
        sqrtTerm = 1- 10 * self.thisSlope + (self.thisSlope ** 2) + 8 * self.thisSlope * self.col_fd
        newIntercept = 0.25 * (1-self.thisSlope) + np.sqrt(sqrtTerm) # need to check this equation works in all cases
        # I guess after getting new intercept I need to calculate the new maxOneRank
        self.intercept = newIntercept
        self.OneMaxRank = (1-newIntercept)/self.thisSlope

    def CompIntFun(self):  # calculates the med rank, returns med rank
        self.getIntercept()
        self.calSlope()
        self.maxRankCheck()
        print [self.thisSlope, self.intercept]
        return [self.thisSlope, self.intercept] #  self.low_rnk # 
