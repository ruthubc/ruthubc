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
        if self.intercept > 1:  # i.e. if the intercept is above one
            print "intercept greater than one"
            #topExp = -3 + (6 * self.intercept) - (4 * np.square(self.intercept)) # I think these are wrong
            #botExp = -3 + (2 * self.intercept) - (2 * self.col_fd)
            #self.thisSlope = topExp / botExp
            self.thisSlope = (-1 + (2 * self.intercept)) / (-1 + (2 * self.col_fd))
            print "calculated slope", self.thisSlope
            self.OneMaxRank = np.floor(((1 - self.intercept) / -self.thisSlope))
            self.thisMaxRank = np.floor((self.intercept / self.thisSlope))
            print "oneMaxRank", self.OneMaxRank
            print "thisMaxRank", self.thisMaxRank
        else:  # intercept < 1
            self.thisSlope = - np.square(self.intercept) / (self.intercept - (2 * self.col_fd))
            #self.thisSlope = (- 1 + 2 * self.intercept) / (1 + 2 * self.col_fd) # only true if intercept below 1, I think this is wrong
            #includes the max rank as the point where the line crosses x-axis
            print "calSlope", self.thisSlope
            self.thisMaxRank = np.floor(self.intercept / self.thisSlope)
            print "max Rank = ", self.thisMaxRank

    def maxRankCheck(self):  # checking if the x-axis intercept is greater than the number of juvs -1
        if self.thisMaxRank > self.maxRank:
            print "max rank larger than number of juvs"
            self.exceedMaxRank()

    def exceedMaxRank(self):
        print "old slope", self.thisSlope
        EstIntercept = ((self.maxRank * self.thisSlope) + ((self.maxRank ** 2) * self.thisSlope) +
                           (2 * self.col_fd)) / (2 *(1 + self.maxRank)) # calculates whether the intercept will be above or below one
        print 'Estimated Intercept', EstIntercept

        if EstIntercept <= 1:  # this means that the actual intercept will likely be under 1 - need to check this is always the case
            # or what to do if it is not. Perhaps put in some error catching thing
            topExp = (self.maxRank * self.thisSlope) + (np.square(self.maxRank) * self.thisSlope) + (2 * self.col_fd)
            newIntercept = topExp / (2 * (1 + self.maxRank))
        else: # If the estimated intercept is above one
            print "estimated intercept is above one"
        # increase intercept, if that is not enough then increase slope
            #wrong I think, sqrtTerm = -(24 * self.maxRank) + (9 * self.thisSlope) + (24 * self.maxRank * self.thisSlope) + (16 * (self.maxRank ** 2) * self.thisSlope) + (24 * self.col_fd) 
            sqrtTerm = 8 + (8 * self.maxRank) + self.thisSlope - (8 * self.col_fd)
            if sqrtTerm < 0:
                raise ValueError('Square root term -comp intercept -  in exed max rank negative')
            print "square Root term", sqrtTerm
            #think this is wrong -nonSqrtTerm = 6 - (3 * self.thisSlope) - (2 * self.maxRank * self.thisSlope)
            nonSqrtTerm = 2 + self.thisSlope + (2 * self.maxRank * self.thisSlope)
            # I think this is wrong, newIntercept = Fraction(1, 6) * (nonSqrtTerm + (np.sqrt(self.thisSlope) * np.sqrt(sqrtTerm))) # need to check this equation works in all cases
            newIntercept = 0.5 * (nonSqrtTerm - (np.sqrt(self.thisSlope) * np.sqrt(sqrtTerm)))
        self.intercept = newIntercept
        print "new intercept", self.intercept
        self.OneMaxRank = np.floor((1 - newIntercept) / -self.thisSlope)
        print "New One Max Rank", self.OneMaxRank

    def CompIntFun(self):  # calculates the intercept and slope, depending on how much food there is
        self.getIntercept()
        self.calSlope()
        self.maxRankCheck()
        print [self.thisSlope, self.intercept]
        return [self.thisSlope, self.intercept] #  self.low_rnk # 
