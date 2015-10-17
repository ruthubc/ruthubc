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


class Comp(object):
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

    def getIntercept(self):
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


    def CalSlope(self):
        self.getIntercept()
        if self.slp > 1:
            # some code to account for the multiple individuals getting more than one food
            print "slope greater than one"
            self.thisSlope = (self.intercept - 2 * (self.intercept ** 2)) / (1 + self.intercept - self.col_fd)
            print "calculated slope", self.thisSlope
            self.OneMaxRank = (1- self.intercept)/self.thisSlope
            self.thisMaxRank = self.intercept/self.thisSlope
        else:
            self.thisSlope = -(3 * (self.intercept ** 2)) / (3 * self.intercept - self.col_fd) # only true if intercept below 1, 
            #includes the max rank as the point where the line crosses xaxis
            print "calSlope", self.thisSlope
            self.thisMaxRank = self.intercept/self.thisSlope
            print "max Rank = ", self.thisMaxRank

    def maxRankCheck(self):
        if self.thisMaxRank > self.maxRank:
            print "max rank larter than number of juvs"
            #some code for moving the intercept OR changing the slope
        else:
            #basically all done???? Return this slpe and intercept and job done?



    def CompFunction(self):  # calculates the med rank, returns med rank
        # print 'comp num juvs', self.num_juv
        self.xbr = float(self.col_fd) / self.num_juv

        run = 0
        ## print "starting loop, colony food =  %s" % self.col_fd
        # TODO: Come up with better conditions for the loop
        while self.high_tot == -1 or self.low_tot == -1:
            run += 1
            if run < 50000:
                self.comp_loop_function()
            else:
                # print "col food input", self.col_fd, 'num juvs', self.num_juv
                raise Exception("med rank loop infinite")
        # print "Now the loop has ended, cal tot = %s " % self.cal_tot
        # print "actual col food total we were aiming for = %s " % self.col_fd
        # print "high total = %s, low tot = %s " % (self.high_tot, self.low_tot)
        if self.cal_tot == self.col_fd:
            return [self.cal_tot, self.med_rnk] # self.med_rnk #
        else:
            fin_md_rnk = nsmallest(1, [self.low_tot, self.high_tot], key = lambda x: abs(x - self.col_fd))[0]  # returns the number nearest to actual col_fd
            if fin_md_rnk == self.low_tot:
                return   [self.cal_tot, self.low_rnk] #  self.low_rnk # 
            else:
                return  [self.cal_tot, self.high_rnk] # self.high_rnk # 
            #when testing chang output to [self.cal_tot, self...]