'''
Created on Dec 27, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long

import numpy as np
from heapq import nsmallest


class Comp(object):
    '''making colony class'''

    def __init__(self, col_fd, num_juv, slp):
        self.oneRnk = 0
        self.mxRnk = 0
        self.cal_tot = 0
        self.col_fd = float(col_fd)
        self.num_juv = num_juv
        self.slp = slp
        self.xbr = 0
        self.med_rnk = self.num_juv / 2
        self.med_diff = float(num_juv) / 1000
        self.high_tot = -1
        self.low_tot = -1
        self.low_rnk = 0
        self.high_rnk = 0

    def fnd_tot(self):  # returns the calculated total food for a new med_rank etc.

        sq_tot = self.oneRnk + 1  # as the highest ranked ind is rank zero
        if self.oneRnk > 0 and self.mxRnk <= (self.num_juv-1):  # (1) some get 1 food, others get zero food -> sum of square bit and then sum of slp
            #print "option one"
            slp_tot = (1 + self.slp) / (2 * self.slp)
            tot = sq_tot + slp_tot

        elif self.oneRnk > 0 and self.mxRnk > (self.num_juv - 1):  # (2) some get 1 food, none get zero food -> sum of square bit then the slope bit to num_spis -1
            # eqn for slp total already taking into account num_spi -1
            #print "option two"
            fstBkt = -1 + (self.med_rnk * self.slp) + self.xbr - (self.slp * self.num_juv)
            sndBkt = 1 + self.slp + (self.med_rnk * self.slp) + self.xbr - (self.slp * self.num_juv)
            slp_tot = -((fstBkt * sndBkt) / (2 * self.slp))
            tot = sq_tot + slp_tot

        elif self.oneRnk <= 0 and self.mxRnk <= (self.num_juv - 1):  # (3) no max or minimum, none get 1 food, some get zero food
            #print "option three"
            tot = (((self.med_rnk * self.slp) + self.xbr) * (self.slp + (self.med_rnk * self.slp) + self.xbr)) / (2 * self.slp)  # there is no square bit

        elif self.oneRnk <= 0 and self.mxRnk > (self.num_juv - 1):
            #print "option four"
            tot = -0.5 * self.num_juv * (-self.slp - (2 * self.med_rnk * self.slp) - (2 * self.xbr) + (self.slp * self.num_juv))

        else:
            print "It didn't work"

        self.cal_tot = tot

    def adjustMed_rnk(self):  # adjusts the med rank to make it closer to the actual colony food
            if self.cal_tot > self.col_fd:  # changing the med rank! cal is too high
                self.high_rnk = self.med_rnk
                self.med_rnk -= self.med_diff
                self.high_tot = self.cal_tot
                #print "cal food higher than colony food, col fd = %s, cal food = %s,  med rnk = %s, high_tot = %s" % (self.col_fd, self.cal_tot, self.med_rnk, self.high_tot)

            elif self.cal_tot < self.col_fd:  # cal food is too low
                self.low_rnk = self.med_rnk
                self.med_rnk += self.med_diff
                self.low_tot = self.cal_tot
                #print "cal food lower than colony food, col fd = %s, cal food = %s,  med rnk = %s, low_tot = %s" % (self.col_fd, self.cal_tot, self.med_rnk, self.low_tot)

            elif self.cal_tot == self.col_fd:
                self.low_tot = -2
                self.high_tot = -2


    def comp_loop_function(self):

        self.oneRnk = np.floor((-1 + self.med_rnk * self.slp + self.xbr) / self.slp)  # The max rank where everyone gets 1 (max) food
        self.mxRnk = np.floor(((self.med_rnk * self.slp) + self.xbr) / self.slp)  # the max rank that receives food

        #print "one rank", self.oneRnk
        #print "max rank", self.mxRnk

        self.fnd_tot()  # finding the tot with the new med_rnk
        self.adjustMed_rnk()  # adjusting med_rnk

        #print "calculated total = ", self.cal_tot

    def CompFunction(self):  # calculates the med rank, returns med rank
        print 'comp num juvs', self.num_juv
        self.xbr = float(self.col_fd / self.num_juv)
        run = 0
        #print "starting loop, colony food =  %s" % self.col_fd

        # TODO: Come up with better conditions for the loop
        while self.high_tot == -1 or self.low_tot == -1:
            run += 1
            if run < 2000:
                self.comp_loop_function()
            else:
                print "infinite loop, oh dear"
                break

        print "Now the loop has ended, cal tot = %s " % self.cal_tot
        print "actual col food total we were aiming for = %s " % self.col_fd
        print "high total = %s, low tot = %s " % (self.high_tot, self.low_tot)

        if self.cal_tot == self.col_fd:
            return [self.cal_tot, self.med_rnk]
        else:
            fin_md_rnk = nsmallest(1, [self.low_tot, self.high_tot], key = lambda x: abs(x - self.col_fd))[0]  # returns the number nearest to actual col_fd
            if fin_md_rnk == self.low_tot:
                return  [self.cal_tot, self.low_rnk]
            else:
                return  [self.cal_tot, self.high_rnk]
