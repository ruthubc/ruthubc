'''
Created on Dec 27, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long
#pep8: -- ignore=E251

import numpy as np
from heapq import nsmallest


def CompFunction(slp, num_juv, col_fd):  # calculates the med rank, returns med rank
    print "starting loop"
    tot = 0
    med_rnk = num_juv / 2
    med_diff = float(num_juv) / 100  # depends how accurate I want it to be

    xbr = float(col_fd / num_juv)  # the average amount of food per individual
    high_tot = -1
    low_tot = -1

    print "starting loop, colony food =  %s" % col_fd

    # TODO: Come up with better conditions for the loop
    while high_tot == -1 or low_tot == -1:
        oneRnk = np.floor((-1 + med_rnk * slp + xbr) / slp)  # The max rank where everyone gets 1 (max) food
        mxRnk = np.floor(((med_rnk * slp) + xbr) / slp)  # the max rank that receives food

        sq_tot = oneRnk + 1  # as the highest ranked ind is rank zero
        print "one rank:",
        print oneRnk,
        print "max Rank:",
        print mxRnk

        if oneRnk > 0 and mxRnk <= num_juv:  # (1) some get 1 food, others get zero food -> sum of square bit and then sum of slp
            print "option one"
            slp_tot = (1 + slp) / (2 * slp)
            tot = sq_tot + slp_tot

        elif oneRnk > 0 and mxRnk > (num_juv - 1):  # (2) some get 1 food, none get zero food -> sum of square bit then the slope bit to num_spis -1
            # eqn for slp total already taking into account num_spi -1
            print "option two"
            fstBkt = -1 + (med_rnk * slp) + xbr - (slp * num_juv)
            sndBkt = 1 + slp + (med_rnk * slp) + xbr - (slp * num_juv)
            slp_tot = -((fstBkt * sndBkt) / (2 * slp))
            tot = sq_tot + slp_tot

        elif oneRnk <= 0 and mxRnk <= (num_juv - 1):  # (3) no max or minimum, none get 1 food, some get zero food
            print "option three"
            tot = (((med_rnk * slp) + xbr) * (slp + (med_rnk * slp) + xbr)) / (2 * slp)  # there is no square bit

        elif oneRnk <= 0 and mxRnk > (num_juv - 1):
            print "option four"
            tot = -0.5 * num_juv * (-slp - (2 * med_rnk * slp) - (2 * xbr) + (slp * num_juv))

        else:
            print "It didn't work"

        print "calculated total = ",
        print tot

        if tot > col_fd:  # changing the med rank! cal is too high
            high_rnk = med_rnk
            med_rnk -= med_diff
            high_tot = tot
            print "cal food higher than colony food,  med rnk = %s, new med_rank = %s, high_tot = %s" % (high_rnk, med_rnk, high_tot)

        elif tot < col_fd:  # cal food is too low
            low_rnk = med_rnk
            med_rnk += med_diff
            low_tot = tot
            print "cal food higher than colony food, med rnk = %s,  new med_rank = %s, low_tot = %s" % (low_rnk, med_rnk, low_tot)

        else:
            print "done"

    print "Now the loop has ended, cal tot = %s " % tot
    print "actual col tot we were aiming for = %s " % col_fd
    print "high total = %s, low tot = %s " % (high_tot, low_tot)
    fin_md_rnk = nsmallest(1, [low_tot, high_tot], key = lambda x: abs(x - col_fd))[0]  # returns the number nearest to actual col_fd

    if fin_md_rnk == low_tot:
        return low_rnk
    else:
        return high_rnk
