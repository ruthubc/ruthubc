'''
Created on Dec 15, 2014
Testing the new individual competition equation discussed with Leticia after meeting Dec9th
The adults capture the food and all food is split between juvs and ads in the colony
@author: Ruth
'''
#TODO: check the extremes. Perhpas have different equations for very few spiders

import numpy as np
from core.SpiderClass import Spider
from core.JuvClass import Juv
from core.AdultClass import Adult
from heapq import nsmallest
import matplotlib.pyplot as plt


num_juv = 70 # when having comp only between instars this is the total number of instars
rankList = list(range(0, num_juv))

food_cap = 0.16203 # food per capita 
num_ads = 70 # have to get this to actually count the number of adults -> if .0 = float

col_fd = food_cap * num_ads # getting the total amt of food as only ads engage in prey capture

xbr = float(col_fd/num_juv)

share = 100
## Inputing the slope
slp = share/float(num_juv) # the slope of the equation

print "new slope:", 
print slp




def CompFunction(slp,  num_juv, col_fd): # calculates the med rank needed to make the total food equal total colony food
    print "starting loop"
    tot = 0
    med_rnk = num_juv/2
    med_diff = float(num_juv)/100 # -> depends how accurate I want it to be
  

    xbr = float(col_fd/num_juv) # the average amount of food per individual
    high_tot = -1
    low_tot = -1

    print "starting loop, colony food =  %s" % col_fd

    # TODO: Come up with better conditions for the loop
    while high_tot  == -1 or  low_tot == -1 :
        oneRnk = np.floor ( (-1 + med_rnk * slp + xbr)/ slp ) # The max rank where everyone gets 1 (max) food
        #mxRnk = np.floor((med_rnk * slp + xbr) / slp) # the max rank that receives food
        mxRnk = np.floor(((med_rnk * slp) + xbr) / slp)

        sq_tot = oneRnk + 1 # as the highest ranked ind is rank zero 
        print "one rank:", 
        print oneRnk,
        print "max Rank:",
        print mxRnk
        

        if oneRnk > 0 and mxRnk <= num_juv: # (1) sum of square bit and then sum of slp 
            print "option one"
            slp_tot = (1+slp)/(2*slp)
            tot = sq_tot + slp_tot

        elif oneRnk > 0 and mxRnk > (num_juv -1): # (2) sum of square bit then the slope bit to num_spis -1
            # eqn for slp total already taking into account num_spi -1 
            print "option two"
            fstBkt = -1 + (med_rnk*slp) + xbr - (slp* num_juv)
            sndBkt = 1 + slp  + (med_rnk*slp) + xbr - (slp * num_juv)
            slp_tot = -((fstBkt * sndBkt)/ 2 * slp)
            tot = sq_tot + slp_tot

        elif oneRnk <=0 and mxRnk <= (num_juv -1): #(3) 
            print "option three"
            tot = (((med_rnk * slp) + xbr) * (slp + (med_rnk * slp) + xbr)) / (2 * slp) # there is no square bit

        elif oneRnk <= 0 and mxRnk > (num_juv -1):
            print "option four"
            tot = -0.5 * num_juv * (-slp - (2* med_rnk * slp) - (2 * xbr) + (slp * num_juv))

        else:
            print "It didn't work"


        print "calculated total = ",
        print tot

        if tot > col_fd: # changing the med rank! cal is too high
            high_rnk = med_rnk
            med_rnk -= med_diff
            high_tot = tot
            print "cal food higher than colony food,  med rnk = %s, new med_rank = %s, high_tot = %s" % (high_rnk, med_rnk, high_tot)
            print 
        elif tot < col_fd: # cal food is too low
            low_rnk = med_rnk
            med_rnk += med_diff
            low_tot = tot
            print "cal food higher than colony food, med rnk = %s,  new med_rank = %s, low_tot = %s" % (low_rnk, med_rnk, low_tot)

        else:
            print "done"

    print "Now the loop has ended, cal tot = %s "  % tot
    print "actual col tot we were aiming for = %s " % col_fd
    print "high total = %s, low tot = %s " % (high_tot, low_tot)
    fin_md_rnk=  nsmallest(1, [low_tot, high_tot], key = lambda x: abs(x-col_fd))[0] # returns the number nearest to actual col_fd

    if fin_md_rnk == low_tot:
        return low_rnk
    else:
        return high_rnk

Cal_md_rnk = CompFunction(slp, num_juv, col_fd)

print "calculated median rank: ", 
print Cal_md_rnk


def AssignIndFd(slp, med_rnk,  num_juv, rnk):
    xbr = float(col_fd/num_juv)
    tm1 = slp * med_rnk
    tm2 = xbr - ((xbr *rnk)/med_rnk)
    tm12 = (tm1 * tm2) / np.power(xbr, 2)

    CompEqn = xbr * (1+ tm12) # need to put in the if statements for those that get one and get 0 and check is correct
    if CompEqn > 1:
        IndFd = 1
    elif CompEqn < 0:
        IndFd = 0
    else:
        IndFd = CompEqn
    return IndFd




foodList = []

for i in rankList:
    food = AssignIndFd(slp, Cal_md_rnk, num_juv, i)
    foodList = foodList + [food]

print "sum of food list"

print sum(foodList)

print "difference cal - fd list sum",
print col_fd - sum(foodList)

plt.plot(rankList, foodList)
plt.axis([0, num_juv, 0, 1])

plt.show()


'''
        oneRnk = np.floor ( (-1 + med_rnk * slp + xbr)/ slp ) # The max rank where everyone gets 1 (max) food
        mxRnk = np.floor(med_rnk * slp + xbr) / slp # the max rank that receives food
        
'''