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

Ranks = np.array([6, 1, 0, 4, 3, 5, 2]) # has to be an array

food_cap = 0.603 # food per capita i.e the number of adults -> float
num_ads = 500 # have to get this to actually count the number of adults -> if .0 = float
num_spi = 1000 #float(len(Ranks)) # when having comp only between instars this is the total number of instars
col_fd = food_cap * num_ads # getting the total amt of food as only ads engage in prey capture
xbr = float(col_fd/num_spi)

## Inputing the slope
slp = 1.7 # the slope of the equation


#CompEqn = xbr * (1.0 + (slp * med_rnk) * (xbr - (xbr * rnk/med_rnk))/xbr^2) #not working



def CompFunction(slp,  num_spi, col_fd):
    tot = 0
    med_rnk = num_spi/2
    med_diff = num_spi/1000


    xbr = float(col_fd/num_spi) # the average amount of food per individual
    high_tot = -1
    low_tot = -1

    print "starting loop, colony food =  %s" % col_fd

    # TODO: Come up with better conditions for the loop
    while high_tot  == -1 or  low_tot == -1 :
        oneRnk = np.floor ( (-1 + med_rnk * slp + xbr)/ slp ) # The max rank where everyone gets 1 (max) food
        mxRnk = np.floor(med_rnk * slp + xbr) / slp # the max rank that receives food
        sq_tot = oneRnk + 1 # as the highest ranked ind is rank zero 

        if oneRnk > 0 and mxRnk <= num_spi: # (1) sum of square bit and then sum of slp 
            print "option one"
            slp_tot = (1+slp)/2*slp
            tot = sq_tot + slp_tot

        elif oneRnk > 0 and mxRnk > (num_spi -1): # (2) sum of square bit then the slope bit to num_spis -1
            # eqn for slp total already taking into account num_spi -1 
            print "option two"
            fstBkt = -1 + med_rnk*slp + xbr - slp* num_spi
            sndBkt = 1 + slp  + med_rnk*slp + xbr - slp * num_spi
            slp_tot = -((fstBkt * sndBkt)/ 2 * slp)
            tot = sq_tot + slp_tot

        elif oneRnk <=0 and mxRnk <= (num_spi -1): #(3) 
            print "option three"
            tot = ((med_rnk * slp + xbr) * (slp + med_rnk * slp + xbr)) / (2 * slp) # there is no square bit

        elif oneRnk <= 0 and mxRnk > (num_spi -1):
            print "option four"
            tot = -0.5 * num_spi * (-slp - (2* med_rnk * slp) - 2 * xbr + slp * num_spi)

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
    print "actual col tot = %s " % col_fd
    print "high total = %s, low tot = %s " % (high_tot, low_tot)
    fin_md_rnk=  nsmallest(1, [low_tot, high_tot], key = lambda x: abs(x-col_fd))[0] # returns the number nearest to actual col_fd

    if fin_md_rnk == low_tot:
        return low_rnk
    else:
        return high_rnk
    
#TODO: need to return the median rank instead of the 

nmbr = CompFunction(slp, num_spi, col_fd)

print nmbr


#print nsmallest(1, [2.31, 7.01], key = lambda x: abs(x-3.037))[0]


ad1 = Spider(rank = 581)

def AssignIndFd(xbr, slp, med_rnk, spider):
    rnk = spider.rank
    #tm1 = slp * med_rnk
    #tm2 = xbr - ((xbr *rnk)/med_rnk)

    CompEqn = xbr * (1.0 + (slp * med_rnk) * (xbr - (xbr * rnk/med_rnk))/np.power(xbr,2)) # need to put in the if statements for those that get one and get 0 and check is correct
    if CompEqn > 1:
        IndFd = 1
    elif CompEqn < 0:
        IndFd = 0
    else:
        IndFd = CompEqn
    return IndFd

print "comp eqn"
print AssignIndFd(xbr, slp, nmbr, ad1)


'''
num = 2.26
print type(num)

while 0.0 <= num <= 6.0 or 6.0 <= num <= 0.0:
    num += 1
    print num
print"loop done"
print num
'''