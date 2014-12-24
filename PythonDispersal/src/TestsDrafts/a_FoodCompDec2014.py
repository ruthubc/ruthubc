'''
Created on Dec 15, 2014
Testing the new individual competition equation discussed with Leticia after meeting Dec9th
The adults capture the food and all food is split between juvs and ads in the colony
@author: Ruth
'''

import numpy as np
import matplotlib.pyplot as plt

Ranks = np.array([6, 1, 0, 4, 3, 5, 2]) # has to be an array

food_cap = 0.41 # food per capita i.e the number of adults -> float
num_ads = 3.0 # have to get this to actually count the number of adults -> if .0 = float
num_spi = float(len(Ranks)) # when having comp only between instars this is the total number of instars

col_fd = food_cap * num_ads # getting the total amt of food as only ads engage in prey capture
xbr = float(col_fd/num_spi) # the average amount of food per individual

med_rnk = num_spi/2 ## check

rnkChg = med_rnk /10 # maybe make this just one and find the most accurate in that band??

#TODO check this is correct and decide what to do with half ranks


slp = 0.5 # the slope of the equation


rnk = 3.0

#CompEqn = xbr * (1.0 + (slp * med_rnk) * (xbr - (xbr * rnk/med_rnk))/xbr^2) #not working


oneRnk = np.floor ( (-1 + med_rnk * slp + xbr)/ slp ) # could change to celling
mxRnk = np.floor(med_rnk * slp + xbr) / slp # could change to celling but i don't think that would work

## calculating 
sq_tot = oneRnk + 1 # as the highest ranked ind is rank zero

# do a loop to find the correct value of med rank

# do the math for each bit, change the med rank then rerun until it works??
if oneRnk > 0 and mxRnk <= num_spi: # (1) sum of square bit and then sum of slp 
# +1 because highest ranked ind is rank 0
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
    tot = ((med_rnk * slp + xbr) * (slp + med_rnk * slp + xbr)) / 2 * slp # there is no square bit

elif oneRnk <= 0 and mxRnk > (num_spi -1):
    print "option four"
    tot = -0.5 * num_spi * (-slp - (2* med_rnk * slp) - 2 * xbr + slp * num_spi)

else:
    print "It didn't work"

print tot

## put in a limit to the number of times this can run??
## next if statement looking at med rank
if tot > col_fd: # cal is too high
    med_rnk -= rnkChg
    print "cal food higher than colony food"
elif tot < col_fd: # cal food is too low
    med_rnk += rnkChg
    print "tot food lower than colony food"
else:
    "done"
