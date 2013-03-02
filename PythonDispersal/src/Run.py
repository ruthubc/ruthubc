'''
Created on 2013-02-28

@author: Ruth

The acutal code
'''

import numpy as np
from SpiderClass import Spider
from ColonyClass import Colony

#Start with one adult spider in one colony

# One list containing all the colonies


def run(colony, no_offspring, scale, carrying_capacity,
        skew, intercept, slope, instar_list):

    # keeping track of age of spiders
    colony.age_increment()

    # (1) Reproduction
    colony.reproduction(no_offspring)

    #(2) feeding

    colony.colony_food(scale, carrying_capacity, skew)
    ''' calculating how much food the whole colony gets'''
    colony.rel_size()  # updates the relative size of each spider in colony
    colony.ind_food()  # amount of food to each individual

    #(3) growing
    colony.ind_growth(intercept, slope)  # amount each individual grows
    colony.update_instar(instar_list)  # updating instar

    #(4) adults die

    #(5) dispersal




'''whole colony food function
scale is a, in_c = C, skew = d '''
IN_SCALE = 5
IN_C = 0.2
IN_SKEW = 1.2

'''individual growth function linear function parameters'''
IN_INTERCEPT = -5
IN_SLOPE = 5

'''instar list'''
IN_INSTAR = range(1, 21, 2)
"range (start, stop, step), nb: does not include last number!"

"number of offspring an female has"
IN_NO_OFFSP = 5


START_COL = Colony([Spider(7, 7)])


run(START_COL, IN_NO_OFFSP, IN_SCALE, IN_C,
    IN_SKEW, IN_INTERCEPT, IN_SLOPE, IN_INSTAR)

