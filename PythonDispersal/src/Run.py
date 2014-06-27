'''
Created on 2013-02-28

@author: Ruth

The acutal code
'''

import numpy as np
from core.SpiderClass import Spider
from core.ColonyClass import Colony

#Start with one adult spider in one colony

# One list containing all the colonies


def run(colony, no_offspring, carrying_capacity, food_skew, ind_growth_rate, instar_list, die_ad_min, die_juv_max, die_curve, die_max_age):

    # keeping track of age of spiders
    colony.age_increment()

    # (1) Reproduction
    colony.reproduction(no_offspring)

    #(2) feeding

    colony.colony_food(carrying_capacity, food_skew)
    ''' calculating how much food the whole colony gets'''
    colony.rel_size()  # updates the relative size of each spider in colony
    colony.ind_food()  # amount of food to each individual

    #(3) growing
    colony.ind_growth(ind_growth_rate)  # amount each individual grows
    colony.update_instar(instar_list)  # updating instar

    #(4) adults die
    colony.dying(die_ad_min, die_juv_max, die_curve, die_max_age)

    #(5) dispersal

    return colony


"number of offspring an female has"
IN_NO_OFFSP = 1

'''whole colony food function
scale is a, in_c = C, skew = d '''
IN_CAR_CAP = 5
IN_FOOD_SKEW = 0.2

'''individual growth function linear function parameters'''
IN_IND_GROWTH = 0.1


'''instar list'''
IN_INSTAR = np.arange(0.1, 1, 0.1)
"range (start, stop, step), nb: does not include last number!"

'''death'''
IN_MIN_PROB = 0.0001
IN_MAX_PROB = 0.5
IN_DIE_CURVE = 2
DIE_MAX_AGE = 50


START_COL = Colony([Spider(0.8), Spider(0.8),Spider(0.8)])


for i in range(5):
    START_COL = run(START_COL, IN_NO_OFFSP, IN_CAR_CAP, IN_FOOD_SKEW, IN_IND_GROWTH,
                   IN_INSTAR, IN_MIN_PROB, IN_MAX_PROB, IN_DIE_CURVE, DIE_MAX_AGE)


print START_COL.print_spiders()

print len(START_COL.colony_list)

