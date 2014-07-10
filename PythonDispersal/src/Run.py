'''
Created on Jun 27, 2014

@author: Ruth

'''
import numpy as np
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from numpy.oldnumeric.linear_algebra import inverse



def run_function(colony,
        adult_size=0.8,
        number_offspring = 5,
        carrying_capacity = 10.0,
        cc_skew = 2,
        comp_type = 1, # 0 = scramble and 1 = contest
        growth_per_food = 0.2, # max size =1
        age_die = 10,
        prob_death = 0.01,
        cat_prob = 0.001,
        cat_perc_die = 0.7):
    
    
    """updates colony age by one"""
    colony.col_age_increase()
    
    #TODO: check that this creates floats
    inverse_carr_cap = 1/carrying_capacity

    """ updates the age of all spiders within the colony"""
    colony.age_increment()

    """ (1) Reproduction"""
    
    colony.reproduction(number_offspring, adult_size)
    
    """(2) feeding """
    colony.cal_colony_food(inverse_carr_cap, cc_skew) #calculating how much food the colony gets
    colony.update_rank()
    colony.ind_food(comp_type) # 0 = scramble and 1 = contest
    
    """(3) growth """
    colony.apply_growth(growth_per_food)
    
    """(4) death """
    
    colony.dieOrCtphe(age_die, prob_death, cat_prob, cat_perc_die)