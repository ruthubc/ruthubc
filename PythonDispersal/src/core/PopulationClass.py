'''
Created on Jul 9, 2014

@author: Ruth
'''
import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider
from ColonyClass import Colony


class Poplt(object):
    '''
    List of all colonies in the population    '''

    def __init__(self, poplt_list, # list of colonies
                 adult_size=0.8, 
                 number_offspring = 5, 
                 carrying_capacity = 10.1,
                 cc_skew = 2,
                 comp_type = 1, # 0 = scramble and 1 = contest
                 growth_per_food = 0.2, # max size =1
                 age_die = 10,
                 prob_death = 0.01,
                 cat_prob = 0.001,
                 cat_perc_die = 0.7,
                 instar_list = [0.5], # list of size transitions
                 poplt_age = 0,
                 colony_number = 0):
        self.poplt_list = poplt_list
        self.adult_size = adult_size
        self.number_offspring = number_offspring
        self.carrying_capacity = carrying_capacity
        self.cc_skew = cc_skew 
        self.comp_type = comp_type # 0 = scramble and 1 = contest
        self.growth_per_food = growth_per_food # max size =1
        self.age_die = age_die
        self.prob_death = prob_death
        self.cat_prob = cat_prob
        self.cat_perc_die = cat_perc_die
        self.inverse_carr_cap = 1/self.carrying_capacity
        self.instar_list = instar_list
        self.poplt_age = poplt_age
        self.colony_number = colony_number

    def ind_col_timestep(self, i):
            """updates colony age by one"""
            self.poplt_list[i].col_age_increase()
            """ updates the age of all spiders within the colony"""
            self.poplt_list[i].spider_age_increase()

            """ (1) Reproduction"""
            self.poplt_list[i].reproduction(self.number_offspring, self.adult_size)

            """(2) feeding """
            self.poplt_list[i].cal_colony_food(self.inverse_carr_cap, self.cc_skew) #calculating how much food the colony gets
            self.poplt_list[i].update_rank()
            self.poplt_list[i].ind_food(self.comp_type) # 0 = scramble and 1 = contest

            """(3) growth """
            self.poplt_list[i].apply_growth(self.growth_per_food)
            self.poplt_list[i].update_instar(self.instar_list) #updating instar

            """(4) death """
            self.poplt_list[i].dieOrCtphe(self.age_die, self.prob_death, self.cat_prob, self.cat_perc_die)

            """ (5) dispersal"""

            """(6) exporting data"""
            export = [self.poplt_age] +  self.poplt_list[i].colony_export() 
            return export

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for j in range(len(self.poplt_list)):
            self.ind_col_timestep(j)

    def DelColony(self): # deletes a colony from the population if it goes extinct
        self.poplt_list = [i for i in self.poplt_list if self.poplt_list.col_alive == 'alive'] #pylint: disable=line-too-long

    def add_new_cols(self, dispersal_list):
        for spider in dispersal_list:
            self.colony_number += 1  # TODO: add iterative colony number to each new colony created.
            self.poplt_list.extend([Colony(spider, self.colony_number)])

    def update_poplt_age(self):
        self.poplt_age += 1

    def poplt_dict(self):
        poplt_dict = {'poplt_age': self.poplt_age,
                      'comp_type': self.comp_type
                      }
        return poplt_dict
