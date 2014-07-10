'''
Created on Jul 9, 2014

@author: Ruth
'''
import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider
from ColonyClass import Colony


class ColonyPopulation(object):
    '''
    List of all colonies in the population    '''

    def __init__(self, poplt_list,
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
        self.poplt_list = poplt_list
        self.adult_size = adult_size,
        self.number_offspring = number_offspring,
        self.carrying_capacity = carrying_capacity,
        self.cc_skew = cc_skew, 
        self.comp_type = comp_type, # 0 = scramble and 1 = contest
        self.growth_per_food = growth_per_food, # max size =1
        self.age_die = age_die,
        self.prob_death = prob_death,
        self.cat_prob = cat_prob,
        self.cat_perc_die = cat_perc_die
        self.inverse_carr_cap = 1/self.carrying_capacity #TODO: check that this creates floats

    def run_function(self, i):
            """updates colony age by one"""
            self.poplt_list[i].col_age_increase()
            """ updates the age of all spiders within the colony"""
            self.poplt_list[i].age_increment()

            """ (1) Reproduction"""
            self.poplt_list[i].reproduction(self.number_offspring, self.adult_size)

            """(2) feeding """
            self.poplt_list[i].cal_colony_food(self.inverse_carr_cap, self.cc_skew) #calculating how much food the colony gets
            self.poplt_list[i].update_rank()
            self.colony.ind_food(self.comp_type) # 0 = scramble and 1 = contest

            """(3) growth """
            self.poplt_list[i].apply_growth(self.growth_per_food)

            """(4) death """
            self.poplt_list[i].dieOrCtphe(self.age_die, self.prob_death, self.cat_prob, self.cat_perc_die)

            """ (5) dispersal"""



    def population_OneTimestep(self): #iterates through all colonies in popultion for one time step
        for j in range(len(self.poplt_list)):
            self.run_function(j)

    def AddColony(self, colony): # appends a colony aka a dispersing spider to make a new colony
        self.poplt_list.append(colony)
        
    def DelColony(self): #deletes a colony from the populatoin
        #TODO: add this in
        