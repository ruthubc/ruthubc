'''
Created on Jul 9, 2014

@author: Ruth
'''
import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider
from ColonyClass import Colony
from collections import OrderedDict
import csv


class Poplt(object):
    '''
    List of all colonies in the population    '''

    def __init__(self, poplt_list, # list of colonies
                 filename = "",
                 dispersal_list = [],
                 export_list = [],
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
                 colony_number = 1):
        self.poplt_list = poplt_list
        self.filename = filename
        self.export_list = export_list
        self.dispersal_list = dispersal_list
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
        self.colony_count = len(self.poplt_list)

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
            
            #TODO add dispersal
            
            # (6) mark dead colonies
            self.poplt_list[i].col_alive()

            """(6) exporting data"""
            # need to append colony dict values  to a list
            output_list = self.poplt_list[i].colony_list_to_append()
            self.export_list = output_list #TODO FIX.. not working!
            
            print self.poplt_list[i].colony_dict()

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for j in range(len(self.poplt_list)):
            self.ind_col_timestep(j)

    def DelColony(self): # deletes a colony from the population if it goes extinct
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive'] 

    def add_new_cols(self):
        for spider in self.dispersal_list:
            self.colony_number += 1  # TODO: add iterative colony number to each new colony created.
            self.poplt_list.extend([Colony(spider, self.colony_number)])

    def update_poplt_age(self): #adds one to the age
        self.poplt_age += 1

    def poplt_dict(self):
        d = OrderedDict()
        d['poplt_age']= self.poplt_age
        d['comp_type']= self.comp_type
        return d

    def poplt_export(self):
        f = open(self.filename, 'ab')
        appender = csv.writer(f)

        for i in range (0, self.colony_count): # writes list to file
            print self.export_list[i]
            appender.writerow(self.poplt_dict().values() + self.export_list[i])

        list = [] # clears the list
        
        
    def one_poplt_timestep(self):
        #(1) Add one to population age
        self.update_poplt_age()
        
        #(2) Colony time step for all colonies
        self.allCols_OneTimestep()
        
        #(3) write data to file
        self.poplt_export()
        
        #(4) Remove dead colonies from the population
        self.DelColony()
        
        #(5) Make dispersed spiders into new colonies
        self.add_new_cols()
        
        
        

