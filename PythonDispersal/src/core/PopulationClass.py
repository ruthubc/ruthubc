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
#TODO: remove unnecessary variables
    def __init__(self, poplt_list, # list of colonies
                 filename = "",
                 dispersal_list = [],
                 export_list = [],
                 number_offspring = 2,
                 carrying_capacity = 10.1,
                 cc_skew = 2,
                 comp_type = 1, # 0 = scramble and 1 = contest
                 growth_per_food = 0.5, # max size =1
                 age_die = 10,
                 prob_death = 0.01,
                 cat_prob = 0.001,
                 cat_perc_die = 0.7,
                 num_instars = 2,
                 poplt_age = 0,
                 colony_number = 1,
                 min_food = 1.2,
                 new_cols = []):
        self.poplt_list = poplt_list
        self.filename = filename
        self.export_list = export_list
        self.dispersal_list = dispersal_list
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
        self.num_instars = num_instars
        self.poplt_age = poplt_age
        self.colony_number = colony_number
        #self.colony_count = len(self.poplt_list) This needs to be in the code to be reliable
        self.min_food = min_food
        self.new_cols = new_cols

    def __str__(self):
        return "Pop_age: %s, # cols: %s" % (self.poplt_age, len(self.poplt_list))
    
    
    def update_poplt_age(self): # adds one to the age
        self.poplt_age += 1

    def update_dispersal_list(self, j):
        dispersing_spis =  self.poplt_list[j].dispersers # writes the spiders to the dispersal list colony variable
        self.dispersal_list.extend(dispersing_spis) # appends the colony dispersers to the population dispersers list

    def del_colony(self): # deletes a colony from the population if it goes extinct
        #works, checked Aug 14th        j
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive']  

    def create_new_col(self): # sets up the dispersing spiders as new colonies
        #checked: works Aug 14th
        for spider in self.dispersal_list:
            self.colony_number += 1
            col = Colony([spider], self.colony_number)
            self.new_cols.extend([col]) # TOOD: new colonies not adding to list

    def new_cols_to_lst(self): # add the dispersed colonies to the population list and empties new_col list
        self.poplt_list.extend(self.new_cols)
        self.new_cols = []
        
    
    
    

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for colony in self.poplt_list:
            colony.colony_timestep(STUFF) #TODO: put the stuff inside
            self.update_dispersal_list(colony)


    def disp_col_timestep(self):
        for colony in self.new_cols:
            colony.core_colony_timestep(STUFF) #TODO: add the stuff



    def poplt_dict(self):  # population dictionary
        d = OrderedDict()
        d['poplt_age']= self.poplt_age
        d['comp_type']= self.comp_type
        d['num cols'] = len(self.poplt_list)
        return d

    def poplt_export(self):  # appends one time step of information to file
        f = open(self.filename, 'ab')
        appender = csv.writer(f)

        for i in range(len(self.export_list)): # writes list to file
            print self.export_list[i]
            appender.writerow(self.poplt_dict().values() + self.export_list[i])

        self.export_list = [] # clears the list once it has been appended to the csv file

    
    
    def one_poplt_timestep(self):
        #(1) Add one to population age
        self.update_poplt_age()

        #(2) Colony time step for all colonies
        self.one_poplt_timestep()
        
        #(3) Make dispersers into new colonies
        self.create_new_col()
        
        #(4) Iterate through new col list
        self.disp_col_timestep()
        
        #(5) adds new colonies to the pop list and clears new colony list
        self.new_cols_to_lst()
        
        #(6) export results
        #TODOL: make export
        
        #(7) Delete dead colonies
        self.del_colony()
        
        
        
        

        #(3) write data to file
        self.poplt_export()


