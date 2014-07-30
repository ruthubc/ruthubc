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
                 colony_number = 1,
                 min_food = 0.1):
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
        #self.colony_count = len(self.poplt_list) This needs to be in the code to be reliable
        self.min_food = min_food

    def __str__(self):
        return "Pop_age: %s, # cols: %s" % (self.poplt_age, len(self.poplt_list))
    
    def ind_col_timestep(self, i): 

        ''' the reason I put this in the population is the population variables. I could still move anyway
        '''
            # (1) age increase

        self.poplt_list[i].col_age_increase()  # updates colony age by one
        self.poplt_list[i].spider_age_increase()  # updates the age of all spiders within the colony

            #(2) feeding
        self.poplt_list[i].cal_colony_food(self.inverse_carr_cap, self.cc_skew) #calculating how much food the colony gets
        self.poplt_list[i].update_rank()
        self.poplt_list[i].ind_food(self.comp_type) # 0 = scramble and 1 = contest

            # (3) growth
        self.poplt_list[i].apply_growth(self.growth_per_food)
        self.poplt_list[i].update_instar(self.instar_list) #updating instar

            # (4) death or catastrophe
        self.poplt_list[i].die_or_ctphe(self.age_die, self.prob_death, self.cat_prob, self.cat_perc_die)

            # (5) dispersal or reproduction
        self.poplt_list[i].rep_or_disp(self.min_food, self.adult_size) # marks each spider to reproduce or disperse
        self.poplt_list[i].reproduction(self.number_offspring, self.adult_size) # new offspring added to colony
        print self.poplt_list[i].spis_to_dis_lst() # writes the spiders to the dispersal list colony variable
        self.dispersal_list.extend(self.poplt_list[i].spis_to_dis_lst()) # appends the colony dispersers to the population dispersers list


            # (6) marking dead colonies (colonies with no spiders)
        self.poplt_list[i].col_alive()

            # (7) appending colony info to form to list to export
        output_list = self.poplt_list[i].colony_list_to_append()
        self.export_list.append(output_list)
        print self.poplt_list[i].colony_dict()

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for j in range(len(self.poplt_list)):
            self.ind_col_timestep(j)

    def del_colony(self): # deletes a colony from the population if it goes extinct
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive']

    def add_new_cols(self): # sets up the dispersing spiders as new colonies
        for spider in self.dispersal_list:
            self.colony_number += 1  # TODO: add iterative colony number to each new colony created.
            #print self.colony_number
            col = Colony([spider], self.colony_number)
            #print col.colony_dict()
            self.poplt_list.extend([col]) # TOOD: new colonies not adding to list

    def update_poplt_age(self): # adds one to the age
        self.poplt_age += 1

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
        self.allCols_OneTimestep()

        #(3) write data to file
        self.poplt_export()

        #(4) Remove dead colonies from the population
        self.del_colony()

        #(5) Make dispersed spiders into new colonies
        self.add_new_cols()


