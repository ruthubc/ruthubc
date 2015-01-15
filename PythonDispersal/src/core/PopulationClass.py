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
                 min_food = 1.2):
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

    def __str__(self):
        return "Pop_age: %s, # cols: %s" % (self.poplt_age, len(self.poplt_list))

    def update_dispersal_list(self, j):
        dispersing_spis =  self.poplt_list[j].spis_to_dis_lst() # writes the spiders to the dispersal list colony variable
        [k.dis_to_two() for k in dispersing_spis]
        self.dispersal_list.extend(dispersing_spis) # appends the colony dispersers to the population dispersers list

    def ind_col_timestep(self, i): # I have moved this to colony class 14th Jan

        ''' the reason I put this in the population is the population variables. I could still move anyway
        '''
            # (1) age increase

        self.poplt_list[i].col_age_increase()  # updates colony age by one


            #(2) Food

        self.poplt_list[i].update_rank()
        self.poplt_list[i].cal_colony_food(self.inverse_carr_cap, self.cc_skew) #calculating how much food the colony gets
        self.poplt_list[i].ind_food(self.comp_type) # 0 = scramble and 2 = mid contest, 3 = full contest

            # (3) adults reproduce or disperse and reproduce, then die
        self.poplt_list[i].colDispersal_choice(self.ad_min_fd, self.ad_max_fd) #dispersal decision TODO: add variables to pop class
        self.dispersal_list = self.poplt_list[i].spis_to_dis_lst() + self.dispersal_list #adds spiders to dispersal list
        self.poplt_list[i].
        #all adults within the colonies reproduce only if dispersal does not = 1, juvs put into array, or just marked new/old 
        #all adults die

            # (4) old juvs moult or die
        #new function: juvs reach specific size and moult or die

            # (5) new juvs added to colony
        #new function
        
        # (6) making new juvs and new adults into old adults and old juvs

            # (7) marking dead colonies (colonies with no spiders) 
        self.poplt_list[i].col_alive()


            # (7) exporting the data (appending colony info to form to list to export)
        output_list = self.poplt_list[i].colony_list_to_append()
        self.export_list.append(output_list)
        print self.poplt_list[i].colony_dict()

        self.poplt_list[i].print_spiders()


  #make new colonies from disperses but make sure that the new dispersed colonies also reproduce in the 

            # (4) death or catastrophe
        #self.poplt_list[i].die_or_ctphe(self.age_die, self.prob_death, self.cat_prob, self.cat_perc_die)

            # (5) dispersal or reproduction
        self.poplt_list[i].rep_or_disp(self.adult_size, self.min_food) # marks each spider to reproduce or disperse
        self.poplt_list[i].reproduction(self.number_offspring) # new offspring added to colony
        self.update_dispersal_list(i) # adds dispersing spiders to list and updates their disperse to 2

            # (6) marking dead colonies (colonies with no spiders) 
        self.poplt_list[i].col_alive()




    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for j in range(len(self.poplt_list)):
            self.ind_col_timestep(j)
        print self.poplt_dict()

    def del_colony(self): # deletes a colony from the population if it goes extinct
        #works, checked Aug 14th        j
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive']

    def add_new_cols(self): # sets up the dispersing spiders as new colonies
        #checked: works Aug 14th
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


