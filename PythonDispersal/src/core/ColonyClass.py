'''
Created on 2013-02-12

@author: ruth
'''

from collections import OrderedDict

from AdultClass import Adult
from JuvClass import Juv
from SpiderClass import Spider
import numpy as np
import random as rndm
import scipy.stats as ss



class Colony(object):
    '''making colony class'''

    def __init__(self, ad_list = [],
                 juv_list =[],
                 new_ad_list = [],
                 new_juv_list = [],
                 colony_ID,
                 colony_food=0.0,
                 colony_age=0,
                 alive = 'alive',
                 dispersers = [],
                 pot_juv_food = 0,
                 cal_med_rnk = 0):
        self.ad_list = ad_list
        self.juv_list = juv_list
        self.new_ad_list = new_ad_list,
        self.new_juv_list = new_juv_list,
        self.colony_ID = colony_ID
        self.colony_food = colony_food
        self.colony_age = colony_age
        self.alive = alive
        self.dispersers = dispersers
        self.pot_juv_food = pot_juv_food
        self.cal_med_rnk = cal_med_rnk

    def __str__(self):
        return "ColID: %s, age: %s, col_food: %s, %s, num spiders: %s" % (self.colony_ID, self.colony_age, self.colony_food, self.alive, len(self.colony_list))

    def print_adults(self):  # prints all instances of adults in the colony
        for i in range(len(self.ad_list)):
            print "i = %s: %s" % (i, self.ad_list[i])

    def print_dets(self):  # prints summary of colony
        print "# col age: %s, ads: %s, juvs: %s, colony food: %s, dispersal?: %s" % (self.colony_age,
                                                                                     len(self.ad_list), len(self.juv_list), self.colony_food, self.dispersers)

    def colony_dict(self):  # the info about each colony to export
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['number_ads'] = len(self.ad_list)
        d['number_juvs'] = len(self.juv_list)
        d['colony_food'] = self.colony_food
        return d

    def col_age_increase(self):  # increases colony age by one
        self.colony_age += 1

    #TODO: check this works, that the function works correctly
    def colony_food(self, F_Ln, K):  # returns tot colony food per capita
        # calculates and updates the food to the colony, F_Ln is food to lone individual (n=0+
        N = len(self.ad_list) - 1  # to maKe F_Ln actually lone ind food rather than colony of size
        K = K - 1  # same reason
        intcp = 1 / (1 - F_Ln)  # intercept
        cal_colFood = (intcp + (1 - (N / K)) * (-1 - (N / K))) / intcp
        tot_col_food = cal_colFood * len(self.colony_list)
        return tot_col_food

    #TODO: change the offspring variables to a list
    def col_num_off(self, OMin, OMax, SMin, SMax):  # Calculating the number of offspring and assigning number to adult
        [i.noOffspring(OMin, OMax, SMin, SMax) for i in self.ad_list]
        off_list = [i.no_off for i in self.ad_list]
        no_new_off = sum(off_list) # calc the total number of new offspring for the colony
        return no_new_off


    def cal_pot_juv_food(self, F_Ln, K, OMin, OMax, SMin, SMax): #updates potential juv food
        tot_food = self.colony_food(F_Ln, K)
        pot_juvs = self.col_num_off(OMin, OMax, SMin, SMax)
        pot_juv_fd = tot_food/pot_juvs
        self.pot_juv_food = pot_juv_fd

    def colDispersal_choice(self, juv_fd_lmt, disp_fd_lmt): # deciding whether to reproduce or disperse
        if self.pot_juv_food < juv_fd_lmt:
            [i.disperseChoice(disp_fd_lmt) for i in self.ad_list]
        else:
            print "no dispersers"

    def spis_to_dis_lst(self):  # makes a list of dispersers
        self.dispersers = [i for i in self.ad_list if i.disperse == 1]
        return self.dispersers
    
    def remove_dispersed(self): # removes the spiders that will disperse
        self.ad_list = [i for i in self.ad_list if i.disperse == 0]
    def reproduction(self):  # all remaining adults reproduce, number of offspring depend on adult size
        no_new_off = sum([i.no_off for i in self.ad_list])
        self.juv_list = list([Juv() for i in range(no_new_off)]) # puts the new offspring into the colony

    def juv_rnk_assign(self): # all juvs are the same size so ranked by location in list i.e randomly
        for index in enumerate(self.juv_list):
            i = index[0]
            self.juv_list[i].rank = i

    def moult(self, min_juvFd): #
        moultList = [i for i in self.juv_list if i.juv_fd >= min_juvFd]
        self.ad_list = [Adult(i.SpiderList(), tot_fd = i.juv_fd) for i in list] # making adults from juvs
        self.juv_list = [] #emptying the old juv list

######### One Colony Time Step ##################

    def colony_timestep(self,dispersal_list, output_list, min_juv_fd):
            # (1) add one to colony age
        self.col_age_increase()  # updates colony age by one

            # (2) adults decide whether to disperse
        self.cal_pot_juv_food(F_Ln, K, OMin, OMax, SMin, SMax) # calculating potental juv food , written to colony
        self.colDispersal_choice(self.ad_min_fd, self.ad_max_fd) #dispersal decision TODO: add variables to pop class
        self.dispersal_list = self.spis_to_dis_lst() + self.dispersal_list #adds spiders to dispersal list
        self.spis_to_dis_lst() #puts the dipersed into a seperate list
        self.remove_dispersed() # removing the dispersed spiders from the colony list

        #rest of the steps -> which will also apply to the newly dispersed spiders, but have to set up to run seperately on those colonies
        self.Core_colony_timestep(dispersal_list, output_list, min_juv_fd)


    def core_colony_timestep(self, dispersal_list, output_list, min_juv_fd):
            #Use just this one for colonies where females dispersed

            # (3) Adults reproduce
        self.reproduction() #all adults within the colonies reproduce, juvs added straight to the colony

            #(4) Calculate colony food + random fluctuation
        self.colony_food(F_Ln, K)

            #(5) food calculated and assigned to juvs with random
        self.juv_rnk_assign()
        #TODO make food distribution function

            # (5) Adults die

        self.ad_list = [] #emptying the adult list - all adults die

            #(6) Juvs moult or die

        self.moult(min_juv_fd) # new juvs added directly to adult list and emptying juv list

            # (7) marking dead colonies (colonies with no spiders) 
        self.col_alive()

            # (8) exporting the data (appending colony info to form to list to export)
        output_list = self.colony_list_to_append()
        self.export_list.append(output_list)


        #TODO: I think these need to go into the population class

        print self.poplt_list[i].colony_dict()

        self.poplt_list[i].print_spiders()