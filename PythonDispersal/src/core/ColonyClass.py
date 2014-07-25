'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider
from collections import OrderedDict


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list,
                 colony_ID,
                 colony_food=0.0,
                 colony_age=0, 
                 alive = 'alive',
                 dispersers = []):
        self.colony_list = colony_list
        self.colony_ID = colony_ID
        self.colony_food = colony_food
        self.colony_age = colony_age
        self.alive = alive
        self.dispersers = []


    def MaxAndMinAges(self):
        col_indAge= [i.age for i in self.colony_list]
        age_max = max(col_indAge)
        age_min = min(col_indAge)
        return age_max, age_min  # returns a tuple

    def MaxAndMinSize(self):  # returns the max and min spider size
        col_indSize= [i.size for i in self.colony_list]
        size_max = max(col_indSize)
        size_min = min(col_indSize)
        return size_max, size_min #returns a tuple

    def print_spiders(self):  #  prints all instances of spider in the colony
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def size_list(self):  # returns a list of the size of all individuals in the colony
        return [i.size for i in self.colony_list]

    def print_dets(self):
        print "# col age: %s, spis: %s, size(max: %s, min: %s), age(max: %s, min: %s), colony food: %s " % (self.colony_age, len(self.colony_list), self.MaxAndMinSize()[0], self.MaxAndMinSize()[1],
                                                                               self.MaxAndMinAges()[0], self.MaxAndMinAges()[1], self.colony_food)

    def colony_dict(self):  # the info about each colony to export
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['number_spiders']= len(self.colony_list)
        d['min_size'] = self.MaxAndMinSize()[0]
        d['max_size']= self.MaxAndMinSize()[1]
        d['min_age'] = self.MaxAndMinAges()[0]
        d['max_age'] = self.MaxAndMinAges()[1]
        d['colony_food']= self.colony_food
        return d



    def col_age_increase(self):  # increases colony age by one
        self.colony_age += 1

    def spider_age_increase(self):  # adds one to the age of all spiders in colony
        [i.spi_age_add1 for i in self.colony_list]

    def rep_or_disp(self, min_food, ad_size):  # deciding whether to reproduce or disperse
        [i.dispORrep(min_food) for i in self.colony_list if (i.size >= ad_size and i.reproduce == 0)]

    def reproduction(self, no_off, ad_size):  # ad_size is the size spiders have to be to reproduce
        no_ad = sum(i.reproduce == 1 for i in self.colony_list)  # calcs number of adults reproducing
        [i.update_repr_Two for i in self.colony_list]   # updates reproduce to 2 if it is one.
        no_new_off = no_off * no_ad # calc the total number of new offspring for the colony
        new_spiders = list([Spider() for i in range(no_new_off)]) # makes the spiders
        self.colony_list = self.colony_list + new_spiders  # adds new colony

    def spis_to_dis_lst(self):  # adds the dispersing spiders to the dispersers list
        self.dispersers = [i for i in self.colony_list if i.disperse == 1]

    def update_instar(self, instar_levels): # updating all instars
        [i.instar_inc(instar_levels) for i in self.colony_list]

    def cal_colony_food(self, c, d):  # calculates and updates the food to the colony, 1/c = carrying capacity, d = level of skew
        N = len(self.colony_list)
        mxFd = np.exp(-d) * np.power((d/c), d) # used to scale the equation to make max food = 1
        cal_colFood = (1/mxFd) * np.power(N, d) * np.exp(-c*N)
        self.colony_food = cal_colFood

    #TODO: Maybe just make these variable of colony???

    def update_rank(self):  # updates the rank of spiders # 1 lowest rank,if ties,rank in order in list
        Ranks =  ss.rankdata(self.size_list(), method = 'ordinal')  # assigns ties in order -> arbritary order.
        for i, j in zip(self.colony_list, Ranks):
            i.update_rank(j)

    def scramble(self):  # pure scramble competition, everyone gets the same
        [i.update_indFood(self.colony_food) for i in self.colony_list]

    def contest(self): # the highest ranks get 1 food, everyone else gets zero
        fraction = len(self.colony_list) - (self.colony_food * len(self.colony_list))
        [i.update_indFood(1) for i in self.colony_list if i.rank > fraction] # need to check numbers?

    def ind_food(self, comp): # comp: 0 = scramble, 1 = contest
        [i.update_indFood(0) for i in self.colony_list] # updating all indfood to 0
        if comp == 0:
            self.scramble()
        elif comp == 1:
            self.contest()

#TODO: need to test this more throughly
    def apply_growth(self, growth_amt): # grows every spider in the colony
        '''growth rate is the amount an individual grows per unit time'''
        [i.growth_eq(growth_amt) for i in self.colony_list]  #

    def dying(self, old_age, prob):
        [i.death(old_age, prob) for i in self.colony_list] # marking spiders due to die

    def die_or_ctphe(self, old_age, die_prob, cat_prob, cat_perc_die):
        ranNo = rndm.random()  # gives random numbers between 0 and 1
        if ranNo < cat_prob:
            self.dying(old_age, cat_perc_die)  # higher percentage of individuals die
        else:
            self.dying(old_age, die_prob)  # normal death

    def removing_spiders(self):  # removes spiders that are dead or have dispersed
        self.colony_list = [i for i in self.colony_list if i.die == 0 and i.disperse == 0] 

    def col_alive(self): # testing whether colony is dead
        if not self.colony_list:
            self.alive = 'dead'

    def colony_list_to_append(self): # returns dictionary values
        return self.colony_dict().values()


