'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
import scipy.stats as ss
from SpiderClass import Spider


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, colony_food=0.0):
        self.colony_list = colony_list
        self.colony_food = colony_food

    def __str__(self):
        return "number of spiders in colony: %s" % (len(self.colony_list))

    def print_spiders(self):
        '''prints all instances of spider in the colony'''
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def age_increment(self): # adds one to age of all spiders in colony
        [i.age_add1() for i in self.colony_list]

    def reproduction(self, no_off, ad_size): # ad_size is the size spiders have to be to reproduce
        #adding new offspring to colony
        no_ad = len([i for i in self.colony_list if i.size >= ad_size])
        no_new_off = no_off * no_ad
        new_spiders = list([Spider() for i in range(no_new_off)])
        self.colony_list = self.colony_list + new_spiders

    def MaxAndMin(self): #returns the max and min spider size
        col_indSize = [i.size for i in self.colony_list] ## Maybe put this in a seperate function?
        size_max = max(col_indSize)
        size_min = min(col_indSize)
        return size_max, size_min #returns a tuple

    def update_col_relSize(self): #updates each spider with its relative size
        maxSz = self.MaxAndMin()[0]
        minSz = self.MaxAndMin()[1]
        [i.update_relSize(i.cal_relSize(maxSz,minSz)) for i in self.colony_list]

    def cal_colony_food(self, c, d): # calculates the food to the colony
        N = len(self.colony_list)
        mxFd = np.exp(-d) * np.power((d/c), d) # used to scale the equation to make max food = 1
        cal_colFood = (1/mxFd) * np.power(N, d) * np.exp(-c*N)
        self.colony_food = cal_colFood



    def size_list(self):
        return [i.size for i in self.colony_list] ## Maybe put this in a seperate function?

    def update_rank(self): #updates the rank of spiders # 1 lowest rank, May have to switch round;
        Ranks =  ss.rankdata(self.size_list())
        for i, j in zip(self.colony_list, Ranks):
            i.update_rank(j)

    def scramble(self): #pure scramble comptition 
        [i.update_indFood(self.colony_food) for i in self.colony_list]

    def contest(self):
        fraction = len(self.colony_list) - (self.colony_food * len(self.colony_list))
        [i.update_indFood(1) for i in self.colony_list if i.rank > fraction] # need to check numbers?
        #what happens if there are ties? Fractions ??

    def ind_food(self, comp): # comp: 0 = scramble, 1 = contest
        [i.update_indFood(0) for i in self.colony_list] # updating all indfood to 0
        if comp == 0:
            self.scramble()
        elif comp == 1:
            self.contest()

#TODO: need to test this more throughly
    def apply_growth(self, growth_amt):
        [i.growth_eq(growth_amt) for i in self.colony_list]

    def dying(self, old_age, prob):
        [i.death(old_age, prob) for i in self.colony_list] # making spiders due to die
        self.colony_list = [i for i in self.colony_list if i.die == 0] # removes spiders that are dead
#TODO: check if I need to update the other aspects of colony


