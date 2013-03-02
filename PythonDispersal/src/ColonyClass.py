'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
from SpiderClass import Spider


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, total_size=0.0, total_instar=0.0,
                 total_food=0.0):
        self.colony_list = colony_list
        self.total_size = total_size
        self.total_instar = total_instar
        self.total_food = total_food

    def print_spiders(self):
        '''prints all instances of spider in the colony'''
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def col_size(self):
        '''calculates the total size of all spiders in a colony'''
        self.total_size = 0.0
        for i in range(len(self.colony_list)):
            self.total_size += self.colony_list[i].size
        return self.total_size

    def rel_size(self):
        '''calculates the relative size of ind compared to colony
        size varies between 0 and 1 to determine prop of food to ind'''
        self.col_size()  # getting total colony size
        for i in range(len(self.colony_list)):
            self.colony_list[i].rel_size = (self.colony_list[i].size
                                            / self.total_size)

    def instar_sum(self):
        '''calculates the total instar in the colony'''
        for i in range(len(self.colony_list)):
            self.total_instar += self.colony_list[i].instar
        return self.total_instar

    def colony_food(self, scaling, car_cap, skew):
        '''total amount of food colony gets
        skew > 1'''
        col_tot_instar = self.instar_sum()  # total instar of colony
        self.total_food = (scaling * np.power(col_tot_instar, skew)
                           * np.exp(-car_cap * col_tot_instar))
        return self.total_food

    def ind_food(self):
        '''this right now is pure scramble competition'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].ind_food = (self.colony_list[i].rel_size
                                            * self.total_food)

    def ind_growth(self, intercept, slope):
        '''linear function for % growth, m is the slope
        a is the intercept which has to be negative
            no maximum value added yet'''
        for i in range(len(self.colony_list)):
            per_gro = intercept + self.colony_list[i].ind_food * slope
            self.colony_list[i].size = per_gro * self.colony_list[i].size

    def update_instar(self, instar_levels):
        '''updating all instars in spider with predefined instar levels
        function instar_inc from spider class'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].instar_inc(instar_levels)

    def reproduction(self, no_offspring):
        '''adding new offspring to colony'''
        no_ads = len([spi for spi in self.colony_list if spi.instar > 6])
        offsp = [Spider()] * (no_offspring * no_ads)
        self.colony_list.extend(offsp)

    def dying(self, index):
        '''given the index of the individual in the colony list,
        deletes the spider at that location'''
        del self.colony_list[index]

    def age_increment(self):
        "increases age of spider by one"
        for i in range(len(self.colony_list)):
            self.colony_list[i].age += 1
