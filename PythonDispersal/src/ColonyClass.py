'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
from SpiderClass import Spider


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, total_food=0.0):
        self.colony_list = colony_list
        self.total_food = total_food

    def __str__(self):
        return "number of spiders in colony: %s" % (len(self.colony_list))

    def print_spiders(self):
        '''prints all instances of spider in the colony'''
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def MaxAndMin(self): #returns the max and min spider size
        col_indSize = [i.size for i in self.colony_list]
        size_max = max(col_indSize)
        size_min = min(col_indSize)
        return size_max, size_min #returns a tuple

    def update_col_relSize(self): #updates each spider with its relative size
        maxSz = self.MaxAndMin()[0]
        minSz = self.MaxAndMin()[1]
        [i.update_relSize(i.cal_relSize(maxSz,minSz)) for i in self.colony_list]



    def colony_food(self, car_cap, skew):
        '''total amount of food colony gets
        skew > 1'''
        col_tot_instar = self.instar_sum()  # total instar of colony
        self.total_food = (np.exp(skew - car_cap * col_tot_instar) *
                           np.power((car_cap * col_tot_instar / skew), skew))
        return self.total_food

    def ind_food(self):
        '''this right now is pure scramble competition'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].ind_food = self.total_food

    def ind_growth(self, growth_rate):
        '''non linear function that increases size based on food
        and current size'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].size = (1 - ((1 - self.colony_list[i].size) *
                                             np.exp(-growth_rate * self.colony_list[i].ind_food)))

    def update_instar(self, instar_levels):
        '''updating all instars in spider with predefined instar levels
        function instar_inc from spider class'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].instar_inc(instar_levels)

    def reproduction(self, no_offspring):
        '''adding new offspring to colony'''
        no_ads = len([spi for spi in self.colony_list if spi.instar >= 5])
        no_new_ofs = no_offspring * no_ads
        #offsp = [Spider()] * no_new_ofs
        #self.colony_list = offsp + self.colony_list
        new_spiders = list([Spider() for i in range(no_new_ofs)])
        #self.colony_list([Spider() for i in range(no_new_ofs)])
        self.colony_list = self.colony_list + new_spiders

    def dying(self, ad_min, juv_max, curve, max_age):
        '''given the index of the individual in the colony list,
        deletes the spider at that location'''
        for i in range(len(self.colony_list)):
            mort = self.colony_list[i].death(ad_min, juv_max, curve, max_age)
            index = []
            if mort == 1:
                index.append(i)

        self.colony_list = [k for j, k in enumerate(self.colony_list) if j not in index]

    def age_increment(self):
        "increases age of spider by one"
        for i in range(len(self.colony_list)):
            self.colony_list[i].age += 1

