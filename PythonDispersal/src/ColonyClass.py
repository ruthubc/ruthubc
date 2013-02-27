'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np

class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, total_size=0.0, total_instar=0.0, total_food=0.0 ):
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
            self.colony_list[i].rel_size = self.colony_list[i].size / self.total_size

    def instar_sum(self):
        '''calculates the total instar in the colony'''
        for i in range(len(self.colony_list)):
            self.total_instar += self.colony_list[i].instar
        return self.total_instar

    def colony_food(self, a, c, d):
        '''total amount of food colony gets'''
        I = self.instar_sum()
        self.total_food = a * np.power(I, d) * np.exp(-c*I)
        return self.total_food

    def ind_food(self):
        '''this right now is pure scramble competition'''
        for i in range (len(self.colony_list)):
            self.colony_list[i].ind_food = self.colony_list[i].rel_size * self.total_food

    def ind_growth(self, a, m):
        '''linear function for % growth, m is the slope
        a is the intercept which has to be negative
            no maximum value added yet'''
        for i in range (len(self.colony_list)):
            per_gro = self.colony_list[i].ind_food * m
            self.colony_list[i].size = per_gro * self.colony_list[i]

    def all_instar(self, instar_levels):
        '''updating all instars in spider with predefined instar levels
        function instar_inc from spider class'''
        for i in range(len(self.colony_list)):
            self.colony_list[i].instar_inc(instar_levels)

