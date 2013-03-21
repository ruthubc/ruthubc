'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random


class Spider(object):

    def __init__(self, size=0.1, instar=1, rel_size=0.0, ind_food=0.0, age=0.0):
        '''defining the spider object'''
        self.instar = instar
        self.size = size
        self.rel_size = rel_size
        self.ind_food = ind_food
        self.age = age  # incremented after each time tick.

    def __str__(self):
        return "instar: %s, size: %s, rel size: %s, age: %s" % (self.instar,
                self.size, self.rel_size, self.age)

    def instar_inc(self, instar_levels_list):
        '''updates the instars of individuals after feeding
        instar_levels_list defines the size limits to progress instars'''

        for k in range(len(instar_levels_list)):

            if (self.size >= instar_levels_list[k]):
                self.instar = k + 1  # because starts at 0

    def death(self, ad_min, juv_max, curve, max_age):
        scale = -np.log(ad_min / juv_max)
        ran = random.random()
        if self.age > max_age:
            prob = 0.9
        else:
            prob = juv_max * np.exp(-curve * scale * self.size)

        if ran < prob:
                x = 0
        elif ran >= prob:
                x = 1
        return x
