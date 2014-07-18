'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random as rndm


class Spider(object):

    def __init__(self, size=0.1, instar = 1, rel_size=0.0, ind_food=0.0, age=0.0, rank=1, die = 0, reproduce = 0, disperse = 0):
        '''defining the spider object'''
        self.size = size
        self.rel_size = rel_size
        self.ind_food = ind_food
        self.age = age  # incremented after each time tick.
        self.rank = rank
        self.die = die # 0 means not to die, 1 means die!
        self.reproduce = reproduce # 0 = no, 1 = yes, 2 = already reproduced
        self.disperse = disperse # 0 = no, 1 = yes
        self.instar= instar

    def __str__(self):
        return "size: %s, age: %s, rank: %s, indFood: %s, die: %s" % (self.size, self.age, self.rank, self.ind_food, self.die)

    def instar_inc(self, instar_levels_list):
        for k in range(len(instar_levels_list)):
            if (self.size >= instar_levels_list[k]):
                self.instar = k + 1  # because starts at 0

    def death(self, max_age, mn_prob):  # making mortality age independent: need to define max age somewhere
        ran = rndm.random()  # gives random numbers between 0 and 1
        if self.age > max_age:
            self.die = 1  # you def die after a certain age
        else:
            if ran > mn_prob:
                self.die = 0
            elif ran <= mn_prob:
                self.die = 1

    def cal_relSize(self, maxSize, minSize):
        x= (self.size-minSize)/(maxSize-minSize)
        return x

    def update_repr_One(self):
        self.reproduce = 1

    def update_repr_Two(self): #updates reproduction to two if it equals one
        if self.reproduce == 1:
            self.reproduce = 2 

    def update_disperse(self):
        self.disperse = 1

    def dispORrep (self, min_food): # disperses if gets less than min food, cond for reprod= 0 in colony class
        if self.ind_food < min_food and self.disperse == 0:
            self.update_disperse
        else:
            self.update_reproduce(1)

    def update_relSize(self, x): # to update relative size
        self.rel_size = x

    def spi_age_add1(self): # add one to the age of a spider
        self.age += 1

    def update_rank(self, x): # to update relative size
        self.rank = x

    def update_indFood(self, x):
        self.ind_food = x

    def growth_eq(self, growth_rate):
        term1 = 1-self.size
        term2 = np.exp(-growth_rate * self.ind_food)
        self.size = 1- (term1*term2)

    def update_instar(self):
        self.instar += 1



