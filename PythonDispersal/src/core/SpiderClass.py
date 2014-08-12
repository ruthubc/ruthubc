'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random as rndm


class Spider(object):

    def __init__(self, size=0.1, instar = 1, ind_food=0.0, age=3, rank=1, die = 0, reproduce = 0, disperse = 0):
        '''defining the spider object'''
        self.size = size
        self.ind_food = ind_food
        self.age = age  # incremented after each time tick.
        self.rank = rank
        self.die = die # 0 means not to die, 1 means die!
        self.reproduce = reproduce # 0 = no, 1 = yes, 2 = already reproduced
        self.disperse = disperse # 0 = no, 1 = yes
        self.instar= instar

    def __str__(self):
        return "size: %s, age: %s, rank: %s, indFood: %s, die: %s, reproduce: %s, disperse: %s" % (self.size, self.age, self.rank, self.ind_food, self.die, self.reproduce, self.disperse)

    def instar_inc(self, instar_levels_list): # instar list is a population variable
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

    def cal_relSize(self, maxSize, minSize):  # calculating relative size, but i don't think 
        x= (self.size-minSize)/(maxSize-minSize)
        return x

    def update_repr_One(self):  # updates self.reproduce to one
        self.reproduce = 1

    def update_repr_Two(self):  # updates reproduction to 2 if it equals 1
        #works checked aug 11th
        if self.reproduce == 1:
            self.reproduce = 2

    def dispORrep (self, ad_size, min_food): # disperses if gets less than min food, min_food population variable
        #works!! (tested 11th Aug)
        if self.size >= ad_size and self.reproduce == 0:
            if self.ind_food < min_food and self.disperse == 0:
                self.disperse = 1
            else:
                self.reproduce = 1

    def spi_age_add1(self): # add one to the age of a spider
        self.age += 1

    def update_rank(self, x): # to update relative rank to x
        self.rank = x

    def update_indFood(self, x):
        self.ind_food = x

    def growth_eq(self, growth_rate): # individual growth equation, growth rate poplt variable
        term1 = 1-self.size
        term2 = np.exp(-growth_rate * self.ind_food)
        self.size = 1- (term1*term2)




