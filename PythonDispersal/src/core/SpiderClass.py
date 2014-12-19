'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random as rndm


class Spider(object):

    def __init__(self, rank=1, die = 0):
        '''defining the spider object'''
        self.rank = rank
        self.die = die # 0 means not to die, 1 means die!

    def __str__(self):
        return "age: %s, rank: %s, instar: %s, , die: %s, reproduce: %s, disperse: %s" % ( self.age, self.rank, self.instar, self.die, self.reproduce, self.disperse)

    def SpiderList(self):
        return [self.rank, self.die]

    def update_repr_One(self):  # updates self.reproduce to one
        self.reproduce = 1

    def update_rank(self, x): # to update relative rank to x
        self.rank = x

    def update_indFood(self, x):
        self.ind_food = x


    def contest_many_ind(self, m): #find the rank for each spider
        food = 1- m * self.rank
        if food < 0:
            Fd_ind = 0
        else:
            Fd_ind =  round(food, 4)
        self.ind_food = Fd_ind





