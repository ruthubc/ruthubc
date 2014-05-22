'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random as rndm


class Spider(object):

    def __init__(self, size=0.1, rel_size=0.0, ind_food=0.0, age=0.0):
        '''defining the spider object'''
        self.size = size
        self.rel_size = rel_size
        self.ind_food = ind_food
        self.age = age  # incremented after each time tick.

    def __str__(self):
        return "size: %s, rel size: %s, age: %s" % (self.size, self.rel_size, self.age)


    def death(self, max_age, mn_prob):  # making mortality age independent: need to define max age somewhere
        ran = rndm.random()  # gives random numbers between 0 and 1
        if self.age > max_age:
            x = 1  # you def die after a certain age
        else:
            if ran > mn_prob:
                x = 0
            elif ran <= mn_prob:
                x = 1
        return x, ran

    def size(self):
        return self.size

    def cal_relSize(self, maxSize, minSize):
        x= (self.size-minSize)/(maxSize-minSize)
        return x

    def update_relSize(self, x): # to update relative size
        self.rel_size = x

    def age_add1(self): # add one to the age of a spider
        self.age += 1



