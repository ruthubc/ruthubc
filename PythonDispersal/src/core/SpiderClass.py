'''
Created on 2012-12-23

@author: ruth

creating a spider class for each individual with the same method as in test2.
'''
#pylint: disable=C0111
import numpy as np
import random as rndm


class Spider(object):

    def __init__(self, instar = 1, juv_fd=0.0, ad_fd =0.0, rank=1, die = 0, reproduce = 0, disperse = 0):
        '''defining the spider object'''
        self.instar = instar
        self.juv_fd = juv_fd
        self.ad_fd = ad_fd
        self.rank = rank
        self.die = die # 0 means not to die, 1 means die!
        self.reproduce = reproduce # 0 = no, 1 = yes, 2 = already reproduced
        self.disperse = disperse # 0 = no, 1 = yes

    def __str__(self):
        return "age: %s, rank: %s, instar: %s, , die: %s, reproduce: %s, disperse: %s" % ( self.age, self.rank, self.instar, self.die, self.reproduce, self.disperse)

    def instar_inc(self, instar_levels_list): # instar list is a population variable
        for k in range(len(instar_levels_list)):
            if (self.size >= instar_levels_list[k]):
                self.instar = k + 2  # because starts at 0

    def juv_mltORDth(self, jv_min_fd):  # if juv doesn't get enough food she dies before turning into an adult
        if self.juv_fd < jv_min_fd:
            self.die = 1
        else: # increase instar
            self.instar += 1
            
    def ad_death(self, num_instars): # kills all adults, num_instars comes from PopulationClass
        if self.instar == num_instars:
            self.die = 1


    def update_repr_One(self):  # updates self.reproduce to one
        self.reproduce = 1

    def update_repr_Two(self):  # updates reproduction to 2 if it equals 1
        #works checked aug 11th
        if self.reproduce == 1:
            self.reproduce = 2

    def dispORrep (self, ad_min_fd, ad_max_fd): # disperses if gets less than min food, min_food population variable
        if self.ad_fd >= ad_min_fd and self.ad_fd <= ad_max_fd:
                self.disperse = 1

    def update_rank(self, x): # to update relative rank to x
        self.rank = x

    def update_indFood(self, x):
        self.ind_food = x


#TODO: remove this equation
    def growth_eq(self, growth_rate): # individual growth equation, growth rate poplt variable
        term1 = 1-self.size
        term2 = np.exp(-growth_rate * self.ind_food)
        self.size = 1- (term1*term2)

#TODO: remove this function??
    def dis_to_two(self): # not sure we will need this anymore as all adults will die after one generation
        self.disperse = 2

    def contest_many_ind(self, m): #find the rank for each spider
        food = 1- m * self.rank
        if food < 0:
            Fd_ind = 0
        else:
            Fd_ind =  round(food, 4)
        self.ind_food = Fd_ind





