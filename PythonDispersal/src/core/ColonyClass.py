'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list, colony_food=0.0, colony_age = 0):
        self.colony_list = colony_list
        self.colony_food = colony_food
        self.colony_age = colony_age
        
    def __str__(self):
        return "please use the function print_dets"

    def col_age_increase(self):
        self.colony_age += 1   

    def print_spiders(self):
        '''prints all instances of spider in the colony'''
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

    def age_increment(self): # adds one to age of all spiders in colony
        [i.age_add1() for i in self.colony_list]
        
        
    def reprodDisChoice(self, min_food, ad_size): # deciding whether to reproduce or disperse   
        [i.dispORrep(min_food) for i in self.colony_list if i.size >= ad_size and i.reproduce == 0] 
        

    def reproduction(self, no_off, ad_size): # ad_size is the size spiders have to be to reproduce      
        no_ad = sum(i.size >=ad_size and i.reproduce == 1 for i in self.colony_list) # cals number of adults to reproduce
        [i.update_rep_Two for i in self.colony_list]   #updates if adult already reproduced
        no_new_off = no_off * no_ad
        new_spiders = list([Spider() for i in range(no_new_off)])
        self.colony_list = self.colony_list + new_spiders # adds new colony 

    def spis_to_dis_lst(self, dispersal_list): #makes a list 
        dispersers= [i for i in self.colony_list if i.disperse ==1]# if i.disperse == 1] #makes list of spiders who disperse
        dispersal_list = dispersal_list + dispersers
        return dispersal_list
        

    def cal_colony_food(self, c, d): # calculates and updates the food to the colony, 1/c = carrying capacity, d = level of skew
        N = len(self.colony_list)
        mxFd = np.exp(-d) * np.power((d/c), d) # used to scale the equation to make max food = 1
        cal_colFood = (1/mxFd) * np.power(N, d) * np.exp(-c*N)
        self.colony_food = cal_colFood
                         
    #TODO: Maybe just make these variable of colony
    def MaxAndMinSize(self): #returns the max and min spider size
        col_indSize= [i.size for i in self.colony_list]
        size_max = max(col_indSize)
        size_min = min(col_indSize)
        return size_max, size_min #returns a tuple

    def update_col_relSize(self): #updates each spider with its relative size
        maxSz = self.MaxAndMinSize()[0]
        minSz = self.MaxAndMinSize()[1]
        [i.update_relSize(i.cal_relSize(maxSz,minSz)) for i in self.colony_list]

    def size_list(self):
        return [i.size for i in self.colony_list]        
    
    def update_rank(self): #updates the rank of spiders # 1 lowest rank, May have to switch round;
        Ranks =  ss.rankdata(self.size_list(), method = 'ordinal')
        #assins ties in order -> arbritary order.
        for i, j in zip(self.colony_list, Ranks):
            i.update_rank(j)

    def scramble(self): #pure scramble competition
        [i.update_indFood(self.colony_food) for i in self.colony_list]

#TODO: what happens if there are ties?
    def contest(self):
        fraction = len(self.colony_list) - (self.colony_food * len(self.colony_list))
        [i.update_indFood(1) for i in self.colony_list if i.rank > fraction] # need to check numbers?


    def ind_food(self, comp): # comp: 0 = scramble, 1 = contest
        [i.update_indFood(0) for i in self.colony_list] # updating all indfood to 0
        if comp == 0:
            self.scramble()
        elif comp == 1:
            self.contest()

#TODO: need to test this more throughly
    def apply_growth(self, growth_amt): # growth rate is the amount an individual grows per unit 
        [i.growth_eq(growth_amt) for i in self.colony_list]

    def dying(self, old_age, prob):
        [i.death(old_age, prob) for i in self.colony_list] # marking spiders due to die

#TODO: check if I need to update the other aspects of colony


    def MaxAndMinAges(self):
        col_indAge= [i.age for i in self.colony_list]
        age_max = max(col_indAge)
        age_min = min(col_indAge)
        return age_max, age_min #returns a tuple
    
    #TODO: check is working!
    def dieOrCtphe(self, old_age, die_prob, cat_prob, cat_perc_die):
        ranNo = rndm.random() # gives random numbers between 0 and 1
        if ranNo<cat_prob:
            self.dying(old_age, cat_perc_die) # higher percentage of individuals die
        else:
            self.dying(old_age, die_prob) # normal death
            
    def removing_spiders(self):  
        self.colony_list = [i for i in self.colony_list if i.die == 0 and i.disperse == 0] 
        # removes spiders that are dead or have dispersed    
        
    def col_alive(self):
        if self.colony_list:
            return 'alive'
        else:
            return 'dead'       
         

    def print_dets(self):
        print "# col age: %s, spis: %s, size(max: %s, min: %s), age(max: %s, min: %s), colony food: %s " % (self.colony_age, len(self.colony_list), self.MaxAndMinSize()[0], self.MaxAndMinSize()[1],
                                                                               self.MaxAndMinAges()[0], self.MaxAndMinAges()[1], self.colony_food)
        


