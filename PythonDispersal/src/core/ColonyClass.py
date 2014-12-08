'''
Created on 2013-02-12

@author: ruth
'''

import numpy as np
import scipy.stats as ss
import random as rndm
from SpiderClass import Spider
from collections import OrderedDict


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_list,
                 colony_ID,
                 colony_food=0.0,
                 colony_age=0, 
                 alive = 'alive',
                 dispersers = []):
        self.colony_list = colony_list
        self.colony_ID = colony_ID
        self.colony_food = colony_food
        self.colony_age = colony_age
        self.alive = alive
        self.dispersers = dispersers # TODO not sure I need this anymore.

    def __str__(self):
        return "ColID: %s, age: %s, col_food: %s, %s, num spiders: %s" % (self.colony_ID, self.colony_age, self.colony_food, self.alive, len(self.colony_list))

    def tot_col_fd(self):
        return self.colony_food * len(self.colony_list)

    def MaxAndMinAges(self):
        col_indAge= [i.age for i in self.colony_list]
        age_max = max(col_indAge)
        age_min = min(col_indAge)
        return age_max, age_min  # returns a tuple

#TODO: delete
    def MaxAndMinSize(self):  # returns the max and min spider size
        col_indSize= [i.size for i in self.colony_list]
        size_max = max(col_indSize)
        size_min = min(col_indSize)
        return size_max, size_min #returns a tuple

    def print_spiders(self):  #  prints all instances of spider in the colony
        for i in range(len(self.colony_list)):
            print "i = %s: %s" % (i, self.colony_list[i])

#TODO: delete
    def size_list(self, instar):  # returns a list of the size of all individuals in the colony
        return [i.size for i in self.colony_list if i.instar == instar]

    def juvFd_list(self):  # returns a list of the juv food of all individuals in the colony
        return [i.juv_fd for i in self.colony_list]
    
    def rank_list(self):  # returns a list of the size of all individuals in the colony
        return [i.rank for i in self.colony_list]

    def print_dets(self):
        print "# col age: %s, spis: %s, colony food: %s, dispersal? : %s " % (self.colony_age, len(self.colony_list), self.colony_food, self.dispersers)

    def colony_dict(self):  # the info about each colony to export
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['number_spiders']= len(self.colony_list)
        d['colony_food']= self.colony_food
        return d

    def num_spi(self):
        return len(self.colony_list)

    def col_age_increase(self):  # increases colony age by one
        self.colony_age += 1

#TODO: delete
    def spider_age_increase(self):  # adds one to the age of all spiders in colony
        [i.spi_age_add1() for i in self.colony_list]

    def rep_or_disp(self, ad_min_fd, ad_max_fd):  # deciding whether to reproduce or disperse
        # works checked 11th Aug
        [i.dispORrep(ad_min_fd, ad_max_fd) for i in self.colony_list]

    def reproduction(self, no_off):  # ad_size is the size spiders have to be to reproduce
        no_ad = sum(i.reproduce == 1 for i in self.colony_list)  # calcs number of adults reproducing
        print "number of reproducing adults:",
        print no_ad
        [j.update_repr_Two() for j in self.colony_list]   # updates reproduce to 2 if it is one. #TODO: NOT WORKING
        no_new_off = no_off * no_ad # calc the total number of new offspring for the colony
        new_spiders = list([Spider() for i in range(no_new_off)]) # makes the spiders
        self.colony_list = self.colony_list + new_spiders  # adds new colony

    def spis_to_dis_lst(self):  # adds the dispersing spiders to the dispersers list
        self.dispersers = [i for i in self.colony_list if i.disperse == 1]
        return self.dispersers

    def update_instar(self, instar_levels): # updating all instars
        [i.instar_inc(instar_levels) for i in self.colony_list]

    def cal_colony_food(self, c, d):  # calculates and updates the food to the colony, 1/c = carrying capacity, d = level of skew
        NonJuv = [spi.instar for spi in self.colony_list if spi.instar != 1] # juvs don't contribute to food
        N = len(NonJuv)
        mxFd = np.exp(-d) * np.power((d/c), d) # used to scale the equation to make max food = 1
        cal_colFood = (1/mxFd) * np.power(N, d) * np.exp(-c*N)
        self.colony_food = cal_colFood

    #TODO: Maybe just make these variable of colony???

    def colony_instars(self):
        all_instars = [i.instar for i in self.colony_list]
        unq_instars = list(set(all_instars)) # getting unique instars in colony
        return unq_instars 


    def update_rank(self):  # updates the rank of spiders # 1 lowest rank,if ties,rank in order in list
        for k in self.colony_instars(): # ranks individuals within ranks
            #Ranks = ss.rankdata(self.size_list(k), method = 'ordinal')  # assigns ties in order -> arbritary order.
            PreRanks = np.argsort(self.size_list(k))
            Ranks = np.argsort(PreRanks)
            spds_instr = [i for i in self.colony_list if i.instar == k]
            for i, j in zip(spds_instr, Ranks):
                i.update_rank(j)

    def scramble(self):  # pure scramble competition, everyone gets the same
        [i.update_indFood(self.colony_food) for i in self.colony_list]

    def full_contest(self): # the highest ranks get 1 food, everyone else gets zero
        for k in self.colony_instars():
            insr_len = [j for j in self.colony_list if j.instar == k]
            fraction = len(insr_len) - (self.colony_food * len(insr_len))
            [i.update_indFood(1) for i in self.colony_list if i.rank > fraction and i.instar == k] # need to check numbers?

    def ind_food(self, comp): # comp: 0 = scramble, 1 = mid_contest, 2 = full contest
        [i.update_indFood(0) for i in self.colony_list] # updating all indfood to 0
        if comp == 0:
            print "scramble"
            self.scramble()
        elif comp == 1:
            print "mid_contest"
            self.mid_contest()
        elif comp == 2:
            print "full contest"
            self.full_contest()

#TODO: need to test this more throughly
    def apply_growth(self, growth_amt): # grows every spider in the colony
        '''growth rate is the amount an individual grows per unit time'''
        [i.growth_eq(growth_amt) for i in self.colony_list]  #

    def dying(self, old_age, prob):
        [i.death(old_age, prob) for i in self.colony_list] # marking spiders due to die

    def die_or_ctphe(self, old_age, die_prob, cat_prob, cat_perc_die):
        ranNo = rndm.random()  # gives random numbers between 0 and 1
        print ranNo
        if ranNo < cat_prob:
            self.dying(old_age, cat_perc_die)  # higher percentage of individuals die
        else:
            self.dying(old_age, die_prob)  # normal death

    def removing_spiders(self):  # removes spiders that are dead or have dispersed
        self.colony_list = [i for i in self.colony_list if i.die == 0 and i.disperse != 1] 

    def col_alive(self): # testing whether colony is dead
        if not self.colony_list:
            self.alive = 'dead'

    def colony_list_to_append(self): # returns dictionary values
        return self.colony_dict().values()

    def find_m(self): # finding the gradient for the competition equation
        if self.colony_food < 0.5:
            m = 1/(2*self.colony_food*self.num_spi()-1)
        else :
            m = -(2*(self.colony_food-1))/ (self.num_spi() -1) # everyone gets fed if food_tot >=  0.5
        return m
#TODO: put in exception if m is below or zero!



    def replacing_min_fd(self): # replacing the minimum amount to make sure the numbers add up
        minFdInx = self.food_list().index(min(x for x in self.food_list() if x != 0)) # gets the index of the last spider to be fed
        minSpiFd = self.food_list()[minFdInx]
        tot_ind_fd = sum(self.food_list())
        corrMinAmt = self.tot_col_fd() - (tot_ind_fd - minSpiFd)
        corrMinAmt = round(corrMinAmt, 4)
        self.colony_list[minFdInx].ind_food = corrMinAmt


    def contest_few_inds(self):
        minRankInx = self.rank_list().index(min(self.rank_list()))
        self.colony_list[minRankInx].ind_food = self.tot_col_fd()
    
        
    def mid_contest(self):
        if self.tot_col_fd() < 1.0:
            print "few inds"
            self.contest_few_inds()
        else:
            m = self.find_m()
            print "lots inds m:",
            print m
            [spi.contest_many_ind(m) for spi in self.colony_list]
            self.replacing_min_fd()


