'''
Created on 2013-02-12

@author: ruth
'''

from collections import OrderedDict

from AdultClass import Adult
from JuvClass import Juv
from SpiderClass import Spider
import numpy as np
import random as rndm
import scipy.stats as ss


class Colony(object):
    '''making colony class'''

    def __init__(self, ad_list = [],
                 juv_list =[],
                 new_ad_list = [],
                 new_juv_list = [],
                 colony_ID,
                 colony_food=0.0,
                 colony_age=0,
                 alive = 'alive',
                 dispersers = []):
        self.ad_list = ad_list
        self.juv_list = juv_list
        self.new_ad_list=  new_ad_list,
        self.new_juv_list = new_juv_list,
        self.colony_ID = colony_ID
        self.colony_food = colony_food
        self.colony_age = colony_age
        self.alive = alive
        self.dispersers = dispersers # TODO not sure I need this anymore.

    def __str__(self):
        return "ColID: %s, age: %s, col_food: %s, %s, num spiders: %s" % (self.colony_ID, self.colony_age, self.colony_food, self.alive, len(self.colony_list))

    def tot_col_fd(self): # amt of food per capita
        return self.colony_food * len(self.colony_list)


    def print_adults(self):  #  prints all instances of adults in the colony
        for i in range(len(self.ad_list)):
            print "i = %s: %s" % (i, self.ad_list[i])

    def print_dets(self): # prints summary of colony
        print "# col age: %s, ads: %s, juvs: %s, colony food: %s, dispersal? : %s " % (self.colony_age, len(self.ad_list), len(self.juv_list),self.colony_food, self.dispersers)

    def colony_dict(self):  # the info about each colony to export
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['number_ads']= len(self.ad_list)
        d['number_juvs']= len(self.juv_list)
        d['colony_food']= self.colony_food
        return d

    def col_age_increase(self):  # increases colony age by one
        self.colony_age += 1

    #TODO: test function
    def reproduction(self, OMin, OMax, SMin, SMax):  # now all adults reproduce, number of offspring depend on adult size
        off_list = []
        [i.noOffspring(OMin, OMax, SMin, SMax) for i in self.ad_list]
        off_list = [i.no_off for i in self.ad_list if i.disperse == 0]
        no_new_off = sum(off_list) # calc the total number of new offspring for the colony
        self.new_juv_list = list([Juv() for i in range(no_new_off)]) # puts the new offspring into the colony

    def colDispersal_choice(self, ad_min_fd, ad_max_fd): # deciding whether to reproduce or disperse
        [i.disperseChoice() for i in self.ad_list]
    
    def spis_to_dis_lst(self):  # makes a list of dispersers
        self.dispersers = [i for i in self.ad_list if i.disperse == 1]
        return self.dispersers

    def cal_colony_food(self, F_Ln, K):  # calculates and updates the food to the colony, F_Ln is food to lone individual (n=0+
        N = len(self.juv_list) -1 # to male F_Ln actually lone ind food rather than colony of size
        K = K-1 # same reason
        int = 1/(1-F_Ln)
        cal_colFood = (int + (1-N/K)*(-1-N/K))/int
        self.colony_food = cal_colFood

    #TODO: Maybe just make these variable of colony???

    def newJuvsToColony(self): # adding juvs from new juvs list to old juvs list
        self.juv_list = self.new_juv_list
        self.new_juv_list = []
        

    def ad_rnk_assign(self):  # updates the rank of spiders # 1 lowest rank,if ties,rank in order in list
        #Ranks = ss.rankdata(self.size_list(k), method = 'ordinal')  # assigns ties in order -> arbritary order.
        PreRanks = np.argsort(self.tot_fd_list()) # I might need to use juv food instead of tot_food
        Ranks = np.argsort(PreRanks)
        spds_instr = [i for i in self.ad_list]
        for i, j in zip(spds_instr, Ranks):
            i.update_rank(j)

    def juv_rnk_assign(self): # all juvs are the same size so ranked by location in list i.e randomly
        for index in enumerate(self.juv_list):
            i = index[0]
            self.juv_list[i].rank = i

    def moult(self, min_juvFd): # writing the new juvs to a new adults list
        moultList = [i for i in self.juv_list if i.juv_fd >= min_juvFd]
        self.new_ad_list = [Adult(i.SpiderList(), tot_fd = i.juv_fd) for i in list] # making adults from juvs
        self.juv_list = [] #emptying the old juv list

    
    #Original competition equations
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

    #TODO: check if this actually works
    def col_alive(self): # testing whether colony is dead
        if not self.ad_list and not self.juv_list:
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


    def colony_time_step(self, dispersal_list, output_list, min_juv_fd):
                
            # (1) colony age increase
        self.col_age_increase()  # updates colony age by one

            #(2) Food, colony food then food to 
        self.update_rank()
            #TODO: add competition functions

            # (3) adults reproduce or disperse and reproduce, then die
        self.colDispersal_choice(self.ad_min_fd, self.ad_max_fd) #dispersal decision TODO: add variables to pop class
        self.dispersal_list = self.spis_to_dis_lst() + self.dispersal_list #adds spiders to dispersal list
        self.reproduction() #all adults within the colonies reproduce only if dispersal does not = 1, juvs put into new_juv_list
        self.ad_list = [] #emptying the adult list - all adults die

            # (4) old juvs moult or die
            
        self.moult(min_juv_fd) # new juvs added to new adult list, all juv lists emptied
        
            # (5) new juvs added to colony
        self.newJuvsToColony()      
    
            # (6) marking dead colonies (colonies with no spiders) 
        self.col_alive()

            # (7) exporting the data (appending colony info to form to list to export)
        output_list = self.colony_list_to_append()
        self.export_list.append(output_list)
        print self.poplt_list[i].colony_dict()

        self.poplt_list[i].print_spiders()