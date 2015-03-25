'''
Created on 2013-02-12

@author: ruth
'''
#pylint: disable= too-many-statements, line-too-long, C

from collections import OrderedDict
import numpy as np
from AdultClass import Adult
from JuvClass import Juv
from SpiderClass import Spider
from Competition import Comp
import random
# import core.Functions


class Colony(object):
    '''making colony class'''

    def __init__(self, colony_ID = 0,
                 ad_list = [],
                 juv_list = [],
                 colony_food=0.0,
                 slope = 0.10,
                 colony_age=0,
                 dispersers = [],
                 pot_juv_food = 0  # potential food to juvs
                 ):
        self.colony_ID = colony_ID
        self.ad_list = ad_list
        self.juv_list = juv_list
        self.colony_food = float(colony_food)
        self.slope = slope
        self.colony_age = colony_age
        self.dispersers = dispersers
        self.pot_juv_food = pot_juv_food
        self.num_juvs = 0  # as the juv list gets wiped before the end of the loop
        self.num_ads = 0  # to make sure that I get 
        self.num_dis = 0
        self.alive = 'alive'
        self.cal_med_rnk = 0.0

    def __str__(self):
        return "ColID: %s, age: %s, col_food: %s, %s, num spiders: %s" % (self.colony_ID, self.colony_age, self.colony_food, self.alive, len(self.colony_list))

    def print_adults(self):  # prints all instances of adults in the colony
        for i in range(len(self.ad_list)):
            print "i = %s: %s" % (i, self.ad_list[i])

    def print_dets(self):  # prints summary of colony
        print "# col age: %s, ads: %s, juvs: %s, colony food: %s, dispersal?: %s" % (self.colony_age,
                                                                                     len(self.ad_list), len(self.juv_list), self.colony_food, self.dispersers)

    def colony_dict(self):  # the info about each colony to export
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['num ads'] = self.num_ads
        d['numjuvs'] = self.num_juvs
        d['colony_food'] = self.colony_food
        d['dispersers'] = self.num_dis
        d['pot_juv_fd'] = self.pot_juv_food
        d['cal_med_rnk'] = self.cal_med_rnk
        d['colAlive'] = self.alive
        return d

    def col_age_increase(self):  # increases colony age by one
        self.colony_age += 1

    def col_alive(self): # testing whether colony is dead
        if not self.juv_list and not self.ad_list:
            self.alive = 'dead'    #checked feb  12, but need to make sure that the variables are the correct type

    def cal_col_food(self, F_Ln, K):  # returns tot colony food per capita
        # calculates and updates the food to the colony, F_Ln is food to lone individual (n=0+
        N_tot = len(self.ad_list) # to maKe F_Ln actually lone ind food rather than colony of size
        N = N_tot - 1  # to maKe F_Ln actually lone ind food rather than colony of size
        K = K - 1  # same reason
        brac = 1-(N/K)
        F = 1 / (1 - F_Ln)  # intercept
        cal_colFood = (F + brac*(-brac))/F
        tot_col_food = cal_colFood * N_tot
        return tot_col_food

    def col_food_random(self, F_Ln, K, K_var, FLn_var): # randomly fluctuates colony food
        #TODO: Test this function and what numbers are produced
        from core.Functions import random_gus
        New_K = random_gus(K, K_var)
        print "newK", New_K
        New_FLn = random_gus(F_Ln, FLn_var)
        print "NewFln", New_FLn
        food = self.cal_col_food(New_FLn, New_K)
        if food <= 0:
            raise ValueError("Colony food was negative or zero")
        else:
            self.colony_food = food
            print "randomColFood", food

    def col_num_off(self, Off_M, Off_C):  # Calculating the number of offspring and assigning number to adult
        #TODO: test num offspring equation
        [i.noOffspring(Off_M, Off_C) for i in self.ad_list]
        off_list = [i.no_off for i in self.ad_list]
        print off_list
        no_new_off = sum(off_list)  # calc the total number of new offspring for the colony
        return no_new_off  #TODO: do I really have to return no new off??

    def cal_pot_juv_food(self, F_Ln, K, Off_M, Off_C):  # updates potential juv food
        tot_food = self.cal_col_food(F_Ln, K)
        pot_juvs = self.col_num_off(Off_M, Off_C)
        pot_juv_fd = tot_food / pot_juvs
        self.pot_juv_food = pot_juv_fd

    def colDispersal_choice(self, juv_disFd_lmt, ad_disFd_lmt, disp_rsk):  # deciding whether to reproduce or disperse
        if self.pot_juv_food < (float(juv_disFd_lmt) * disp_rsk):
            [i.disperseChoice(ad_disFd_lmt) for i in self.ad_list] # dispersal choice ensures adult over specific size to disperse
        else:
            print "no dispersers"

    def spis_to_dis_lst(self):  # makes a list of dispersers and removes them from the old colony
        self.dispersers = [i for i in self.ad_list if i.disperse == 1]
        print "number of dispersers:", len(self.dispersers)
        self.num_dis = len(self.dispersers)
        self.ad_list = [i for i in self.ad_list if i.disperse == 0 and i.die == 0]

    def reproduction(self):  # all remaining adults reproduce, number of offspring depend on adult size
        no_new_off = sum([i.no_off for i in self.ad_list])
        self.num_juvs = no_new_off
        print "number of offspring",
        print no_new_off
        for num in range (0, no_new_off):
            self.juv_list.extend([Juv()])
        print 'length of juv list', len(self.juv_list)
        # Could not do this, as everytime updated one juv, all were updatedself.juv_list = [Juv()] * no_new_off 

    def juv_rnk_assign(self):  # all juvs are the same size so ranked by location in list i.e randomly
        for index in enumerate(self.juv_list):
            i = index[0]
            self.juv_list[i].rank = i

    def comp_slope(self):
        return float(self.slope) / float(len(self.juv_list))

    def cal_ind_food(self, ind_rnk):  # TODO: check this works
        slope = self.comp_slope()
        xbr = float(self.colony_food) / float(len(self.juv_list))
        #tm1 = slope * self.cal_med_rnk
        #tm2 = xbr - ((xbr * float(ind_rnk)) / self.cal_med_rnk)
        #tm12 = (tm1 * tm2) / np.power(xbr, 2)
        topTerm = (slope * self.cal_med_rnk) * (xbr - ((xbr * ind_rnk) / self.cal_med_rnk))
        fracTerm = topTerm / (np.power(xbr, 2))
        CompEqn = (1+ fracTerm) * xbr
        #CompEqn = xbr * (1 + tm12)
        if CompEqn > 1:
            return 1
        elif CompEqn < 0:
            return 0
        else:
            return CompEqn

    def juv_fd_assign(self):
        for spider in self.juv_list:
            jv_rnk = spider.rank
            ind_fd = self.cal_ind_food(jv_rnk)
            spider.juv_fd = ind_fd
        return [jv.juv_fd for jv in self.juv_list]

    def zeroSlp_jv_fd(self):  # dist food if comp slope = 1
        ind_fd = self.colony_food / float(len(self.juv_list))
        for spider in self.juv_list:
            spider.juv_fd = ind_fd

    def oneSlp_jv_fd(self):
        if self.colony_food >= 1:
            #assign all to top ranks
            num_get_fd = np.floor(self.colony_food)
            remain = self.colony_food - num_get_fd
            for spider in self.juv_list:
                if spider.rank <= num_get_fd -1:  # as rank starts at zero
                    spider.juv_fd = 1
                elif spider.rank == num_get_fd:
                    spider.juv_fd = remain
                else:
                    spider.juv_fd = 0
        else:
            for spider in self.juv_list:
                if spider.rank == 0:  # as rank starts at zero
                    spider.juv_fd = remain
                else:
                    spider.juv_fd = 0

    def distr_food(self):
        if len(self.juv_list) <= 1:
            self.juv_list[0].food = self.colony_food  #TODO: maybe put something in here to make sure that nver abv1
        if self.slope < 0.0000001:
            self.zeroSlp_jv_fd()
        elif self.slope == 10.0: # arbiarity number! maybe make this more a range jsut to make sure it is captured in the code.
            # TODO: make range
            self.oneSlp_jv_fd()
        else:
            c_slpe = self.comp_slope() 
            self.juv_rnk_assign()  # assign ranks to juvs
            cmp_obj = Comp(self.colony_food, len(self.juv_list), c_slpe)  # making competition object
            self.cal_med_rnk = cmp_obj.CompFunction()
            self.juv_fd_assign()

    def moult(self, min_juvFd):
        moult_list = [i for i in self.juv_list if i.juv_fd >= min_juvFd]
        self.ad_list = [Adult(i.SpiderList()) for i in moult_list]  # making adults from juvs
        print 'number of juvs moulting', len(self.ad_list)
        self.juv_list = []  # emptying the old juv list

    def colony_list_to_append(self):  # returns dictionary **values** in list form
        return self.colony_dict().values()

######### One Colony Time Step ##################

    def core_colony_timestep(self, F_Ln, FLn_var, K, K_var, min_juv_fd, pop_export_list):
            #Use just this one for colonies of females dispersed

            # (3) Adults reproduce


        self.reproduction()  # all adults within the colonies reproduce, juvs added straight to the colony

            #(4) Calculate colony food + random fluctuation
        self.col_food_random(F_Ln, K, K_var, FLn_var)

            #(5) food calculated and assigned to juvs with random
        self.distr_food()

            #(6) Adults die
        self.num_ads = len(self.ad_list)
        self.ad_list = []  # emptying the adult list - all adults die

            #(7) Juvs moult or die
        self.moult(min_juv_fd)  # new juvs added directly to adult list and emptying juv list

            # (8) marking dead colonies (colonies with no spiders)
        self.col_alive()

            # (9) exporting the data (appending colony info to form to list to export)
        pop_export_list.append(self.colony_list_to_append())  # appends the dictionary values to population export list

            # (10) Printing some things to the console
        print self.colony_dict()

    def colony_timestep(self, F_Ln, FLn_var, K, K_var, Off_M, Off_C, juv_disFd_lmt, ad_disFd_lmt, pop_dis_list, min_juv_fd, disp_risk, pop_export_list):
            # (1) add one to colony age
        self.col_age_increase()  # updates colony age by one

            # (2) adults decide whether to disperse
        self.cal_pot_juv_food(F_Ln, K, Off_M, Off_C)  # calculating potental juv food , written to colony
        self.colDispersal_choice(juv_disFd_lmt, ad_disFd_lmt, disp_risk)
        self.spis_to_dis_lst()
        pop_dis_list.extend(self.dispersers) # adds spiders to population dispersal list
        self.dispersers = []  # clears dispersal list

        #TODO: check why colony being written to file twice if all disperse
        self.col_alive()
        if self.alive == 'dead':
            pop_export_list.append(self.colony_list_to_append())  # appends the dictionary values to population export list
            print self.colony_dict()
            print "all spiders dispersed"
        else:  # rest of the steps -> which will also apply to the newly dispersed spiders, but have to set up to run seperately on those colonies
            self.core_colony_timestep(F_Ln, FLn_var, K, K_var,  min_juv_fd, pop_export_list)
