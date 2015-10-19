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
import csv

# import core.Functions


class Colony(object):
    '''making colony class'''

    def __init__(self, indFile = "n", colony_ID = 0,
                 ad_list = [],
                 slope = 0.10,
                 colony_age=0,
                 dispersers = [],
                 pot_juv_food = 0  # potential food to juvs
                 ):
        self.indFile = indFile
        self.colony_ID = colony_ID
        self.ad_list = ad_list
        self.juv_list = []
        self.colony_food = 0.0
        self.slope = slope
        self.colony_age = colony_age
        self.dispersers = dispersers
        self.pot_juv_food = pot_juv_food
        self.num_juvs = 0  # as the juv list gets wiped before the end of the loop
        self.num_ads = 0  # to make sure that I get 
        self.num_dis = 0
        self.alive = 'alive'
        self.cal_med_rnk = 0.0
        self.num_moult = 0.0
        self.newCol = "No"
        self.adSz_B4 = ['NA', 'NA', 'NA', 'NA', 'NA'] # list order is min, max, mead, standard dev, count
        self.adSz_AF = ['NA', 'NA', 'NA', 'NA', 'NA']
        self.jvSz_B4 = ['NA', 'NA', 'NA', 'NA', 'NA']
        self.jvSz_AF = ['NA', 'NA', 'NA', 'NA', 'NA']

    def __str__(self):
        return "ColID: %s, age: %s, col_food: %s, %s, num adults: %s" % (self.colony_ID, self.colony_age, self.colony_food, self.alive, len(self.ad_list))

    def indStats(self, spiList): # so that I won't have to output all the individuals
        if len(spiList) > 0:
            minm = min(spiList)
            mx = max(spiList)
            mn = np.mean(spiList)
            sd = round(np.std(spiList), 4)  # see if I can change to (x - xbar) ^2
            ct = len(spiList)
        else:
            minm = "NA"
            mx = "NA"
            mn = "NA"
            sd = "NA"
            ct = "NA"
        return [minm, mx, mn, sd, ct]

    def print_adults(self):  # prints all instances of adults in the colony
        for i in range(len(self.ad_list)):
            print "i = %s: %s" % (i, self.ad_list[i])

    def print_dets(self):  # prints summary of colony
        print "colID: %s, numAds: %s, slope %s, numJuvs %s, colFood = %s" % (self.colony_ID, len(self.ad_list), self.slope,  len(self.juv_list), self.colony_food)

    def colony_dict(self):  # the info about each colony to export
        # TODO: check that the correct things are being exported i.e. the correct number of juvs
        d = OrderedDict()
        d['colony_ID'] = self.colony_ID
        d['colony_age'] = self.colony_age
        d['num_adsB4_dispersal'] = self.num_ads
        d['num_ads'] = self.num_ads - self.num_dis
        d['numjuvs'] = self.num_juvs
        d['colony_food'] = self.colony_food
        d['ave_food']= 0 if self.num_juvs == 0 else self.colony_food/self.num_juvs
        d['dispersers'] = self.num_dis
        d['pot_juv_fd'] = self.pot_juv_food
        d['cal_med_rnk'] = self.cal_med_rnk
        d['colAlive'] = self.alive
        d['num_juvs_moulting'] = self.num_moult
        d['new_col?'] = self.newCol
        d['adSz_B4_min'] = self.adSz_B4[0] # list order is min, max, mean, standard dev
        d['adSz_B4_max'] = self.adSz_B4[1]
        d['adSz_B4_mean'] = self.adSz_B4[2]
        d['adSz_B4_SD'] = self.adSz_B4[3]
        d['adSz_B4_ct'] = self.adSz_B4[4]
        d['adSz_AF_min'] = self.adSz_AF[0] # list order is min, max, mean, standard dev
        d['adSz_AF_max'] = self.adSz_AF[1]
        d['adSz_AF_mean'] = self.adSz_AF[2]
        d['adSz_AF_SD'] = self.adSz_AF[3]
        d['adSz_AF_ct'] = self.adSz_AF[4]
        d['jvSz_B4_min'] = self.jvSz_B4[0] # list order is min, max, mean, standard dev
        d['jvSz_B4_max'] = self.jvSz_B4[1]
        d['jvSz_B4_mean'] = self.jvSz_B4[2]
        d['jvSz_B4_SD'] = self.jvSz_B4[3]
        d['jvSz_B4_ct'] = self.jvSz_B4[4]
        d['jvSz_AF_min'] = self.jvSz_AF[0] # list order is min, max, mean, standard dev
        d['jvSz_AF_max'] = self.jvSz_AF[1]
        d['jvSz_AF_mean'] = self.jvSz_AF[2]
        d['jvSz_AF_SD'] = self.jvSz_AF[3]
        d['jvSz_AF_ct'] = self.jvSz_AF[4]

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
        NOvK = (N/K)
        int = np.log(1/F_Ln)
        F = 1 / (1 - F_Ln)  # intercept
        cal_colFood = np.exp((1-NOvK)*(NOvK-1) *int)
        cap_col_food = cal_colFood
        if cap_col_food < 0:
            print "cal colony food", cal_colFood
            raise ValueError("cal Colony food was negative or zero, numads s%:, cap_col_food s%" % (N_tot, cap_col_food))
        else:
            print ("no error in cap col food", cap_col_food)
            return cap_col_food

    def col_food_random(self, F_Ln, K, var, food_scale):  # randomly fluctuates colony food 
        from core.Functions import random_gus
        cap_food = self.cal_col_food(F_Ln, K)
        cap_food = random_gus(cap_food, var) * food_scale
        tot_food = cap_food * len(self.ad_list)
        if len(self.ad_list) and len(self.juv_list) == 0:
            raise ValueError("no spiders in colony")
        elif tot_food < 0:
            raise ValueError("Colony food was negative or zero")
        elif tot_food > len(self.juv_list):
            self.colony_food = len(self.juv_list)
        else:
            self.colony_food = tot_food
        print "randomColFood with scaling", self.colony_food

    def col_num_off(self, Off_M, Off_C):  # Calculating the number of offspring and assigning number to adult
        #TODO: test num offspring equation
        [i.noOffspring(Off_M, Off_C) for i in self.ad_list]
        off_list = [i.no_off for i in self.ad_list]
        #print off_list
        no_new_off = sum(off_list)  # calc the total number of new offspring for the colony
        return no_new_off  #TODO: do I really have to return no new off??

    def cal_pot_juv_food(self, F_Ln, K, Off_M, Off_C, food_scale):  # updates potential juv food
        tot_food = self.cal_col_food(F_Ln, K) * len(self.ad_list) * food_scale
        pot_juvs = self.col_num_off(Off_M, Off_C)
        pot_juv_fd = tot_food / pot_juvs
        self.pot_juv_food = pot_juv_fd
        #print "adfood", [i.food for i in self.ad_list]
        print "pot juv food", pot_juv_fd

    def colDispersal_choice(self,  ad_disFd_lmt):  # deciding whether to reproduce or disperse
            [i.disperseChoice(ad_disFd_lmt) for i in self.ad_list] # dispersal choice ensures adult over specific size to disperse
            print "tot num of adults before dispersal colDis_choice", len(self.ad_list)

    def spis_to_dis_lst(self, adDisLmt):  # makes a list of dispersers and removes them from the old colony
        #print "ad food before dispersal"
        #print [i.food for i in self.ad_list]
        self.dispersers = [i for i in self.ad_list if i.disperse == 1]
        self.num_dis = len(self.dispersers)
        self.ad_list = [i for i in self.ad_list if i.disperse == 0]
        for ad in self.ad_list:
            if ad.food > adDisLmt:
                raise Exception ("Non-dispered adults larger then", adDisLmt, "which was", ad.food)
        #print [i.food for i in self.ad_list]
        print "spis to dis lst"
        print "new no of ads:", len(self.ad_list), "num dispersers:", self.num_dis
        if self.num_dis > 0:
            self.adSz_AF = self.indStats([i.food for i in self.ad_list]) # ad sizes after dispersal
        else:
            self.adSz_AF = self.indStats([])

    def reproduction(self):  # all remaining adults reproduce, number of offspring depend on adult size
        no_new_off = sum([i.no_off for i in self.ad_list])
        self.num_juvs = no_new_off
        print "number of offspring", no_new_off
        for num in range (0, no_new_off):
            self.juv_list.extend([Juv()])
        # print 'length of juv list', len(self.juv_list)
        # Could not do this, as everytime updated one juv, all were updatedself.juv_list = [Juv()] * no_new_off 

    def juv_rnk_assign(self):  # all juvs are the same size so ranked by location in list i.e randomly
        for index in enumerate(self.juv_list):
            i = index[0]
            self.juv_list[i].rank = i

    def comp_slope(self):
        return float(self.slope) / float(len(self.juv_list)-1)

    def cal_ind_food(self, ind_rnk):
        slope = self.comp_slope()
        ind_rnk = float(ind_rnk)
        xbr = float(self.colony_food) / float(len(self.juv_list))
        if xbr > 1:
            raise Exception("xbar greater than one:", xbr)
        else:
            topTerm = (slope * self.cal_med_rnk) * (xbr - ((xbr * ind_rnk) / self.cal_med_rnk))
            fracTerm = topTerm / (np.power(xbr, 2))
            CompEqn = (1+ fracTerm) * xbr
            if CompEqn > 1:
                return 1
            elif CompEqn < 0.001:
                return 0
            else:
                return CompEqn

    def fd_assign_corretions(self):  # correcting to make equal to colony food
        rnk1jv = next(i for i in self.juv_list if i.rank == 1)  # juv rank number one, so 2nd ranked juv
        any_fd_zrs = len([i for i in self.juv_list if i.food < 0.001])  # checking if any inds have zero food
        print "length any food zeros", any_fd_zrs
        ass_tot = sum([jv.food for jv in self.juv_list])  # total amount of food allocated
        self.juv_list.sort(key = lambda i: i.rank, reverse = False)  # sorting juv list by rank

        #print "ordered jv rank list", [jv.rank for jv in self.juv_list]

        if rnk1jv.food < 0.001 and self.colony_food <= 1:  # if too little food and too few tot juvs the 0 (top) ranked ind doesn't get correct amt food sometimes
            print "rank1juv option and col food equal to or less than one"
            rnk0jv = next(i for i in self.juv_list if i.rank == 0)
            inx_0jv = self.juv_list.index(rnk0jv)
            self.juv_list[inx_0jv].food = self.colony_food  # correcting the highest ranked jv's food

        elif rnk1jv.food < 0.001 and self.colony_food > 1:
            print "2nd rnked ind with zero food but col food > 1!!??"
            rnk0jv = next(i for i in self.juv_list if i.rank == 0)
            inx_0jv = self.juv_list.index(rnk0jv)
            self.juv_list[inx_0jv + 1].food = self.colony_food - self.juv_list[inx_0jv].food

        elif any_fd_zrs > 0:  # checking if some inds didn't get any food
            print "some inds had zero food"
            rnknoFd = next(i for i in self.juv_list if i.food < 0.0001)  # highest rank of indivdual with no food
            inx_zr_fd = self.juv_list.index(rnknoFd)  # index of highest ranked ind with no food
            min_ass_ind_fd = self.juv_list[inx_zr_fd - 1].food  # food for lowest ranked ind that gets food
            fd_to_min_ind = self.colony_food - (ass_tot - min_ass_ind_fd)

            if fd_to_min_ind >= 0:  # checking that the tot ass food - smallest assigned food is below col food!
                print " fd to min ind above zero"
                self.juv_list[inx_zr_fd - 1].food = fd_to_min_ind
            else:
                "too much food assigned fd to min ind below zero"
        else:  # all inds get fed, adjusting the lowest ranked inds food
            print"all inds get fed, adjusting lowest ranked inds food"
            num_juvs = len(self.juv_list)
            rem_food = self.colony_food - (ass_tot - self.juv_list[num_juvs - 1].food)
            print "rem_food", rem_food
            self.juv_list[num_juvs - 1].food = rem_food

    def juv_fd_assign(self):
        for spider in self.juv_list:
            jv_rnk = spider.rank
            ind_fd = self.cal_ind_food(jv_rnk)
            spider.food = ind_fd
            #print 'ind_fd', spider.food

        if len(self.juv_list) < 20 or self.colony_food < 1:
        #if len(self.juv_list) <  0 or self.colony_food < 0:
            print "running food correction code"
            self.fd_assign_corretions() # correcting to make equal to colony food

        jvFdLst = [jv.food for jv in self.juv_list]
        ass_tot = sum(jvFdLst)  # total amount of food allocated - redoing after changing some foods!
        perdiff = (abs(ass_tot - self.colony_food) / self.colony_food) * 100
        print 'percentage difference', perdiff
        if perdiff >= 200.5:
            print "percentage difference is", perdiff
            raise ValueError("assigned food greater than 2.5% different from calculated food")
        else: # comment when not testing
            return [jv.food for jv in self.juv_list]  # for testing, comment when not testing




    def zeroSlp_jv_fd(self):  # dist food if comp slope = 1
        ind_fd = self.colony_food / float(len(self.juv_list))
        for spider in self.juv_list:
            spider.food = ind_fd

    def oneSlp_jv_fd(self):  # full contest competition
        print "one food being used"
        if self.colony_food > 1:  # total colony food is more than one, i.e. more than one juv will get food
            #assign all to top ranks
            num_get_fd = np.floor(self.colony_food)
            remain = self.colony_food - num_get_fd
            for spider in self.juv_list:
                if spider.rank <= num_get_fd - 1:  # as rank starts at zero
                    spider.food = 1.0
                elif spider.rank == num_get_fd:
                    spider.food = remain
                else:
                    spider.food = 0.0
        else: # just one spider gets food
            for spider in self.juv_list:
                if spider.rank == 0:  # as rank starts at zero
                    spider.food = self.colony_food
                else:
                    spider.food = 0.0

    def assign_food(self):
        if len(self.juv_list) <= 1:
            self.juv_list[0].food = self.colony_food  #TODO: maybe put something in here to make sure that nver abv1
        if self.slope < 0.001:
            self.zeroSlp_jv_fd()
        elif self.slope == 10.0: # arbiarity number! maybe make this more a range jsut to make sure it is captured in the code.
            # TODO: make range
            self.juv_rnk_assign()  # assign ranks to juvs
            self.oneSlp_jv_fd()
        else:
            c_slpe = self.comp_slope()
            self.juv_rnk_assign()  # assign ranks to juvs
            cmp_obj = Comp(self.colony_food, len(self.juv_list), c_slpe)  # making competition object
            self.cal_med_rnk = cmp_obj.CompFunction()
            self.juv_fd_assign()
            


    def distr_food(self):
        if self.colony_food > self.num_juvs:
            raise Exception("food greater than num jvs, numJuvs", self.num_juvs, 'colFd', self.colony_food, 'numads', self.num_ads)
        else:
            self.assign_food()
            jvFdLst = [spi.food for spi in self.juv_list]
            self.jvSz_B4 = self.indStats(jvFdLst) # returns the ind stat

    def moult(self, min_juvFd):
        moult_list = [i for i in self.juv_list if i.food >= min_juvFd]
        self.num_moult = len(moult_list)
        if self.num_moult > 0:
            self.ad_list = [Adult(i.SpiderList()) for i in moult_list]  # making adults from juvs
            print 'number of juvs moulting', len(self.ad_list)
            self.juv_list = []  # emptying the old juv list
        else:
            self.ad_list = []
            self.juv_list = []
            print " no juvs moulting"
        jvMltLst = [i.food for i in self.ad_list]
        self.jvSz_AF = self.indStats(jvMltLst) # stats for juvs that moulted

    def colony_list_to_append(self):  # returns dictionary **values** in list form
        return self.colony_dict().values()

    def spiderExport(self, filename, ind_list):
        f = open(filename + "_inds.csv", "ab")
        appender = csv.writer(f, dialect = 'excel')
        for i in range(len(ind_list)):  # writes list to file
            appender.writerow(ind_list[i])
        f.close()

    def Ads_export(self, filename):
        adexport = []
        for ad in self.ad_list:
            adexport.append(["Ad", self.colony_age, self.colony_ID, ad.rank, ad.die, ad.food, ad.disperse, ad.no_off])
        #print "ad export List", adexport
        self.spiderExport(filename, adexport)

    def Juv_export(self, filename):
        juvexport = []
        for juv in self.juv_list:
            juvexport.append(["juv", self.colony_age, self.colony_ID, juv.rank, juv.die, juv.food, 'NA', 'NA'])
        #print "juv export list", juvexport
        self.spiderExport(filename, juvexport)

######### One Colony Time Step ##################

    def core_colony_timestep(self, F_Ln, K, var, min_juv_fd, pop_export_list, filename, food_scale):
            #Use just this one for colonies of females dispersed

            # (3) Adults reproduce

        self.reproduction()  # all adults within the colonies reproduce, juvs added straight to the colony

            #(4) Calculate colony food + random fluctuation
        self.col_food_random(F_Ln, K, var, food_scale)

        if self.colony_food >  min_juv_fd:
            #(5) food calculated and assigned to juvs with random
            self.distr_food()

        else:  # not enough food for any juvs to moult
            print "not enough food for any spiders to moult"  # TODO check everything that needs to be done here is being done.

        if self.indFile == 'y':
            self.Juv_export(filename)
            self.Ads_export(filename)

            #(6) Adults die
        self.ad_list = []  # emptying the adult list - all adults die

        #(7) Juvs moult or die
        self.moult(min_juv_fd)  # new juvs added directly to adult list and emptying juv list

            # (8) marking dead colonies (colonies with no spiders)
        self.col_alive()

            # (9) exporting the data (appending colony info to form to list to export)
        pop_export_list.append(self.colony_list_to_append())  # appends the dictionary values to population export list

            # (10) Printing some things to the console
        #print self.colony_dict()

    def colony_timestep(self, F_Ln, K, var, Off_M, Off_C, juv_disFd_lmt, ad_disFd_lmt, pop_dis_list, min_juv_fd,  pop_export_list, filename, food_scale):
        print "***NEW COLONY TIMESTEP****"

            # (1) add one to colony age
        self.col_age_increase()  # updates colony age by one

            # (2) adults decide whether to disperse
        self.juv_list =[]
        self.num_moult = 0
        self.num_juvs = 0
        self.num_dis = 0
        self.num_ads = len(self.ad_list)
        self.cal_pot_juv_food(F_Ln, K, Off_M, Off_C, food_scale)  # calculating potental juv food , written to colony

        self.adSz_B4 = self.indStats([i.food for i in self.ad_list]) # ad fd stats before dispersal

        print "juv_disFdLmt", juv_disFd_lmt
        if self.pot_juv_food < float(juv_disFd_lmt): # risk calculation already cal'ed in population class
            self.colDispersal_choice(ad_disFd_lmt)
            self.spis_to_dis_lst(ad_disFd_lmt)
            pop_dis_list.extend(self.dispersers) # adds spiders to population dispersal list
        else:
            print "no dispersers - pot juv food too high"

        self.dispersers = []  # clears the colony dispersal list


        #TODO: check why colony being written to file twice if all disperse
        self.col_alive() # this happens if all spiders disperse
        if self.alive == 'dead':
            pop_export_list.append(self.colony_list_to_append())  # appends the dictionary values to population export list
            #print "colony dictionary", self.colony_dict()
            print "all spiders dispersed, colony dead"
        else:  # rest of the steps -> which will also apply to the newly dispersed spiders, but have to set up to run seperately on those colonies
            print("number spiders left after dispersal")
            print(len(self.ad_list))
            self.core_colony_timestep(F_Ln, K, var, min_juv_fd, pop_export_list, filename, food_scale)
