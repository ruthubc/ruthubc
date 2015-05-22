'''
Created on Jul 9, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long

from SpiderClass import Spider
from ColonyClass import Colony
from collections import OrderedDict
import numpy as np
import csv
import random

# The four things to change, 
#1) slope [0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0, 10.0]
#2) Risk of dispersal [0, 0.1, 0.2, 0.3, 0.4]
#3) mean K
#4) variance in k and FLN



class Poplt(object):
    '''
    List of all colonies in the population    '''
    def __init__(self, poplt_list,  # list of colonies
                 filename = "", # output file name
                 comp_slp = 0.1,
                 disp_rsk = 0.5,
                 K = 100.0, # carrying capacity
                 amt_var = 0.5, # varies between 0 and 1
                 min_juv_fd = 0.1,
                 min_no_off = 2,
                 max_no_off = 4,
                 ad_disFd_lmt = 0.6,
                 F_Ln = 0.4
                 ):
        self.poplt_list = poplt_list
        self.filename = filename
        self.comp_slp = float(comp_slp)
        self.disp_rsk = disp_rsk
        self.K = float(K)
        self.amt_var = amt_var
        self.min_juv_fd = float(min_juv_fd)
        self.ad_disFd_lmt = ad_disFd_lmt
        self.F_Ln = F_Ln
        self.off_nmbr_list = [min_no_off, max_no_off, min_juv_fd, 1] # [min no off, max no off, min ad size, max_ad Size]
        self.pop_dispersal_list = []
        self.pop_export_list = []
        self.pop_age = 0
        self.colony_count = 10
        self.new_cols = []
        self.Off_C = 0  # used in making the offspring equation
        self.Off_M = 0  # used in making the offspring equation
        self.juv_disFd_lmt = self.disp_rsk * self.F_Ln  # if below this adults should disperse
        self.food_scale = min_no_off
        self.maxNumCols = 200  #TODO make this an option at the top

    def __str__(self):
        return "Pop_age: %s, # cols: %s" % (self.poplt_age, len(self.poplt_list))

    def update_offVar(self):  # updates the number of offspring an adult have depends on food while juv and adult
        OMn = np.log(float(self.off_nmbr_list[0]))  # min number of offspring
        OMx = np.log(float(self.off_nmbr_list[1]))  # max number of offspring
        SMn = np.log(float(self.off_nmbr_list[2]))  # min adult size that can reproduce
        SMx = np.log(float(self.off_nmbr_list[3]))
        self.Off_C = ((-OMn * SMx) + (OMx * SMn)) / (SMx - SMn)
        self.Off_M = (OMn - OMx) / (SMx - SMn)

    def update_pop_age(self):  # adds one to the age
        self.pop_age += 1

    def del_colony(self):  # deletes a colony from the population if it goes 
        #works, checked Aug 14th
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive']

    def too_many_cols(self): # removes spiders at random from disperers list
        num_sps_lft = self.maxNumCols - len(self.poplt_list)
        while len(self.pop_dispersal_list) > num_sps_lft:
            self.pop_dispersal_list.remove(random.choice(list(self.pop_dispersal_list)))
        # print '#remining dispersers=', len(self.pop_dispersal_list)

    def disp_rsk_remove_spiders(self):
        num_list = len(self.pop_dispersal_list) * self.disp_rsk
        while len(self.pop_dispersal_list) > num_list:
            self.pop_dispersal_list.remove(random.choice(list(self.pop_dispersal_list)))

    def create_new_col(self):  # sets up the dispersing spiders as new colonies
        print "number of disps before rsk death:", len(self.pop_dispersal_list)
        self.disp_rsk_remove_spiders() 
        print "number of disps AFTER rsk death:", len(self.pop_dispersal_list)
        self.too_many_cols()
        print "number of dispds After too many cols:", len(self.pop_dispersal_list)
        for spider in self.pop_dispersal_list:
            self.colony_count += 1
            #print 'new colony made, id:', self.colony_count
            col = Colony(colony_ID = self.colony_count, ad_list = [spider], slope = self.comp_slp, colony_age = 0, dispersers = [], pot_juv_food = 0)
            #col.print_dets()
            self.new_cols.extend([col])

    def new_cols_to_lst(self):  # add the dispersed colonies to the population list and empties new_col list
        self.poplt_list.extend(self.new_cols)
        self.new_cols = []

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for col in self.poplt_list:
            col.colony_timestep(self.F_Ln, self.K, self.amt_var, self.Off_M, self.Off_C, self.juv_disFd_lmt, self.ad_disFd_lmt,
                                self.pop_dispersal_list, self.min_juv_fd, self.disp_rsk, self.pop_export_list, self.filename, self.food_scale)

    def disp_col_timestep(self):
        for colony in self.new_cols:
            colony.core_colony_timestep(self.F_Ln, self.K, self.amt_var, self.min_juv_fd, self.pop_export_list, self.filename, self.food_scale)

    def poplt_dict(self):  # population dictionary
        d = OrderedDict()
        d['pop_age'] = self.pop_age
        d['Comp_slope'] = self.comp_slp
        d['num_cols'] = len(self.poplt_list)
        d['meanK'] = self.K
        d['Fd_ln'] = self.F_Ln
        d['input_var'] = self.amt_var
        d['disp_rsk'] = self.disp_rsk
        d['ad_dsp_fd'] = self.ad_disFd_lmt
        d['min_juv_fd'] = self.min_juv_fd
        d['min_no_off'] = self.off_nmbr_list[0] #[min no off, max no off, min ad size, max ad size]
        d['max_no_off'] = self.off_nmbr_list[1]
        d['min_ad_sze_off'] = self.off_nmbr_list[2]
        d['max_ad_sze_off'] = self.off_nmbr_list[3]
        return d

    def poplt_export(self):  # appends one time step of information to file
        f = open(self.filename + ".csv", 'ab')
        appender = csv.writer(f)
        for i in range(len(self.pop_export_list)):  # writes list to file
            #print 'pop export list', self.pop_export_list[i]
            appender.writerow(self.poplt_dict().values() + self.pop_export_list[i])
        f.close()
        self.pop_export_list= []  # clears the list once it has been appended to the csv file

    def one_poplt_timestep(self):
        print "***new population time step****"
        #(1) Add one to population age
        self.update_pop_age()

        #(2) Colony time step for all colonies
        self.allCols_OneTimestep()

        if self.pop_dispersal_list:
            print "size of population dispersal list", len(self.pop_dispersal_list)
            #(3) Make dispersers into new colonies
            # print 'pop dis list length', len(self.pop_dispersal_list)
            self.create_new_col()

            #(4) Iterate through new col list
            self.disp_col_timestep()

            #(5) adds new colonies to the pop list and clears new colony list
            self.new_cols_to_lst()
        #else:
            # print "no colonies dispersing"

        #(6) export results
        self.poplt_export()

        #(7) Delete dead colonies
        self.del_colony()

