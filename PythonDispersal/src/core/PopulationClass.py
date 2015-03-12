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


class Poplt(object):
    '''
    List of all colonies in the population    '''
    def __init__(self, poplt_list,  # list of colonies
                 filename = "",
                 off_nmbr_list = [4, 8, 0.1, 1],  # [min no off, max no off, min ad size, max ad size]
                 F_Ln = 0.2, # food to lone individual
                 K = 100.0,
                 ad_disFd_lmt = 0,
                 min_juv_fd = 0,
                 comp_slp = 0.1,
                 disp_rsk = 0.1,
                 K_var = 50
                 ):
        self.poplt_list = poplt_list
        self.filename = filename
        self.off_nmbr_list = off_nmbr_list
        self.F_Ln = F_Ln
        self.K = float(K)
        self.ad_disFd_lmt = float(ad_disFd_lmt)
        self.min_juv_fd = float(min_juv_fd)
        self.comp_slp = float(comp_slp)
        self.disp_rsk = disp_rsk
        self.K_var = K_var
        self.pop_dispersal_list = []
        self.pop_export_list = []
        self.pop_age = 0
        self.colony_count = 1
        self.new_col = []
        self.juv_disFd_lmt = self.disp_rsk * self.F_Ln
        self.Off_C = 0
        self.Off_M = 0

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

    def create_new_col(self):  # sets up the dispersing spiders as new colonies
        for spider in self.pop_dispersal_list:
            self.colony_count += 1
            print 'new colony made, id:', self.colony_count
            col = Colony(colony_ID = self.colony_count, ad_list = [spider], juv_list = [], colony_food = 0.0,  dispersers = [])
            self.new_cols.extend([col])

    def new_cols_to_lst(self):  # add the dispersed colonies to the population list and empties new_col list
        self.poplt_list.extend(self.new_cols)
        self.new_cols = []

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for col in self.poplt_list:
            col.colony_timestep(self.F_Ln, self.K, self.Off_M, self.Off_C, self.juv_disFd_lmt, self.ad_disFd_lmt,
                                self.pop_dispersal_list, self.min_juv_fd, self.pop_export_list)

    def disp_col_timestep(self):
        for colony in self.new_cols:
            colony.core_colony_timestep(self.F_Ln, self.K, self.min_juv_fd, self.pop_export_list)

    def poplt_dict(self):  # population dictionary
        d = OrderedDict()
        d['pop_age'] = self.pop_age
        d['Comp_slope'] = self.comp_slp
        d['num cols'] = len(self.poplt_list)
        return d

    def poplt_export(self):  # appends one time step of information to file
        f = open(self.filename, 'ab')
        appender = csv.writer(f)
        for i in range(len(self.pop_export_list)):  # writes list to file
            print 'pop export list', self.pop_export_list[i]
            appender.writerow(self.poplt_dict().values() + self.pop_export_list[i])
        self.pop_export_list= []  # clears the list once it has been appended to the csv file

    def one_poplt_timestep(self):
        print "***new population time step****"
        #(1) Add one to population age
        self.update_pop_age()

        #(2) Colony time step for all colonies
        self.allCols_OneTimestep()

        if self.pop_dispersal_list:
            #(3) Make dispersers into new colonies
            print 'pop dis list length', len(self.pop_dispersal_list)
            self.create_new_col()

            #(4) Iterate through new col list
            self.disp_col_timestep()

            #(5) adds new colonies to the pop list and clears new colony list
            self.new_cols_to_lst()
        else:
            print "no colonies dispersing"

        #(6) export results
        self.poplt_export()

        #(7) Delete dead colonies
        self.del_colony()

