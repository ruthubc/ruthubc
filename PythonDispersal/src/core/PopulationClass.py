'''
Created on Jul 9, 2014

@author: Ruth
'''
#pylint: disable= too-many-statements, line-too-long

from SpiderClass import Spider
from ColonyClass import Colony
from collections import OrderedDict
import csv


class Poplt(object):
    '''
    List of all colonies in the population    '''
#TODO: remove unnecessary variables
    def __init__(self, poplt_list,  # list of colonies
                 filename = "",
                 pop_dispersal_list = [],
                 pop_export_list = [],
                 pop_age = 0,
                 colony_ID = 1,
                 new_cols = [],
                 off_nmbr_list = [2, 4, 0.1, 1],  # [min no off, max no off, min ad size, max ad size]
                 F_Ln = 0.2,
                 K = 10,
                 juv_disFd_lmt = 0,
                 ad_disFd_lmt = 0,
                 min_juv_fd = 0,
                 comp_slp = 0.1
                 ################### continue adding variables so colony time step will work!
                 ):
        self.poplt_list = poplt_list
        self.filename = filename
        self.pop_dispersal_list = pop_dispersal_list
        self.pop_export_list = pop_export_list
        self.pop_age = pop_age
        self.colony_ID = colony_ID
        self.new_cols = new_cols
        self.off_nmbr_list = off_nmbr_list
        self.F_Ln = F_Ln
        self.K = K
        self.juv_disFd_lmt = juv_disFd_lmt
        self.ad_disFd_lmt = ad_disFd_lmt
        self.min_juv_fd = min_juv_fd
        self.comp_slp = comp_slp

    def __str__(self):
        return "Pop_age: %s, # cols: %s" % (self.poplt_age, len(self.poplt_list))

    def update_pop_age(self):  # adds one to the age
        self.pop_age += 1

    def del_colony(self):  # deletes a colony from the population if it goes extinct
        #works, checked Aug 14th
        self.poplt_list = [i for i in self.poplt_list if i.alive == 'alive']

    def create_new_col(self):  # sets up the dispersing spiders as new colonies
        #TODO: check works
        for spider in self.pop_dispersal_list:
            self.colony_number += 1
            col = Colony([spider], self.colony_number)
            self.new_cols.extend([col])

    def new_cols_to_lst(self):  # add the dispersed colonies to the population list and empties new_col list
        self.poplt_list.extend(self.new_cols)
        self.new_cols = []

    def allCols_OneTimestep(self):  # iterates through all colonies in population for one time step
        for col in self.poplt_list:
            col.colony_timestep(self.F_Ln, self.K, self.comp_slp, self.off_nmbr_list, self.juv_disFd_lmt, self.ad_disFd_lmt,
                                self.pop_dispersal_list, self.min_juv_fd, self.pop_export_list)
            self.update_dispersal_list(col)

    def disp_col_timestep(self):
        for colony in self.new_cols:
            colony.core_colony_timestep(self.F_Ln, self.K, self.comp_slp, self.min_juv_fd, self.pop_export_list)

    def poplt_dict(self):  # population dictionary
        d = OrderedDict()
        d['pop_age'] = self.pop_age
        d['Comp_slope'] = self.comp_slp
        d['num cols'] = len(self.poplt_list)
        return d

    def poplt_export(self):  # appends one time step of information to file
        f = open(self.filename, 'ab')
        appender = csv.writer(f)
        for i in range(len(self.export_list)):  # writes list to file
            print self.export_list[i]
            appender.writerow(self.poplt_dict().values() + self.export_list[i])
        self.export_list = []  # clears the list once it has been appended to the csv file

    def one_poplt_timestep(self):
        #(1) Add one to population age
        self.update_pop_age()

        #(2) Colony time step for all colonies
        self.allCols_OneTimestep()()

        #(3) Make dispersers into new colonies
        self.create_new_col()

        #(4) Iterate through new col list
        self.disp_col_timestep()

        #(5) adds new colonies to the pop list and clears new colony list
        self.new_cols_to_lst()

        #(6) export results
        self.poplt_export()

        #(7) Delete dead colonies
        self.del_colony()

