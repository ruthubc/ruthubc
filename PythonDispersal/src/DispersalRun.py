'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt
from core.Functions import *#export_rownames

filename = 'file.csv'
sim_len = 20 # the number of runs for the simulation1

#(1) write rownames to csv file

export_rownames(filename) # creating file with rownames

#(2) Initial Population

ad_spider = Spider(0.9)

start_col = Colony([ad_spider], 1) #TODO: link colony id between poplt and colony

this_pop = Poplt([start_col], filename)

print start_col.colony_list_to_append()

#(3) Repeated population timesteps

for run in range(0, sim_len):
    this_pop.one_poplt_timestep()
    print this_pop.poplt_age

#TODO: put in code to stop if the population dies


#(4) END
print "end, check file"

