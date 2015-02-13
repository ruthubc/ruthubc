'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.AdultClass import Adult
from core.JuvClass import Juv
from core.PopulationClass import Poplt
from core.Functions import export_rownames


def run(filename, sim_len):
    #(1) write rownames to csv file

    export_rownames(filename) # creating file with rownames

    #(2) Initial Population

    ad_spider = Adult([1, 0, 0.2], 0, 0)

    print ad_spider

    start_col = Colony(1, [ad_spider]) #TODO: link colony id between poplt and colony

    print len(start_col.ad_list)
    start_col.print_dets()

    print start_col.colony_dict()

    this_pop = Poplt([start_col], filename)

    print start_col.colony_list_to_append()

   #(3) Repeated population timesteps

    for run in range(0, sim_len):
        if not this_pop.poplt_list:
            print "population has gone extinct"
            break
        this_pop.one_poplt_timestep()
        print "population age:", 
        print this_pop.poplt_age

    #(4) END
    print "end, check file"


run("pink_cookie", 2)