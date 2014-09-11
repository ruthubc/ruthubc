'''
Created on Sep 10, 2014

@author: user

'''
from core.PopulationClass import Poplt

this_pop = Poplt(poplt_list = [])
sim_len = 10

for run in range(0, sim_len):
    print run
    if not this_pop.poplt_list:
        print "population has gone extinct"
        break
    this_pop.one_poplt_timestep()
    print "population age:", 
    print this_pop.poplt_age