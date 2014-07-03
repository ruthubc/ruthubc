'''
Created on Jul 1, 2014

@author: user
'''

from Run import run_function
from core.ColonyClass import Colony
from core.SpiderClass import Spider

ad_spider = Spider(0.9)

start_colony = Colony([ad_spider])

print "inital colony one adult:"
start_colony.print_spiders()
"""
for x in range(0,60):
    run_function(start_colony)
    print "run number: %d" % (x)
    print start_colony.print_dets()
    #start_colony.print_spiders()
"""
x=1
while len(start_colony.colony_list)>0:
    x += 1    
    print "run number: %d" % (x)
    print start_colony.print_dets()
    start_colony.print_spiders()
    run_function(start_colony)
    

print "colony died at age %s" % start_colony.colony_age
"""
temp = 115
while temp > 112:
    temp = temp -1
    print temp
"""

                                     
                                     
