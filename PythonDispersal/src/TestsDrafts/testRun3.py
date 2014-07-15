'''
Created on Jul 9, 2014

@author: Ruth
'''

from core.Population import ColonyPopulation
from core.ColonyClass import Colony
from core.SpiderClass import Spider

ad_spider = Spider(0.9)

mycol = Colony(ad_spider)

start_colonies = [Colony([ad_spider])]

print "inital colony one adult:"
#start_colony.print_spiders()

start_population = ColonyPopulation(start_colonies)

print start_population.carrying_capacity

print start_population.inverse_carr_cap

print Colony([ad_spider]).colony_export()

print start_population.ind_col_timestep(0)