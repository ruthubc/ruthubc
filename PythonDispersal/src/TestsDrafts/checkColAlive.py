'''
Created on Jul 21, 2014

@author: user
'''
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt

ad_spider = Spider(0.9)

start_col = Colony([ad_spider], 1) #TODO: link colony id between poplt and colony

start_col.col_alive()
print start_col.alive

print start_col.colony_list_to_append()


pop = Poplt([start_col])
'''
for i in range(0, 7):
    pop.update_poplt_age()    
    print pop.poplt_age
    print pop.poplt_dict()
    
    
print "end", 
print pop.poplt_age

'''