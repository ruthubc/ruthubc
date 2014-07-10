'''
Created on Jul 9, 2014

@author: Ruth
'''

from Run import run_function
from core.ColonyClass import Colony
from core.SpiderClass import Spider

ad_spider = Spider(0.9)

start_colonies = [Colony([ad_spider]), Colony([ad_spider])]

print "inital colony one adult:"
#start_colony.print_spiders()

[i.print_spiders() for i in start_colonies]