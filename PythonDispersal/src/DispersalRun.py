'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt
from core.Functions import export_rownames

filename = 'file.csv'

#(1) write rownames to csv file

export_rownames(filename) # creating file with rownames

#(2) Inital Population

ad_spider = Spider(0.9)

start_col = Colony([ad_spider], 1) #TODO: link colony id between poplt and colony

start_pop = Poplt([start_col])

