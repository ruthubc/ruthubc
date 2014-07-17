'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt


#(1) open csv file and make writing object (or whatever it is)

resultFile = open("mypythonfile.csv",'wb')

wr = csv.writer(resultFile, dialect='excel')


# best way to put the tiles in the csv file is jsut to create a fake population???

def fake_dict():
    """make fake dictionary to write row names to file"""
    fake_col = Colony([Spider()], 0)
    fake_poplt = Poplt([fake_col])
    fake_pop_dict = fake_poplt.poplt_dict()
    fake_col_dict = fake_col.colony_export()
    newdict = dict(fake_pop_dict.items() + fake_col_dict.items())
    ROWNAMES = newdict.keys()
    return ROWNAMES

print fake_dict()
wr.writerow(fake_dict()) #prints 

