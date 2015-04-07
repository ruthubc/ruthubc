'''
Created on Jul 18, 2014

@author: Ruth
'''
import csv
#import core.ColonyClass as CC
#from core.PopulationClass import Poplt
from collections import OrderedDict
import numpy as np

def fake_dict(): # returns row names
    """make fake dictionary to write row names to file"""
    import core.ColonyClass as CC
    import core.PopulationClass as PP
    fake_col = CC.Colony()
    fake_poplt = PP.Poplt([fake_col])
    fake_pop_dict = fake_poplt.poplt_dict()
    fake_col_dict = fake_col.colony_dict()
    newdict = OrderedDict(fake_pop_dict.items() + fake_col_dict.items())
    ROWNAMES = newdict.keys()
    return ROWNAMES


def export_rownames(filename, rowheaders = fake_dict()):
    f = open(filename,'wb')
    writer = csv.writer(f, dialect='excel')
    writer.writerow(rowheaders)
    f.close()
 
def random_gus(mean, variance): # so the random function doesn't produce negative number
    ran_num = -1
    while (ran_num < 0):
        ran_num = np.random.normal(mean, variance)
    return ran_num


    