'''
Created on Jul 18, 2014

@author: Ruth
'''
import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt

def fake_dict(): # returns row names
    """make fake dictionary to write row names to file"""
    fake_col = Colony([Spider()], 0)
    fake_poplt = Poplt([fake_col])
    fake_pop_dict = fake_poplt.poplt_dict()
    fake_col_dict = fake_col.colony_dict()
    newdict = dict(fake_pop_dict.items() + fake_col_dict.items())
    ROWNAMES = newdict.keys()
    return ROWNAMES


def export_rownames(filename, rowheaders = fake_dict()):
    f = open(filename,'wb')
    writer = csv.writer(f, dialect='excel')
    writer.writerow(rowheaders)
    f.close()