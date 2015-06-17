'''
Created on Aug 14, 2014

@author: Ruth
'''


from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt


list1 = [1,2,3,4,5,6,7]
list2 = []


if not list1 and not list2:
    print "dead"
else:
    print "alive"
    

test = [i for i in list1 if i > 13]

print len(test)