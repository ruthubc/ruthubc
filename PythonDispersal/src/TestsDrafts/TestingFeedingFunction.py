'''
Created on 2013-01-11

@author: Ruth
'''
from SpiderClass import Spider
from FeedingFunctions import rel_size

spiList = [Spider(3, 5.0, 0.0), Spider(1, 0.5, 0.0), Spider(7, 3.0, 0.0)]

rel_size(spiList)

print (spiList[1].rel_size)

