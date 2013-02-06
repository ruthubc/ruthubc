'''
Created on 2012-12-23

@author: ruth


'''

from SpiderClass import Spider
from FeedingFunctions import *

INSTAR_LEVELS = range(1, 21, 2)  # defines the size of the instar levels
"range (start, stop, step), nb: does not include last number!"


SPI_LIST = [Spider(0, 3.0, 0.0), Spider(0, 12.0, 0.0), Spider(0, 18.0, 0.0)]


rel_size(SPI_LIST)  # updates the spider  with relative size compared to colony


instar_inc(SPI_LIST, INSTAR_LEVELS)  # updates the instar to the new size
