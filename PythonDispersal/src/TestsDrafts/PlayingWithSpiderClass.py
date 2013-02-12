'''
Created on 2012-12-23

@author: ruth
'''
from SpiderClass import Spider


#spiList = [spi1, spi2] # creates a list of object spiders

spiList = [Spider(3, 6, 5), Spider(1, 0, 5)]



print spiList[1]


#updating an object within a list
print spiList[1].instar

spiList[1].instar += 1

print spiList[1].instar
