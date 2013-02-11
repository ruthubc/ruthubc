'''
Created on 2012-12-23

@author: ruth
'''
from SpiderClass import Spider

SPI_1 = Spider(3, 5, 5)
spi2 = Spider(5, 6, 5)

#spiList = [spi1, spi2] # creates a list of object spiders

spiList = [Spider(3, 6, 5), Spider(1, 0, 5)]

print spi1.size

print (spi1.instar)

print spiList[1]

print spi1


#updating an object within a list
print spiList[1].instar

spiList[1].instar += 1

print spiList[1].instar
