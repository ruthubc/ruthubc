'''
Created on May 20, 2014

@author: Ruth
'''
from SpiderClass import Spider
from ColonyClass import Colony
import numpy as np

########### having a test of spider class ####
spi1 = Spider(15.0, 0.2, 0.2, 4)

spi2= Spider(2, 0.3, 0.2, 5)

spi3= Spider(25.6, 0.3, 0.2, 5)

spi4= Spider(7.6, 0.3, 0.2, 5)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4]

for i in colList:
    print(i.size)
    

spi1.death(11, 0.5)

myCol = Colony(colList, 0)
#print([elem.size for elem in colList])

print(spi1)

mandm = myCol.MaxAndMin()

print(mandm[0])
print(mandm[1])
print("new rel size")

relLst = [elm.cal_relSize(mandm[0],mandm[1]) for elm in colList] # give me the new relative size for all spiders in the colony

[i.update_relSize(i.cal_relSize(mandm[0],mandm[1])) for i in colList]

print(relLst[1])

print(spi1)
print(spi2)
print(spi3)

print([j for j in relLst])

print"trying colony class"
myCol.update_col_relSize()
myCol.print_spiders()

print len(myCol.colony_list)

myCol.cal_colony_food(0.01, 2)

print myCol.colony_food

myCol.print_spiders()
myCol.age_increment()
print "gap"
myCol.print_spiders()
