'''
Created on May 20, 2014

@author: Ruth
'''
from core.SpiderClass import Spider
from core.ColonyClass import Colony
import numpy as np
import scipy.stats as ss

########### having a test of spider class ####
spi1 = Spider(0.75, 0.2, 0.2, 4)

spi2= Spider(0.89, 0.3, 0.2, 5)

spi3= Spider(0.02, 0.3, 0.2, 5)

spi4= Spider(0.4, 0.3, 0.2, 5)



## having a test of colony class

colList = [spi1, spi2, spi3, spi4]

for i in colList:
    print(i.size)
    

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
'''
sl = np.array(myCol.size_list())

print sl

sort =  sl.argsort()
print "sort:", sort

spiders = myCol.colony_list

ranks = [2, 1, 3, 4]

#[i.update_rank([2, 7, 3, 4][j]+=1]) for i in spiders]

for i, j in zip(spiders, sort):
    i.update_rank(j)
    print i
'''   
myCol.update_rank()
print "updated"
myCol.print_spiders()
'''
sl = myCol.size_list()
slnew = np.array(sl)
print slnew
print slnew.argsort()
print "scipy try"
print ss.rankdata(sl)
'''

update = [x for x in myCol.colony_list if x.rank > 2.0]

#[i.update_indFood(20) for i in myCol.colony_list if i.rank >2]
print "scramble comp"
myCol.colony_food = 0.7
myCol.scramble()

print "testing colony food:", myCol.colony_food
myCol.print_spiders()

print "contest"
myCol.colony_food = 0.5
myCol.contest()
print "colony food:", myCol.colony_food
myCol.print_spiders()

print "trying compeition function"
myCol.colony_food = 0.74
myCol.ind_food(1)
myCol.print_spiders()

print"trying growth function"
myCol.apply_growth(0.5)
myCol.print_spiders()

print "testing death"
myCol.dying(7, 0.25)
myCol.print_spiders()


print "testing reproduction"
myCol.reproduction(5, 0.8)
myCol.print_spiders()
