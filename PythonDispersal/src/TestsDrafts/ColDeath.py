'''
Created on Aug 14, 2014

@author: Ruth
'''


from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.PopulationClass import Poplt


spi1 = Spider(0.75, 0.2, 4)

spi2= Spider(0.89, 0.3)

spi3= Spider(0.02, 0.3, 5)

spi4= Spider(0.4, 0.3, 5)


col1 = Colony([spi1, spi2], 3, alive = 'alive')

col2 = Colony([spi1, spi2, spi4], 1, alive = 'dead')

col3 = Colony([], 5)

col3.col_alive()

print col3

pop = Poplt([col1, col2])


print pop

pop.del_colony()

print "death",
print pop