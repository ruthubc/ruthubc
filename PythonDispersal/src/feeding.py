'''
Created on 2012-12-23

@author: ruth


'''

from SpiderClass import Spider
from ColonyClass import Colony

INSTAR_LEVELS = range(1, 21, 2)  # defines the size of the instar levels
"range (start, stop, step), nb: does not include last number!"

print INSTAR_LEVELS

s = Spider(0, 15)

print s

s.instar_inc(INSTAR_LEVELS)

print s

j = Colony([Spider(0, 6.0, 5), Spider(0, 7.0, 5), Spider(0, 8.0, 7)])

j.all_instar(INSTAR_LEVELS)

print j.total_size

j.col_size()


print "relsize"

j.rel_size()

j.print_spiders()

print j.instar_sum()

j.col_size()

print j.total_size

print len(j.colony_list)

'''
j = Colony([Spider(3, 6, 5), Spider(1, 0, 5), Spider(3, 8, 7)])

print j.colony_list[1]

j.print_spiders()

spi = Spider(3, 6, 9)


print spi

j.all_instar(10)

j.print_spiders()

print j.col_size()
'''
