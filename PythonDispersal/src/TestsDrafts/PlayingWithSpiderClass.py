'''
Created on 2012-12-23

@author: ruth
'''
from SpiderClass import Spider


#spiList = [spi1, spi2] # creates a list of object spiders

spiList = [Spider(3, 6, 5), Spider(1, 0, 5)]



spi=Spider(1, 4, 5)
print spiList[1]

spiList[1].test()

print  spiList[1]

map_me = ['a', 'b', 'c', 'd']
#result = map(lambda x: "the letter is %s", % x, map_me)

par= {"a":"b", "d":"e"}
print par.items()



