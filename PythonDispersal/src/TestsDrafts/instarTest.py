'''
Created on 2013-01-08

@author: Ruth
'''
from spiderClass import Spider
from FeedingFunctions import instrUp


lst = range(1, 21, 2)  # instar levels in order
"range (start, stop, step), nb: does not include last number!"


spiList = [Spider(0, 3), Spider(0, 12), Spider(0, 18)]  # instar, size

instrUp(spiList, lst)

"checking whether it works"
for i in range(len(spiList)):
    print "listNumber = %d" % i
    print "instar Number = %d" % spiList[i].instar

print "testing"
