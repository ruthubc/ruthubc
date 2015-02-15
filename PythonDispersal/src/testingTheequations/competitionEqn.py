'''
Created on Feb 14, 2015

@author: Ruth
'''
import numpy as np
from heapq import nsmallest
from core.Competition import Competition
import itertools

a = [[20,30],[40,50],[0.1, 0.2]] # col fod, [um juvs] [slp]

combinations = list(itertools.product(*a))

tup=  combinations[5]

print tup[0]



for i in range(0, len(combinations)):
    tup = combinations[i]
    compOb = Competition(tup[0], tup[1], tup[2])
    rank = compOb.CompFunction()
    print "new calculated rank",
    print rank
    print tup


