'''
Created on Feb 14, 2015

@author: Ruth
'''
import numpy as np
from heapq import nsmallest
from core.Competition import Comp
import itertools
import pandas as pd

#a = [[20.0,30.0, 40.0, 50.0, 500.0, 458.4],[1, 2, 5, 7],[0.001, 0.02, 0.1, 0.2, 0.9, 0.5]] # col fod, [num juvs] [slp]

a = [[20.0,30.0, 486.54] ,[1, 2, 7],[0.1, 0.2, 0.5, 0.9]] # col fod, [num juvs] [slp]
combinations = list(itertools.product(*a))

print combinations

df = []

for i in range(0, len(combinations)):
    tup = combinations[i]
    print tup
    numJuv = round(tup[0]*tup[1])
    s= float(tup[2]* numJuv)
    compOb = Comp(tup[0], numJuv, s)
    rankLst = compOb.CompFunction()
    OutputList = [numJuv, tup[2], s, tup[0]] + rankLst
    print 'output list', OutputList
    df.append(OutputList)
    print "df loop:", df


print "final output list", OutputList
data = pd.DataFrame(df, columns = [ 'numJuvs', 'input_slp', 'cal_slp', 'colFd', 'calTot', 'med_rnk'])
print data
data.to_csv('examp_output.csv', sep = ',')


