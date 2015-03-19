'''
Created on Feb 14, 2015

@author: Ruth
'''
import numpy as np
from heapq import nsmallest
from core.Competition import Comp
import itertools
import pandas as pd
from core.JuvClass import Juv
from core.ColonyClass import Colony
import matplotlib.pyplot as plt

# nb changesa = [[20.0,30.0, 40.0, 50.0, 500.0, 458.4],[1, 2, 5, 7],[0.001, 0.02, 0.1, 0.2, 0.9, 0.5]] # col fod, [num juvs] [slp]

#a = [[0.1], [100.0], [0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0]] # xbr, [num juvs] [slp]

a = [[0.5], [10000.0], [0.2]]
combinations = list(itertools.product(*a))



df = []

for i in range(0, len(combinations)):
    tup = combinations[i]
    print tup
    numJuv = tup[1]
    colFd = (float(numJuv) * tup[0])
    s = tup[2]/float(numJuv)
    compOb = Comp(colFd, numJuv, s)
    rankLst = compOb.CompFunction()

    juv_list = []
    for i in range(0, int(numJuv)):
        new_juv = Juv()
        juv_list.extend([new_juv])

    myCol = Colony(1, [], juv_list, colFd, s*numJuv)
    myCol.cal_med_rnk = rankLst[1]
    myCol.juv_rnk_assign()
    ass_tot = myCol.juv_fd_assign()
    print 'ass total', ass_tot
    juv_ranks = [jv.rank for jv in myCol.juv_list]
    numSlope = sum(1 for fd in ass_tot if fd < 1 and fd > 0)
    print "numslpe", numSlope

    OutputList = [numJuv, tup[2], s, colFd] + rankLst + [sum(ass_tot)] + [numSlope]
    print 'output list', OutputList
    df.append(OutputList)
    print "df loop:", df

    plt.plot(juv_ranks, ass_tot)
    plt.ylim([0, 1])
    plt.xlim([0, numJuv])
    plt.title(str(tup[2]))
    plt.draw()




plt.show()

print "final output list", OutputList
data = pd.DataFrame(df, columns = [ 'numJuvs', 'input_slp', 'cal_slp', 'colFd', 'calTot', 'med_rnk', 'ass_tot', "NumFdBtwn"])
print data
data.to_csv('compEqn.csv', sep = ',')


    

