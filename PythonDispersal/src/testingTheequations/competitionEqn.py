'''
Created on Feb 14, 2015

@author: Ruth
'''
import numpy as np
from heapq import nsmallest
from core.CompetitionIntercept import CompInt
from core.Competition import Comp
import itertools
import pandas as pd
from core.JuvClass import Juv
from core.ColonyClass import Colony
import matplotlib.pyplot as plt
import random

# nb changesa = [[20.0,30.0, 40.0, 50.0, 500.0, 458.4],[1, 2, 5, 7],[0.001, 0.02, 0.1, 0.2, 0.9, 0.5]] 
# # compeition options [0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0]
a = [[1.24], [3, 4], [0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0]] # xbar, [num juvs] [slp]
#a = [[0.8*2], [3, 4], [5.0]]
#a = [[0.5], [100], [0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0]]
# not sure whether the col food is with or without scaling


###########################
## TO get this to work change the output of the compeition function and the juv_fd_assign

# food scale is taken into account in colony class when setting colony food NOT in any of the competition code

print ("have you changed the comp functin and juv fd assign functiun output???")
combinations = list(itertools.product(*a))
compType = "N" # I or N

print combinations

df = []
ass_lists = []

for i in range(0, len(combinations)):
    tup = combinations[i]
    numJuv = tup[1]
    #colFd = (float(numJuv) * tup[0])
    colFd = tup[0]

    juv_list = []
    for i in range(0, int(numJuv)):
        new_juv = Juv()
        juv_list.extend([new_juv])

    myCol = Colony(compType, "n", 1, [], tup[2])  # change I to no
    myCol.juv_list = juv_list
    myCol.colony_food = colFd
    #print "if this doens't work you have not changed outputs in main file"

    if compType == "I":
        s = tup[2]
        compOb = CompInt(colFd, numJuv, s)
        rankLst = compOb.CompIntFun()
        myCol.assign_food()
    else: # if competition type is N
        s = tup[2]/float(numJuv-1) # might have to change this when doing intercept competition
        compOb = Comp(colFd, numJuv, s)
        rankLst = compOb.CompFunction()
        myCol.cal_med_rnk = rankLst[1]
        myCol.juv_rnk_assign()
        myCol.juv_fd_assign()
        print "mycol med rank", myCol.cal_med_rnk

    print "s", s

    #print [i.rank for i in myCol.juv_list]
    ass_tot = myCol.juv_fd_assign()
    #print 'slope', tup[2], 'xbar:', tup[0]
    #print 'ass total', ass_tot, 
    juv_ranks = [jv.rank for jv in myCol.juv_list]
    numSlope = sum(1 for fd in ass_tot if fd < 1 and fd > 0)
    #print "numslpe", numSlope
    ass_lists.append(ass_tot)
    calcTot = sum(ass_tot)
    percDiff = (np.abs(colFd - calcTot)/ colFd) * 100
    OutputList = [numJuv, tup[2], s, colFd] + rankLst + [calcTot] + [numSlope] + [percDiff]

    df.append(OutputList)
    #print "df loop:", df

    plt.plot(juv_ranks, ass_tot)
    plt.ylim([0, 1.1])
    plt.xlim([0, numJuv])
    plt.title(str(tup[2]))
    plt.draw()
    
print "ass_lists"
print ass_lists[0]

print "len df"
print len(df)

print "final output list", OutputList
col1 = str(tup[1])
d = dict(col1 = ass_lists[0], col2 = ass_lists[1])#, col3 = ass_lists[2], col4 = ass_lists[3])
data2 = pd.DataFrame(df, columns = [ 'numJuvs', 'input_slp', 'cal_slp', 'colFd', 'calTotOrIncptSlp', 'med_rnkOrInct', 'ass_tot', "NumFdBtwn", "PercDiff"])
#d = {"col1": ass_lists[0], "col2": ass_lists[1], "col3": ass_lists[2]}


food_dict = {}
for i in range(len(df)):
    name = 'slp' + str(df[i][1]) + 'juvs' + str(df[i][0])
    food_dict[name] = ass_lists[i]

data = pd.DataFrame({k : pd.Series(v) for k, v in food_dict.iteritems()})
#data = pd.DataFrame(d)


print data2
data.to_csv('compEqn.csv', sep = ',')

plt.show()

