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

#a = [[20.0,30.0, 40.0, 50.0, 500.0, 458.4],[1, 2, 5, 7],[0.001, 0.02, 0.1, 0.2, 0.9, 0.5]] # col fod, [num juvs] [slp]

a = [[20, 400] ,[2, 4],[0.9, 0.5]] # col fod, [num juvs] [slp]
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
    
    juv_list = []
    for i in range(0, int(numJuv)):
        new_juv = Juv()    
        juv_list.extend([new_juv])
        
    myCol = Colony(1, [], juv_list, tup[0], s)
    myCol.cal_med_rnk = rankLst[1]
    myCol.juv_rnk_assign()   
    ass_tot = myCol.juv_fd_assign()
    print 'ass total', ass_tot
    juv_ranks = [jv.rank for jv in myCol.juv_list]
    
    OutputList = [numJuv, tup[2], s, tup[0]] + rankLst + [sum(ass_tot)]
    print 'output list', OutputList
    df.append(OutputList)
    print "df loop:", df    
    
    plt.plot(juv_ranks, ass_tot) # 
    plt.show()



print "final output list", OutputList
data = pd.DataFrame(df, columns = [ 'numJuvs', 'input_slp', 'cal_slp', 'colFd', 'calTot', 'med_rnk', 'ass_tot'])
print data
data.to_csv('examp_output.csv', sep = ',')


    

