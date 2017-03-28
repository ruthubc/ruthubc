'''
Created on Mar 27, 2017

@author: Ruth
'''

import sys
import itertools
import csv
import time
import platform
import socket
import os

indFile = "n"


slopes = [0.4] * 8 # 8 reps
dispersalRisks = [0.3] 
meanK = [300]
Vars = [0.8]
adDisSizes = [0.6, 0.8]
off_list = [[4, 6]]
F_Lns = [0.61]


sim_len = 500

comp_type = "N"

runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
combinations = list(itertools.product(*runs))


## Adding an extra combination
mySlope = 0
myVariance = 0.8
myDispSize = 0.6

extraTup = tuple([mySlope, dispersalRisks[0], meanK[0],  myVariance, myDispSize, off_list[0], F_Lns[0]])
combinations.extend([extraTup])

# extra combination

slopes = [0.4] * 7 # 8 reps
dispersalRisks = [0.3] 
meanK = [300]
Vars = [0.8]
adDisSizes = [0.6]
off_list = [[4, 6]]
F_Lns = [0.61]


sim_len = 500

comp_type = "N"

runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
combinations.extend(list(itertools.product(*runs)))

print combinations
print type(combinations)

print combinations[20]

print type(combinations[2])

print len(combinations)

pbsName = "_27March_array"

pbsNameList = [s + pbsName for s in ["2hrs", "20hrs", "35hrs", "72hrs"]]

print pbsNameList
