'''
Created on Jan 4, 2016

@author: rvsharpe
'''
import itertools
import csv
'''
slopes = [0, 0.4, 0.8, 1.25, 2.5, 1]
dispersalRisks = [0.3]
meanK = [300]
Vars = [0, 0.1, 0.2, 0.3, 0.4]
adDisSizes = [0.2, 0.4, 0.6, 0.8, 1.0, 1.2]
off_list = [[0, 2], [2, 4], [4, 6], [6, 8], [8, 10], [10, 12]]
F_Lns = [0.61]
'''

slopes = [0, 0.4, 0.8, 1.25, 2.5, 10] # 10 is full contest competition
dispersalRisks = [0.3, 0.3]
meanK = [300]
Vars = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]
adDisSizes = [0.2, 0.6, 0.8, 1.2]
off_list = [[4, 6]]
F_Lns = [0.61]


#runs = [[0 - slope], [1- risk of dispersal], [2- meanK], [3- Var], [4- ad dispersal limit], [5- min off], [6- max off], [7- F_ln]]
runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
c = list(itertools.product(*runs))

combinations = list(itertools.product(*runs))

print len(c)




'''
for i in range(0, len(combinations)):  # actually produces the files
    tup = combinations[i]
    slope = tup[0]
    risk = tup[1]
    K = tup[2]
    var = tup[3]
    ad_disFd_lmt = tup[4]
    min_juv_size = 0.205
    off_list = tup[5]
    F_Ln = tup[6]
    if ad_disFd_lmt > 1:
        print "ad food limit > 1"
        #print tup
    if ad_disFd_lmt == 0.4 and slope > 0.7  and slope < 1.3 and off_list[1] > 5 and off_list[1] < 7:
        print "multiple conditions"
        #print tup
    elif off_list[1] < 5:
        print "off less than 5"
        #print tup
    else:
        print "all else"
        #print tup
    



# function to flatten sequence from http://stackoverflow.com/questions/5828123/nested-list-and-count
def flatten(seq,container=None):  
    if container is None:
        container = []
    for s in seq:
        if hasattr(s,'__iter__'):
            flatten(s,container)
        else:
            container.append(s)
    return container

c = flatten(c)

print len(c)
'''