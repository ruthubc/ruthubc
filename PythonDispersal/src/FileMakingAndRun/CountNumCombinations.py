'''
Created on Jan 4, 2016

@author: rvsharpe
'''
import itertools
'''
slopes = [0, 0.4, 0.8, 1.25, 2.5, 1]
dispersalRisks = [0.3]
meanK = [300]
Vars = [0, 0.1, 0.2, 0.3, 0.4]
adDisSizes = [0.2, 0.4, 0.6, 0.8, 1.0, 1.2]
off_list = [[0, 2], [2, 4], [4, 6], [6, 8], [8, 10], [10, 12]]
F_Lns = [0.61]
'''

slopes = [0, 0.4, 0.8, 1.25, 2.5, 1]
dispersalRisks = [0.3]
meanK = [200, 400]
Vars = [0]
adDisSizes = [0.2, 0.6, 1.0]
off_list = [[2, 4], [4, 6], [6, 8]]
F_Lns = [0.61]



#runs = [[0 - slope], [1- risk of dispersal], [2- meanK], [3- Var], [4- ad dispersal limit], [5- min off], [6- max off], [7- F_ln]]
runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
c = list(itertools.product(*runs))

print len(c)

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