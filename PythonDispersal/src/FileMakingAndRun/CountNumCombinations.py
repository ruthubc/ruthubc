'''
Created on Jan 4, 2016

@author: rvsharpe
'''
import itertools

slopes = [0, 0.4, 0.8, 1.25, 2.5, 1]
dispersalRisks = [0.05]
meanK = [300]
Vars = [0, 0.1, 0.2, 0.3, 0.4, 0.5]
adDisSizes = [0.2, 0.4, 0.6, 0.8, 1.0]
minOffNo = [1]
maxOffNo = [4]
F_Lns = [0.61]



#runs = [[0 - slope], [1- risk of dispersal], [2- meanK], [3- Var], [4- ad dispersal limit], [5- min off], [6- max off], [7- F_ln]]
runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, minOffNo, maxOffNo, F_Lns]
c = list(itertools.product(*runs))

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