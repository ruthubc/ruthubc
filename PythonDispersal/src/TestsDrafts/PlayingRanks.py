'''
Created on May 23, 2014

@author: Ruth
'''

import matplotlib.pyplot as plt
import numpy as np

array = np.array([4,2,7,1])
order = array.argsort()
ranks = order.argsort()



seq = [1,2,1,2,1,2,1,2]
seq2 = [2,2,2,2,1,1,1,1]
print seq

def add(x,y): return x+y

new = map(add, seq, seq2)

print new

all = [1,2,3]

