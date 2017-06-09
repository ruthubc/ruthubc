'''
Created on Mar 16, 2017

@author: Ruth
'''

import os

filename = "14432_slp0.4_Rsk0.3_K300_var0.5_dslm1.2_maxOff6.py.csv"

try:
    os.remove(filename)
except OSError:
    pass

print "end"