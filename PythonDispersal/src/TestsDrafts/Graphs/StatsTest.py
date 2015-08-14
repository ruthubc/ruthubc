'''
Created on Aug 13, 2015

@author: Ruth
'''

import numpy as np

mylist = [2.0]


print len(mylist)

def indStat(spiList):
        if len(spiList) > 0:
            minm = min(spiList)
            mx = max(spiList)
            mn = np.mean(spiList)
            sd =  np.std(spiList)
            ct = len(spiList)
        else:
            minm = "NA"
            mx = "NA"
            mn = "NA"
            sd = "NA"
            ct = "NA"
        return [minm, mx, mn, sd, ct]

print indStat(mylist)[3]
'''
adSz_B4 = ['NA', 'NA', 'NA', 'NA']

print adSz_B4[3]

print len(mylist)

'''