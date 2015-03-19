'''
Created on Mar 18, 2015

@author: Ruth
'''


med_rnk = 50.0
numJuvs = 100.0
inputSlope = 2.0
xbr = 0.5
slp = inputSlope/numJuvs




oneRnk = ((-1 + (med_rnk * slp) + xbr) /slp)  # The max rank where everyone gets 1 (max) food
mxRnk = (((med_rnk * slp) + xbr) / slp)  # the max rank that receives food

print "one rank", oneRnk
print "max rank", mxRnk