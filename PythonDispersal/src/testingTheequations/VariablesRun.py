'''
Created on Aug 22, 2014

@author: user
'''

from core.DispersalRun import run

filename = "output.csv"
sim_len = 10

#1) slope [0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0, 10.0]
#2) Risk of dispersal [0, 0.1, 0.2, 0.3, 0.4]
#3) mean K [100, 200, 300, 400, 500]
#4) variance in k and FLN [0, 0.1, 0.2, 0.3, 0.4, 0.5]

run(filename, sim_len, slope, disp_risk, )
