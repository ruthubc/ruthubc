'''
Created on Feb 25, 2015

@author: Ruth
'''
from core.DispersalRun import disperal_run

sim_len = 100
filename = "yellow_banana"
comp_slp = 1.25
disp_risk = 0.1
K = 100
amt_var = 0.2

disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var)