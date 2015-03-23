'''
Created on Feb 25, 2015

@author: Ruth
'''
from core.DispersalRun import disperal_run

sim_len = 2
filename = "pink_cookie.csv"
comp_slp = 0.2
disp_risk = 0.1
K = 100
amt_var = 0.2


disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var)