'''
Created on Feb 25, 2015

@author: Ruth
'''
from core.DispersalRun import disperal_run


sim_len = 5
filename = "pink_cookie.csv"
off_variables = [4, 8, 0.1, 1]  # [min no off, max no off, min ad size, max ad size]
F_Ln = 0.2  # food to lone individual
K = 100  # optimum group size
jv_dis_lmt = 0.4  # limit where adults dispers
ad_dis_lmt = 0.9 # have to be larger than this to disperse
juv_mlt_lmt = 0.01  # juvs wont' moult below this
comp_slp = 0.2

disperal_run(sim_len, filename, off_variables, F_Ln, K, jv_dis_lmt, ad_dis_lmt, juv_mlt_lmt, comp_slp)