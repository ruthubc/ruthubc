'''
Created on 2013-03-19

@author: Ruth
 '''    
import numpy as np
num_ads = 1135
K = 100
F_Ln = 0.4

      
N_tot = num_ads # to maKe F_Ln actually lone ind food rather than colony of size
N = N_tot - 1  # to maKe F_Ln actually lone ind food rather than colony of size
K = K - 1  # same reason
NOvK = (N/K)
int = np.log(1/F_Ln)
F = 1 / (1 - F_Ln)  # intercept
cal_colFood = np.exp((1-NOvK)*(NOvK-1) *int)
print cal_colFood