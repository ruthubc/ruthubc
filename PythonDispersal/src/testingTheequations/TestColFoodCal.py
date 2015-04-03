'''
Created on Feb 12, 2015

@author: Ruth
'''
from core.ColonyClass import Colony
import numpy as np

def cal_col_food(N_tot, F_Ln, K):  # returns tot colony food per capita
    N = N_tot - 1  # to maKe F_Ln actually lone ind food rather than colony of size
    K = K - 1  # same reason
    NOvK = (N/K)
    int = np.log(1/F_Ln)
    F = 1 / (1 - F_Ln)  # intercept
    cal_colFood = np.exp((1-NOvK)*(NOvK-1) *int)
    tot_col_food = cal_colFood * N_tot
    return tot_col_food

    


Fd_lone = 0.6
Carr_cap = float(10.0)

List_of_N = range(1, int(Carr_cap*3.5))


colFd = []

for N in List_of_N:
    outcome = cal_col_food(N, Fd_lone, Carr_cap)
    colFd = colFd + [outcome]

print List_of_N
print colFd


import matplotlib.pyplot as plt
plt.plot(List_of_N, colFd)
plt.ylim([0, (Carr_cap+3)])
plt.xlim([1, int(Carr_cap*3.5)])
plt.show()