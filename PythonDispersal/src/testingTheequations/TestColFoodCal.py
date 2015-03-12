'''
Created on Feb 12, 2015

@author: Ruth
'''
from core.ColonyClass import Colony

def cal_col_food(N, F_Ln, K):  # returns tot colony food per capita
        # calculates and updates the food to the colony, F_Ln is food to lone individual (n=0+
    N = N - 1  # to maKe F_Ln actually lone ind food rather than colony of size
    K = K - 1  # same reason
    brac = 1-(N/K)    
    F = 1 / (1 - F_Ln)  # intercept
    cal_colFood = (F + brac*(-brac))/F
    return cal_colFood
    


Fd_lone = 0.2
Carr_cap = float(100.0)

List_of_N = range(1, int(Carr_cap*2.5))
print List_of_N

colFd = []

for N in List_of_N:
    outcome = cal_col_food(N, Fd_lone, Carr_cap)
    colFd = colFd + [outcome]
    


import matplotlib.pyplot as plt
plt.plot(List_of_N, colFd)
plt.ylim([0, 1.1])
plt.xlim([1, int(Carr_cap*2.5)])
plt.show()