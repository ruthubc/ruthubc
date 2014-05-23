# Comp 2


import numpy as np
import matplotlib.pyplot as plt

N_col = 30
Grad = 0.11
col_food = 0.2

'''
term1 = -8*col_food*Grad*N_col
term2 = np.square(Grad)
term3 = 4*Grad

termsqrt = np.sqrt(term1 + term2 + term3 + 4) 

N_f = (-termsqrt -Grad +2)/(2 * Grad)

print N_f'''


mS_top = 2*(col_food -1)
mS_bottom = (N_col -1)

max_Slope = 2*(mS_top/mS_bottom)

print max_Slope

Ranks = np.arange(0, (N_col-1), 1) # have to have the ranks starting at zero so the equation works

#F_ind = np.where(Ranks <= N_f, -Grad*Ranks + 1, 0 )
F_ind = -Grad*Ranks + 1

plt.ylim(0,1.1)

plt.plot(Ranks, F_ind)

plt.show()