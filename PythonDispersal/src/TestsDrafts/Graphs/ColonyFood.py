'''
Created on 2013-03-19

@author: Ruth
 '''    
 
num_ads = 20
K = 100

      
N_tot = num_ads # to maKe F_Ln actually lone ind food rather than colony of size
N = N_tot - 1  # to maKe F_Ln actually lone ind food rather than colony of size
K = K - 1  # same reason
        NOvK = (N/K)
        int = np.log(1/F_Ln)
        F = 1 / (1 - F_Ln)  # intercept
        cal_colFood = np.exp((1-NOvK)*(NOvK-1) *int)
        cap_col_food = cal_colFood
        if cap_col_food <= 0:
            raise ValueError("cal Colony food was negative or zero")
        else:
            return cap_col_food