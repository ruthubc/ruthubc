# From https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.curve_fit.html


import numpy as np
import pandas as pd
from scipy.optimize import curve_fit
import matplotlib.pyplot as plt
from lmfit import Model, Parameters

# example from above website
'''
def func(x, a, b, c):
    return a * np.exp(-b * x) + c

xdata = np.linspace(0, 4, 50)
y = func(xdata, 2.5, 1.3, 0.5)
ydata = y + 0.2 * np.random.normal(size=len(xdata))

popt, pcov = curve_fit(func, xdata, ydata)
#Constrain the optimization to the region of 0 < a < 3, 0 < b < 2 and 0 < c < 1:

#popt, pcov = curve_fit(func, xdata, ydata, bounds=(0, [3., 2., 1.]))

print "popt", popt

print "pcov", pcov
'''
# my data

#### Need to make nntplus1 data table.

df = pd.read_csv("G:/ownCloud/DisperalSimulationOutput/Testing/13775_slp2.5_Rsk0.3_K300_var0_dslm0.6_maxOff6.py.csv")

#print df.columns.values

#print df.info() 

df[['pop_age']] = df[['pop_age']].astype(float)
#print df[['pop_age']]
df = df[df['pop_age'] >= 3]
        
        
print "after subset"



#print df.info()

NN_df = df[["colony_ID", "num_adsB4_dispersal", "colony_age"]]

NN_df[['num_adsB4_dispersal']] = NN_df[['num_adsB4_dispersal']].astype(float)

NNPlusOne = NN_df.rename(columns = {"num_adsB4_dispersal":"num_plusOne", "colony_age":"colony_age_plusOne"})

NN_df['colony_age_plusOne'] = NN_df['colony_age'] + 1





NNPlusOne = pd.merge(NN_df, NNPlusOne, on=['colony_age_plusOne', 'colony_ID']) # change merge to be all left or something

NNPlusOne['growth'] =  (NNPlusOne['num_plusOne'] - NNPlusOne['num_adsB4_dispersal']) /NNPlusOne['num_adsB4_dispersal']

print NNPlusOne.head(5)

#print NNPlusOne.info()

def growth_func(N, c, r, a, b):
    eqn =  c + r * np.exp((1 + (a * N/1000) ) * (1 - (b * N/1000)))
    return eqn

Ndata = np.array(NNPlusOne['num_adsB4_dispersal'])

print "max Ndata", max(Ndata)


rdata = NNPlusOne['growth']

guess = (-0.2, 0.4, 1, 2)

popt, pcov = curve_fit(growth_func, Ndata, rdata, guess)
#Constrain the optimization to the region of 0 < a < 3, 0 < b < 2 and 0 < c < 1:

#popt, pcov = curve_fit(func, xdata, ydata, bounds=(0, [3., 2., 1.]))

print "popt", popt

print "pcov", pcov

NNPlusOne.to_csv("G:/ownCloud/DisperalSimulationOutput/Testing/OutputTest.csv")

## differciate to find maximum



fmodel = Model(growth_func)
params = Parameters()
params.add('c', value=-0.2, vary=True)
params.add('r', value=0.4, vary=True)
params.add('a', value=1, vary=True, max=5)
params.add('b', value = 2, vary=True)

result = fmodel.fit(rdata, params, N=Ndata)
print(result.fit_report())

plt.plot(Ndata, rdata, 'ro')

xdata = np.linspace(0, max(Ndata), 50)
ydata = growth_func(xdata, popt[0], popt[1], popt[2], popt[3])

plt.plot(xdata, ydata)

plt.show()

