# From https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.curve_fit.html

import numpy as np
import pandas as pd
from scipy.optimize import curve_fit

# example from above website
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

# my data

#### Need to make nntplus1 data table.

df = pd.read_csv("G:/ownCloud/DisperalSimulationOutput/Testing/13478_slp0.4_Rsk0.3_K300_var0.05_dslm1.2_maxOff4.py.csv")

#print df.columns.values

#print df.info() 

df[['pop_age']] = df[['pop_age']].astype(float)
#print df[['pop_age']]
df = df[df['pop_age'] >= 3]
        
        
print "after subset"



#print df.info()

NN_df = df[["colony_ID", "num_adsB4_dispersal", "colony_age"]]

NNPlusOne = NN_df.rename(columns = {"num_adsB4_dispersal":"num_plusOne", "colony_age":"colony_age_plusOne"})

NN_df['colony_age_plusOne'] = NN_df['colony_age'] + 1





NNPlusOne = pd.merge(NN_df, NNPlusOne, on=['colony_age_plusOne', 'colony_ID']) # change merge to be all left or something

NNPlusOne['growth'] =  (NNPlusOne['num_plusOne'] - NNPlusOne['num_adsB4_dispersal']) /NNPlusOne['num_adsB4_dispersal']

print NNPlusOne.head(5)

#print NNPlusOne.info()


def growth_func(N, c, r, a, b):
    return c + r * np.exp((1 + a * N )(1 - b * N))

Ndata = np.array(NNPlusOne['num_adsB4_dispersal'])


ydata = np.array(NNPlusOne['growth'])

popt, pcov = curve_fit(growth_func, xdata, ydata)
#Constrain the optimization to the region of 0 < a < 3, 0 < b < 2 and 0 < c < 1:

#popt, pcov = curve_fit(func, xdata, ydata, bounds=(0, [3., 2., 1.]))

print "popt", popt

print "pcov", pcov