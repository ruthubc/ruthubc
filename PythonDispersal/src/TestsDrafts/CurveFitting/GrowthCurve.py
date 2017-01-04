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

print df.columns.values 

def growth_func(c, r, a, b, N):
    return c + r * np.exp((1 + a * N )(1 - b * N))



Ndata = df.num_adsB4_dispersal

rdata = df.

popt, pcov = curve_fit(func, xdata, ydata)
#Constrain the optimization to the region of 0 < a < 3, 0 < b < 2 and 0 < c < 1:

#popt, pcov = curve_fit(func, xdata, ydata, bounds=(0, [3., 2., 1.]))

print "popt", popt

print "pcov", pcov