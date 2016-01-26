'''
Created on Jan 22, 2016

@author: user
'''


import matplotlib.pyplot as plt
from matplotlib import animation
import numpy as np
import pandas as pd
import UpdateColSpace

data = pd.read_csv("AnimationData.csv", sep = ',') # import data

UpdateColSpace.main(data)  ## updates the colony space


# first set up fig, the axis and the plot element we want to animate

maxColSize = max(data.num_adsB4_dispersal) # for y axis

'''

subData = data[data.pop_age == 1]

plt.figure(figsize=(16,8)) # changes the size of the figure

plt.bar(subData.col_space.values, subData.num_adsB4_dispersal.values)

plt.xlim(1,201)
plt.ylim(0, maxColSize)
plt.subplots_adjust(left=0.03, right=0.99, top=0.99, bottom=0.03)

plt.show()
'''


numBars = 200

fig = plt.figure(figsize=(16,8))

position = np.arange(numBars)# + .5 # number of bars

#plt.tick_params(axis = 'x', colors = '#072b57')
#plt.tick_params(axis = 'y', colors = '#072b57')

#speeds = [1, 2, 3, 4, 1, 2, 3]
heights = [0]*200
rects = plt.bar(position, heights, align = 'center', color = '#b8ff5c')
#plt.xticks(position, ('A', 'B', 'C', 'D', 'E', 'F'))

#plt.xlabel('X Axis', color = '#072b57')
#plt.ylabel('Y Axis', color = '#072b57')
#plt.title('My Chart', color = '#072b57')

plt.ylim((0,maxColSize))
plt.xlim((0,numBars))

plt.grid(True)

rs = [r for r in rects]


def init():
    return rs

def animate(i):
    print i
    global rs, heights
    subData = data[data.pop_age == i]
    subData.sort_index(by=['col_space'])
    heights =  subData.num_adsB4_dispersal.values
    for h,r in zip(heights,rs):
        r.set_height(h)
    return rs

anim = animation.FuncAnimation(fig, animate, init_func=init,frames=200, interval=1, blit=True) # frames is the number of time to iteriate

plt.show()