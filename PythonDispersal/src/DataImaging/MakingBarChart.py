'''
Created on Jan 22, 2016

@author: user
'''


import matplotlib.pyplot as plt
from matplotlib import animation
import numpy as np
import pandas as pd
#from matplotlib.animation import ArtistAnimation


data = pd.read_csv("AnimationData.csv", sep = ',') # import data




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

# Setting up formatting for movie files mp4 files
#Writer = animation.writers['ffmpeg']
#writer = Writer(fps=15, metadata=dict(artist='Me'), bitrate=1800)


numBars = 200

numFrames = max(data.pop_age.values)

fig = plt.figure(figsize=(16,8))

position = np.arange(numBars)# + .5 # number of bars

#plt.tick_params(axis = 'x', colors = '#072b57')
#plt.tick_params(axis = 'y', colors = '#072b57')

#speeds = [1, 2, 3, 4, 1, 2, 3]
heights = [0]*numBars
rects = plt.bar(position, heights, align = 'center', color = '#b8ff5c')
#fig.annotate("text", xy = (100, 100))
#plt.xticks(position, ('A', 'B', 'C', 'D', 'E', 'F'))

#plt.xlabel('X Axis', color = '#072b57')
#plt.ylabel('Y Axis', color = '#072b57')
#plt.title('My Chart', color = '#072b57')

plt.ylim((0,maxColSize))
plt.xlim((0,numBars))

plt.grid(True)

rs = [r for r in rects] # rects a BarContainer, rs is the same as rects except it is a list containing rectangle objects


def init():
    return rs

def animate(i):
    print i
    global rs, heights
    subData = data[data.pop_age == i]
    subData.sort_index(by=['col_space'])
    heights =  subData.num_adsB4_dispersal.values
    heights = heights.tolist()
    numZeros = numBars - len(heights)
    heights = heights + [0] * numZeros
    #colours = ['r'] * 200
    disps = subData.state.values
    colours = ['r' if x =='Dalive' else 'k' if x == 'Ndead' else 'y' if x == 'Ddead' else 'b' for x in disps]  + ['b'] * numZeros
    # black if dead, red if disperse, yellow if dispersing and dying, otherwise blue
    for h,r, c in zip(heights,rs, colours):
        r.set_height(h)
        r.set_facecolor(c)
    return rs

# to output to screen
#anim = animation.FuncAnimation(fig, animate, init_func=init,frames=numFrames, interval=1, blit=True) # frames is the number of time to iteriate
#plt.show()

# To ouput as gif
#anim = animation.FuncAnimation(fig, animate, frames=10, interval=1)
#anim.save('Animation.gif', writer='imagemagick')

anim = animation.FuncAnimation(fig, animate, init_func = init, frames = numFrames, interval=1, blit=True)
anim.save('DispSim4010.mp4',  fps=5, extra_args=['-vcodec', 'libx264'])



print "finished"