'''
Created on Jan 22, 2016

@author: user
'''


import matplotlib.pyplot as plt
from matplotlib import animation
import matplotlib.patches as mpatches
import numpy as np
import pandas as pd
#from matplotlib.animation import ArtistAnimation

dataTitle = '13539_slp0.8_Rsk0.3_K300_var0_dslm0.8_maxOff6.py'


data = pd.read_csv("AnimationDataOutput.csv", sep = ',') # import data
# first set up fig, the axis and the plot element we want to animate

maxColSize = max(data.num_adsB4_dispersal) # for y axis


numBars = 200

numFrames = max(data.pop_age.values+1)

fig = plt.figure(figsize=(16,8))

fig.suptitle(dataTitle, fontsize=14, fontweight='bold')

black_patch = mpatches.Patch(color='black', label='dying')
red_patch = mpatches.Patch(color = 'red', label = 'dispersing')
yellow_patch = mpatches.Patch(color = 'yellow', label = 'dispersing, then dying')
plt.legend(handles=[red_patch, black_patch, yellow_patch], fontsize = 11)

ax = fig.add_subplot(111)

position = np.arange(numBars)# + .5 # number of bars

heights = [0]*numBars
rects = plt.bar(position, heights, align = 'center', color = '#b8ff5c')


plt.ylim((0,maxColSize))
plt.xlim((0,numBars))

plt.grid(True)

rs = [r for r in rects] # rects a BarContainer, rs is the same as rects except it is a list containing rectangle objects

time_text = ax.text(0.83, 0.82, '', transform=ax.transAxes, fontsize=15)

def init():
    """initialize animation"""
    #time_text.set_text('')
    rs.append(time_text) 
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
    time_text.set_text('generation = %.1f' % i)
    rs.append(time_text)    
    return rs


# to output to screen
#anim = animation.FuncAnimation(fig, animate, init_func=init,frames=numFrames, interval=1, blit=True) # frames is the number of time to iteriate
#plt.show()

animFileName = dataTitle + '.mp4'
print animFileName

anim = animation.FuncAnimation(fig, animate, init_func = init, frames = numFrames, interval=1, blit=True)
anim.save(animFileName,  fps=3, extra_args=['-vcodec', 'libx264']) # min no of fps is 3


print "finished"