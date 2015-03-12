'''
Created on Mar 12, 2015

@author: user
'''
from core.PopulationClass import Poplt
from core.ColonyClass import Colony
from core.AdultClass import Adult
import numpy as np
import matplotlib.pyplot as plt

lala = np.arange(0.1, 1.1, 0.1)

print lala
ad_list = []


for fd in lala:
    print fd
    ad = Adult([0.1, 0, fd], 0, 0)
    ad_list.extend([ad])


col = Colony(1, ad_list) 
pplt = Poplt([col])  

pplt.update_offVar()
print pplt.Off_C
print pplt.Off_M
#col.print_adults() 
fdlst=  [ad.food for ad in ad_list]
fdlst = [np.log(m) for m in fdlst]


num_off= col.col_num_off(pplt.Off_M, pplt.Off_C)
num_off = [np.log(n) for n in num_off]

plt.plot(fdlst, num_off)
plt.show()
    