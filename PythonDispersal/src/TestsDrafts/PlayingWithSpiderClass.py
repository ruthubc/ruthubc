'''
Created on 2012-12-23

@author: ruth
'''
from SpiderClass import Spider
from ColonyClass import Colony


#spiList = [spi1, spi2] # creates a list of object spiders

spi5 = Spider(4, 8, 9)

spiList = [Spider(3, 6, 5), Spider(1, 0, 5), Spider(5, 9, 7), spi5]

def PS(spiList):
    for i in range(len(spiList)):

        print spiList[i]



PS(spiList)

#spiList.pop(1) #removes item at 
#spiList.remove(spi5)
print ("spilist2")

spiList2 = [spi for spi in spiList if spi.instar > 3]

PS(spiList2)

print ('miniusin')



#list_1 = [['good',100, 20, 0.2],['bad', 10, 0, 0.0],['change', 1, 2, 2]]
#list_1 = [item for item in list_1 if item[2] >= 5 or item[3] >= 0.3]

#print("removed")

#PS(spiList)