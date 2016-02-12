'''
Created on Jan 24, 2016

@author: user
'''

import numpy as np
import pandas as pd


def missing_elements(L, start, end):
    if end - start <= 1:   
        if L[end] - L[start] > 1:
            for i in range (L[start] + 1, L[end]):
                yield i
        return

    index = start + (end - start) // 2

    # is the lower half consecutive?
    consecutive_low =  L[index] == L[start] + (index - start)
    if not consecutive_low:
        for el in missing_elements(L, start, index):
            yield el

    # is the upper part consecutive?
    consecutive_high =  L[index] == L[end] - (end - index)
    if not consecutive_high:
        for p in missing_elements(L, index, end):
            yield p

def checkMissing(L, max):
    L = L[~np.isnan(L)] # removes NAN's
    L = L.tolist()
    L = [int(x) for x in L]
    L.extend([0, max + 1])
    L.sort()
    missingList = list(missing_elements(L,0,len(L)-1))
    #L = range(10, 21)
    #print(list(missing_elements(L,0,len(L)-1)))
    return missingList

def updateColSpaceWholeTbl(data, minColID, maxColID):
    for i in range(minColID, maxColID):
        colID = i
        colSpace =  max(data[data.colony_ID == colID].col_space)
        data.loc[data.colony_ID == colID, 'col_space'] = colSpace

def updateMissingColSpace(data, pop_age):
    indexList = data[(pd.isnull(data['col_space'])) & (data['pop_age'] == pop_age)].index.tolist() # list of index that don't have a colony_space
    if len(indexList) > 1:
        colSpaceList = data[data.pop_age == pop_age].col_space.values  # getting the list of currenlty assigned col spaces
        missing = checkMissing(colSpaceList, 200)
        numToFill = len(indexList)
        cols = data[(pd.isnull(data['col_space'])) & (data['pop_age'] == pop_age)].colony_ID
        minID = min(cols)
        maxID = max(cols)
        for i in range(0, numToFill):
            rowInx = indexList[i]
            newSpace = missing[i]
            data.ix[rowInx, 4] = newSpace
        updateColSpaceWholeTbl(data, minID, maxID)


def main(data):
    data["col_space"] = np.nan # creates an empty column col_space

    data.loc[data.pop_age == 1, 'col_space'] = data.colony_ID  # updates the col_space as col ID for pop_age 1

    # updates the rest of the table
    minColID = 1
    maxColID =  max(data[data.pop_age == 1].colony_ID)
    updateColSpaceWholeTbl(data, minColID, maxColID)

    maxPopAge = max(data.pop_age)

    for i in range(2, maxPopAge + 1):

        print 'age', i, 'of', maxPopAge
        updateMissingColSpace(data, i)
    #return data
