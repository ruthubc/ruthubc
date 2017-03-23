'''
Created on Mar 22, 2017

@author: Ruth
'''

#import pandas as pd
import os

#CSV = pd.read_csv('filesToDelete.csv')

import csv
with open('filesToDelete.csv', 'rb') as csvfile:
    myFile = csv.reader(csvfile, delimiter=',')
    for row in myFile:
        runName = ', '.join(row)
        fileName = runName + ".csv"
        print fileName
        try:
            os.remove(fileName)
            print "file deleted"
        except OSError:
            print "file not found"
        



