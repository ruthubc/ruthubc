'''
Created on Mar 22, 2017

@author: Ruth
'''

import os


import csv
with open('CSV_files_to_delete.csv', 'rb') as csvfile:
    myFile = csv.reader(csvfile, delimiter=',')
    for row in myFile:
        runName = ', '.join(row)
        fileName = runName + ".csv"
        try:
            os.remove(fileName)
            print fileName, "file deleted"
        except OSError:
            print fileName, "file not found"
        



