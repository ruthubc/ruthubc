'''
Created on Mar 23, 2017

@author: Ruth
'''

from os import listdir
import csv

print "starting run"

dirFiles =  listdir("OutputFiles/.")
print "number of files: ", len(dirFiles)

outputFileSummary = open("outputResults_15March.csv", "wb")

wr = csv.writer(outputFileSummary, dialect = 'excel')
wr.writerow(["filename", "lines", "finished"])

for filename in dirFiles:
    with open("OutputFiles/" + filename) as currentFile:
        all_lines = currentFile.readlines()
        numLines = len(all_lines)
        if (numLines < 9):
            lines = "NotEnoughLines"
        else:
            lines = str(all_lines[9])
            lines = lines.replace("file is: ", "").replace('\n', '').replace('\r', '')

        if any("end, check file" in s for s in all_lines):
            
            finished = "TRUE"
        else:
            print filename, "- Not Finished"
            finished = "FALSE"
        wr.writerow([filename, lines, finished])

outputFileSummary.close()

print "finished running"