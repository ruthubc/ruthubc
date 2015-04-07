'''
Created on Feb 25, 2015

@author: Ruth
'''
import sys
import itertools
import csv

def run_numbers(numbersFile):
    i=[]
    with open(numbersFile, 'rb') as f:
        reader = csv.reader(f)
        for row in reader:
            i = i + [int(row[0])]
    maxNum =  max(i)
    
    with open(numbersFile, 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([maxNum +1])
    return maxNum

def writePBS(FileName):
    print('Creating new file')
    name =FileName + '.pbs' # Name of text file coerced with +.txt
    file = open(name,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n")
    file.write("#PBS -S /bin/bash\n")
    file.write("#PBS -M rvsharpe.ubc@gmail.com\n")
    file.write("#PBS -m bea\n")
    file.write("#PBS -l walltime=00:30:00\n")
    file.write("#PBS -l pmem=240mb\n")
    file.write('#PBS -l procs=4\n')
    file.write("""echo "Current working directory is `pwd`"\n""")
    file.write("""echo "Starting run "$0" at: `date`"\n""")
    file.write("module load python/2.7.5.anaconda\n")
    file.write("cd $PBS_O_WORKDIR\n")
    file.write("python " + FileName + ".py\n")
    file.write("""echo "Program "$0" finished with exit code $? at: `date`" """)
    file.close()

def writePythonRun(FileName, comp_slp, disp_risk, K, amt_var):
    name = FileName + '.py'
    file = open(name, 'w+')
    file.write("from core.DispersalRun import disperal_run\n")
    file.write("sim_len = 10\n")
    file.write('filename = "'  + FileName + '.py"\n')
    file.write("comp_slp = " + str(comp_slp) + "\n")
    file.write("disp_risk =" + str(disp_risk)+ "\n")
    file.write("K=" + str(K)+ "\n")
    file.write("amt_var=" + str(amt_var)+ "\n")
    file.write("disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var)\n")
    file.close()

#1) slope [0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0, 10.0]
#2) Risk of dispersal [0, 0.1, 0.2, 0.3, 0.4]
#3) mean K
#4) variance in k and FLN

runs = [[0.6], [0.4], [10], [0.1]] # slope, risk of dispersal, MeanK , Var k
combinations = list(itertools.product(*runs))

print combinations
fileNameLst = []

for i in range(0, len(combinations)):
    number = run_numbers("RunNumbers.csv")
    tup = combinations[i]
    slope = tup[0]
    risk = tup[1]
    K = tup[2]
    var = tup[3]
    filename = 'slp' + str(slope) + "_Rsk" + str(risk) + "_K" + str(K) + "_var" + str(var) + '_rn' + str(number)
    print "tup", tup
    print filename
    writePythonRun(filename, slope, risk, K, var)
    writePBS(filename)
    fileNameLst.extend([filename])
    
print "filenameList:", fileNameLst

file = open("qsubFile.txt", 'w+')


for name in fileNameLst:
    print name
    file.write("qsub " + name + ".pbs; ")
    with open("FilesCreated.csv", 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([name])
