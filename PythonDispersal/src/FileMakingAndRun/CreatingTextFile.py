'''
Created on Feb 25, 2015

@author: Ruth

******#TODO: put in code to delete run.sh before writing to it!
'''
import sys
import itertools
import csv
import time

def run_numbers(numbersFile):  # Assigns a unique number to each run
    i=[]
    with open(numbersFile, 'rb') as f:
        reader = csv.reader(f)
        for row in reader:
            i = i + [int(row[0])]
    maxNum = max(i)

    with open(numbersFile, 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([maxNum +1])
    return maxNum

def writePBS(FileName):  # writes the PBS file for each run
    print('Creating new file')
    name =FileName + '.pbs' # Name of text file coerced with +.txt
    file = open(name,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n")
    file.write("#PBS -S /bin/bash\n")
    file.write("#PBS -M rvsharpe.ubc@gmail.com\n")
    file.write("#PBS -m bea\n")
    file.write("#PBS -l walltime=15:30:00\n")
    file.write("#PBS -l pmem=2000mb\n")
    file.write('#PBS -l procs=4\n')
    file.write("""echo "Current working directory is `pwd`"\n""")
    file.write("""echo "Starting run "$0" at: `date`"\n""")
    file.write("module load python/2.7.5.anaconda\n")
    file.write("cd $PBS_O_WORKDIR\n")
    file.write("python " + FileName + ".py\n")
    file.write("""echo "Program "$0" finished with exit code $? at: `date`" """)
    file.close()

def writePythonRun(FileName, comp_slp, disp_risk, K, amt_var, min_juv_size, min_no_off,
                   max_no_off, ad_disFd_lmt, F_Ln): # writing the python file  that runs the model
    name = FileName + '.py'
    file = open(name, 'w+')
    file.write("from core.DispersalRun import disperal_run\n")
    file.write("sim_len = 2000\n")
    file.write('filename = "'  + FileName + '.py"\n')
    file.write("comp_slp = " + str(comp_slp) + "\n")
    file.write("disp_risk =" + str(disp_risk)+ "\n")
    file.write("K=" + str(K)+ "\n")
    file.write("amt_var=" + str(amt_var)+ "\n")
    file.write("min_juv_size="  + str(min_juv_size) + "\n")
    file.write("min_no_off="  + str(min_no_off) + "\n")
    file.write("max_no_off="  + str(max_no_off) + "\n")
    file.write("ad_disFd_lmt ="  + str(ad_disFd_lmt) + "\n")
    file.write("F_Ln="  + str(F_Ln) + "\n")
    file.write("disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, min_no_off, max_no_off, ad_disFd_lmt, F_Ln)\n")
    file.close()

#1) slope [0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0, 10.0]
#2) Risk of dispersal [0, 0.1, 0.2, 0.3, 0.4]
#3) mean K
#4) variance in k and FLN

runs = [[0, 0.8, 2.5, 10], [0.05], [100], [0]] # slope, risk of dispersal, MeanK , Var k
combinations = list(itertools.product(*runs))

print combinations
fileNameLst = []

for i in range(0, len(combinations)):  # actually produces the files
    number = run_numbers("RunNumbers.csv")
    tup = combinations[i]
    slope = tup[0]
    risk = tup[1]
    K = tup[2]
    var = tup[3]
    min_juv_size = 0.19
    min_no_off = 2
    max_no_off = 4
    ad_disFd_lmt = 0.6
    F_Ln = 0.4    
    filename = 'slp' + str(slope) + "_Rsk" + str(risk) + "_K" + str(K) + "_var" + str(var) + '_rn' + str(number)
    print "tup", tup
    print filename
    writePythonRun(filename, slope, risk, K, var, min_juv_size, min_no_off,
                   max_no_off, ad_disFd_lmt, F_Ln)
    writePBS(filename)
    fileNameLst.extend([filename])

print "filenameList:", fileNameLst

file = open("qsubFile.txt", 'w+')


for name in fileNameLst: # writes the names of the files created to csv file for my records and 
    print name
    file.write("qsub " + name + ".pbs; ")
    with open("FilesCreated.csv", 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([name, time.strftime("%d/%m/%Y")])
        
file = open("run.sh", "w+")
file.write("for i in")

for name in fileNameLst:
    file.write(" " + name + ".pbs")
file.write ("; do qsub $i; done")
    
