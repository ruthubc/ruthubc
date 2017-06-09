'''
Created on Feb 25, 2015

@author: Ruth

******#TODO: put in code to delete run.sh before writing to it!

# Run numbers, the next file number is the last number in the runNumbers.csv file

# magic f_ln number is 0.61
'''
import sys
import itertools
import csv
import time
import platform
import socket
import os

compName = platform.node()
fileTxtName = "100hrs_Orcinus.txt"

pbsName = "100hrs_29Mar_Orcinus"
runtime = 100


if compName == 'Sony-PC':
    print("This is work computer")
    savePath  = 'G:\\Dropbox\\RuthSync\\SimulationFiles\\RunFiles\\'
elif compName == 'MacPC':
    savePath  = 'C:\\Users\\Ruth\\Dropbox\\RuthSync\\SimulationFiles\\RunFiles\\'
    print("this is the MacPC")    
else:
    savePath  = 'C:\\Work\\Dropbox\\RuthSync\\SimulationFiles\\RunFiles\\'
    print("this is the home computer")


indFile = "n"


slopes = [0.4] * 3 # 8 reps
dispersalRisks = [0.3] 
meanK = [300]
Vars = [0.7]
adDisSizes = [0.6, 0.8]
off_list = [[4, 6]]
F_Lns = [0.61]


sim_len = 500

comp_type = "N"

runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
combinations = list(itertools.product(*runs))


## Adding an individual extra combination
mySlope = 0
myVariance = 0.8
myDispSize = 0.6

#extraTup = tuple([mySlope, dispersalRisks[0], meanK[0],  myVariance, myDispSize, off_list[0], F_Lns[0]])
#combinations.extend([extraTup])

# extra combinations
slopes = [0] * 3 # 7 reps
dispersalRisks = [0.3] 
meanK = [300]
Vars = [0.8]
adDisSizes = [0.6]
off_list = [[4, 6]]
F_Lns = [0.61]


sim_len = 500

comp_type = "N"

runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
combinations.extend(list(itertools.product(*runs)))




def run_numbers(numbersFile):  # Assigns a unique number to each run
    i=[]
    completeName = os.path.join(savePath, numbersFile)
    with open(completeName, 'rb') as f:
        reader = csv.reader(f)
        for row in reader:
            i = i + [int(row[0])]
    maxNum = max(i)

    with open(completeName, 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([maxNum +1])
    return maxNum

# pbs instructions https://www.westgrid.ca/files/PBS%20Script_0.pdf


def writePBS(PBSFileName, pyNameFile, runtime, numFiles):  # writes the PBS file for each run
    completeName = os.path.join( savePath, "ForCluster", PBSFileName + '.pbs')
    file = open(completeName,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n")
    file.write("#PBS -S /bin/bash\n")
    file.write("#PBS -M rvsharpe.ubc@gmail.com\n")
    file.write("#PBS -m ea\n")  # only send email when job is aborted or terminated
    file.write("#PBS -l walltime=" + runtime + "\n")
    file.write("#PBS -l mem=10gb\n")
    file.write('#PBS -l procs=1\n')
    file.write('#PBS -l partition=DDR\n') 
    file.write("#PBS -t 1-" + str(numFiles) + "\n")
    file.write("module load python/2.7.3\n")# on grex: file.write("module load python/2.7.5.anaconda\n")
    file.write("cd $PBS_O_WORKDIR\n")
    file.write("""echo "Task index number : $PBS_ARRAYID"\n""")
    file.write("\n")
    file.write("file=`sed -n ''${PBS_ARRAYID}'p' " + pyNameFile + "`\n")
    file.write("""echo "file is: $file"\n""")
    file.write("python $file\n")
    file.write("""echo "Program "$0" finished with exit code $? at: `date`"\n""")
    file.write("""echo "file is: $file"\n""")
    file.close()


def writePythonRun(FileName, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list,
                    ad_disFd_lmt, F_Ln, sim_len, compType): # writing the python file  that runs the model

    name = FileName + '.py'
    completeName = os.path.join( savePath, "ForCluster", name)
    file = open(completeName, 'w+')
    file.write("from core.DispersalRun import disperal_run\n")
    file.write("sim_len = " + str(sim_len) + "\n")
    file.write('filename = "'  + FileName + '.py"\n')
    file.write("comp_slp = " + str(comp_slp) + "\n")
    file.write("disp_risk =" + str(disp_risk)+ "\n")
    file.write("K=" + str(K)+ "\n")
    file.write("amt_var =" + str(amt_var)+ "\n")
    file.write("min_juv_size ="  + str(min_juv_size) + "\n")
    file.write("off_list ="  + str(off_list) + "\n")
    file.write("ad_disFd_lmt ="  + str(ad_disFd_lmt) + "\n")
    file.write("F_Ln ="  + str(F_Ln) + "\n")
    file.write("indFile=" + '"' + str(indFile) +  '"' + "\n")
    file.write("compType=" + '"' + str(comp_type) +  '"' + "\n")
    file.write("disperal_run(indFile, sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list, ad_disFd_lmt, F_Ln, compType)\n")
    file.close()


## making file name lists
fileNameLst = []  # for all files
runTimeList = []
fileName_forTxt = []

for i in range(0, len(combinations)):  # actually produces the files
    number = run_numbers("RunNumbers.csv")
    tup = combinations[i]
    slope = tup[0]
    risk = tup[1]
    K = tup[2]
    var = tup[3]
    ad_disFd_lmt = tup[4]
    min_juv_size = 0.205
    off_list = tup[5]
    F_Ln = tup[6]

    filename =  str(number) + "_" + 'slp' + str(slope) + "_Rsk" + str(risk) + "_K" + str(K) + "_var" + str(var) +  "_dslm" + str(ad_disFd_lmt) + "_maxOff" + str(off_list[1])
    print filename

    writePythonRun(filename, slope, risk, K, var, min_juv_size, off_list,
                   ad_disFd_lmt, F_Ln, sim_len, comp_type) # writes the .py file for each run

    fileName_forTxt.extend([filename])
    
    fileNameLst.extend([(filename, runtime)])
    runTimeList.extend([runtime])

#print "filenameList:", fileNameLst

#file = open("qsubFile.txt", 'w+')


for name in fileNameLst: # writes the names of the files created to csv file for my records and 
    #print name
    #file.write("qsub " + name + ".pbs; ")
    completeName = os.path.join( savePath, "FilesCreated.csv")
    with open(completeName, 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([name[0], time.strftime("%d/%m/%Y"), sim_len, name[1]])

# writing to the files for array job
def writeArrayTxtFiles(listName, fileName):
    completeName = os.path.join( savePath, "ForCluster", fileName)
    file = open(completeName, "wb")
    for name in fileName_forTxt:
        file.write(name + ".py")
        file.write("\n")

writeArrayTxtFiles(fileNameLst, fileTxtName)


writePBS(pbsName, fileTxtName, str(runtime) + ":00:00", len(fileName_forTxt))


print "Total number of combinations", len(combinations)

print "end"
