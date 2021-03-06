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

indFile = "n"

slopes = [0, 0.4, 0.8, 1.25, 2.5, 10] # 10 is full contest competition
dispersalRisks = [0.3]
meanK = [300]
Vars = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]
adDisSizes = [0.2]
off_list = [[4, 6]]
F_Lns = [0.61]

sim_len = 500
comp_type = "N"


#runs = [[0 - slope], [1- risk of dispersal], [2- meanK], [3- Var], [4- ad dispersal limit], [5- min off], [6- max off], [7- F_ln]]
runs = [slopes, dispersalRisks, meanK, Vars, adDisSizes, off_list, F_Lns]
combinations = list(itertools.product(*runs))





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

# pbs instructions https://www.westgrid.ca/files/PBS%20Script_0.pdf

def writePBS(FileName, runtime):  # writes the PBS file for each run
    print('Creating new file')
    name =FileName + '.pbs' # Name of text file coerced with +.txt
    file = open(name,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n")
    file.write("#PBS -S /bin/bash\n")
    file.write("#PBS -M rvsharpe.ubc@gmail.com\n")
    file.write("#PBS -m ea\n")  # only send email when job is aborted or terminated
    file.write("#PBS -l walltime=" + runtime + "\n")
    file.write("#PBS -l mem=300mb\n")
    file.write('#PBS -l procs=1\n')
    file.write("module load python/2.7.2\n")# on grex: file.write("module load python/2.7.5.anaconda\n")
    file.write("cd $PBS_O_WORKDIR\n")
    file.write("python " + FileName + ".py\n")
    file.write("""echo "Program "$0" finished with exit code $? at: `date`" """)
    file.close()

def writePythonRun(FileName, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list,
                    ad_disFd_lmt, F_Ln, sim_len, compType): # writing the python file  that runs the model
    name = FileName + '.py'
    file = open(name, 'w+')
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




fileNameLst = []


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
    print "tup", tup
    print filename
    writePythonRun(filename, slope, risk, K, var, min_juv_size, off_list,
                   ad_disFd_lmt, F_Ln, sim_len, comp_type)

    ### runtime if statement, getting correct runtime 
    if ad_disFd_lmt > 1:
        runtime = "02:00:00"
    if ad_disFd_lmt == 0.4 and slope > 0.7  and slope < 1.3 and off_list[1] > 5 and off_list[1] < 7:
        runtime = "72:00:00"
    elif off_list[1] < 5:
        runtime = "35:00:00"
    else:
        runtime = "15:00:00"

    writePBS(filename, runtime)
    fileNameLst.extend([filename])

#print "filenameList:", fileNameLst

#file = open("qsubFile.txt", 'w+')



for name in fileNameLst: # writes the names of the files created to csv file for my records and 
    #print name
    #file.write("qsub " + name + ".pbs; ")
    with open("FilesCreated.csv", 'ab') as f:
        writer = csv.writer(f, dialect='excel')
        writer.writerow([name, time.strftime("%d/%m/%Y"), sim_len])

file = open("run.sh", "w+")
file.write("for i in")

# making .sh file

for name in fileNameLst:
    file.write(" " + name + ".pbs")
file.write ("; do qsub $i; done")

print "number of combinations", len(combinations)
print "end"
    
