'''
Created on Feb 25, 2015

@author: Ruth
'''
import sys
import itertools

def writePBS(FileName):
    print('Creating new file')


    name =FileName + '.pbs' # Name of text file coerced with +.txt
    file = open(name,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n#PBS -S /bin/bash\n#PBS -M rvsharpe.ubc@gmail.com\n#PBS -m bea\n")
    file.write("""#PBS -l walltime=00:30:00\nPBS -l pmem=240mb\necho "Current working directory is `pwd`""")
    file.write("""echo "Starting run "$0" at: `date`"\nmodule load python/2.6.7\ncd $PBS_O_WORKDIR\n""")
    file.write(FileName + '.py' + """\necho "Program "$0" finished with exit code $? at: `date`""")
    file.close()

'''
file = 'pythonTest1.py'
writePBS(file)
'''

def writePythonRun(FileName, comp_slp, disp_risk, K, amt_var):
    
    name = 'FileName.py'
    file = open(name, 'w+')
    file.write("from core.DispersalRun import disperal_run")
    file.write("sim_len = 10")
    file.write("filename = " + FileName + ".py")
    file.write("comp_slp = " + comp_slp)
    file.write("disp_risk =" + disp_risk)
    file.write("K=" + K)
    file.write("amt_var" + amt_var)
    file.write("disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var)")

#1) slope [0, 0.2, 0.4, 0.6, 0.8, 1, 1.25, 1.666667, 2.5, 5.0, 10.0]
#2) Risk of dispersal [0, 0.1, 0.2, 0.3, 0.4]
#3) mean K
#4) variance in k and FLN

runs = [[0, 1], [0.1], [10], [0.1]]
combinations = list(itertools.product(*runs))

print combinations

for i in range(0, len(combinations)):
    tup = combinations[i]
    filename = 'name' + str(tup[2])    
    print tup
    print filename
    


