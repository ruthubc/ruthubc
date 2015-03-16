'''
Created on Feb 25, 2015

@author: Ruth
'''
import sys

def writePBS(runName):
    print('Creating new asdtext file')


    name ='FileName.py'#+'.txt'  # Name of text file coerced with +.txt
    file = open(name,'w+')   # Trying to create a new file or open one'
    file.write("#!/bin/bash\n#PBS -S /bin/bash\n#PBS -M rvsharpe.ubc@gmail.com\n#PBS -m bea\n")
    file.write("""#PBS -l walltime=00:30:00\nPBS -l pmem=240mb\necho "Current working directory is `pwd`""")
    file.write("""echo "Starting run "$0" at: `date`"\nmodule load python/2.6.7\ncd $PBS_O_WORKDIR\n""")
    file.write(runName + """\necho "Program "$0" finished with exit code $? at: `date`""")
    file.close()


file = 'pythonTest1.py'
writePBS(file)