'''
Created on Jul 15, 2014

@author: user
'''

import csv
import os
from core.StartColsClass import StartCols
from core.PopulationClass import Poplt
from core.Functions import export_rownames


def disperal_run(indFile, sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list, ad_disFd_lmt, F_Ln, compType):
    #(1) write rownames to csv fileh
    
    try:
        os.remove(filename)
    except OSError:
            pass

    export_rownames(filename + ".csv") # creating file with rownames 

    ### write inds to new file
    if indFile == "y":
        indFiles = open(filename + "_inds.csv", "wb")
        wr = csv.writer(indFiles, dialect = 'excel')
        wr.writerow(["type", "col_age", "col_id", "rank", "die", "food", "disperse", "no_off"])
        indFiles.close()

    #(2) Initial Population
    cols = StartCols(indFile, compType, K, comp_slp, ad_fd = 0.8) # ad food is the size of the adults
    cols.make_col_list()
    col_list = cols.col_list

    min_no_off = off_list[0]
    max_no_off = off_list[1]


    this_pop = Poplt(col_list, compType, indFile, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, min_no_off, max_no_off, ad_disFd_lmt, F_Ln, cols.col_no)
    this_pop.update_offVar()

    #print start_col.colony_list_to_append()

    #(3) Repeated population timesteps

    for run in range(0, sim_len):
        if not this_pop.poplt_list:
            print "population has gone extinct"
            break
        this_pop.one_poplt_timestep()
        print "population age:", 
        print this_pop.pop_age

    #(4) END
    print "end, check file"

