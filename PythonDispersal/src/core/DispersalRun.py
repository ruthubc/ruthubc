'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.AdultClass import Adult
from core.JuvClass import Juv
from core.StartColsClass import StartCols
from core.PopulationClass import Poplt
from core.Functions import export_rownames


def disperal_run(sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, min_no_off, max_no_off, ad_disFd_lmt, F_Ln):
    #(1) write rownames to csv fileh

    export_rownames(filename + ".csv") # creating file with rownames 

    ### write inds to new file

    indFiles = open(filename + "_inds.csv", "wb")
    wr = csv.writer(indFiles, dialect = 'excel')
    wr.writerow(["type", "col_age", "col_id", "rank", "die", "food", "disperse", "no_off"])
    indFiles.close()

    #(2) Initial Population
    cols = StartCols(K, comp_slp, 0.6)
    cols.make_col_list()
    col_list = cols.col_list

    print "col_list", col_list

    this_pop = Poplt(col_list, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, min_no_off, max_no_off, ad_disFd_lmt, F_Ln)
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

