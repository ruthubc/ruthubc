'''
Created on Jul 15, 2014

@author: user
'''

import csv
from core.ColonyClass import Colony
from core.SpiderClass import Spider
from core.AdultClass import Adult
from core.JuvClass import Juv
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

    start_col1 = Colony(1, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col2 = Colony(2, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col3 = Colony(3, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col4 = Colony(4, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col5 = Colony(5, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col6 = Colony(6, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col7 = Colony(7, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col8 = Colony(8, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col9 = Colony(9, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)
    start_col10 = Colony(10, [Adult([1, 0, 0.8], 0, 0)], slope = comp_slp)

    #print start_col.colony_dict()
    col_list = [start_col1, start_col2, start_col3, start_col4, start_col5, start_col6, start_col7, start_col8, start_col9, start_col10]

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

