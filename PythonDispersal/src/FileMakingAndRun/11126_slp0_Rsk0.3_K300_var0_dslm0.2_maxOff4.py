from core.DispersalRun import disperal_run
sim_len = 5
filename = "TEST_11126_slp0_Rsk0.3_K300_var0_dslm0.2_maxOff4.py"
comp_slp = 0
disp_risk =0.3
K=300
amt_var =0
min_juv_size =0.205
off_list =[2, 4]
ad_disFd_lmt =0.2
F_Ln =0.61
indFile="n"
compType="N"
disperal_run(indFile, sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list, ad_disFd_lmt, F_Ln, compType)
