from core.DispersalRun import disperal_run
sim_len = 20
filename = "TESTING.py"
comp_slp = 0.8
disp_risk =0.3
K=300
amt_var =0.0
min_juv_size =0.205
off_list =[2, 4]
ad_disFd_lmt =0.2
F_Ln =0.61
indFile="n"
compType="N"
disperal_run(indFile, sim_len, filename, comp_slp, disp_risk, K, amt_var, min_juv_size, off_list, ad_disFd_lmt, F_Ln, compType)