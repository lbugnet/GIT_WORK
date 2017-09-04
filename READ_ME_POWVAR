READ ME POWVAR

Program tree: (compile everything + KADACS)

A2ZPL.pro .main program call
PREPARE_FILES_A2ZPL.pro -> type, champ, ...
COMPUTE_PSD_DATA.pro  -> create the PSD  
  Light_curve_path_to_SPD.pro 
  Light_curve_to_PSD.pro 
  PSD_path_to_PSD.pro 
  KIC_to PSD.pro 
  KIC_Path_to_PSD.pro 
POWVAR.pro -> compute the metric (PSD-PSDnoise) 
SAVE_SLOPE_POWVAR_NUMAX.pro -> compute res, slope of metric 
  FLAG_BAD_STARS.pro -> flag stars
  DETECT_OUTLIERS_POWVAR.pro -> plot and save outliers
VARLAW.pro -> guess numax from metric 


INPUT: light curves or light curve paths or PSDs or PSD paths or kic list or kic path list
OUTPUT: P_var values, guess of numax , flagged stars

first try to call a2zpl:
put STAR_PATH_PSD_LIST=file_search(PATH_DATA+'RG_DR25/K???/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_INP', '*COR_PSD_filt_inp.fits')
put calcul_slope=1 if you want to detect outliers and get POWVAR law for numax and dnu
put PATH_OUTPUT='~/DATA/METRIC/' ; Where you want your parameters to be saved
put PATH_TABLE='~/DATA/TABLE/'   ; A2Z, everest, etc. DATA
put PATH_DATA='/Volumes/TEMP/'   ; KEPLER OR K2 DATA
let everything untouched!


You can use 'routine_name, /help' on every routine to get help

OUPUT GUIDE: (main output_files)
KEPLER_varlaw_(numax or dnu or ...)20J_ALL_KEPLER_0.700000_.sav -> contains     res where (res(1)*x +res(0)), 
                                                                slope_fit=(res(1)*x +res(0)), 
                                                                residuals=(yy-slope_fit), 
                                                                threshold=1, 
                                                                xx=alog10(numax or dnu or ...), 
                                                                yy=alog10(P_var), 
                                                                flag(outliers high, low or OK)

LC__0.700000_output_numax_all.sav               -> contains OUTPUT_A2Z (KIC, P_var, ..)

detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt  -> contains KIC of too high stars in P_var VS numax( or dnu) diagram




