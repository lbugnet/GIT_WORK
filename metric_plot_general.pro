PRO metric_plot_general
device, decomposed=0
loadct, 39

;------------------------------------------------------------------------------
;------- PLOT OF THE SPECTRUM OF STARS CONTAINED BOTH IN file 1 and file 2-----
;------- with numax detected by file 1 ----------------------------------------
;------- and guesses for numax detected by the metric file 3 ------------------
;------------------------------------------------------------------------------

;file 1: Contient les étoiles en commun dont on connait les masses
restore, '/Users/lbugnet/DATA/TABLES/masses_stars.sav', /verbose ;xx=alog10(numax(i1)), yy=alog10(output_a2zp[i2,1]),residuals,threshold,M,flag, kic_s, numax, dnu
;file 2: Contient la liste des directoires de ces étoiles
restore, '/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25.sav', /verbose ; files_arr(string, direction du fichier),id_stars_arr
;file 3: Contient la métrique
restore, '/Users/lbugnet/DATA/TABLES/metric_all_stars.sav', /verbose ; xx=alog10(numax(i1)),yy=alog10(output_a2zp[i2,1]),res(0), res(1), slope_fit, output_a2zp (non resizé), output_resize(resizé 16266),  out (stars not in common)
;file 4:
restore, '/Users/lbugnet/DATA/KEPLER_LC/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/RESULTS_A2Zp_variab_metrics_Lor.sav', /verbose ;OUTPUT_A2ZP, BD_STARS, slope_fit
;file 5; Contient la metrique OK
restore,'/Users/lbugnet/DATA/TABLES/stars_OK.sav'    ; xx_ok, yy_ok, residuals_ok, threshold_ok, slope_fit_ok, res_ok

;--------- remove stars not in common file 1 and file 2 -----------------------
match, kic_s,long(id_stars_arr), ind1, ind2, count=nn
xx=xx(ind1)
yy=yy(ind1)
residuals=residuals(ind1)
M=M(ind1)
flag=flag(ind1)
kic_s=kic_s(ind1)
numax=numax(ind1)
dnu=dnu(ind1)
files_arr=files_arr(ind2)
id_stars_arr=id_stars_arr(ind2)
;---------- PLOT JPEG ---------------------------------------------------------
;multi_plot_routine, 1, numax, files_arr, res, threshold, flag, kic_s, xx, yy, M ; stars low
;multi_plot_routine, 2, numax, files_arr, res, threshold, flag, kic_s, xx, yy, M ; stars high
;multi_plot_routine, 3, numax, files_arr, res, threshold, flag, kic_s, xx, yy, M ; stars OK
;multi_plot_routine, 0, numax, files_arr, res, threshold, flag, kic_s, xx, yy, M ; stars right
;
;---------- PLOT METRIC/PREDICTED NUMAX FOR OUT STARS -------------------------

plot_metric_type, output_resize, yy_ok, xx_ok, res, threshold, flag, slope_fit, numax

;---------- PLOT METRIC/PREDICTED NUMAX FOR OUT STARS AND PLOT METRIC/MOYBRUIT FOR OUT STARS-------------------------

metric_predict_numax, out, res, threshold, flag_SN=flag_SN   ; appliyed on OUT stars (etoiles qui ne sont pas calculés par A2Z)
;plot_metric_bruit_nuyquist, out,flag_SN,output_a2zp=output_a2zp

;---------- PLOT METRIC/MOYBRUIT 15000 STARS-----------------------------------

plot_metric_bruit_zone, output_resize,flag,output_a2zp=output_a2zp






stop

END