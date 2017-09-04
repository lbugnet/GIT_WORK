PRO plot_metric_K2_C7_a2z

;file 1: toutes les metriques des K2 C7
;restore, '/Users/lbugnet/DATA/METRIC/K2/C7/C7RESULTS_A2Zp_variab_metrics_Lor_K2_C7.sav', /verbose; output_a2zp  ;remplacé par metric_all_stars_K2_C7.sav qui contient tout
restore, '/Users/lbugnet/DATA/METRIC/K2/C7/metric_all_stars_K2_C7.sav', /verbose;xx,yy,res,slope_fit,output_a2zp,output_resize,out
;file 2: numax des etoiles observés par A2Z
readcol, '/Users/lbugnet/DATA/METRIC/K2/C7/A2Z_results_K2_C7_Everest_2017_05_03.txt',EPIC, numax, err_numax, dnu, err_Dnu, Ama, err_Amax, FORMAT='A,D,D,D,D,D,D', /silent

;;; match des KIC
match, long(output_a2zp[*,0]), long(epic), i_out, i_epic, count=n

;;;plot
pp=plot((numax(i_epic)), (output_a2zp[i_out,1]), xtitle='(numax a2z K2C7)', xlog=1,ylog=1,ytitle='(metric)',symbol="D", SYM_FILLED=0, linestyle="none")
pp=plot((numax(i_epic)), 10^(res(1)*alog10(numax(i_epic))+res(0)), /overplot, color='magenta')
pp.save, '/Users/lbugnet/DATA/METRIC/K2/C7/metric_all_stars_K2_C7_plot.png'






stop






END