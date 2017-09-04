PRO use_metric_for_K2

;;;; on recupère la métrique faite sur les données Kepler
;restore, file='/Users/lbugnet/DATA/TABLES/metric_all_stars.sav', /verbose;, xx, yy, res, slope_fit, output_a2zp, output_resize,  out
;output_Kepler=output_resize
;;xx=alog10(numax)
;;yy=alog10(output)
;
;;; on récupere les données K2
restore, file='/Users/lbugnet/DATA/METRIC/K2/C4/metric_all_stars_K2_C4.sav', /verbose;, xx, yy, res, slope_fit, output_a2zp, output_resize,  out  ; prédit avec les numax de A2Z, xx=alog(numax)
output_K2=output_resize
numax_guess_K2=dblarr(n_elements(output_K2[*,1]))
numax_guess_K2(*)=10.^((alog10(output_K2[*,1])-res(0))/res(1)) ; calcul de numax pour les etoiles de K2C4
;numax_guess_K2=10.^(numax_guess_K2)

;;;PLOT AND SAVE
;
pp5=plot(10^xx,10^yy,xlog=1,ylog=1,xtitle='log(numax K2C4 predicted)', ytitle='log(Metric by K2)', symbol="o" ,color="black",linestyle="none", title="NUMAX GUESS FOR ALL K2 C4 STARS")
pp5.thick=1
pp5=plot((numax_guess_K2), ((output_K2[*,1])) ,xlog=1,ylog=1,symbol="D", SYM_FILLED=0, color="dark orchid",linestyle="none", /overplot)
pp5.thick=3
pp5.save, '/Users/lbugnet/DATA/METRIC/K2/C4/guess_numax_K2_C4_all.png'
save,file='/Users/lbugnet/DATA/METRIC/K2/C4/guess_numax_K2_C4_all.sav', output_K2, numax_guess_K2
close,2
openw,2,'/Users/lbugnet/DATA/METRIC/K2/C4/guess_numax_K2_C4_all.txt'
close,2
openw,2, '/Users/lbugnet/DATA/METRIC/K2/C4/guess_numax_K2_C4_all.txt', /append
printf,2, 'KIC K2 C4, numax_guess_K2, PSD_metric_K2'
for i=0, n_elements(output_K2[*,0])-1 do printf, 2, string(long(output_K2[i,0])), numax_guess_K2(i), output_K2[i,1],format=‘(i10, 2x, f12.2)’
close,2
stop
;---------------------------------------------
;----- plot multiplot ------------------------
;;--------------------------------------------
;
restore, '/Users/lbugnet/DATA/METRIC/K2/C4/masses_stars_K2_C4.sav';, donne threshold

multi_plot_routine_K2_C4, numax_guess_K2,  res, threshold,  output_K2[*,0], numax_guess_K2, output_K2[*,1]

END