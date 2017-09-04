PRO use_metric_for_K2_C4

  ;------------- recupere les données de la métrique de variabilité pour le champ considéré --------
  ;------------- calcule le numax_guess des étoiles du champ ---------------------------------------
  ;------------- multiplot -------------------------------------------------------------------------
  ;------------- détection des étoiles thrusters ---------------------------------------------------
  
  
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
pp5.close
;---------------------------------------------
;----- plot multiplot ------------------------
;;--------------------------------------------

restore, '/Users/lbugnet/DATA/METRIC/K2/C4/masses_stars_K2_C4.sav';, donne threshold
;files_arr=file_search('/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/*')
;path_e='/Users/lbugnet/DATA/METRIC/K2/C4/plot_metric_numax_guess_K2_C4_stars_panel_'
;multi_plot_routine_K2_C4, numax_guess_K2,  res, threshold, numax_guess_K2, output_K2[*,1], files_arr, path_e, string(long(output_K2[*,0]))

;---------------------------------------------
;----- plot multiplot stars cluster ----------
;;--------------------------------------------

readcol, '/Users/lbugnet/DATA/METRIC/K2/C4/bad_r_K2_C4.txt',  KIC_miss, good_numax_b, numax_b, good_numax_r, numax_r, good_numax_y, numax_y, numax_guess, output, FORMAT='a,a, D,a,D,a, D,D,D' 
output_clump=[]
numax_r_keep=[]
numax_guess_keep=[]
kic_miss_keep=[]
good_numax_y_keep=[]
good_numax_b_keep=[]
numax_b_keep=[]
numax_y_keep=[]
for ii=0,n_elements(kic_miss)-1 do begin
  if (numax_r(ii) lt 400) and (numax_r(ii) gt 40) and ((numax_r(ii)) ne -9999) and (output(ii) lt 100000) and (output(ii) gt 1000) then begin
    output_clump=[output_clump,output(ii)]
    numax_r_keep=[numax_r_keep,numax_r(ii)]
    numax_guess_keep=[numax_guess_keep,numax_guess(ii)]
    kic_miss_keep=[kic_miss_keep,kic_miss(ii)]
    good_numax_b_keep=[good_numax_b_keep,good_numax_b(ii)]
    good_numax_y_keep=[good_numax_y_keep,good_numax_y(ii)]
    numax_b_keep=[numax_b_keep, numax_b(ii)]
    numax_y_keep=[numax_y_keep, numax_y(ii)]
  endif
endfor

files_arr=strarr(n_elements(numax_guess_keep))
for ii=0, n_elements(numax_guess_keep)-1 do begin
  files_arr(ii)=file_search('/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/*'+strcompress(kic_miss_keep(ii))+'*')
  ;print, files_arr(ii)
endfor
;stop

p=plot(numax_r_keep,output_clump, xlog=1, ylog=1, symbol='o',linestyle="none")
Thrust_freq = 47.2281
for j=0,6 do p=plot([(Thrust_freq*j),(Thrust_freq*j)],[1000,100000], xlog=1, ylog=1, xr=[10,1000], yr=[1000,100000],/overplot,  xtitle='numax_r',linestyle=2, color='dark blue', title='thrusters K2C4')
p.save, '/Users/lbugnet/DATA/METRIC/K2/C4/thruster_r_K2_C4.png'
;p.close

;files_arr=strarr(n_elements(numax_guess_keep))
;for ii=0, n_elements(numax_guess_keep)-1 do begin
;  files_arr(ii)=file_search('/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/*'+strcompress(kic_miss_keep(ii))+'*')
;  ;print, files_arr(ii)
;endfor
close,12
openw,12,'/Users/lbugnet/DATA/METRIC/K2/C4/thruster_r_K2_C4.txt', WIDTH=80
close,12
OpenW, 12, '/Users/lbugnet/DATA/METRIC/K2/C4/thruster_r_K2_C4.txt',  /append
printf, 12, 'EPIC_thrusters_stars, numax_r, power_metrique, numax_guess_metrique, flag_b, numax_b, flag_y, numax_y'
for ii=0, n_elements(numax_r_keep)-1 do begin
  printf,12, kic_miss_keep(ii), numax_r_keep(ii), output_clump(ii) ,numax_guess_keep(ii), good_numax_b_keep(ii), numax_b_keep(ii), $
    good_numax_y_keep(ii), numax_y_keep(ii), FORMAT='(a10, 3x, 3(D12.2,3x), a10, 3x, D12.2, 3x, a10, 3x, D12.2)'

endfor
close, 12

kic_miss_r_ok=[]
output_clump_ok=[]
numax_r_keep_ok=[]
numax_guess_keep_ok=[]
good_numax_y_keep_ok=[]
good_numax_b_keep_ok=[]
numax_b_keep_ok=[]
numax_y_keep_ok=[]
for ii=0, n_elements(kic_miss_keep)-1 do begin
  if (good_numax_b_keep(ii) eq 'ok_b=') or (good_numax_y_keep(ii) eq 'ok_y=') then begin
    kic_miss_r_ok=[kic_miss_r_ok,kic_miss_keep(ii)]
    output_clump_ok=[output_clump_ok,output_clump(ii)]
    numax_r_keep_ok=[numax_r_keep_ok,numax_r_keep(ii)]
    numax_guess_keep_ok=[numax_guess_keep_ok,numax_guess_keep(ii)]
    good_numax_b_keep_ok=[good_numax_b_keep_ok,good_numax_b_keep(ii)]
    good_numax_y_keep_ok=[good_numax_y_keep_ok,good_numax_y_keep(ii)]
    numax_b_keep_ok=[numax_b_keep_ok, numax_b_keep(ii)]
    numax_y_keep_ok=[numax_y_keep_ok, numax_y_keep(ii)]
  endif
endfor

close,13
openw,13,'/Users/lbugnet/DATA/METRIC/K2/C4/thruster_r_ok_K2_C4.txt', WIDTH=80
close,13
OpenW, 13, '/Users/lbugnet/DATA/METRIC/K2/C4/thruster_r_ok_K2_C4.txt',  /append
printf, 13, 'EPIC_thrusters_stars_ok_for_b_or_y, numax_r, power_metrique, numax_guess_metrique, flag_b, numax_b, flag_y, numax_y'
for ii=0, n_elements(kic_miss_r_ok)-1 do begin
  printf,13, kic_miss_r_ok(ii), numax_r_keep_ok(ii), output_clump_ok(ii) ,numax_guess_keep_ok(ii), good_numax_b_keep_ok(ii), numax_b_keep_ok(ii), $
    good_numax_y_keep_ok(ii), numax_y_keep_ok(ii), FORMAT='(a10, 3x, 3(D12.2,3x), a10, 3x, D12.2, 3x, a10, 3x, D12.2)'

endfor
close, 13

;
;path_e='/Users/lbugnet/DATA/METRIC/K2/C4/multiplot_metric_clump_K2_C4_stars_panel_'
;multi_plot_routine_K2_C4, numax_guess_keep,  res, threshold, numax_guess_K2, output_K2[*,1], files_arr, path_e, kic_miss_keep
;
;readcol, '/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/hlsp_everest_k2_llc_210944700-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale', time, amp
;readcol, '/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/hlsp_everest_k2_llc_210953329-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale', time1, amp1
;readcol, '/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/hlsp_everest_k2_llc_210949096-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale', time2, amp2
END