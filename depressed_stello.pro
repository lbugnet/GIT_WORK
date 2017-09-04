PRO DEPRESSED_STELLO

readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1.txt', KIC,Numax ,Dnu  , Mass , LowV,  stringSKIP='#',format='A,D,D,D,D', /silent

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
kic_s=output_a2z[*,0]
wb=where(LowV eq 0)
wc=where(LowV eq 1)
KIC_black=KIC(wb)
KIC_color=KIC(wc)


restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_LC0.700000_.sav', /verbose;, res, slope_fit, residuals, threshold, xx, yy ; a 20J
pp1=plot(10^xx, 10^yy,xr=[45,250], dim=[800,400], xtitle='$\nu_{max} (\mu Hz)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$', xlog=1, font_size=13, font_name='Times', ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='black', transparency=100)


match, long(KIC_black), long(kic_s), i1,i2, count=nn

pp=plot(numax(wb(i1)), output_a2z[i2,1], xlog=1, ylog=1, linestyle='none', symbol='o',transparency=80, sym_filled=1, color='orange', name='NON DEPRESSED', /overplot)
rr=sort(xx)
poly=polygon([10^xx(rr),10^reverse(xx(rr))], [10^(res(1)*alog10(10^xx(rr))+res(0)-threshold),reverse(10^(res(1)*alog10(10^xx(rr))+res(0)+threshold))], target=pp, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=70, TRANSPARENCY=0, name='UNKNOWN')

match, long(KIC_color), long(kic_s), i1,i2, count=nn

ppp=plot(numax(wc(i1)), output_a2z[i2,1], xlog=1, ylog=1, linestyle='none', symbol='o',transparency=80, sym_filled=1, color='medium aquamarine', /overplot, name='DEPRESSED')
ll=LEGEND(TARGET=[pp, ppp], POSITION=[109, 90000], $
  /DATA, /AUTO_TEXT_COLOR)
  
  ppp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1_without_correction.png'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


N_stars_tot=n_elements(kic)
;tab_path_psd_list=strarr(n_elements(kic))
;for ii=0, n_elements(kic)-2 do begin
;  print, ii
;  tab_path_psd_list(ii)=file_search('/Volumes/TEMP/RG_DR25/K???/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/','*'+strtrim(kic(ii),1)+'*PSD*')
;endfor

;save, file='/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1_paths.sav', tab_path_psd_list
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1.txt', KIC,Numax ,Dnu  , Mass , LowV,  stringSKIP='#',format='A,D,D,D,D', /silent

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1_paths.sav', /verbose

output_a2z=DBLARR(N_STARS_TOT,11)
for ns=0, n_elements(kic)-2 do begin
  STAR_PATH_PSD=tab_PATH_PSD_LIST(ns)
  ID_STAR=KIC(ns)
  FREQ_INIC_GR=numax(ns)-3*dnu(ns)
  FREQ_FIN_GR=numax(ns)+3*dnu(ns)
  FREQ_INIC_GR_NS=273
  FREQ_FIN_GR_NS=285
  COMPUTE_PSD_DATA, STAR_PATH_PSD=STAR_PATH_PSD,  TYPE=TYPE, CHAMP=CHAMP, STAR_TAB_PSD=STAR_TAB_PSD, N_STARS_TOT=N_STARS_TOT, ID_STAR=ID_STAR
  powvar, STAR_TAB_PSD=STAR_TAB_PSD, OUTPUT_A2Z_1=OUTPUT_A2Z_1, NS=NS, FREQ_INIC_GR, FREQ_FIN_GR, FREQ_INIC_GR_NS, FREQ_FIN_GR_NS, ID_STAR=ID_STAR, STAR_PATH_PSD=STAR_PATH_PSD
  OUTPUT_A2Z[ns,*]=(OUTPUT_A2Z_1)
endfor

kic_s=output_a2z[*,0]
wb=where(LowV eq 0)
wc=where(LowV eq 1)
KIC_black=KIC(wb)
KIC_color=KIC(wc)

match, long(KIC_black), long(kic_s), i1,i2, count=nn
pp=plot(numax(wb(i1)), output_a2z[i2,1], dim=[800,400], xr=[45,250], yr=[30,4000], xtitle='$\nu_{max} (\mu Hz)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$', xlog=1, font_size=13, sym_size=0.75,  font_name='Times', ylog=1, linestyle='none', symbol='o',transparency=50, sym_filled=1, color='dark orange', name='NON DEPRESSED')
res_b = poly_fit(alog10(numax(wb(i1))),alog10(output_a2z[i2,1]),1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
slope_fit_b=res_b(0) + res_b(1)*alog10(numax(wb(i1)))
p2=plot(numax(wb(i1)), 10^slope_fit_b, /overplot, color='dark red', thick=2, transparency=99)


match, long(KIC_color), long(kic_s), i1,i2, count=nn
ppp=plot(numax(wc(i1)), output_a2z[i2,1], xlog=1, ylog=1, linestyle='none', symbol='o', sym_filled=1,sym_size=0.75, transparency=50,color='medium aquamarine', /overplot, name='DEPRESSED')
res_c = poly_fit(alog10(numax(wc(i1))),alog10(output_a2z[i2,1]),1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
slope_fit_c=res_c(0) + res_c(1)*alog10(numax(wc(i1)))
p2=plot(numax(wc(i1)), 10^slope_fit_c, /overplot, color='dark blue', thick=2, transparency=99)
rr=sort(xx)
;pp1=plot(10^xx, 10^slope_fit,xr=[45,250],xtitle='$\nu_{max}$', ytitle='POWVAR', xlog=1, ylog=1, color='black', transparency=10, /overplot)

ll=LEGEND(TARGET=[pp, ppp], POSITION=[130, 3000], font_name='Times',$
  /DATA, /AUTO_TEXT_COLOR)

  ppp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/DEPRESSED/Stello_normal_or_lowl1_with_metric_correction.png'

stop




END