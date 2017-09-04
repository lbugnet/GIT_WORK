PRO compare_SC_LC

GOLD=''
GOLD='NO_GOLD'

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/SC_ALLQ_7_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/SC_ALLQ_50_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok

if GOLD eq 'NO_GOLD' then begin ;;;;;;; SHORT CADENCE ;;;;;;;;;;;;;;
  ;----------- Kepler
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_7_.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
;    restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_50_.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
  pp=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", yr=[1, 10000], xr=[200, 10000], dim=[1200,800], SYM_FILLED=1, linestyle="none", font_name='Times', font_size=20, transparency=98)
  
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__7_output_numax_all.sav', /verbose

  
  ;----------- SOLARLIKE confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/Table1_Chaplin.txt', kic_chaplin, numax_chaplin, format='A,D', skipline=29  ; donne kic et numax des etoiles solar_like confirmées
  match, long(output_a2z[*,0]), long(kic_chaplin), ind1, ind2
  numax_chaplin=numax_chaplin(ind2)
  output_chaplin=output_a2z[ind1,1]
    kic_ch=kic_chaplin(ind2)
  
  ;----------- RGB confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/List_361_known_RGs_in_wg1_Survey.txt', kic_SL_RGB, format='A' ; donne le kic des RGB classée dans les non gold par erreur
  restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose ;kic_s, dnu, ednu, fmin, fmax, numax,(numax a2z)
  match, long(kic_s), long(KIC_SL_RGB), ii1, ii2
  numax_SL_RGB=numax(ii1)
  KIC_SL_RGB=KIC_SL_RGB(ii2)
 
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__7_output_numax_all.sav', /verbose ; output_a2z
  match, long(output_a2z[*,0]), long(KIC_SL_RGB), i1, i2
  output_SL_RGB=output_a2z[i1,1]

  pp1=plot((numax_SL_RGB), (output_SL_RGB), linestyle='none', xlog=1, ylog=1, symbol='D', sym_filled=1, color='red', /overplot, transparency=60, name='RGB NO GOLD')
  pp27=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='blue', /overplot, transparency=60, name='SOLAR_LIKE NO GOLD 7')
  
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__30_output_numax_all.sav', /verbose


  ;----------- SOLARLIKE confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/Table1_Chaplin.txt', kic_chaplin, numax_chaplin, format='A,D', skipline=29  ; donne kic et numax des etoiles solar_like confirmées
  match, long(output_a2z[*,0]), long(kic_chaplin), ind1, ind2
  numax_chaplin=numax_chaplin(ind2)
  output_chaplin=output_a2z[ind1,1]
  kic_ch=kic_chaplin(ind2)

  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__30_output_numax_all.sav', /verbose ; output_a2z
  match, long(output_a2z[*,0]), long(KIC_SL_RGB), i1, i2
  output_SL_RGB=output_a2z[i1,1]

  pp1=plot((numax_SL_RGB), (output_SL_RGB), linestyle='none', xlog=1, ylog=1, symbol='D', sym_filled=0, color='red', /overplot, transparency=60, name='RGB NO GOLD')
  pp230=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='gray', /overplot, transparency=60, name='SOLAR_LIKE NO GOLD 30')
  pp2=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=0, color='black', /overplot, transparency=60, name='SOLAR_LIKE NO GOLD 30')


  
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__50_output_numax_all.sav', /verbose
  ;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__30_output_numax_all.sav', /verbose
  
  ;----------- SOLARLIKE confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/Table1_Chaplin.txt', kic_chaplin, numax_chaplin, format='A,D', skipline=29  ; donne kic et numax des etoiles solar_like confirmées
  match, long(output_a2z[*,0]), long(kic_chaplin), ind1, ind2
  numax_chaplin=numax_chaplin(ind2)
  output_chaplin=output_a2z[ind1,1]
  kic_ch=kic_chaplin(ind2)

    restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__50_output_numax_all.sav', /verbose ; output_a2z
    match, long(output_a2z[*,0]), long(KIC_SL_RGB), i1, i2
    output_SL_RGB=output_a2z[i1,1]

    pp1=plot((numax_SL_RGB), (output_SL_RGB), linestyle='none', xlog=1, ylog=1, symbol='D', sym_filled=0, color='red', /overplot, transparency=60, name='RGB NO GOLD')
    pp250=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='black', /overplot, transparency=0, name='SOLAR_LIKE NO GOLD 50')
    pp2=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=0, color='black', /overplot, transparency=0, name='SOLAR_LIKE NO GOLD 50')

    ll=LEGEND(TARGET=[pp27, pp230, pp250], POSITION=[7000, 5000], $
      /DATA, /AUTO_TEXT_COLOR,font_name='Times')
    ;t1 = TEXT(40, 5000,  'Short cadence' , /DATA, FONT_SIZE=15, font_name='Times')


    pp2.save,  '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/SC_NO_GOLD_RGB+SOLAR_LIKE_7_30_50_NO_GOLD_muHz.png'

    
;        restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDSC__30_output_numax_all.sav', /verbose ; output_a2z
;  match, long(output_a2z[*,0]), long(KIC_SL_RGB), i1, i2
;  output_SL_RGB=output_a2z[i1,1]
;  
;  pp1=plot((numax_SL_RGB), (output_SL_RGB), linestyle='none', xlog=1, ylog=1, symbol='D', sym_filled=0, color='red', /overplot, transparency=50, name='RGB NO GOLD')
;  pp2=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=0, color='black', /overplot, transparency=50, name='SOLAR_LIKE NO GOLD')
 
;;-------- ADD GOLD STARS (ORANGE) --------------------

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/GOLDSC_Q5_7_output_numax_all.sav', /verbose
p1=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none",  name='GOLD STARS 7', yr=[1, 10000], xr=[200, 10000], dim=[1200,800], color='aquamarine',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)',  transparency=40)

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/GOLDSC_Q5_30_output_numax_all.sav', /verbose
p2=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none",  name='GOLD STARS 30', color='green',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', /overplot, transparency=40)
p=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=0, linestyle="none",  name='GOLD STARS 30', color='grey',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', /overplot, transparency=40)

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/GOLDSC_Q5_50_output_numax_all.sav', /verbose
p3=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none",  name='GOLD STARS 50', color='cyan',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', /overplot, transparency=0)
p=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=0, linestyle="none",  name='GOLD STARS 50', color='black',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', /overplot, transparency=0)

ll=LEGEND(TARGET=[p1, p2,p3], POSITION=[7000, 5000], $
  /DATA, /AUTO_TEXT_COLOR,font_name='Times')
;t1 = TEXT(40, 5000,  'Short cadence' , /DATA, FONT_SIZE=15, font_name='Times')


 p1.save,  '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/SC_NO_GOLD_RGB+SOLAR_LIKE_7_30_50_GOLD_muHz.png'

;-------------------------- GET OUTLIERS ---------------------------------------------------------------

wnu=where((numax_chaplin gt 500) and (output_chaplin gt 50))
wnu2=where(((10^xx) gt 500) and ((10^yy) gt 30))
kic_ch_out=kic_ch(wnu)
output=output_a2z[where(output_a2z[*,1] lt 200), 0]
kic_ch_out2=long(output(wnu2))
;pp2=plot((numax_chaplin(wnu)), (output_chaplin(wnu)), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='black', sym_size=1.2,/overplot, transparency=50, name='SOLAR_LIKE NO GOLD')
;pp2=plot(10^(xx(wnu2)), 10^(yy(wnu2)), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='black', sym_size=1.2,/overplot, transparency=50, name='SOLAR_LIKE NO GOLD')

wnu3=where((numax_SL_RGB lt 200) and (numax_SL_RGB gt 5) and (output_SL_RGB lt 105))
kic_ch_out3=kic_ch(wnu3)
;pp2=plot(numax_SL_RGB(wnu3), output_SL_RGB(wnu3), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='black', sym_size=1.2,/overplot, transparency=50, name='SOLAR_LIKE NO GOLD')

stop



endif

if GOLD eq 'NO_GOLD' then begin ;;;;;;; LONG CADENCE ;;;;;;;;;;;;;;
  ;----------- Kepler
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_7_.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
  pp=plot(10^xx, 10^yy, xlog=1, ylog=1, xr=[10, 10000], yr=[1, 10000], dim=[1200,800],symbol="D", SYM_FILLED=1, linestyle="none", font_name='Times', font_size=20, transparency=98)

  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDLC__7_output_numax_all.sav', /verbose

  ;----------- SOLARLIKE confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/Table1_Chaplin.txt', kic_chaplin, numax_chaplin, format='A,D', skipline=29  ; donne kic et numax des etoiles solar_like confirmées
  match, long(output_a2z[*,0]), long(kic_chaplin), ind1, ind2
  numax_chaplin=numax_chaplin(ind2)
  output_chaplin=output_a2z[ind1,1]



  ;----------- RGB confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/List_361_known_RGs_in_wg1_Survey.txt', kic_SL_RGB, format='A' ; donne le kic des RGB classée dans les non gold par erreur
  restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose ;kic_s, dnu, ednu, fmin, fmax, numax,(numax a2z)
  match, long(kic_s), long(KIC_SL_RGB), ii1, ii2
  numax_SL_RGB=numax(ii1)
  KIC_SL_RGB=KIC_SL_RGB(ii2)

  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDLC__7_output_numax_all.sav', /verbose ; output_a2z
  
  match, long(output_a2z[*,0]), long(KIC_SL_RGB), i1, i2
  output_SL_RGB=output_a2z[i1,1]

  pp1=plot((numax_SL_RGB), (output_SL_RGB), linestyle='none', xlog=1, ylog=1, symbol='D', font_name='Times', sym_filled=1, color='red',sym_size=1.2, /overplot, transparency=50, name='RGB NO GOLD')
  pp2=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='blue', sym_size=1.2,/overplot, transparency=50, name='SOLAR_LIKE NO GOLD')

   
    
    ;;;-------- ADD GOLD STARS (ORANGE) --------------------

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/LC__7_output_numax_all.sav', /verbose
p=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='dark orange',sym_size=1.2,xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', name='GOLD STARS', /overplot, transparency=50)
;ll=LEGEND(TARGET=[pp1, pp2,p], POSITION=[7000, 5000], $
;  /DATA, /AUTO_TEXT_COLOR,font_name='Times')
;t1 = TEXT(40, 5000,  'Long cadence' , /DATA, FONT_SIZE=15,font_name='Times')
; pp2.save,  '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/LC_NO_GOLD_RGB+SOLAR_LIKE.png'
 
endif






;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_7_.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
;if GOLD eq 'NO_GOLD' then restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/KEPLER_varlaw_20J_ALL_KEPLER_NO_GOLDSC7_.sav', /verbose
;
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
;
;
;  output_a2zp=output_a2z
;  match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
;  output_resize_b=output_a2zp[i2,*]
;  ks=long(output_a2zp[*,0])
;  ks(i2)=-1
;  w=where(ks ne -1)
;  out=output_a2zp[w,*]
;  kic_s=kic_s(i1)
;  xx=alog10(numax(i1))
;  yy=alog10(output_a2zp[i2,1])
;
;p=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='medium spring green',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)')
;p=plot(10^xx, 10^(res_solar(1)*alog10(10^xx)+res_solar(0)), /overplot, color='spring green', name='POWVAR SC SOLARLIKE   ' +GOLD+'  (7 $\mu Hz$)')
;p.thick=2
;rr=sort(xx)
;
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/LC__7_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok
;if GOLD eq 'NO_GOLD' then restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/NO_GOLDLC__7_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
;output_a2zp=output_a2z
;match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
;output_resize_b=output_a2zp[i2,*]
;ks=long(output_a2zp[*,0])
;ks(i2)=-1
;w=where(ks ne -1)
;out=output_a2zp[w,*]
;kic_s=kic_s(i1)
;xx=alog10(numax(i1))
;yy=alog10(output_a2zp[i2,1])
;
;rr=sort(xx)
;pp1=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='dark orange', /overplot)
;pp1=plot(10^xx, 10^(res_solar(1)*alog10(10^xx)+res_solar(0)), /overplot, color='orange', name='POWVAR LC SOLARLIKE   ' +GOLD+'  (7 $\mu Hz$)')
;pp1.thick=2
;
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_7_.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
;;if GOLD eq 'NO_GOLD' then restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/KEPLER_varlaw_20J_ALL_KEPLER_NO_GOLDLC7_.sav', /verbose
;
;ppp2=plot(10^xx, 10^yy, /overplot, symbol='D', linestyle='none', color='black',transparency=99, sym_filled=1, name='POWVAR KEPLER RGB')
;
;pp2=plot(10^xx, 10^(res(1)*alog10(10^xx)+res(0)), /overplot, color='crimson', name='POWVAR KEPLER RGB  ' +GOLD+'  (7 $\mu Hz$)')
;  pp2.thick=2
;  
;rr=sort(xx)
;poly=polygon([10^xx(rr),10^reverse(xx(rr))], [10^(res(1)*alog10(10^xx(rr))+res(0)-threshold),reverse(10^(res(1)*alog10(10^xx(rr))+res(0)+threshold))], target=pp, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=70, TRANSPARENCY=90)
;ll=LEGEND(TARGET=[p,pp1, pp2], POSITION=[7000, 50000], $
;  /DATA, /AUTO_TEXT_COLOR)
;pp2.save, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'compare_SC_LC_slopes_7'+'.png'


;--------------------------
; compare 7 <-> 20
;--------------------------
;
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'SC_ALLQ_7_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
;output_a2zp=output_a2z
;match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
;output_resize_b=output_a2zp[i2,*]
;ks=long(output_a2zp[*,0])
;ks(i2)=-1
;w=where(ks ne -1)
;out=output_a2zp[w,*]
;kic_s=kic_s(i1)
;xx=alog10(numax(i1))
;yy=alog10(output_a2zp[i2,1])
;
;pppp=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='medium spring green',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)')
;pppp=plot(10^xx, 10^(res_solar(1)*alog10(10^xx)+res_solar(0)), /overplot, color='spring green', name='POWVAR SC SOLARLIKE (7 $\mu Hz$)')
;pppp.thick=2
;
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'SC_ALLQ_20_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok
;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
;output_a2zp=output_a2z
;match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
;output_resize_b=output_a2zp[i2,*]
;ks=long(output_a2zp[*,0])
;ks(i2)=-1
;w=where(ks ne -1)
;out=output_a2zp[w,*]
;kic_s=kic_s(i1)
;xx=alog10(numax(i1))
;yy=alog10(output_a2zp[i2,1])
;
;ppp=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='steel blue',xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', /overplot)
;ppp=plot(10^xx, 10^(res_solar(1)*alog10(10^xx)+res_solar(0)), /overplot, color='steel blue', name='POWVAR SC SOLARLIKE (20 $\mu Hz$)')
;ppp.thick=2
;
;lll=LEGEND(TARGET=[pppp,ppp], POSITION=[8000, 50], /DATA, /AUTO_TEXT_COLOR)
;ppp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/'+GOLD+'compare_SC_slopes_7_20'+'.png'







END