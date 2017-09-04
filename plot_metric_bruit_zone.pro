PRO plot_metric_bruit_zone, output_resize, flag, output_a2zp=output_a2zp
;---------------------- PLOT THE GRAPH METRIC VS NOISE --------------------------------

;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose avant de lancer le programme
restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
match, long(kic_s), long(output_a2zp[*,0]), i1,i2, count=nn
output_a2zp=output_a2zp[i2,*]

x=indgen(1e6)
p=plot(x,x, xlog=1, ylog=1, font_size=13,font_name='Times', dim=[500,400], color= 'dark blue')
p.thick=3
  stars_type_1=1
  stars_type_2=2
  stars_type_3=3
  p=plot(output_a2zp[*,7], output_a2zp[*,3], yr=[10,1e6], /overplot, xr=[0.1,1e5],sym_size=1.5,xlog=1, ylog=1,  symbol="D", transparency=90, SYM_FILLED=1, name="NO GOLD SC STARS",linestyle="none", xtitle='$(BRUIT DE PHOTONS)_{moy}$',ytitle='PSD$_{moy}$')     ; metric en fonction de moyenne du bckgrnd
  ;p.SYM_COLOR = "black"

  ;file_out='/Users/lbugnet/DATA/METRIC/KEPLER/metric_vs_noise_savita_stars_16000.png'
  file_out='/Users/lbugnet/DATA/METRIC/KEPLER/metric_vs_noise_nona2z_stars.png'
  ;file_out='/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/metric_vs_noise_NO_GOLDSC_STARS.png'
  
  test=strpos(file_out, 'nona2z')
  if test eq -1 then begin
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low

  match, long(output_a2zp[*,0]), long(KIC_high), ind1, ind2

  xx=alog10(output_a2zp[ind1,7])
  yy=alog10(output_a2zp[ind1,3])
  pp=plot(10^xx, 10^yy,  symbol="D", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=60, sym_size=1.5,  font_size=12);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  
  match, long(output_a2zp[*,0]), long(KIC_low), ind11, ind22

  xx=alog10(output_a2zp[ind11,7])
  yy=alog10(output_a2zp[ind11,3])

  pp=plot(10^xx, 10^yy,  symbol="D", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=60, sym_size=1.5,  font_size=13);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")


  ;w1=where(flag eq stars_type_1 )
  ;p1=plot([output_resize[w1,7],output_resize[w1,7]], [output_resize[w1,3],output_resize[w1,3]],   name="LOW STARS" ,symbol="D", SYM_FILLED=1, color="dark magenta",linestyle="none" ,/overplot)
  ;w2=where(flag eq stars_type_2 )
  ;p2=plot([output_resize[w2,7],output_resize[w2,7]], [output_resize[w2,3],output_resize[w2,3]],   name="HIGH STARS" ,symbol="D", SYM_FILLED=1, color="lime green",linestyle="none",/overplot)
;  w3=where(flag eq stars_type_3 )
;  p3=plot( [output_resize[w3,7],output_resize[w3,7]], [output_resize[w3,3],output_resize[w3,3]],   symbol="D",   name="RIGHT STARS" ,SYM_FILLED=1,color="orange", linestyle="none",/overplot)


  ;ll=legend(target=[p], position=[1.e5,10] , /DATA, /AUTO_TEXT_COLOR)
  
  endif else begin
    
    restore, '/Users/lbugnet/DATA/METRIC/numax_predicted_non_A2Z_stars.sav';, numax_predicted_out, xerror, flag_SN, flag_SM
    w1=where(flag_SN eq 1)
    w2=where(flag_SM eq 1)
    p1=plot([out[*,7],out[*,7]], [out[*,3],out[*,3]],symbol="D", SYM_FILLED=1, color="orange",linestyle="none", sym_size=1.5, /overplot, transparency=95)
    p1=plot([out[w1,7],out[w1,7]], [out[w1,3],out[w1,3]],   name="HIGH MASS STARS" ,symbol="D", SYM_FILLED=1, sym_size=1.5,color="indigo",linestyle="none" ,/overplot, transparency=90)
    p1=plot([out[w2,7],out[w2,7]], [out[w2,3],out[w2,3]],   name="SUPER NYQUIST" ,symbol="D", SYM_FILLED=1,sym_size=1.5, color="medium spring green",linestyle="none" ,/overplot, transparency=80)
stop
  endelse
  
  ;p.save, file_out










END