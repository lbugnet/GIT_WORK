PRO plot_powvar_teff, TYPE=TYPE, CHAMP=CHAMP, GOLD=GOLD, cadence=cadence, fill=fill, SOLAR_LIKE=SOLAR_LIKE, freq_inic_gr=freq_inic_gr
;plot_powvar_teff, TYPE='K2'
;
;------------- POWVAR/Teff------------------------------
if TYPE eq 'KEPLER' then begin
  if n_elements(freq_inic_gr) eq 0 then begin
    restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
  endif else begin
  restore, '/Users/lbugnet/DATA/METRIC/'+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.sav'
  endelse
restore, '/Users/lbugnet/TABLE/Q1_17_closeout_starproperties_final.idl', /verbose

;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)

restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
match, long(kic_s), long(output_a2z[*,0]), i1,i2, count=nn
output_a2z=output_a2z[i2,*]


;kic=kic(where(strpos(prov(2,*), 'SPE') eq 0))
match, long(kic), long(output_a2z[*,0]), i1,i2, count=nn

xx=alog10(Teff(i1))
yy=alog10(output_a2z[i2,1])
yy_re=alog10(output_a2z[i2,1])
xx_re=alog10(Teff(i1))

pp=plot(10^xx, 10^yy, xr=[3000,6000], yr=[10,1e6], ylog=1, dim=[800,450], font_size=13, font_name='Times', xlog=0, xtitle='$T_{eff} (K)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="D", SYM_FILLED=1, transparency=99, sym_size=1.5, color="black",linestyle="none");'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low

match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2

xx=alog10(Teff(i1(ind1)))
yy=alog10(output_a2z[i2(ind1),1])

pp=plot(10^xx, 10^yy,  ylog=1, symbol="D", transparency=60, sym_size=1.5,name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")


match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22

xx=alog10(Teff(i1(ind11)))
yy=alog10(output_a2z[i2(ind11),1])

pp=plot(10^xx, 10^yy,  ylog=1, symbol="D", transparency=80, sym_size=1.5,name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;pp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/powvar_teff.png'


;------------- log(g)/Teff------------------------------
restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
restore, '/Users/lbugnet/TABLE/Q1_17_closeout_starproperties_final.idl', /verbose

;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)

restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
match, long(kic_s), long(output_a2z[*,0]), i1,i2, count=nn
output_a2z=output_a2z[i2,*]


match, long(kic), long(output_a2z[*,0]), i1,i2, count=nn

xx=alog10(Teff(i1))
yy=alog10(logg(i1))

pp=plot(10^xx, 10^yy, xr=[3000,6000],  xlog=0, yr=[4,0], font_size=13, dim=[800,450],font_name='Times',xtitle='$T_{eff} (K)$', ytitle='$log_{10}(g) (m/s)$',symbol="D", SYM_FILLED=1, color="black",transparency=99, sym_size=1.5,linestyle="none");'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low

match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2

xx=alog10(Teff(i1(ind1)))
yy=alog10(logg(i1(ind1)))

pp=plot(10^xx, 10^yy,  symbol="D", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot,transparency=60, sym_size=1.5);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")


match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22

xx=alog10(Teff(i1(ind11)))
yy=alog10(logg(i1(ind11)))

pp=plot(10^xx, 10^yy, symbol="D", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=80, sym_size=1.5);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;pp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/logg_teff.png'

;------------- log(g)/POWVAR------------------------------
restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
restore, '/Users/lbugnet/TABLE/Q1_17_closeout_starproperties_final.idl', /verbose
restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_LC0.700000_.sav', /verbose
;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)

restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
match, long(kic_s), long(output_a2z[*,0]), i1,i2, count=nn
output_a2z=output_a2z[i2,*]


match, long(kic), long(output_a2z[*,0]), i1,i2, count=nn

xx=alog10(logg(i1))
yy=alog10(output_a2z[i2,1])
flag=strarr(n_elements(xx))
for ii=0, n_elements(xx)-1 do begin
  if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
  if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
endfor
wmid=where(flag eq '')
pp=plot(10^xx(wmid), 10^yy(wmid),   ylog=1, xr=[0,4],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low

match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22

xx=alog10(logg(i1(ind11)))
yy=alog10(output_a2z[i2(ind11),1])

pp=plot(10^xx, 10^yy,  symbol="o", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")


match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2

xx=alog10(logg(i1(ind1)))
yy=alog10(output_a2z[i2(ind1),1])

pp=plot(10^xx, 10^yy,  symbol="o", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")



match, long(2856769), long(output_a2z[*,0]), ii1, ii2
match, long(kic), long(output_a2z[ii2,0]), iii1,iii2
yy=alog10(output_a2z[ii2,1])
p=plot(logg(iii1), 10^yy, yr=[10,1e6],linestyle='none', transparency=0, symbol='star', sym_filled=1, color='lime green', sym_size=2.75, /overplot)
p=plot([logg(iii1),logg(iii1)], [10^yy, 10^yy],  xr=[0,4],yr=[10,1e6],linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=2.75, /overplot)
t1 = TEXT(0.65, 200000,  'KIC 2856769' , /DATA, FONT_SIZE=12, font_name='Times')

match, long(2011582), long(output_a2z[*,0]), ii1, ii2
match, long(kic), long(output_a2z[ii2,0]), iii1,iii2
yy=alog10(output_a2z[ii2,1])
p=plot(logg(iii1), 10^yy, yr=[10,1e6],linestyle='none', transparency=0, symbol='star', sym_filled=1, color='orange', sym_size=3.25, /overplot)
p=plot([logg(iii1),logg(iii1)], [10^yy, 10^yy],  xr=[0,4],yr=[10,1e6],linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=3.25, /overplot)
t1 = TEXT(2.2, 5000,  'KIC 2011582' , /DATA, FONT_SIZE=12, font_name='Times')

match, long(4482016), long(output_a2z[*,0]), ii1, ii2
match, long(kic), long(output_a2z[ii2,0]), iii1,iii2
yy=alog10(output_a2z[ii2,1])
p=plot([logg(iii1),logg(iii1)], [10^yy, 10^yy], yr=[10,1e6],  linestyle='none', transparency=0, symbol='star', sym_filled=1, color='dark magenta', sym_size=2.75, /overplot)
p=plot([logg(iii1),logg(iii1)], [10^yy, 10^yy],  linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=2.75, /overplot)
t1 = TEXT(3.0, 1000,  'KIC 4482016' , /DATA, FONT_SIZE=12, font_name='Times')

pp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/logg_powvar.png'
stop
endif

;----------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------
;-----------------------------------------------------K2---------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------------------------------------------------------------------------------

if TYPE eq 'K2' then begin

  ;------------- log(g)/POWVAR------------------------------
  
  if n_elements(freq_inic_gr) eq 0 then begin
    file_name='/Users/lbugnet/DATA/METRIC/K2/3__0.700000_output_numax_all.sav'
    file_name='/Users/lbugnet/DATA/METRIC/K2/6_BEN__0.700000_output_numax_all.sav'
    restore, '/Users/lbugnet/DATA/METRIC/K2/3__0.700000_output_numax_all.sav', /verbose
    restore, '/Users/lbugnet/DATA/METRIC/K2/6_BEN__0.700000_output_numax_all.sav', /verbose
  endif else begin
    file_name='/Users/lbugnet/DATA/METRIC/'+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.sav'
  restore, '/Users/lbugnet/DATA/METRIC/'+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.sav'
  endelse

if n_elements(CHAMP) eq 0 then begin
  if STREGEX(file_name, '3__') ne -1 then champ='3'
    if STREGEX(file_name, '4__') ne -1 then champ='4'
      if STREGEX(file_name, '6_BEN__') ne -1 then champ='6_BEN'
      if STREGEX(file_name, '6__') ne -1 then champ='6'
        if STREGEX(file_name, '7__') ne -1 then champ='7'
        endif
  ;restore, '/Users/lbugnet/DATA/METRIC/K2/__0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/TABLES/C'+CHAMP+'_KP.txt',  EPIC, tt, ttu, ttl, gg, ggu, ggl, fe, feu, fel, rr, rru,rrl, mm, mmu, mml, ro, rou, rol, dd, ddu, ddl, kpp,stringskip='#', /silent, format='D,D,D,D, D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D,D'
  
  ;save, file='/Users/lbugnet/DATA/TABLES/C'+CHAMP+'_KP.txt', EPIC, tt, ttu, ttl, gg, ggu, ggl, fe, feu, fel, rr, rru,rrl, mm, mmu, mml, ro, rou, rol, dd, ddu, ddl, kpp
  restore, '/Users/lbugnet/DATA/TABLES/C3_KP.txt', /verbose
  ;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_LC0.700000_.sav', /verbose
  ;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)

  ;restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
  ;match, long(kic_s), long(output_a2z[*,0]), i1,i2, count=nn
  ;output_a2z=output_a2z[i2,*]


  match, long(EPIC), long(output_a2z[*,0]), i1,i2, count=nn

  xx=alog10(gg(i1))
  yy=alog10(output_a2z[i2,1])
;  flag=strarr(n_elements(xx))
;  for ii=0, n_elements(xx)-1 do begin
;    if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
;    if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
;  endfor
;  wmid=where(flag eq '')
  ;pp=plot(10^xx(wmid), 10^yy(wmid),   ylog=1, xr=[0,4],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp=plot(10^xx, 10^yy,   ylog=1, xr=[0,6],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low
;
;  match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22
;
;  xx=alog10(logg(i1(ind11)))
;  yy=alog10(output_a2z[i2(ind11),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;
;
;  match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2
;
;  xx=alog10(logg(i1(ind1)))
;  yy=alog10(output_a2z[i2(ind1),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

pp.save, '/Users/lbugnet/DATA/METRIC/K2/C'+CHAMP+'/'+'logg_powvar'+CHAMP+'.png'

;----------------TEFF POWVAR ----------------------------------------------------------------
xx=alog10(tt(i1))
yy=alog10(output_a2z[i2,1])
;  flag=strarr(n_elements(xx))
;  for ii=0, n_elements(xx)-1 do begin
;    if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
;    if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
;  endfor
;  wmid=where(flag eq '')
;pp=plot(10^xx(wmid), 10^yy(wmid),   ylog=1, xr=[0,4],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
pp=plot(10^xx, 10^yy,   ylog=1, yr=[10,1e6], xr=[6000,3000],dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='teff$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low
;
;  match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22
;
;  xx=alog10(logg(i1(ind11)))
;  yy=alog10(output_a2z[i2(ind11),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;
;
;  match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2
;
;  xx=alog10(logg(i1(ind1)))
;  yy=alog10(output_a2z[i2(ind1),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

pp.save, '/Users/lbugnet/DATA/METRIC/K2/C'+CHAMP+'/'+'teff_powvar'+CHAMP+'.png'


;----------------TEFF POWVAR ----------------------------------------------------------------
xx=alog10(tt(i1))
yy=alog10(gg(i1))
;  flag=strarr(n_elements(xx))
;  for ii=0, n_elements(xx)-1 do begin
;    if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
;    if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
;  endfor
;  wmid=where(flag eq '')
;pp=plot(10^xx(wmid), 10^yy(wmid),   ylog=1, xr=[0,4],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
pp=plot(10^xx, 10^yy,  dim=[700,500], axis_style=1, xr=[6000,3000],font_name='Times', font_size=13, xtitle='teff$', ytitle='$log(g)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low
;
;  match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22
;
;  xx=alog10(logg(i1(ind11)))
;  yy=alog10(output_a2z[i2(ind11),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;
;
;  match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2
;
;  xx=alog10(logg(i1(ind1)))
;  yy=alog10(output_a2z[i2(ind1),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

pp.save, '/Users/lbugnet/DATA/METRIC/K2/C'+CHAMP+'/'+'teff_logg'+CHAMP+'.png'



;----------------TEFF POWVAR ----------------------------------------------------------------
xx=alog10(output_a2z[i2,7])
yy=alog10(output_a2z[i2,1]+output_a2z[i2,7])
;  flag=strarr(n_elements(xx))
;  for ii=0, n_elements(xx)-1 do begin
;    if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
;    if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
;  endfor
;  wmid=where(flag eq '')
;pp=plot(10^xx(wmid), 10^yy(wmid),   ylog=1, xr=[0,4],yr=[10,1e6], dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='$log_{10}(g)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
pp=plot(10^xx, 10^yy,  ylog=1, xlog=1,dim=[700,500], axis_style=1, font_name='Times', font_size=13, xtitle='bruit$', ytitle='SIGNAL',symbol="o", SYM_FILLED=1, color="black",linestyle="none", transparency=80, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jhigh_stars.txt', KIC_high
;  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/detect_outliers_powvar_KEPLER_LC0.700000__20Jlow_stars.txt', KIC_low
;
;  match, long(output_a2z[i2,0]), long(KIC_low), ind11, ind22
;
;  xx=alog10(logg(i1(ind11)))
;  yy=alog10(output_a2z[i2(ind11),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='LOW POWER STARS', SYM_FILLED=1, color="dark magenta",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;
;
;  match, long(output_a2z[i2,0]), long(KIC_high), ind1, ind2
;
;  xx=alog10(logg(i1(ind1)))
;  yy=alog10(output_a2z[i2(ind1),1])
;
;  pp=plot(10^xx, 10^yy,  symbol="o", name='HIGH POWER STARS',SYM_FILLED=1, color="lime green",linestyle="none", /overplot, transparency=50, sym_size=0.75);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")

pp.save, '/Users/lbugnet/DATA/METRIC/K2/C'+CHAMP+'/'+'signal_bruit'+CHAMP+'.png'

ENDIF


END