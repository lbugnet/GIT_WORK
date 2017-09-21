pro plot_paper_powvar, all=all

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_LC0.700000_.sav', /verbose
restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose
;---------------------------------------------------------------------------------
;ALL STARS METRIC
;---------------------------------------------------------------------------------
match, long(KIC_s), long(output_a2z[*,0]), i1,i2

  flag=strarr(n_elements(xx))
  for ii=0, n_elements(xx)-1 do begin
    if (10^yy(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
    if (10^yy(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
  endfor
  wup=where(flag eq 'up')
  p=plot(numax(i1(wup)), output_a2z[i2(wup),1], xlog=1, ylog=1, dim=[700,500], xr=[1, 3e2], yr=[10,1e6], axis_style=1,transparency=50, xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR $(ppm^2/ \mu Hz)$',  color='lime green', font_size=13, linestyle='none', symbol='o', font_name='Times',sym_size=0.75, sym_filled=1)
  wdown=where(flag eq 'down')
  p=plot(numax(i1(wdown)), output_a2z[i2(wdown),1], xlog=1, ylog=1,  transparency=50,  /overplot, color='dark magenta', font_size=13, linestyle='none', symbol='o', sym_size=0.75,sym_filled=1)
  wn=where(xx gt alog10(1))
  rr=sort(xx(wn))
  poly=polygon([10^xx(wn(rr)), reverse(10^xx(wn(rr)))], [10^(slope_fit(wn(rr))+threshold), reverse(10^(slope_fit(wn(rr))-threshold))],target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=70, TRANSPARENCY=99)
  wmid=where(flag eq '')
  p=plot(numax(i1(wmid)), output_a2z[i2(wmid),1], xlog=1, ylog=1, linestyle='none', transparency=80, symbol='o', sym_filled=1, sym_size=0.75, /overplot)
  pp5=plot(10^xx, 10^(slope_fit), xlog=1, ylog=1,color="crimson",linestyle=1 ,/overplot, transparency=99 )
  pp5.thick=3
  match, long(2856769), long(output_a2z[*,0]), ii1, ii2
  yy=alog10(output_a2z[ii2,1])
  p=plot([15.0982,15.0982], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=1, color='lime green', sym_size=2.75, /overplot)
  p=plot([15.0982,15.0982], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=2.75, /overplot)
  t1 = TEXT(10, 150000,  'KIC 2856769' , /DATA, FONT_SIZE=12, font_name='Times')
  
    match, long(4482016), long(output_a2z[*,0]), ii1, ii2
  yy=alog10(output_a2z[ii2,1])
  p=plot([3.9084,3.9084], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=1, color='dark magenta', sym_size=2.75, /overplot)
  p=plot([3.9084,3.9084], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=2.75, /overplot)
  t1 = TEXT(2, 100,  'KIC 4482016' , /DATA, FONT_SIZE=12, font_name='Times')
  
  match, long(2011582), long(output_a2z[*,0]), ii1, ii2
  yy=alog10(output_a2z[ii2,1])
  p=plot([38.2610,38.2610], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=1, color='orange', sym_size=3.25, /overplot)
  p=plot([38.2610,38.2610], [10^yy, 10^yy], xlog=1, ylog=1, linestyle='none', transparency=0, symbol='star', sym_filled=0, sym_thick=2, color='black', sym_size=3.25, /overplot)
  t1 = TEXT(22, 200,  'KIC 2011582' , /DATA, FONT_SIZE=12, font_name='Times')
p.save, '/Users/lbugnet/Documents/powvar.png'
   
   stop
; ZOOM
p=plot(numax(i1(wup)), output_a2z[i2(wup),1], dim=[300,500],xlog=1, ylog=1,  transparency=50,  xr=[10,150], yr=[1e2,1e4], axis_style=1, xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR $(ppm^2/ \mu Hz)$',  color='lime green', font_size=13, linestyle='none', symbol='o', sym_size=0.75, sym_filled=1)
p=plot(numax(i1(wdown)), output_a2z[i2(wdown),1], xlog=1, ylog=1,  transparency=50,  /overplot, color='dark magenta', font_size=13, linestyle='none', symbol='o', sym_size=0.75,sym_filled=1)
poly=polygon([10^xx(wn(rr)), reverse(10^xx(wn(rr)))], [10^(slope_fit(wn(rr))+threshold), reverse(10^(slope_fit(wn(rr))-threshold))],target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=70, TRANSPARENCY=99)
p=plot(numax(i1(wmid)), output_a2z[i2(wmid),1], xlog=1, ylog=1, linestyle='none',transparency=90, symbol='o', sym_filled=1, sym_size=0.75, /overplot)
pp5=plot(10^xx, 10^(slope_fit), xlog=1, ylog=1,color="crimson",linestyle=1 ,/overplot, transparency=99 )
pp5.thick=3

stop
;---------------------------------------------------------------------------------
;MASS
;---------------------------------------------------------------------------------

restore, '/Users/lbugnet/TABLE/Q1_17_closeout_starproperties_final.idl', /verbose
if all eq 'all' then begin
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
endif
if all eq 'APOKASC' then begin
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
endif

feh=feh_in
match, long(kic), long(output_a2z[*,0]), i1,i2
match, long(KIC_s), long(output_a2z[i2,0]), in1,in2
wp=where(numax(in1) gt 0)
wspe=where(strpos(prov(2,wp), 'SPE') eq 0) ; on garde que les étoilse avec FeH déterminé par la spectro
yout=output_a2z[i2(in2(wp(wspe))),1]
kicc=output_a2z[i2(in2(wp(wspe))),0]
mass=mass(i1(in2(wp(wspe))))
feh=feh(i1(in2(wp(wspe))))
slope_fit=slope_fit(i2(in2(wp(wspe))))
  flag=strarr(n_elements(yout))
  for ii=0, n_elements(yout)-1 do begin
    if (yout(ii)) gt (10^(slope_fit(ii)+threshold)) then flag(ii)='up'
    if (yout(ii)) lt (10^(slope_fit(ii)-threshold)) then flag(ii)='down'
  endfor
  wup=where(flag eq 'up')
  wdown=where(flag eq 'down')

; PLOT ENRICO NEW LAW
X=alog(numax(in1(wp(wspe)))/3147.0)
Y=alog(mass)
W=(feh)
;A = [1.18, -1.33, -0.40, 0.38]
Z=alog(yout)
;Z = A[0] + A[1]*X + A[2]*Y + A[3]*W
XYW=[transpose(X),transpose(Y),transpose(W)]



; Create a vector of dependent variable data:
;Z = 1.17  -1.33*X -0.40*Y + 0.38*W
; Assume Gaussian measurement errors for each point:
measure_errors = REPLICATE(0.01, N_ELEMENTS(Y))
; Compute the fit, and print the results:

yfit = REGRESS(XYW, Z, SIGMA=sigma, CONST=const, $
  MEASURE_ERRORS=measure_errors)
;yfit = CURVEFIT(XYW, Z, weights, A,sigma,  FUNCTION_NAME='mymulticurvefit')
order=sort(numax(in1(wp(wspe))))
lnBmM=const
s=yfit[0]
t=yfit[1]
u=yfit[2]
pp3=plot(numax(in1(wp(wspe(order)))), yout(order), xlog=1, ylog=1, color='grey', linestyle='non', symbol='o', sym_filled=1, transparency=50)
A = [1.17, -1.33, -0.40, 0.38]
pp3=plot(numax(in1(wp(wspe(order)))), exp(A[0] + A[1]*X(order) + A[2]*Y(order) + A[3]*W(order)),   xlog=1, ylog=1, color='brown', /overplot,transparency=50)
pp3=plot(numax(in1(wp(wspe(order)))), exp(const + yfit[0]*X(order) + yfit[1]*Y(order) + yfit[2]*W(order)),   xlog=1, ylog=1, color='black', /overplot,transparency=50)
pp3=plot(numax(in1(wp(wspe(order)))), exp(yfit[0]*X(order)),   xlog=1, ylog=1, color='black', /overplot,transparency=50)


;;MASS
;  p1=plot( mass, yout*exp(-u*feh)^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s) ,DIM=[600,400], yr=[0,20] ,xr=[0.5,4],ylog=0, rgb_table=33,font_name='Times',SYM_SIZE=1,vert_colors= 255./(200.-20.)*(numax(in1(wp(wspe)))-20.), SYM_THICK=1,$
;     transparency=90, font_size=15, xtitle='$M/M_{\odot}$', ytitle='$POWVAR e^{-u*[Fe]/[H]} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol='D', SYM_FILLED=1, color='blue')
;
;   XX = mass
;   YY = yout*exp(-u*feh)^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s)
;   wgood=where(YY lt 20)
;   XX=XX(wgood)
;   YY=YY(wgood)
;   measure_errors = REPLICATE(0.01, n_elements(XX))
;   result = POLY_FIT(XX, YY, 1, MEASURE_ERRORS=measure_errors, $
;     SIGMA=sigma)
;
;   p1=plot(XX, XX*result(1)+result(0), thick=2, /overplot)
;   cb = COLORBAR(POSITION=[0.45,0.75,0.85,0.8], RGB_TABLE=33, RANGE=[20,200], font_size=10,TEXT_ORIENTATION=1, TITLE='$\nu_{max}$')
;   p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/mass_effect_all_RG.png'
;  
;   ;METAL VS NUMAX
;   p1=plot( feh, yout*mass^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s) ,DIM=[600,400], yr=[0,20] ,xr=[-2,1],ylog=0, rgb_table=33,font_name='Times',SYM_SIZE=1,vert_colors= 255./(200.-20.)*(numax(in1(wp(wspe)))-20.), SYM_THICK=1,$
;     transparency=90, font_size=15, xtitle='$[Fe/H]$', ytitle='$POWVAR M^{-t} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol='D', SYM_FILLED=1, color='blue')
;
;   ;on fit a partir de FE/H > -1 pour ne pas prendre en compte le peu de points en dessous
;   wh=where(feh gt -1)
;   XX = feh(wh)
;   YY = yout(wh)*mass(wh)^(-t)*(numax(in1(wp(wspe(wh))))/3147.0)^(-s)
;   wgood=where(YY lt 20)
;   XX=XX(wgood)
;   YY=YY(wgood)
;   measure_errors = REPLICATE(0.01, n_elements(XX))
;   result = POLY_FIT(XX, YY, 1, MEASURE_ERRORS=measure_errors, $
;     SIGMA=sigma)
;
;   p1=plot(XX, XX*result(1)+result(0), thick=2, /overplot)
;   cb = COLORBAR(POSITION=[0.2,0.75,0.6,0.8], RGB_TABLE=33, RANGE=[20,200], font_size=10, TEXT_ORIENTATION=1, TITLE='$\nu_{max}$')
;   p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/metal_effect_all_RG.png'
;   
    
   ;MASS VS METAL
   p1=plot( mass, yout*exp(-u*feh)^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s) ,DIM=[600,400], yr=[0,20] ,xr=[0.5,4],ylog=0, rgb_table=33,font_name='Times',SYM_SIZE=1,vert_colors=  255./(0.56+1.98)*(feh+1.98), SYM_THICK=1,$
     transparency=50, font_size=15, xtitle='$M/M_{\odot}$', ytitle='$POWVAR e^{-u*[Fe]/[H]} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol='D', SYM_FILLED=1, color='blue')

   XX = mass
   YY = yout*exp(-u*feh)^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s)
   wgood=where(YY lt 20)
   XX=XX(wgood)
   YY=YY(wgood)
   measure_errors = REPLICATE(0.01, n_elements(XX))
   result = POLY_FIT(XX, YY, 1, MEASURE_ERRORS=measure_errors, $
     SIGMA=sigma)

   p1=plot(XX, XX*result(1)+result(0), thick=2, /overplot)
   cb = COLORBAR(POSITION=[0.45,0.75,0.85,0.8], RGB_TABLE=33, RANGE=[-1.98,0.56], font_size=10,TEXT_ORIENTATION=1, TITLE='$[Fe/H]$')
   ;p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/mass_metal_effect_all_RG.png'
   p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/mass_metal_effect_'+all+'_RG_SPE.png'

;METAL VS MASS  
p1=plot( feh, yout*mass^(-t)*(numax(in1(wp(wspe)))/3147.0)^(-s) ,DIM=[600,400], yr=[0,20] ,xr=[-2,1],ylog=0, rgb_table=33,font_name='Times',SYM_SIZE=1,vert_colors= 255./(3.72-0.33)*(mass-0.33), SYM_THICK=1,$
  transparency=50, font_size=15, xtitle='$[Fe/H]$', ytitle='$POWVAR M^{-t} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol='D', SYM_FILLED=1, color='blue')

    ;on fit a partir de FE/H > -1 pour ne pas prendre en compte le peu de points en dessous
    wh=where(feh gt -1)
    XX = feh(wh)
  YY = yout(wh)*mass(wh)^(-t)*(numax(in1(wp(wspe(wh))))/3147.0)^(-s)
  wgood=where(YY lt 20)
  XX=XX(wgood)
  YY=YY(wgood)
  measure_errors = REPLICATE(0.01, n_elements(XX))
  result = POLY_FIT(XX, YY, 1, MEASURE_ERRORS=measure_errors, $
    SIGMA=sigma)

  p1=plot(XX, XX*result(1)+result(0), thick=2, /overplot)
 cb = COLORBAR(POSITION=[0.2,0.75,0.6,0.8], RGB_TABLE=33, RANGE=[0.33,3.72], font_size=10, TEXT_ORIENTATION=1, TITLE='$Mass$')
;p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/metal_mass_effect_all_RG.png'
p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/metal_mass_effect_'+all+'_RG_SPE.png'

stop
;list aligned stars

wali=where(((feh lt 0.075) and (feh gt 0.065)) or ((feh lt -0.49) and (feh gt 0.51)) or ((feh lt 0.215) and (feh gt 0.205)) or ((feh lt 0.37) and (feh gt 0.35)) or ((feh lt 0.56) and (feh gt 0.57)))


pp3=plot(numax(in1(wp(wspe(order)))), yout(order), xlog=1, ylog=1, color='black', linestyle='none', symbol='o', sym_filled=1, transparency=99)
pp3=plot(numax(in1(wp(wspe(wali)))), yout(wali), color='crimson', linestyle='none', symbol='o', sym_filled=1, transparency=99, /overplot)


nbwaligood=n_elements(where((numax(in1(wp(wspe(wali)))) gt 20) and (numax(in1(wp(wspe(wali)))) lt 60)))
nbnormalgood=n_elements(where((numax(in1(wp(wspe(order)))) gt 20) and (numax(in1(wp(wspe(order)))) lt 60)))
; OVERPLOT GREEN AND VIOLET
    ;p1=plot( feh(wup), yout(wup)*mass(wup)^(-t)*(numax(in1(wup))/3147.0)^(-s) ,  /overplot, font_name='Times',SYM_SIZE=1, transparency=95,linestyle="none",symbol=6, SYM_FILLED=1, color='lime green')
    ;p1=plot( feh(wdown), yout(wdown)*mass(wdown)^(-t)*(numax(in1(wdown))/3147.0)^(-s) ,  /overplot,font_name='Times',SYM_SIZE=1, transparency=95,linestyle="none",symbol=6, SYM_FILLED=1, color='dark magenta')
  stop
  
;HIGH
a=readfits('/Volumes/TEMP/RG_DR25/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg2640.00_55.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/kplr002856769_754_COR_PSD_filt_inp.fits')
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=90, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13)

abss=[13.888,  16.308399]
ordd=[1e-4,1e-4]
ordu=[1e10,1e10]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="blue", FILL_TRANSPARENCY=40, TRANSPARENCY=99)
match, long(2856769), long(output_a2z[*,0]), i1, i2
yy=alog10(output_a2z[i2,1])
abss=[10^((yy-res(0)+threshold)/res(1)),  10^((yy-res(0)-threshold)/res(1))]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="lime green", FILL_TRANSPARENCY=70, TRANSPARENCY=90)
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=50, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13, /overplot)
  b=text(3,1e8, 'KIC 2856769', /DATA, FONT_SIZE=14, FONT_NAME='Times')

p.save, '/Users/lbugnet/Documents/high2856769.png'

  ;LOW
a=readfits('/Volumes/TEMP/RG_DR25/K004/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg2640.00_55.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/kplr004482016*_COR_PSD_filt_inp.fits')
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=90, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13)

abss=[3.7316000,  4.0851998]
ordd=[1e-4,1e-4]
ordu=[1e8,1e8]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="blue", FILL_TRANSPARENCY=40, TRANSPARENCY=99)
match, long(4482016), long(output_a2z[*,0]), i1, i2
yy=alog10(output_a2z[i2,1])
abss=[10^((yy-res(0)+threshold)/res(1)),  10^((yy-res(0)-threshold)/res(1))]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="dark violet", FILL_TRANSPARENCY=70, TRANSPARENCY=90)
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=50, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13, /overplot)
  b=text(3,1e6, 'KIC 4482016', /DATA, FONT_SIZE=14, FONT_NAME='Times')

p.save, '/Users/lbugnet/Documents/high4482016.png'

;OK
a=readfits('/Volumes/TEMP/RG_DR25/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg2640.00_55.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/kplr002011582_12_COR_PSD_filt_inp.fits')
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=90, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13)

  abss=[38.2610 - 2.6697,  38.2610  +  2.6697]
ordd=[1e-4,1e-4]
ordu=[1e8,1e8]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="blue", FILL_TRANSPARENCY=40, TRANSPARENCY=99)
match, long(2011582), long(output_a2z[*,0]), i1, i2
yy=alog10(output_a2z[i2,1])
abss=[10^((yy-res(0)+threshold)/res(1)),  10^((yy-res(0)-threshold)/res(1))]
polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="orange", FILL_TRANSPARENCY=70, TRANSPARENCY=90)
p=plot(a[0,*]*1e6, a[1,*], xlog=1, ylog=1, dim=[700,500], xr=[1e-1, max((a[0,*])*1.e6+300)], transparency=50, xtitle='$Frequency (\mu Hz)$', ytitle='PSD $(ppm^2/ \mu Hz)$', font_size=13, /overplot)
  b=text(3,1e6, 'KIC 2011582', /DATA, FONT_SIZE=14, FONT_NAME='Times')
  abss=[38.2610 - 2.6697,  38.2610  +  2.6697]
  ordd=[1e-4,1e-4]
  ordu=[1e8,1e8]
  polyg=polygon([abss,reverse(abss)], [ordd, reverse(ordu)], target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="blue", FILL_TRANSPARENCY=40, TRANSPARENCY=99)

p.save, '/Users/lbugnet/Documents/high2011582.png'
end