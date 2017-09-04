pro compute_metric_line

device, decomposed=0
loadct, 39

;file 1: Contient les 23000 étoiles de Kepler
restore, '/Users/lbugnet/DATA/KEPLER_LC/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/RESULTS_A2Zp_variab_metrics_Lor.sav', /verbose ;OUTPUT_A2ZP, BD_STARS, slope_fit 

;file 2: Contient les 16000 étoiles dont on connait un numax par A2Z
;readcol,'/Users/lbugnet/DATA/TABLES/results_A2Z_all.txt',kic_s,dnu,ednu,fmin,fmax,numax,format='L,F,F,F,F,F'
;save,file='/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav',kic_s,dnu,ednu,fmin,fmax,numax
restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax

;--------------------------------------------------------------------
;-------------- MASS STARS ---------------------------
;--------------------------------------------------------------------

;file 3: Catalogue de Savita pour trouver les caractéristiaues des étoiles qui nous intéressent.
restore, '/Users/lbugnet/DATA/TABLES/Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav', /verbose ; KIC, MASS,Teff,....

match, kic, kic_s, aa,bb,count=n                 ; On garde seulement les étoiles en commun= 15000

ks=kic_s
ks(bb)=-1
w=where(ks ne -1)

;missing_stars, kic,kic_s, aa, bb, kic_s, '/Users/lbugnet/DATA/TABLES/missing_stars_s.txt', 'Stars KIC in results_A2Z_all.sav (132 stars in 16398) that are missing in Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav'

Teff_stars=TEFF(bb)
kic_s=kic_s(bb)
numax=numax(bb) ; pas la meme dimension que le numax original
dnu=dnu(bb)
FDRM, dnu,numax,Teff_stars, R, M, logg

  ;--------------------------------------------------------------------
  ;-------------- COMPUTE_METRIC --------------------------------------
  ;--------------------------------------------------------------------

match,long(output_a2zp[*,0]),kic_s,i2,i1,count=n
output_resize_b=output_a2zp[i2,*] ;;16266
;output_resize_c=output_a2zp[i2,*]
;output_resize=output_a2zp[i2,*]
; SAVE KIC STARS NOT IN SAVITA'S kic_s
ks=long(output_a2zp[*,0])
ks(i2)=-1
w=where(ks ne -1)
;missing_stars, kic_s, long(output_a2zp[*,0]), i1, i2, output_a2zp, '/Users/lbugnet/DATA/TABLES/missing_stars_output.txt', 'Stars not in results_A2Z_all.sav but in output_a2zp (RESULTS_A2Zp_variab_metrics_Lor.sav)', w=w
out=output_a2zp[w,*]
;save, file='/Users/lbugnet/DATA/TABLES/missing_stars_output.sav', out  ; reenregistrement dans l'ordre mieux
kic_s=kic_s(i1)
xx=alog10(numax(i1))
yy=alog10(output_a2zp[i2,1])
index_bad_zero = WHERE( finite(xx) lt 1.0)

if (index_bad_zero(0) ne -1) then begin
    xx(index_bad_zero)=0.1
    remove, index_bad_zero,xx,yy,numax,dnu, kic_s

  aa=output_resize_b[*,0]
  remove, index_bad_zero, aa

 ; output_resize_c=dblarr(n_elements(aa),n_elements(out[1,*]))
  for ii=0,10 do begin
    aa=output_resize_b[*,ii]
    remove, index_bad_zero, aa
  endfor
  output_resize_c=dblarr(n_elements(aa),ii)
  for ii=0,10 do begin
    aa=output_resize_b[*,ii]
    remove, index_bad_zero, aa
    output_resize_c[*,ii]=aa(*)
  endfor
endif else begin output_resize_c=output_resize_b
endelse

ee=where(finite(yy) lt 1.0)
if (ee(0) ne -1) then begin
  yy[ee]=0.1
  remove, ee,xx,yy,numax,dnu, kic_s;,output_resize[*,0],output_resize[*,1],output_resize[*,2],output_resize[*,3],output_resize[*,4],output_resize[*,5],output_resize[*,6],output_resize[*,7],output_resize[*,8],output_resize[*,9],output_resize[*,10];, output_a2zp[*,1],output_a2zp[*,2],output_a2zp[*,3],output_a2zp[*,4],output_a2zp[*,5],output_a2zp[*,6],output_a2zp[*,7],output_a2zp[*,8], output_a2zp[*,9], output_a2zp[*,10]     ;,kic_s, M; on enleve le kic des mauvaises etoiles

  aa=output_resize_c[*,0]
  remove, ee, aa

  output_resize=dblarr(n_elements(aa),n_elements(out[1,*]))
  for ii=0,10 do begin
    aa=output_resize_c[*,ii]
    remove, ee, aa
  endfor
  output_resize=dblarr(n_elements(aa),ii)
  for ii=0,10 do begin
    aa=output_resize_c[*,ii]
    remove, ee, aa
    output_resize[*,ii]=aa
  endfor

  
endif else begin output_resize=output_resize_c
endelse



;;;; FIT
res = poly_fit(xx,yy,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
slope_fit = res(0) + res(1)*xx

;--------------------------------------------------------------------
;------------- FLAG BAD STARS ---------------------------------------
;--------------------------------------------------------------------

Residuals = yy-slope_fit
threshold=1*stddev(yy)
index_bad = WHERE(abs(residuals) ge threshold)

flag=dblarr(n_elements(kic_s))
for ii=0,n_elements(kic_s)-1 do begin
  if ((residuals(ii) lt (-threshold)) and (numax(ii) le 200)) then begin
    flag(ii)=1
  endif
  if ((residuals(ii) le (threshold)) and (residuals(ii) ge (-threshold))) then begin
    flag(ii)=0
  endif
  if ((residuals(ii) le 0.0) and (numax(ii) gt 200)) then begin
    flag(ii)=3
  endif
  if (residuals(ii) gt (threshold)) then begin
    flag(ii)=2
  endif
 
endfor

;--------------------------------------------------------------------
;------------- IMPROVE METRIC ---------------------------------------
;--------------------------------------------------------------------

xx_ok=xx(where(flag eq 0))
yy_ok=yy(where(flag eq 0))
res_ok = poly_fit(xx_ok,yy_ok,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
slope_fit_ok = res_ok(0) + res_ok(1)*xx_ok

Residuals_ok = yy_ok-slope_fit_ok
threshold_ok=1*stddev(yy_ok)

w_up=where((residuals_ok) gt (threshold_ok))
w_do=where((residuals_ok) lt (-threshold_ok))

match, xx_ok(w_do), xx, xx_ok1,xx1, count=mm
flag(xx1)=1

match, xx_ok(w_up), xx, xx_ok2,xx2, count=mm
flag(xx2)=2

remove, w_up, xx_ok, yy_ok,residuals_ok,slope_fit_ok
remove, w_do, xx_ok, yy_ok,residuals_ok,slope_fit_ok


;--------------------------------------------------------------------
;------------- PLOT METRIC ------------------------------------------
;--------------------------------------------------------------------
;window, 1
;plot,xx,yy,/yn,psym=3, xtitle='log(numax)', ytitle='log(Metric)'
;oplot, xx, slope_fit, psym=0, color=50
;
;
;for ii=0,n_elements(kic_s)-1 do begin
;  if (M(ii) lt 1.3) then begin
;    plotsym,0,0.5,/fill,thick=0.5,color=c
;    oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=100
;  endif
;  if (M(ii) gt 2.5) then begin
;    plotsym,0,0.5,/fill,thick=0.5,color=c
;    oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=200
;  endif
;endfor
;
;window, 2
;plot,xx, residuals,psym=3, xtitle='numax', ytitle='Metric-slope'
;oplot, [0,2.5], [threshold,threshold], psym=0, color=240, thick=2
;oplot, [0,2.5], [-threshold,-threshold], psym=0, color=240, thick=2
;oplot, [0,2.5], [0,0], psym=0, color=70, thick=2
;for ii=0,n_elements(kic_s)-1 do begin
;  if (M(ii) lt 1.3) then begin
;    plotsym,0,0.5,/fill,thick=0.5,color=c
;    oplot,[xx(ii),xx(ii)], [residuals(ii),residuals(ii)],psym=8, color=100
;  endif
;  if (M(ii) gt 2.5) then begin
;    plotsym,0,0.5,/fill,thick=0.5,color=c
;    oplot,[xx(ii),xx(ii)], [residuals(ii),residuals(ii)],psym=8, color=200
;  endif
;endfor
;

;--------------------------------------------------------------------
;-------------------------------SAVE FILES --------------------------
;--------------------------------------------------------------------

res=res_ok
slope_fit=slope_fit_ok
residuals=Residuals_ok
threshold=threshold_ok

save, file='/Users/lbugnet/DATA/TABLES/metric_all_stars.sav', xx, yy, res, slope_fit, output_a2zp, output_resize,  out ;=(stars not in common) 
save, file='/Users/lbugnet/DATA/TABLES/masses_stars.sav'    , xx, yy, residuals, threshold, M, flag, kic_s, numax, dnu
save, file='/Users/lbugnet/DATA/TABLES/stars_OK.sav'    , xx_ok, yy_ok, residuals, threshold, slope_fit, res

;SAVE LOW STARS
w_low=where(flag eq 1)
  close,1
  openw,1,'/Users/lbugnet/DATA/TABLES/low_stars_s.txt'
  close,1
  openw,1, '/Users/lbugnet/DATA/TABLES/low_stars_s.txt', /append
  printf,1, kic_s(w_low), M(w_low)
  close,1

;SAVE HIGH STARS
w_high=where(flag eq 2)  
  close,2
  openw,2,'/Users/lbugnet/DATA/TABLES/high_stars_s.txt'
  close,2
  openw,2, '/Users/lbugnet/DATA/TABLES/high_stars_s.txt', /append
  printf,2, kic_s(w_high(*)), M(w_high(*))
  close,2
  

stop

END











;plot_oo,numax(i2),output_a2zp[i1,1],/yn,psym=2 ; plot_oo plot en log/log
;plot_oo,numax(i1),output_a2zp[i2,1],/yn,psym=3,xr=[0.5,300],xst=1,yr=[1,1e6] ; pour les 16000 etoiles de savita, metrique en fct de numax(i1 car donné par savita)
;plot_oo,output_a2zp[i2,7],output_a2zp[i2,3],/yn,psym=3,xr=[1,1e6],xst=1,yr=[1,1e6] ;pour les 16000 etoiles de savita, bruit en fct de signal
