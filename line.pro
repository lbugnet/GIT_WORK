pro line

device, decomposed=0
loadct, 39
;readcol,'/Users/lbugnet/DATA/TABLES/results_A2Z_all.txt',kic_s,dnu,ednu,fmin,fmax,numax,format='L,F,F,F,F,F'
;save,file='/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav',kic_s,dnu,ednu,fmin,fmax,numax
restore, '/Users/lbugnet/DATA/KEPLER_LC/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/RESULTS_A2Zp_variab_metrics_Lor.sav', /verbose ;OUTPUT_A2ZP, BD_STARS, slope_fit 
restore, '/Users/lbugnet/DATA/TABLES/results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
match,kic_s,long(output_a2zp[*,0]),i1,i2,count=n
;plot_oo,numax(i2),output_a2zp[i1,1],/yn,psym=2 ; plot_oo plot en log/log
;plot_oo,numax(i1),output_a2zp[i2,1],/yn,psym=3,xr=[0.5,300],xst=1,yr=[1,1e6] ; pour les 16000 etoiles de savita, metrique en fct de numax(i1 car donné par savita)
;plot_oo,output_a2zp[i2,7],output_a2zp[i2,3],/yn,psym=3,xr=[1,1e6],xst=1,yr=[1,1e6] ;pour les 16000 etoiles de savita, bruit en fct de signal

;------------------------------------
;------- MASS -----------------------
;------------------------------------

restore, '/Users/lbugnet/DATA/TABLES/Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav', /verbose ; KIC, MASS,Teff,....

;Teff
match, kic, kic_s, aa,bb,count=n

;-------------------------------------------------------------------------
;;;;;; SAVING MISSING MASS STARS
; bb est les numeros des etoiles OK
ks=kic_s
ks(bb)=-1
w=where(ks ne -1)


close,5
openw,5,'/Users/lbugnet/DATA/TABLES/missing_stars_s.txt'
close,5
openw,5, '/Users/lbugnet/DATA/TABLES/missing_stars_s.txt', /append
printf,5, 'Stars KIC in results_A2Z_all.sav (132 stars in 16398) that are missing in Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav'
close,5
openw,5, '/Users/lbugnet/DATA/TABLES/missing_stars_s.txt', /append
printf,5, kic_s(w)
close,5
;-------------------------------------------------------------------------

Teff_stars=TEFF(bb)

;sacling relations: MASS
kic_s=kic_s(bb)
numax=numax(bb) ; pas la meme dimension que le numax original
dnu=dnu(bb)

FDRM, dnu,numax,Teff_stars, R, M, logg

;------------------------------------
;------- FIT ------------------------
;------------------------------------

match,kic_s,long(output_a2zp[*,0]),i1,i2,count=n 

;-------------------------------------------------------------------------
;;;;; SAVE KIC STARS NOT IN SAVITA'S kic_s
ks=long(output_a2zp[*,0])
ks(i2)=-1
w=where(ks ne -1)
out=output_a2zp[w,*]
save, file='/Users/lbugnet/DATA/TABLES/missing_stars_output.sav', out

;close,6
;openw,6,'/Users/lbugnet/DATA/TABLES/missing_stars_output.txt'
;close,6
;openw,6, '/Users/lbugnet/DATA/TABLES/missing_stars_output.txt', /append
;printf,6, 'Stars not in results_A2Z_all.sav but in output_a2zp (RESULTS_A2Zp_variab_metrics_Lor.sav)'
;close,6
;for ii=0, n_elements(w)-1 do begin
;  openw,6, '/Users/lbugnet/DATA/TABLES/missing_stars_output.txt', /append
;  printf,6, output_a2zp[w(ii),*]
;  close,6
;endfor
;-------------------------------------------------------------------------

xx=alog10(numax(i1))
yy=alog10(output_a2zp[i2,1])

;;;; enlever les mauvais points
index_bad_zero = WHERE( finite(xx) lt 1.0)
xx(index_bad_zero)=0.1
remove, index_bad_zero,kic_s,M,xx,yy,numax,dnu, output_a2zp[*,1],output_a2zp[*,2],output_a2zp[*,3],output_a2zp[*,4],output_a2zp[*,5],output_a2zp[*,6],output_a2zp[*,7],output_a2zp[*,8], output_a2zp[*,9], output_a2zp[*,10]     ;,kic_s, M; on enleve le kic des mauvaises etoiles
ee=where(finite(yy) lt 1.0)
yy[ee]=0.1
remove, ee,kic_s,M,xx,yy,numax,dnu, output_a2zp[*,1],output_a2zp[*,2],output_a2zp[*,3],output_a2zp[*,4],output_a2zp[*,5],output_a2zp[*,6],output_a2zp[*,7],output_a2zp[*,8], output_a2zp[*,9], output_a2zp[*,10]            ; on enleve le kic des mauvaises etoiles

;;;; fit
res = poly_fit(xx,yy,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b

slope_fit = res(0) + res(1)*xx

;;;; graph
window, 1
plot,xx,yy,/yn,psym=3, xtitle='log(numax)', ytitle='log(Metric)'
oplot, xx, slope_fit, psym=0, color=50


Residuals = yy-slope_fit
threshold=0.75*stddev(yy)
index_bad = WHERE(abs(residuals) ge threshold)

;stop
for ii=0,n_elements(kic_s)-1 do begin
  if (M(ii) lt 1.3) then begin
    plotsym,0,0.5,/fill,thick=0.5,color=c
    oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=100
  endif
  if (M(ii) gt 2.5) then begin
    plotsym,0,0.5,/fill,thick=0.5,color=c
    oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=200
  endif
  ;c=min([((M(ii)-median(M))/max(M)*10000+125),255]);min([300*alog(M(ii)),255])
  ;c=max([c,0])
  ;;print, c
  ;plotsym,0,1,/fill,thick=1,color=c
  ;oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=c

endfor



window, 2
plot,xx, residuals,psym=3, xtitle='numax', ytitle='Metric-slope'
oplot, [0,2.5], [threshold,threshold], psym=0, color=240, thick=2
oplot, [0,2.5], [-threshold,-threshold], psym=0, color=240, thick=2
oplot, [0,2.5], [0,0], psym=0, color=70, thick=2

;;;; extract bad stars
flag=dblarr(n_elements(kic_s))
for ii=0,n_elements(kic_s)-1 do begin
  if ((residuals(ii) lt (-threshold)) and (numax(ii) le 260)) then begin
    ;print, 'aa'
    flag(ii)=1
  endif
  if ((residuals(ii) lt (-threshold)) and (numax(ii) gt 260)) then begin
    ;print, 'aa'
    flag(ii)=3
  endif
  if (residuals(ii) gt (threshold)) then begin
    flag(ii)=2
    ;print, 'bb'
  endif
  if ((residuals(ii) le (threshold)) and (residuals(ii) ge (-threshold))) then begin
    flag(ii)=0
    ;print, 'cc'
  endif
endfor

for ii=0,n_elements(kic_s)-1 do begin
  if (M(ii) lt 1.3) then begin
    plotsym,0,0.5,/fill,thick=0.5,color=c
    oplot,[xx(ii),xx(ii)], [residuals(ii),residuals(ii)],psym=8, color=100
  endif
  if (M(ii) gt 2.5) then begin
    plotsym,0,0.5,/fill,thick=0.5,color=c
    oplot,[xx(ii),xx(ii)], [residuals(ii),residuals(ii)],psym=8, color=200
  endif
  ;c=min([((M(ii)-median(M))/max(M)*10000+125),255]);min([300*alog(M(ii)),255])
  ;c=max([c,0])
  ;;print, c
  ;plotsym,0,1,/fill,thick=1,color=c
  ;oplot,[xx(ii),xx(ii)], [yy(ii),yy(ii)],psym=8, color=c

endfor

print, 'nb_bad_stars_up', n_elements(where(residuals gt threshold)) ; nb d'etoiles au dessus de la limite
print, 'nb_bad_stars_down', n_elements(where(residuals lt -threshold)); nb d'etoiles en dessous de la limite


save, file='/Users/lbugnet/DATA/TABLES/metric_all_stars.sav', xx,yy,res, slope_fit, output_a2zp ;res(0)=b, res(1)=a, ax+b
save, file='/Users/lbugnet/DATA/TABLES/masses_stars.sav', xx, yy, residuals, threshold, M, flag, kic_s, numax, dnu

;;;;;;;;;;; SAVE LOW STARS
w_low=where(flag eq 1)
  close,1
  openw,1,'/Users/lbugnet/DATA/TABLES/low_stars_s.txt'
  close,1
  openw,1, '/Users/lbugnet/DATA/TABLES/low_stars_s.txt', /append
  printf,1, kic_s(w_low), M(w_low)
  close,1

;;;;;;;;;;; SAVE HIGH STARS
w_high=where(flag eq 2)  
  close,2
  openw,2,'/Users/lbugnet/DATA/TABLES/high_stars_s.txt'
  close,2
  openw,2, '/Users/lbugnet/DATA/TABLES/high_stars_s.txt', /append
  printf,2, kic_s(w_high(*)), M(w_high(*))
  close,2
  


;print, flag

stop

END