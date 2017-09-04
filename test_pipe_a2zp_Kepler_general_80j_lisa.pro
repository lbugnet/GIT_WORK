;---------------------------------------------------
; Setting up general variables
;---------------------------------------------------
path_org_str = '/Users/lbugnet/DATA/METRIC/KEPLER/80J/'
file_output_str = 'RESULTS_A2Zp_variab_metrics_Lor_80j.sav'
bd_stars=[]

freq_inic_gr    = 0.2   ; in muHz
freq_inic_gr_ns = 275 ; in muHz

INIC = 0L       ; First star to be analyzed

;---------------------------------------------------
; READ ALL THE STARS 
;---------------------------------------------------

read_kepler_dir,'/Volumes/TEMP/RG_DR25/K???/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg0_80.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/',files_arr,id_stars_arr,wild='kplr*COR_PSD_filt_inp.fits',length=9
save,file='/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25_80j.sav',files_arr,id_stars_arr

restore, '/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25_80j.sav', /verbose
id_stars = LONG(id_stars_arr)
n_stars_tot = N_ELEMENTS(id_stars)

OUTPUT_A2Zp = DBLARR(n_stars_tot,15)

;---------------------------------------------------
; Loop by star
;---------------------------------------------------
FOR ns = INIC, n_stars_tot-1 DO BEGIN
  freq_inic_gr_ns = 275  ; in muHz

  ;---------------------------------------------------
  ; READ one single star (PSD, FITS)
  ;---------------------------------------------------
  data_arr = READFITS(files_arr(ns))
  freq_org = data_arr(0,*) *1e6     ; in muHz
  power_arr    = data_arr(1,*)      ; power
  freq_org_ns = data_arr(0,*) *1e6     ; in muHz
  power_arr_ns    = data_arr(1,*)      ; power
  ; -------------------------------------------
  ; Computing granulation metrics & Noise
  ;--------------------------------------------
  freq_fin_gr_ns = freq_org(-1)   ; Nyquist frequency
  IF freq_fin_gr_ns le freq_inic_gr_ns THEN BEGIN
    freq_inic_gr_ns = freq_fin_gr_ns-5.
    bd_stars=[bd_stars,id_stars_arr(ns)]
  ENDIF
  freq_fin_gr    = freq_fin_gr_ns
  region, freq_inic_gr_ns, freq_fin_gr_ns, freq_org_ns, power_arr_ns, xmat_ns, ymat_ns, out_metrics=out_metrics_ns ;compiler kadacs
  region, freq_inic_gr,    freq_fin_gr,    freq_org,    power_arr,    xmat,    ymat,    out_metrics=out_metrics

  OUTPUT_A2Zp[ns,0] = id_stars[ns]
  OUTPUT_A2Zp[ns,1] = out_metrics[0]-out_metrics_ns[0]
  OUTPUT_A2Zp[ns,3:6] = out_metrics[*]
  OUTPUT_A2Zp[ns,7:10] = out_metrics_ns[*]
  ;out_metrics(0) = av                             ; Mean
  ;out_metrics(1) = sgm                            ; Sigma
  ;out_metrics(2) = med                            ; Median
  ;out_metrics(3) = total_power                    ; Total_power

  goto, fin
  ;------------------------------------------------------------
  ; Calculating slope with Savita's double line fit and a second single line fit
  ;------------------------------------------------------------
  backgnd_corr, freq_org*1e-6, power, slope_fit1
  PSD_norm1 = power/slope_fit1
  REGION,2.5,270,freq_org,PSD_norm1,xx,yy
  xx=alog10(xx)
  yy=alog10(yy)
  res = poly_fit(xx,yy,1,yfit=yfit)

  slope_fit = 10^(res(0))*freq_org^(res(1))
  psd_norm = PSD_norm1/slope_fit

  ;------------------------------------------------------------
  ; Lorentzian fitting
  ;------------------------------------------------------------
  indx_modes = where(freq_org ge (numax(i)-3*dnu(i)) and freq_org le (numax(i)+3*dnu(i)))
  maxi = max(psd_norm(indx_modes))
  guess = [maxi,numax(i),dnu(i)*4.,1.]    ;mean(result(*,0))*0.5
  smth=dnu(i)/freq_org(0)/2.
  yfit = mpfitpeak(freq_org, smooth(psd_norm,smth,/edge_truncate), lor,/LORENTZIAN,/positive,estimates=guess,autoderiv=1,error=replicate(1,n_elements(freq_org)), perror=err,bestnorm=bestnorm)
  err = err*sqrt(bestnorm/(n_elements(freq_org)-4.))
  print,lor
  print,'dnu=',dnu(i),'HWHM=',lor(2),'ratio HWHM/dnu',lor(2)/dnu(i)
  col=2



  fin:
ENDFOR

save,file=path_org_str+file_output_str,output_a2zp

END