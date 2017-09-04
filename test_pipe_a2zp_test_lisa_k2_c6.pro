;---------------------------------------------------
; Setting up general variables
;---------------------------------------------------
;path_org_str = '/Users/lbugnet/DATA/KEPLER_LC/K002/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/'
;path_str = path_org_str + 'LC_CORR_FILT_INP/'
;wild_str = 'kplr*COR_PSD_filt_inp.fits'
;file_output_str = 'RESULTS_A2Zp_variab_metrics_Lor.sav'
;bd_stars=[]
path_org_str = '/Users/lbugnet/DATA/METRIC/K2/C6/'
path_str = path_org_str 
wild_str = 'hlsp_everest_k2_llc_*-c06_kepler_v2.0_lc.txt.clean.res.hipass.rescale'
file_output_str = 'C6RESULTS_A2Zp_variab_metrics_Lor_K2_C6.sav'
bd_stars=[]


;xsz = 1600   
;ysz = 1000 
;WINDOW,xsize=xsz,ysize=ysz   
;PLOT,[0,0],[0,0],background=255

freq_inic_gr    = 1		; in muHz
freq_inic_gr_ns = 275	; in muHz

INIC = 0L				; First star to be analyzed

;---------------------------------------------------
; READ ALL THE STARS in a DIRECTORY
;---------------------------------------------------
;;READ_KEPLER_DIR,path_str,files,id_stars_arr,wild=wild_str, length=9
;OR
;read_kepler_dir,'/Volumes/TEMP/RG_DR25/K???/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/',files_arr,id_stars_arr,wild='kplr*COR_PSD_filt_inp.fits',length=9
;save,file='/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25.sav',files_arr,id_stars_arr

;read_kepler_dir,'/Volumes/TEMP/K2/GAP/JOEL/C6/rescale_C6/',files_arr,id_stars_arr,wild='hlsp_everest_k2_llc_*-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale',length=9
files_arr=file_search('/Volumes/TEMP/K2/GAP/JOEL/C6/rescale_C6/*')
save,file='/Users/lbugnet/DATA/METRIC/K2/C6/LIST_id_C6.sav', files_arr
restore, '/Users/lbugnet/DATA/METRIC/K2/C6/LIST_id_C6.sav', /verbose
;restore, '/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25.sav', /verbose
;id_stars = LONG(id_stars_arr)
n_stars_tot = N_ELEMENTS(files_arr)

OUTPUT_A2Zp = DBLARR(n_stars_tot,15)

;---------------------------------------------------
; Loop by star
;---------------------------------------------------
data_arr=dblarr(2,n_elements(files_arr))
id_stars=dblarr(n_stars_tot)
FOR ns = INIC, n_stars_tot-1 DO BEGIN
	  freq_inic_gr_ns = 275  ; in muHz
    
    ;PRINT,'------------------------------------' ;
    ;PRINT,'   Iteration=',ns,' out of=', n_stars_tot+1,' id= ',id_stars_arr(ns)
    ;PRINT,'------------------------------------';

	;---------------------------------------------------
	; READ one single star (PSD, FITS)
	;---------------------------------------------------
	;data_arr = READFITS(files_arr(ns))
  readcol, files_arr[ns], time,flux, FORMAT='D,D'
  id_stars[ns] = STRMID(files_arr(ns), 60, 9)
  outt=DBLARR(2,n_elements(time))
  outt(0,*) = time(*)-time(0)
  outt(1,*) = flux
  ;in ppm
  ;outt(1,*)=1e6*(outt(1,*)-1)
  outt(1,*)=1e6*(outt(1,*)/median(outt(1,*))-1)
  ;compute PSD
  psd,outt,data_arr_freq,data_arr_power,ofac=ofac; s in Hz;data,freq,power
  data_arr=dblarr(2,n_elements(data_arr_freq))
  data_arr(0,*)=data_arr_freq(*)
  data_arr(1,*)=data_arr_power(*)
	freq_org = data_arr(0,*) *1e6 		; in muHz
	power_arr    = data_arr(1,*)			; power 
  freq_org_ns = data_arr(0,*) *1e6     ; in muHz
  power_arr_ns    = data_arr(1,*)      ; power 

	; -------------------------------------------
	; Computing granulation metrics & Noise
	;--------------------------------------------
	freq_fin_gr_ns = freq_org(-1)		; Nyquist frequency
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