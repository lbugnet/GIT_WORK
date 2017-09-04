PRO LIGHT_CURVE_PATH_TO_PSD, STAR_PATH_LC=STAR_PATH_LC, STAR_TAB_PSD=STAR_TAB_PSD,  EXTENSION=EXTENSION
;light_curve_path_to_psd, STAR_PATH_LC='/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/hlsp_everest_k2_llc_211200040-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale'

;------------------------------------------------------------------------------------
;------------------------- IN -------------------------------------------------------
;------------------------------------------------------------------------------------
;
;--- STAR_PATH_LC contient les PATH vers les LC des étoiles dont on veut la PSD -----

;------------------------------------------------------------------------------------
;------------------------- OUT ------------------------------------------------------
;------------------------------------------------------------------------------------
;
;--- STAR_TAB_PSD contient freq/power des etoiles -----------------------------------
;------------------------------------------------------------------------------------
if n_elements(extension) eq 0 then EXTENSION=' '

if EXTENSION eq 'fits' then begin
  a=readfits( STAR_PATH_LC)
  data_arr_freq=a(0,*)
  data_arr_power=a(1,*)
  
;  endif else if EXTENSION eq 'dat' then begin
;    readcol, STAR_PATH_LC, data_arr_freq, data_arr_power, data_arr_power_error, format='D,D,D', /silent, stringskip='#'
;    close, 1
;    openr, 1, STAR_PATH_LC
;    close, 1
;    openr, 1, STAR_PATH_LC, /append
;    readf, 1, data_arr_freq, data_arr_power
;    close, 1
;
;  
endif else if (where(file_search(star_path_lc + '_time_flux'+'.sav') eq '') ne 0) then begin
  restore, STAR_PATH_LC+ '_time_flux'+'.sav', /verbose;time, flux
  
  outt=DBLARR(2,n_elements(time))
  outt(0,*) = time(*)-time(0)
  outt(1,*) = flux
  ;in ppm
  ;outt(1,*)=1e6*(outt(1,*)-1)
  outt(1,*)=1e6*(outt(1,*)/median(outt(1,*))-1)
  ;compute PSD
  psd,outt,data_arr_freq,data_arr_power,ofac=ofac; s in Hz;data,freq,power


endif else if EXTENSION eq 'sav' then begin
  restore, STAR_PATH_LC, /verbose; files_arr, id_stars_arr
  readcol, files_arr, time, flux, FORMAT='D,D', /silent
  save, file='STAR_PATH_LC'+'_time_flux'+'.sav', time, flux
  
  outt=DBLARR(2,n_elements(time))
  outt(0,*) = time(*)-time(0)
  outt(1,*) = flux
  ;in ppm
  ;outt(1,*)=1e6*(outt(1,*)-1)
  outt(1,*)=1e6*(outt(1,*)/median(outt(1,*))-1)
  ;compute PSD
  psd,outt,data_arr_freq,data_arr_power,ofac=ofac; s in Hz;data,freq,power

endif else begin
  readcol, STAR_PATH_LC, time, flux, FORMAT='D,D', /silent, stringskip='#'
  save, file='STAR_PATH_LC'+'_time_flux'+'.sav', time, flux
  
  outt=DBLARR(2,n_elements(time))
  outt(0,*) = time(*)-time(0)
  outt(1,*) = flux
  ;in ppm
  ;outt(1,*)=1e6*(outt(1,*)-1)
  if (STREGEX(STAR_PATH_LC, 'ktwo') eq -1) then begin
  outt(1,*)=1e6*(outt(1,*)/median(outt(1,*))-1) ; a faire que si pas en ppm
  endif
  ;compute PSD
  psd,outt,data_arr_freq,data_arr_power,ofac=ofac; s in Hz;data,freq,power

endelse

STAR_TAB_PSD=dblarr(2, n_elements(data_arr_power))
STAR_TAB_PSD(0,*)=data_arr_freq
STAR_TAB_PSD(1,*)=data_arr_power   

;print, star_tab_psd
END

