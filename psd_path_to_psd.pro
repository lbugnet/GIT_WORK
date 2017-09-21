PRO PSD_PATH_TO_PSD, STAR_PATH_PSD=STAR_PATH_PSD, STAR_TAB_PSD=STAR_TAB_PSD,  EXTENSION=EXTENSION

  ;------------------------------------------------------------------------------------
  ;------------------------- IN -------------------------------------------------------
  ;------------------------------------------------------------------------------------
  ;
  ;--- STAR_PATH_PSD contient les PATH vers la PSD des étoiles dont on veut la PSD ----
  
  ;------------------------------------------------------------------------------------
  ;------------------------- OUT ------------------------------------------------------
  ;------------------------------------------------------------------------------------
  ;
  ;--- STAR_TAB_PSD contient freq/power des etoiles -----------------------------------
  ;------------------------------------------------------------------------------------
  ;resolve_all, UNRESOLVED=variable, /CONTINUE_ON_ERROR
  if EXTENSION eq 'fits' then begin
    a=readfits(STAR_PATH_PSD)
    data_arr_freq=a(0,*)
    data_arr_power=a(1,*)
  endif else if EXTENSION eq 'sav' then begin
    restore, STAR_PATH_PSD, /verbose; files_arr, id_stars_arr
    readcol, files_arr, data_arr_freq,data_arr_power, FORMAT='D,D'
  endif else begin
    readcol, STAR_PATH_PSD, data_arr_freq,data_arr_power, FORMAT='D,D'
  endelse
;stop
  
  STAR_TAB_PSD=dblarr(2, n_elements(data_arr_power))
  STAR_TAB_PSD(0,*)=data_arr_freq
  STAR_TAB_PSD(1,*)=data_arr_power

END

