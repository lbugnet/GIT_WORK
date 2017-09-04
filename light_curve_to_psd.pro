PRO LIGHT_CURVE_TO_PSD, STAR_LC=STAR_LC, STAR_TAB_PSD=STAR_TAB_PSD
 
  ;------------------------------------------------------------------------------------
  ;------------------------- IN -------------------------------------------------------
  ;------------------------------------------------------------------------------------
  ;
  ;--- STAR_LC contient la light curve de l'etoile dont on veut la PSD ----------------

  ;------------------------------------------------------------------------------------
  ;------------------------- OUT ------------------------------------------------------
  ;------------------------------------------------------------------------------------
  ;
  ;--- STAR_TAB_PSD contient freq/power de l'etoile -----------------------------------
  ;------------------------------------------------------------------------------------


  time=STAR_LC(0,*)
  flux=STAR_LC(1,*)
  outt=DBLARR(2,n_elements(time))
  outt(0,*) = time(*)-time(0)
  outt(1,*) = flux
  ;in ppm
  ;outt(1,*)=1e6*(outt(1,*)-1) ERREUR !!!!!
  out(1,*)=1e6*(out(1,*)/median(out(1,*))-1)
  ;compute PSD
  psd,outt,data_arr_freq,data_arr_power,ofac=ofac; s in Hz;data,freq,power

  STAR_TAB_PSD=dblarr(2, n_elements(data_arr_power))
  STAR_TAB_PSD(0,*)=data_arr_freq
  STAR_TAB_PSD(1,*)=data_arr_power

END