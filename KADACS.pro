;=========================================================
; *-----------------------------------------------------------------------------
; *  Kepler Asteroseismic Data Correction and Analysis Software (KADACS)  
; *-----------------------------------------------------------------------------
; * -----------------------------------------------------------------------------
; *  Created on May, 2011 at Irfu/SAp, CEA Saclay, F-91191, France
; *     Version V1.0
;*      Version V2.0    (25/06/2012)
;*      Version V5.0     (2015)
; * -----------------------------------------------------------------------------
; * © Commissariat a l'Energie Atomique et aux Energies Alternatives (CEA)
; * -----------------------------------------------------------------------------
; *     Authors: 
; *                              R.A. Garcia (rafael.garcia@cea.fr)
; *                              S. Mathur (smathur@SpaceScience.org)
; *     with the help of:
; *                              S. Bloemen
; *     useful comments and discussions from:
; *                              J. Ballot, T. Ceillier, G.R. Davies, S, Hekker, and D. Salabert  
; *             
; * -----------------------------------------------------------------------------
; * -----------------------------------------------------------------------------
; * This software is part of the Kepler Asteroseismic Data Correction and analysis Software 
; * (KADACS) developed at SAp, CEA-Saclay to correct and analyze Kepler light curves.
; * If you use this software or part of it to analyze your data or if you use the resultant products, please acknowledge the author in 
; * any published article or review using such processed data. You are also requested to cite: 
; *          Garcia et al. 2011, MNRAS L239 
; * paper for which these routines or the resultant products were developed.
; * -----------------------------------------------------------------------------
; * -----------------------------------------------------------------------------
; * FREE SOFTWARE LICENCING
; * This software and its related products are governed by the CeCILL license under French law and abiding
; * by the rules of distribution of free software. You can use, modify and/or
; * redistribute the software under the terms of the CeCILL license as circulated
; * by CEA, CNRS and INRIA at the following URL: "http://www.cecill.info".
; * As a counterpart to the access to the source code and rights to copy, modify
; * and redistribute granted by the license, users are provided only with a
; * limited warranty and the software's author, the holder of the economic
; * rights, and the successive licensors have only limited liability. In this
; * respect, the user's attention is drawn to the risks associated with loading,
; * using, modifying and/or developing or reproducing the software by the user in
; * light of its specific status of free software, that may mean that it is
; * complicated to manipulate, and that also therefore means that it is reserved
; * for developers and experienced professionals having in-depth computer
; * knowledge. Users are therefore encouraged to load and test the software's
; * suitability as regards their requirements in conditions enabling the security
; * of their systems and/or data to be ensured and, more generally, to use and
; * operate it in the same conditions as regards security.
; * The fact that you are presently reading this means that you have had
; * knowledge of the CeCILL license and that you accept its terms.
; * -----------------------------------------------------------------------------
; * COMMERCIAL SOFTWARE LICENCING
; * You can obtain this software and its products from CEA under other licencing terms for
; * commercial purposes. For this you will need to negotiate a specific contract
; * with a legal representative of CEA.
; * ;========================================================


PRO median_filter, in_arr, out_arr, filter_width, thresh, x_pos_jumps_arr,draw=draw
  ; width = 25; sigma= 2.5 sur median et non pas sur la derivé

  y_arr = REFORM(in_arr(1,*))
  median_y_arr = MEDIAN(y_arr,filter_width)
  sigma_median_y = SIGMA(median_y_arr)*thresh     ; Computed on the median filtered function 
  abs_deriv_median_arr = abs(median_y_arr(1:*)-median_y_arr(*))
  abs_deriv_median_arr(0:filter_width/2.) = 0.0   ; To remove the first points tha are not filtered
  abs_deriv_median_arr(-filter_width/2.:*) = 0.0    ; To remove the last points tha are not filtered
  ;thresh_sigma = thresh*SIGMA(abs_deriv_median_arr)  ; Computed on the derivative function. The problem is that it flag good point in rotating stars
  indx = WHERE(abs_deriv_median_arr ge sigma_median_y,n_indx)
  x_pos_jumps_arr = [-1,-1]

  IF indx(0) gt -1 THEN BEGIN
    x_pos_jumps_arr = DBLARR(2,n_indx)
    x_pos_jumps_arr(0,*) = indx           ; position of the flagged jumps
    x_pos_jumps_arr(1,*) = in_arr(0,indx)     ; Time of the flagged jumps
  ENDIF
  out_arr = abs_deriv_median_arr            ; output varible for debugging the code
  
  If KEYWORD_SET(draw) THEN BEGIN
    PLOT,median_y_arr-MEDIAN(median_y_arr),/yn,col=12,xst=1
    OPLOT,[0,1e10],[sigma_median_y,sigma_median_y],lin=2,col=7
    OPLOT,abs_deriv_median_arr,col=4,thick=2
    IF x_pos_jumps_arr(0) eq -1 THEN PRINT,'******* NO FLAGGED JUMPS ********'
    PRINT,'Threshold in SIGMA=',sigma_median_y
    ;OPLOT,[0,1e10],[thresh_sigma,thresh_sigma],col=2,thick=3,line=1
  ENDIF
END


;;  dt_LC= 29.4244*60./86400. min
;;  dt_SC=58.84876 s
;-----------------------------------------------------------------
;  Q_DATES
;       Provide the start and end date of each quarter
;-----------------------------------------------------------------
FUNCTION Q_DATES
; Updated to Q14
    QUARTER_DATES=dblarr(18,2)
    QUARTER_DATES(*,0)=[54953.0, 54964.5, 55002.5, 55093.5, 55185.3, 55276.4, 55372.4, 55463.1, 55568.3, 55641.5, 55739.8,55834.1,55932.3,56015.6,56106.6,56205.9,56306,56391.71] 
    QUARTER_DATES(*,1)=[54963.25, 54997.99, 55091.47, 55182.5, 55275.3, 55371.2, 55462.3, 55552.55,55635.35,55738.93,55833.27,55931.34,56015.031,56106.1,56203.8294,56303.64,56391,56423.5120]
RETURN, QUARTER_DATES
END

;-----------------------------------------------------------------
;  PLOT_Q_DATES
;       Plot vertical lines and labels for the quarters
;-----------------------------------------------------------------
PRO plot_q_dates, label=label,start=start,noq0=noq0,id=id,shade_sc=shade_sc
         iinic=0
         quarters=''         
         ;     verif_star_quarter,3733735,quarters
         ;     shade_sc=1
         IF keyword_set(id) then verif_star_quarter,id,quarters
         IF keyword_set(noq0) then iinic=1
         q=q_dates()

        IF keyword_set(start) then begin 
              st=start
        ENDIF ELSE BEGIN
               st=0.0
        ENDELSE
         IF keyword_set(label) THEN BEGIN
                 y=!y.crange(1)-(!y.crange(1)-!y.crange(0))*0.1
                 FOR i=iinic,n_elements(q(*,0))-1 do BEGIN
                        xpos=q(i,0)+((q(i,1)-q(i,0))/2.)*0.5-st         
                         IF xpos ge !x.crange(0) and xpos le !x.crange(1) THEN  BEGIN
                         Qlabel = 'Q'+strtrim(i,2)
                         If i le 9 THEN Qlabel='Q0'+strtrim(i,2)
                               IF keyword_set(id) and strpos(quarters,Qlabel) gt 0 THEN BEGIN
                                       yr = (!y.crange(1)-!y.crange(0)) * 0.02
                                       low=     !y.crange(0) + yr
                                       high=   !y.crange(1) - yr 
                                      IF i le 1 THEN BEGIN 
                                              IF keyword_set(shade_sc) THEN polyfill,[q(i,0)-st,q(i,1)-st,q(i,1)-st,q(i,0)-st],[low,low,high,high],col=600
                                              ; polyfill,[22,26,26,22],[low,low,high,high],col=6 
                                      ENDIF ELSE BEGIN ; If quarters are above 2, Check months
                                          ;PRINT,quarters
                                          ;STOP
                                          FOR j=0,2 DO BEGIN
                                               Qlabel1=Qlabel+'.'+strtrim(j+1,2)
                                               IF strpos(quarters,Qlabel1) gt 0 THEN BEGIN
                                                   xr=(q(i,1)-q(i,0))/3.
                                                   xi = q(i,0)-st+j*xr
                                                   xf = q(i,0)-st+(j+1)*xr
                                                   IF keyword_set(shade_sc) THEN polyfill,[xi,xf,xf,xi],[low,low,high,high],col=600
                                               ENDIF
                                          ENDFOR
                                      ENDELSE
                               ENDIF   
                               ;xyouts,xpos,y,'     Q'+strtrim(i,2),col=4,charthick=1.5
                               xyouts,xpos,y,'Q'+strtrim(i,2),col=4,charthick=1.5
                               ;xyouts,xpos,y,strtrim(i,2),col=4,charthick=1.2
                         ENDIF    
                ENDFOR         
         ENDIF
         for i=0,n_elements(q(*,0))-1 do oplot,[q(i,0)-st,q(i,0)-st],[-1e20,1e20],lin=1,col=4

END

;-----------------------------------------------------------------
;  CAT_NAMES
;       Verify if a given KIC number has a CATname
;-----------------------------------------------------------------
PRO cat_names,kic,cat,silence=silence,all=all
    cat_file='~/bin/CATalogue.txt'
    readcol,cat_file,id,cat_name,format='L,A'
    IF KEYWORD_SET(all) THEN BEGIN
            for i=0,n_elements(id)-1 DO print,id(i),' ',cat_name(i)    
            kic=id
            cat=cat_name
    ENDIF ELSE BEGIN
            cat=''
            a=where(kic eq id,n)
            IF n ge 1 THEN cat=cat_name(a)
            IF not KEYWORD_SET(silence) THEN PRINT,kic,' ',cat            
    ENDELSE
END

;-------------------------------------------------------------------------
; Function that prolonges a vector by doing a symetry around the ending points (T.Ceillier, september 2014)
;          vector: original vector
;          np: number of points to add at each end of the vector
;         new_vector: new vector outputted
;-------------------------------------------------------------------------
function prolongement_vector, vector, np
          v=vector
        ; Add points before the curve
          ind_before=lindgen(np)
          v_before=fltarr(np)
          v_before[-1:0:-1]=2*v[0]-v[1:ind_before[-1]+1]
        ; Add points after the curve
          ind_after=(n_elements(time)-1-lindgen(np))[-1:0:-1]
          v_after=fltarr(np)
          v_after[*]=2*v[-1]-v[-2:ind_after[0]-1:-1]
        ; Make the final vector
          new_vector=[v_before,v,v_after]
          return, new_vector
end

;-------------------------------------------------------------------------
;   Compute a triangular smooth, managing the ends by doing a symmetry
;-------------------------------------------------------------------------
Function trismooth,data,np1
       np=np1
       gd=where(finite(data(1,*)) eq 1,ngd)
       smth=0
       IF ngd gt 100 THEN BEGIN
               time=reform(data(0,*)-data(0,0))
               dt=median(time(gd(1:*))-time(gd(0:*)),/double)
             ;  if dt eq 0 then dt=mean(time(gd(1:*))-time(gd(0:*)),/double)
               res=reform(data(1,*))      
               np_interp=long(max(time)/dt)
               t=lindgen(np_interp)*dt
               res_int=interpol(res(gd),time(gd),t)
               IF np gt n_elements(res_int) Then np=n_elements(res_int)/2.
               new_res_int=prolongement_vector(res_int,np)
               new_t=prolongement_vector(t,np)
                ;smth_int=median(new_res_int,np) ; Median Smoothing
                smth_int=smooth(smooth(new_res_int,np,/nan),np,/nan) ;triangular_smooth
               smth=interpol(smth_int,new_t,time)
         ENDIF      
RETURN,smth

END

;----------------------------------------------------------------
;   Compute a triangular smooth
;----------------------------------------------------------------
Function trismooth_old,data,np
        gd=where(finite(data(1,*)) eq 1)
        time=reform(data(0,*)-data(0,0))
        dt=median(time(gd(1:*))-time(gd(0:*)),/double)
      ;  if dt eq 0 then dt=mean(time(gd(1:*))-time(gd(0:*)),/double)
        res=reform(data(1,*))      
        np_interp=long(max(time)/dt)
        t=lindgen(np_interp)*dt
        res_int=interpol(res(gd),time(gd),t)
        IF np gt n_elements(res_int) Then np=n_elements(res_int)/2.        
        smth_int=smooth(smooth(res_int,np,/nan,/edge_truncate),np,/nan,/edge_truncate) ;triangular_smooth
        smth=interpol(smth_int,t,time)
RETURN,smth
END


;------------------------------------------------------------------------------------------
;   PSD
;------------------------------------------------------------------------------------------
;   IDL Routine to compute the PSD properly calibrated using Lnp_test IDL routine 
;------------------------------------------------------------------------------------------
;   EXAMPLE:
;------------------------------------------------------------------------------------------
;          PSD, data, freq, pow
;------------------------------------------------------------------------------------------
;   Parameters:
;                   data : 2d input array containing (time,flux)
;                   freq  : Output array of frequencies
;                   pow  : Output array of power
;   Optional parameters
;                   ofac : Oversampling Factor
;                   plot : Plot of the PSD
;------------------------------------------------------------------------------------------
PRO psd,data,freq,pot,ofac=ofac,plot=plot
         IF N_params() LT 1 THEN BEGIN
            PRINT,'Syntax -  PSD,data,freq,pow'
            PRINT,'                         [ofac=ofac,plot=plot]' 
            RETURN
        ENDIF
        ; Data(0,*) in days
        IF not KEYWORD_SET(ofac) THEN ofac=1
        np = n_elements(data(0,*))
        t = REFORM(data(0,*)-data(0,0))*86400.   ; time in seconds with t0=0
        res = REFORM(data(1,*))   ;
        gd = WHERE(Finite(res) eq 1 and res ne 0.0)
        ;IF keyword_set(plot) THEN plot,t,res,/yn
        dtav = DOUBLE(mean(data(0,gd(1:*))-data(0,gd(0:*)))*86400.d0)    ; Dc correction, average dt
        dtmed = DOUBLE(median(data(0,1:*)-data(0,0:*))*86400.d0)  ; Dc Correction, median dt
        IF dtmed eq 0 THEN dtmed=dtav
        a = LNP_TEST(t(gd),res(gd),/double,wk1=freq,wk2=pot, ofac=ofac,hifac=dtav/dtmed)
        ; Correction by Parceval
    	length = max(t)/86400.
    	res_var = MOMENT(res(gd), /double)
    	var = res_var(1)
    	pot = pot*var/total(pot, /double)/((freq(1)-freq(0))*1e6)
    	IF keyword_set(plot) THEN PLOT_oo,freq*1e6,pot,background=255,col=0,charsize=2,xst=1,xtit='Frequency (!7l!6Hz)',ytit='PSD (ppm!u2!n/Hz)'
END
;----------------------------------------------------------------
;   READ the PSD file 
;---------------------------------------------------------------
PRO load_psd,freq,pow,file=file,data=data,muhz=muhz
     IF not KEYWORD_SET(FILE) THEN BEGIN
         file=' '
         READ,'GIVE ME THE FILE TO READ',file
     ENDIF
     data = READFITS(FILE)
     FREQ = REFORM(data(0,*))
     POW = REFORM(data(1,*))
     IF KEYWORD_set(muHz) THEN FREQ=FREQ*1e6
END

;--------------------------------------------------------
;   Computes de Duty Cycle
;--------------------------------------------------------
FUNCTION DC,data
good=WHERE(FINITE(data) eq 1,ngd)
np=N_ELEMENTS(data)
dc=100.0*ngd/np
RETURN,dc
END



;----------------------------------------------------------------
;   Convert in ppm and removing 6th order poly fit
;   if dg_pol > 20 THEN => triangular smooth of the dg_pol number of points 
;   if dg_pol = 0 THEN no FIT
;----------------------------------------------------------------
PRO ppm,in, out,dg_pol=dg_pol,oplot=oplot,digital=digital,fit=fit,wc=wc
If not keyword_set(med) THEN med=0
  IF keyword_set(digital) THEN BEGIN
       nscale=13
       mr1d_trans, in(1,*),wc, opt='-n'+string(nscale) 
       fit=reform(in(1,*))
       fit[*]=0
       FOR i=1,dg_pol-1 DO fit=fit+wc.coef(*,nscale-i)
  ENDIF ELSE BEGIN
          IF n_elements(dg_pol) eq 0 THEN dg_pol = 6
          If dg_pol lt 20 THEN BEGIN
             gd=where(finite(in(1,*)) eq 1)
             coef=poly_fit(reform(in(0,gd)),reform(in(1,gd)),/double,dg_pol)
             fit=poly(reform(in(0,*)),coef)
          ENDIF else BEGIN
             fit=trismooth(in,dg_pol)
          ENDELSE
  ENDELSE        
  if keyword_set(oplot) then oplot,in(0,*),fit,col=2,thick=3
  out=in
  out(1,*)=1e6*((in(1,*)/fit)-1)
END


;----------------------------------------------------------------
;   Convert in ppm by quarters and removing 6th order poly fit
;   if dg_pol > 20 THEN => triangular smooth of the dg_pol number of points 
;----------------------------------------------------------------
PRO ppm_quarters,in, out,dg_pol=dg_pol,oplot=oplot,digital=digital,fit=fit
    ts = Q_DATES()
    out=in
    ;xpos=[ts(0,0)-1,(ts(1:*,0)-ts(0:*,1))/2.+ts(0:*,1)]
    xpos=[ts(0,0),(ts(1:*,0)-ts(0:*,1))/2.+ts(0:*,1),ts(n_elements(ts(*,0))-1,1)+1]

    np = n_elements(xpos)
    filt = reform(in(1,*))
    filt[*] = 0
    FOR i = 0,np-2 DO BEGIN
        ngd = 0   
        ndx = WHERE(in(0,*) ge xpos(i) and in(0,*) le xpos(i+1),ngd)        
        IF ngd gt 0 THEN BEGIN
           in_q=in(*,ndx)
            IF keyword_set(digital) THEN BEGIN
               nscale=12
               mr1d_trans, in_q(1,*),wc, opt='-n'+string(nscale) 
               fit=reform(in_q(1,*))
               fit[*]=0
               FOR j=1,dg_pol-1 DO fit=fit+wc.coef(*,nscale-j)
          ENDIF ELSE BEGIN       
              fit=trismooth(in_q,dg_pol)
              fit1=trismooth(in_q,48)
              plot,in_q(0,*)-xpos(i),in_q(1,*),/yn
              oplot,in_q(0,*)-xpos(i),fit,col=4,thick=2
              oplot,in_q(0,*)-xpos(i),fit1,col=2,thick=2
          ENDELSE
          out(1,ndx)=1e6*((out(1,ndx)/fit(*))-1)
          filt[ndx]=fit
        ENDIF
    ENDFOR
          if keyword_set(oplot) then oplot,in(0,*),filt,col=2,thick=3
          fit=filt
END

;--------------------------------------------------------
; Save Jpeg file
;--------------------------------------------------------
PRO save_jpeg,file,help=help,quality=quality,true=true
    IF keyword_set(help) THEN BEGIN
        doc_library,'save_jpeg'
        RETURN
    ENDIF    
    qual=100
    If keyword_set(quality) THEN qual=quality
    if not keyword_set(true) then true=1
    write_jpeg,file,tvrd(true=true),true=true,quality=qual
    RETURN
END

;--------------------------------------------------------
; Convert not-a-number points to NaN
;--------------------------------------------------------
PRO BAD2NAN,data,bad_ndx=bad_ndx
    bad_ndx=WHERE(FINITE(data) ne 1)  ; Detect not-a-number values 
    IF bad_ndx(0) gt -1 THEN data(bad_ndx)= !values.f_nan   ; Change them to NaN
END

;+
; NAME: 
;       REGULAR_GRID
;
; PURPOSE: 
;       Puts the points of data into a regular grid with
;       dt=median(real dt)
;       Points where there are no points are set to zero
;
; CALLING:
;       regular_grid,data,out_reg,dt=dt,maxi=maxi,aver=aver,out_irreg=out_irreg
;
; INPUTS: 
;       data --- irregularly sampled 1d signal
;	
; OUTPUT: 
;       out_reg ---  regularly sampled signal with gaps
;
; KEYWORD:
;       dt --- regular timing value
;       maxi --- to just process the beginning of the time series
;       aver --- resize with larger bien
;       out_irreg --- out no gap: existing points are in the original
;                  timing, gaps are filled with regular grid
;                       IT DOES NOT WORK WHEN AVERAGING!!
;
; HISTORY:
;	Written: R. Garcia 2013
;   OUT PARAMETERS
;
;       reg_ndx : Index of the regularized array with original good points
;       gd_ndx : Index of the good points in the origina array
;-
; Nearest neighbor resampling with the slotting principle (Broersen 2009)
;---------------------------------------------------------
PRO regular_grid,data,out_reg,dt=dt,maxi=maxi,aver=aver,out_irreg=out_irreg,reg_ndx=reg_ndx,gd_ndx=gd_ndx
 
  t=(data(0,*)-data(0,0))
  gd=where(finite(data(1,*)) eq 1)
  gd_ndx = gd
  IF not keyword_set(dt) THEN  dt=double(median(t(1:*)-t(0:*)))
  if not keyword_set(maxi) then begin 
    fin = max(t)
  ENDIF ELSE BEGIN
    fin = maxi
  ENDELSE
  np_int = round((fin+dt)/dt)
;  np_int = round((fin)/dt)
  out_reg = dblarr(2,np_int)
  out_reg(0,*) = lindgen(np_int)*dt   
  IF not keyword_Set(aver) THEN BEGIN
    ndx = round(t(gd)/dt)
    reg_ndx = ndx
    out_reg(1,ndx) = data(1,gd)
    out_irreg = out_reg
    out_irreg(0,*) = out_irreg(0,*)+data(0,0)
    out_irreg(0,ndx) = data(0,gd)
  ENDIF ELSE BEGIN
    gd = WHERE(finite(data(1,*)) eq 1 and data(1,*) ne 0.0)
    ndx=round(t(gd)/dt)
    FOR i=0, np_int -1 DO BEGIN
      a=where(i eq ndx,na)
      IF a(0) gt -1 THEN out_reg(1,i)=total(data(1,gd(a)))/double(na)
    ENDFOR
  ENDELSE
  out_reg(0,*)=out_reg(0,*)+data(0,0)

END


;--------------------------------------------------------
; interpolation of Short Cadence Light curves into Long Cadence
;       Puts the points of data into a regular grid with dt=median(real dt)
;       By default rebin the good data taking into accounts de zeroes
;--------------------------------------------------------
PRO Interpol_LC,data,out_interp,npi=npi,dt=dt
    IF not keyword_set(dt) THEN dt=double(median(data(0,1:*)-data(0,0:*)))
    IF not keyword_set(np) THEN npi = 30.  ; 30 SC in a LC measurement
    gd = WHERE(FINITE(data(1,*)) eq 1, np)
    in = data(*,gd)
    regular_grid,in,out,dt = dt
    npt = n_elements(out(0,*))
    npitot=long(npt/npi)
    out_interp=dblarr(2,npitot)
    FOR i = 0L,npitot-1 DO BEGIN  ; Loop to do the re binning
       ndx_in = i*npi
       ndx_fin = min([ndx_in+npi-1,npt-1])  ; To avoid in case we do not have enough points at the end of the file 
       a = WHERE(out(1,ndx_in:ndx_fin) ne 0,n)
       IF a(0) ne -1 THEN BEGIN
            out_interp(1,i) = total(out(1,ndx_in+a),/double)/double(n)
       ENDIF ELSE BEGIN
            out_interp(1,i) = 0.0       
       ENDELSE
       out_interp(0,i) = TOTAL(out(0,ndx_in:ndx_fin),/double)/double(npi)
    ENDFOR
    a=WHERE(reform(out_interp(1,*)) ne 0.0)
    out_interp=out_interp(*,a)  ; Only get out the good points
END
;--------------------------------------------------------
; To properly concatenate the data by leveling the quarters
;   data1 and data2 format dblarr(2,n) with time and data
;                 cadence='SC' or 'LC'
;
;--------------------------------------------------------
PRO level,data1,data2,data2_out,data_out,cadence,draw=draw,fit=fit,npol=npol

     If not keyword_set(npol) then npol =2 ; Order of the polynomial to fit
     coef = 1.0 ; To change between SC and Long Cadence
     IF cadence eq 'sc' or cadence eq 'Sc' or cadence eq 'SC' THEN coef = 30.
     col = N_ELEMENTS(data1(*,0))   ; Number of columns
     np1=n_elements(data1(0,*))
     np2=n_elements(data2(0,*))
     data_out = dblarr(col,np1+np2)
     
     gd1 = WHERE(finite(data1(1,*)) eq 1,npgd1)
     gd2 = WHERE(finite(data2(1,*)) eq 1,npgd2)
     res1 = data1(1,gd1)
     t1 = data1(0,gd1)-data1(0,gd1(0))
     res2 = data2(1,gd2)
     t2 = data2(0,gd2)-data1(0,gd1(0))  
     np_filt = [10,96,192] * coef
     IF keyword_set(draw) THEN PRINT,'filters times=',np_filt
            np_filt(2)=min([n_elements(reform(res1))-1, np_filt(2)])

             a1=poly_fit(t1(npgd1-np_filt(2)+1:*),res1(npgd1-np_filt(2)+1:*),npol,ff1) ; Fit the end of data1(gd) to 96 points
             ;a2=poly_fit(t2(0:np_filt(1)),res2(0:np_filt(1)),npol,ff2) ; Fit the beginning of data2(gd) to 96 points
            np_filt(0)=min([n_elements(res2)-1, np_filt(0)])
            np_filt(2)=min([n_elements(res2)-1, np_filt(2)])
             res_org1=res2-mean(res2(0:min([np_filt(0)])))+poly(t2(0),a1)  ; using 10 points I remove the average of 2nd part and add the extrapolation of the fit on the first half=> 5 hours data and add the fit
                     np_filt(1)=min([n_elements(reform(res1))-1, np_filt(1)])

             res_org2=res2-mean(res2(0:np_filt(2)))+mean(res1(n_elements(res1)-np_filt(1)+1:*)) ; Using 96 points => just the average on 48 hours.
             
             cof1=poly_fit([t1(npgd1-np_filt(2)+1:*),t2(0:np_filt(2))],[res1(npgd1-np_filt(2)+1:*),res_org1(0:np_filt(2))],2,chisq=chisq1,fit1) 
             cof2=poly_fit([t1(npgd1-np_filt(2)+1:*),t2(0:np_filt(2))],[res1(npgd1-np_filt(2)+1:*),res_org2(0:np_filt(2))],2,chisq=chisq2,fit2) 
             IF keyword_set(fit) THEN chisq1=-1e10   ; If I force to use the fit  
             IF t2(0)-t1(n_elements(t1)-1) gt 10 THEN chisq2=-1e10  ; If the hole is > 10 days do the average
           ;  res2=res_org1
     IF chisq1 LT chisq2 THEN res2=res_org1
     IF chisq2 LE chisq1 THEN res2=res_org2 
     data2_out=data2
     data2_out(1,gd2)=res2
     data_out(0,*)=[reform(data1(0,*)),reform(data2(0,*))]
     data_out(1,*)=[reform(data1(1,*)),reform(data2_out(1,*))]
    ;STOP
     IF keyword_set(draw) THEN BEGIN
             PRINT,'Chisq=',chisq1,chisq2
             !p.multi=[0,1,2]
             PLOT,t1(npgd1-np_filt(1)+1:*),res1(npgd1-np_filt(1)+1:*),/yn,xr=[t1(npgd1-np_filt(1)+1),t1(-1)+20],yr=[res1(npgd1-np_filt(1)+1)*0.99,res1(-1)*1.01]
             oplot,t2(0:np_filt(1)),res2(0:np_filt(1)),col=2     ; Red original fitted region of the second segment
             oplot,t1(npgd1-np_filt(1)+1:*),ff1,col=3   ; green; fit to the first segment
             ;oplot,t2(0:np_filt(1)),ff2,col=3
             oplot,t2,res_org1,col=8
             oplot,t2,res_org2,col=2
             oplot,t2,data2_out(1,0:np_filt(2)),col=4
             STOP
       ENDIF      
 
RETURN
END


;--------------------------------------------------------
; To properly concatenate the data by leveling the quarters but using as reference
;   the points of the second file after start_fit_days
;   data1 and data2 format dblarr(2,n) with time and data
;                 cadence='SC' or 'LC'
;
;--------------------------------------------------------
PRO level_2,data1,data2,data2_out,data_out,cadence,draw=draw
     
     ;mm1=moment(reform(data2(1,*)),/nan,sdev=sigma_org1)
     start_fit_days=1.5    ; number of days that we do not use at the starting of the second segment
     ;IF sigma_org1 gt 7000 THEN start_fit_days=0.0
     npol =5 ; Order of the polynomial to fit
     coef = 1.0 ; To change between SC and Long Cadence
     start_fit=start_fit_days* 48.9390  ; Number of points for LC
     IF cadence eq 'sc' or cadence eq 'Sc' or cadence eq 'SC' THEN BEGIN 
         coef = 30.
         start_fit=start_fit_days* 1468.17  ; Number of points for SC
     ENDIF    
         ;print,sigma_org1,start_fit_days,start_fit
     ;STOP
     col = N_ELEMENTS(data1(*,0))   ; Number of columns
     np1=n_elements(data1(0,*))
     np2=n_elements(data2(0,*))
     data_out = dblarr(col,np1+np2)
     
     gd1 = WHERE(finite(data1(1,*)) eq 1,npgd1)
     gd2 = WHERE(finite(data2(1,*)) eq 1,npgd2)
     res1 = data1(1,gd1)
     t1 = data1(0,gd1)-data1(0,gd1(0))
     res2 = data2(1,gd2)
     t2 = data2(0,gd2)-data1(0,gd1(0))  
     np_filt = [10,96,192] * coef 
     IF keyword_set(draw) THEN PRINT,'filters times=',np_filt

             a1=poly_fit(t1(npgd1-np_filt(1)+1:*),res1(npgd1-np_filt(1)+1:*),npol,ff1) ; Fit the end of data1(gd) to 96 points
             a2=poly_fit(t2(start_fit:np_filt(1)),res2(start_fit:np_filt(1)),npol,ff2) ; Fit the beginning of data2(gd) to 96 points
             
             res_org1=res2-mean(res2(start_fit:start_fit+np_filt(0)))+poly(t2(start_fit),a1)  ; using 10 points I remove the average of 2nd part and add the extrapolation of the fit on the first half=> 5 hours data and add the fit
             ;res_org2=res2-mean(res2(0:np_filt(1)))+mean(res1(n_elements(res1)-np_filt(1)+1:*)) ; Using 96 points => just the average on 48 hours.
             res_org2=res2-mean(res2(start_fit:min([start_fit+np_filt(2),n_elements(res2)-1])))+mean(res1(n_elements(res1)-np_filt(1)+1:*)) ; Using 96 points => just the average on 48 hours.
             
             cof1=poly_fit([t1(npgd1-np_filt(2)+1:*),t2(0:np_filt(2))],[res1(npgd1-np_filt(2)+1:*),res_org1(0:np_filt(2))],2,chisq=chisq1,fit1) 
             cof2=poly_fit([t1(npgd1-np_filt(2)+1:*),t2(0:np_filt(2))],[res1(npgd1-np_filt(2)+1:*),res_org2(0:np_filt(2))],2,chisq=chisq2,fit2) 
             
             IF t2(0)-t1(n_elements(t1)-1) gt 10 THEN chisq2=0  ; If the hole is > 10 days do the average
           ;  res2=res_org1

     IF chisq1 LT chisq2 THEN res2=res_org1
     IF chisq2 LE chisq1 THEN res2=res_org2 
     data2_out=data2
     data2_out(1,gd2)=res2
     data_out(0,*)=[reform(data1(0,*)),reform(data2(0,*))]
     data_out(1,*)=[reform(data1(1,*)),reform(data2_out(1,*))]
    ;STOP
     IF keyword_set(draw) THEN BEGIN
             PRINT,'Chisq=',chisq1,chisq2
             !p.multi=[0,1,2]
             PLOT,t1(npgd1-np_filt(1)+1:*),res1(npgd1-np_filt(1)+1:*),/yn,xr=[t1(npgd1-np_filt(1)+1),t1(-1)+20],yr=[res1(npgd1-np_filt(1)+1)*0.99,res1(-1)*1.01]
             oplot,t2(0:np_filt(1)),res2(0:np_filt(1)),col=2     
             oplot,t1(npgd1-np_filt(1)+1:*),ff1,col=3
             oplot,t2(0:np_filt(1)),ff2,col=3
             oplot,t2,res_org1,col=8
             oplot,t2,res_org2,col=2
             oplot,t2,data2_out(1,0:np_filt(2)),col=4
             STOP
       ENDIF      
 
RETURN
END


;--------------------------------------------------------
; Reading data_file in ASCII format
; ntype => 0: Original data
;                 1: Corrected WG# data (6th Column)
;                  2: Nasa corrected data (PDC)
;--------------------------------------------------------
PRO read_kepler_ascii,name,data,stat,roll_label,no_time_cor=no_time_cor,no_nan=no_nan,type=type,pixel=pixel
    ntype=0
    IF KEYWORD_SET(type) THEN ntype = type
    ntc=1
    IF KEYWORD_SET(no_time_cor) THEN ntc=0
    
    IF not KEYWORD_SET(pixel) THEN BEGIN
            CLOSE,2
            OPENR,2,name
            REPEAT BEGIN
                st='  '
                READF,2,st
                st1=STRMID(st,0,1)  ; Extract the first character
                IF st1 eq '#' THEN BEGIN 
                    IF STRMID(st,2,7) eq 'Version' THEN BEGIN
                       version=strsplit(st,':',/EXTRACT,/REGEX)
                       vers=float(version(1))
                       IF vers ge 2 THEN ntc=0      ; TO AVOID CORRECTING TIME FOR DATA that has already been corrected
                    ENDIF          
                    IF STRMID(st,2,6) eq 'Season' THEN BEGIN 
                       ROLL_st=strpos(st,'Q')   ;Position of Q
                       ROLL_Label=(strmid(st,roll_st))    ; Label
                    ENDIF                
                ENDIF
            ENDREP UNTIL st1 ne '#'
            CLOSE,2
            IF Vers gt 1 THEN BEGIN
               READCOL,name,v1,v2,v3,v4,v5,v6,v7,format='D,D,D,D,D,D,D',/silent, comment='#'
            ENDIF ELSE BEGIN
              ; READCOL,name,v1,v2,v3,v4,format='D,D,D,D',/silent, comment='#'   ; Version 1 only 4 columns
              ; v5=v4
              ; v5[*]=0
            ENDELSE
            time_correction=0.0d0
            ROLL=LONG(STRSPLIT(strupcase(ROLL_Label),'Q',/extract))
            IF  ROLL le 1 and ntc gt 0 THEN BEGIN  
                PRINT,'*********************************************************'
                PRINT,'Reading A Q',STRTRIM(Roll,1), '. Applying Time Correction'
                PRINT,'*********************************************************'
                time_correction=0.5d0 
            ENDIF    
            np=N_ELEMENTS(v1)
            data=DBLARR(5,np)
            data[0,*]=v1+time_correction
            CASE ntype OF
                 0: data[1,*]=v2    ; Raw time series
                 1: data[1,*]=v6    ; New corrected time series
                 2: data[1,*]=v4    ; PDC NASA time series
                 ELSE : STOP,'Bad Reading type'
             ENDCASE       
            data[2,*]=v3
            data[3,*]=v4
            data[4,*]=v5
            IF roll ge 8 THEN BEGIN
                PRINT,'*******************************************'
                PRINT,' ROLL Multiplying by cadence 1625.314 (LC) or 54.18 (SC)'
                PRINT,'*******************************************'
        
               If ntype ne 1 THEN data(1,*)*=54.18      ; TO avoid correcting the "New corrected time series"
               data(3,*)*=54.18
            ENDIF
            IF not keyword_set(no_nan) THEN BAD2NAN,data,bad_ndx=bad_ndx
            stat=N_elements(ndx)
    ENDIF ELSE BEGIN
            READCOL,name,time,data_pix,data_raw,FORMAT='D,D,D'
            np=N_ELEMENTS(time)
            data=DBLARR(5,np)
            data[0,*]=time+54833.0
            data[1,*]=data_pix
            data[3,*]=data_raw
            t_q=q_dates()
            roll=where(data(0,0) gt t_q(*,1))
            roll_label='Q'+strtrim(max(roll)+1,2)
            PRINT,roll_label
    ENDELSE
RETURN
END

;-----------------------------------------------------------
;   Read Files (stars) in a directory
;-----------------------------------------------------------
PRO READ_KEPLER_DIR,path,files,id_stars,wild=wild,prefix=prefix,sufix=sufix,length=length,NotUnique=NotUnique
;READ_KEPLER_DIR,path,files,id_stars,wild='kplr*.jpg',prefix='kplr',length=9    ; for Figs files
;READ_KEPLER_DIR,'/Volumes/RGB_RED_2TB/DATA/KEPLER/PIXEL_DATA/RG_FULL/*',files,id,wild='RESULTS_KADACS_filt_polfitseg2640.00_55.0000d_ppm0_inpaint20/LC_CORR_FILT_INP/kplr*COR_PSD_filt_inp.fits'
;READ_KEPLER_DIR,'/Volumes/RGB_RED_2TB/DATA/KEPLER/PIXEL_DATA/RG_FULL/*',files,id,wild='kplr*COR_PSD_filt_inp.fits'
wd='kplr*'
if keyword_set(wild) then wd=wild
IF not keyword_set(prefix) THEN prefix='kplr'
IF not keyword_set(sufix) THEN sufix='-'
name_stars=FILE_SEARCH(path,wd,count=nstars,/expand_environment) ; all stars
;-------
base_name_stars=file_basename(name_stars)
IF nstars gt 0 THEN BEGIN
        all_id_stars=strarr(nstars)
        id_ext=' '
        ;for i=0l,nstars-1 do all_id_stars(i)=strsplit(base_name_stars(i),'-',/EXTRACT,/REGEX)
        ;for i=0l,nstars-1 do all_id_stars(i)=strsplit(all_id_stars(i),prefix,/EXTRACT,/REGEX)
        for i=0l,nstars-1 do  BEGIN
            id_ext=strsplit(base_name_stars(i),sufix,/EXTRACT,/REGEX)
            all_id_stars[i]=id_ext[0]
        ENDFOR    
        for i=0l,nstars-1 do all_id_stars[i]=strsplit(all_id_stars(i),prefix,/EXTRACT,/REGEX)
        IF keyword_set(length) THEN for i=0l,nstars-1 do all_id_stars[i]=STRMID(all_id_stars(i), 0, Length)
        If not Keyword_set(NotUnique) THEN BEGIN
                id_stars=all_id_stars(UNIQ(all_id_stars,sort(all_id_stars)))    ;id without repeating stars
                ;id_stars=id_stars(0:n_elements(id_stars)-2)
                 files=name_stars(UNIQ(all_id_stars,sort(all_id_stars)))
        ENDIF ELSE BEGIN
                id_stars=all_id_stars(sort(all_id_stars))    ;id without repeating stars
                ;id_stars=id_stars(0:n_elements(id_stars)-2)
                 files=name_stars(sort(all_id_stars))      
        ENDELSE
ENDIF ELSE BEGIN
        files=' '
        id_stars='0'
ENDELSE
RETURN
END

PRO remove_bad_status, data_org, status_flag, bit_flag, data_out, status_out, plot=plot

;---------------------------------------------------------------------------
; REMOVE_BAD_STATUS: to remove points that are flagged 'COARSE POINT',
; 'ARGABRIGHTENING', and 'EXCLUDED' in the original NASA FITS files
;
;INPUTS: - DATA_ORG: original raw data from Kepler
;        - STATUS_FLAG: flag from SAP_QUALITY in the header of the
;          fits file
;        - BIT_FLAG: combination of bit valuescorresponding to known problems
;          in the data that we want to remove:
;          flag for COARSE_POINT=4
;          flag for EARTH POINT=8
;          flag for ARGABRIGHTENING=64
;          flag for excluded point=256
;          flag for LDE OUT OF SYNC = 16384
;
;OUTPUTS: - DATA_OUT: data with the bad points put to NaN
;         - STATUS_OUT: array containing the bit values of the points
;           put to NaN
;
;OPTIONS: - PLOT: plot the original data and overplot the data without
;           the excluded bad points
;
;---------------------------------------------------------------------------

;---------------TO COMMENT IN THE FINAL ROUTINE (beginning)
;res=MRDFITS('/Volumes/My Passport Studio/Kepler_DATA/GKMdwarfs/RAW/kplr002995898-2012088054726_llc.fits', 1, header, columns=['CADENCENO','TIME','TIMECORR','SAP_FLUX','SAP_FLUX_ERR','PDCSAP_FLUX','PDCSAP_FLUX_ERR','SAP_QUALITY']) ;002995898-2012088054726_llc Q12; 002995898-2009259160929_llc Q2
;flux=res.SAP_FLUX
;time=res.TIME;CORR

;status_flag=res.SAP_QUALITY 
;data_org=dblarr(2, n_elements(flux))
;data_org(0,*)=time(*)
;data_org(1,*)=flux(*)
;define the combination of bits for given flags
;bit_flag = 4+64+256
;----------------TO COMMENT IN THE FINAL ROUTINE (end)

data_out=data_org

;-----look for the index where the status_flag is a module of the
;bit_flag corresponding to the points that we want to remove
status_out=reform(status_flag) and bit_flag
bad=where(status_out ne 0, cnt)

;-----put the bad values to NaN in the output data array
if cnt ne 0 then begin

   data_out(1, bad)=!values.f_nan; 0.0
   
endif

if keyword_set(plot) then begin
   plot, data_org(0,*), data_org(1,*), /yn
   good=where(status_out eq 0)
   oplot, data_out(0,good), data_out(1,good), col=2;
   ;ind=where(status_out eq 4)
   ;flux1=flux0
   ;flux1(ind)=0
   ;oplot, time, flux1, col=4
endif

END

; ------------------------------------------------------------------------------
FUNCTION REAL_MOD, time, period
; ------------------------------------------------------------------------------
; Returns the positive remainder in the division of time by period
; ------------------------------------------------------------------------------
; History
; 05/01/2017 - MB - created the function assuming period GT 0.
; ------------------------------------------------------------------------------
    ; time   : positive or negative time
    ; period : orbital period of the binary system
    IF (time GE 0.) THEN BEGIN
        RETURN, time MOD period
    ENDIF ELSE BEGIN
        RETURN, period + (time MOD period)
    ENDELSE
END ; FUNCTION REAL_MOD


; ------------------------------------------------------------------------------
FUNCTION remove_transits, in_arr, period, t_prim, w_prim, w_sec, separation,draw=draw
; ------------------------------------------------------------------------------
; Returns the light curve with NaN values in the data during the transits.
; ------------------------------------------------------------------------------
; History
; 10/01/2017 - MB  - created the function from the current version of test_unfold
; 13/01/2017 - RAG - Remove KIC as not used in the routine
;                  - Change variables to arrrays
; ------------------------------------------------------------------------------
    ; in_arr     : Raw data. 2-column array containing time (in days) and flux
    ; period(0)     : Orbital period(0) of the binary (in days)
    ; t_prim(0)     : Instant of the primary transit (BJD)
    ; w_prim(0)     : Primary transit width (in units of period(0))
    ; w_sec(0)      : Secondary transit width (in units of period(0))
    ; separation(0) : separation(0) between the transits (in units of period(0))
; ------------------------------------------------------------------------------
    draw_flag = 0
    If keyword_set(draw) THEN draw_flag = 1
    ; Output array
    out_arr = in_arr
    ; Time array from the beginning modulo the period(0)
    t_fold_arr = (in_arr(0,*) - in_arr(0,0)) MOD period(0)

    IF draw_flag THEN PLOT, t_fold_arr, out_arr(1,*), PSYM = 3, /YN


    ; Primary transit
    ; ---------------
    ; Calculate the times of the transit modulo the period(0)
    t_prim_cut     = REAL_MOD(t_prim(0) - in_arr(0,0)             , period(0)) ; Transit centre
    t_prim_cut_ini = REAL_MOD(t_prim_cut - w_prim(0) * period(0) / 2., period(0)) ; Beginning of the transit
    t_prim_cut_fin = REAL_MOD(t_prim_cut + w_prim(0) * period(0) / 2., period(0)) ; End of rhe transit

    PRINT, 'period(0)              : ', period(0)
    PRINT, 'Central transit     : ', t_prim_cut
    PRINT, 'Transit time limits : ', t_prim_cut_ini, t_prim_cut_fin

    ; Determine the indices corresponding to the transits
    IF (t_prim_cut_ini LT t_prim_cut_fin) THEN BEGIN ; General case
        ind_prim = WHERE (t_fold_arr GE t_prim_cut_ini AND t_fold_arr LT t_prim_cut_fin)
    ENDIF ELSE BEGIN
        ind_prim = WHERE (t_fold_arr GE t_prim_cut_ini OR t_fold_arr LT t_prim_cut_fin)
    ENDELSE
;STOP
    IF draw_flag THEN OPLOT, t_fold_arr(ind_prim), out_arr(1,ind_prim), PSYM = 2, COLOR = 2

;STOP
    ; Convert transit points into NaN
    out_arr(1,ind_prim) = !VALUES.F_NAN

    ; Secondary transit
    ; -----------------
    IF (w_sec(0) GT 0.0) THEN BEGIN ; If the secondary transit is above 3 * sigma
        ; Calculate the times of the transits modulo the period(0)
        t_sec_cut     = REAL_MOD(t_prim_cut + separation(0) * period(0), period(0)) ; Transit centre
        t_sec_cut_ini = REAL_MOD(t_sec_cut - w_sec(0) * period(0) / 2. , period(0)) ; Beginning of the transit
        t_sec_cut_fin = REAL_MOD(t_sec_cut + w_sec(0) * period(0) / 2. , period(0)) ; End of the transit

        PRINT, '(Sec) Central transit     : ', t_sec_cut
        PRINT, '(Sec) Transit time limits : ', t_sec_cut_ini, t_sec_cut_fin

        ; Determine the indices corresponding to the transits
        IF (t_sec_cut_ini LT t_sec_cut_fin) THEN BEGIN ; General case
            ind_sec = WHERE (t_fold_arr GE t_sec_cut_ini AND t_fold_arr LT t_sec_cut_fin)
        ENDIF ELSE BEGIN
            ind_sec = WHERE (t_fold_arr GE t_sec_cut_ini OR t_fold_arr LT t_sec_cut_fin)
        ENDELSE
        stp = ''
        IF draw_flag THEN BEGIN 
            OPLOT, t_fold_arr(ind_sec), out_arr(1,ind_sec), PSYM = 2, COLOR=2
            READ,'Press a key to continue', stp
        END
        ; Convert transit points into NaN
        out_arr(1,ind_sec) = !VALUES.F_NAN

    ENDIF
    ;OPLOT, t_fold_arr, out_arr(1,*), PSYM = 3, COLOR = 2
    IF draw_flag THEN PLOT, out_arr(0,*), out_arr(1,*), PSYM = 3, /YN

    RETURN, out_arr
END ; FUNCTION REMOVE_TRANSITS

;-----------------------------------------------------------
;  Read & Concat a file of a directory
;  type =>  0: Original data
;                 1: Corrected WG# data (6th Column)
;                 2: Nasa corrected data (PDC)
;                  3: Original data
;   public  -> NASA Public FITS files
;   filevers => give back the file version of the PDC software => >2.1 PDC-MAP
; Example:
;           PDC-MAP:
;    READ_CONCAT_KEPLER,path,wild,data,time_inic=time_inic,roll_all=roll_all,nrolls=nrolls,/public,/pdc,/nojump
;-----------------------------------------------------------
PRO READ_CONCAT_KEPLER,path,wild,data,silent=silent,time_inic=time_inic,no_nan=no_nan,type=type,nojump=nojump,roll_all=roll_all,nrolls=nrolls,public=public,pdc=pdc,pixel=pixel,filevers=filevers,pol_fit_segment=pol_fit_segment,status=status,name_files=name_files,htab0=htab0,bit_flag=bit_flag,binary=binary ;,std_name_files=std_name_files
;print,path, wild
    htab0 =''
    njump = 0
    ntype = 1
    silence = 0
    roll_all = ' '
    bin_FLAG = 0      
    htabKepler = ''
    STATUS_ORG = 0
    IF Keyword_set(binary) THEN bin_flag = 1    ; Flag to check for known binarity in the Kepler Villanova/Mansour catalogue
    IF keyword_set(nojump) THEN njump=1
    IF keyword_set(type) THEN ntype=type
    if keyword_set(silent) THEN silence=1
    IF not Keyword_set(bit_flag) THEN bit_flag =   32 + 64 + 4096; + 4 
    IF not keyword_set(pol_fit_segment) THEN pol_fit_segment=0
    IF not keyword_set(pixel) THEN pixel = 0
    IF not keyword_set(name_files) THEN  name_files = FILE_SEARCH(path,wild,/expand_environment) ; all stars
    If ntype eq 3 then ntype=0

    nfiles = N_ELEMENTS(name_files)
    nrolls = nfiles
    time_inic = DBLARR(nfiles)
    ;PRINT,name_files
    ;sigm=dblarr(ntarg,nfiles)
        FOR j = 0l,nfiles-1 DO BEGIN   
             ; print,j,nfiles, name_files(j)
              If silence eq 0 THEN !p.multi=[0,1,2]
              name = name_files(j)
              If silence ne 1 THEN  PRINT,'Name=',name
              id   = STRMID(file_basename(name),4,9)                  
              fits = STRPOS(name,'fits')
              IF fits lt 0 THEN BEGIN
                     read_kepler_ascii,name,temp,stat,roll_label,type=ntype,no_nan=no_nan,pixel=pixel  ; Corrected data  
                     roll_all = roll_all+roll_label+' '                
              ENDIF ELSE BEGIN
                     IF not KEYWORD_SET(public) THEN BEGIN
                            temp = readfits(name)          
                     ENDIF ELSE BEGIN
                            ;PRINT,'name',name
                            read_kepler_fits,name,dstruc,pixel=pixel,htab0=htab0
                            ;print,'File_version of PDC=',dstruc.fileversion
                            temp=DBLARR(2,N_ELEMENTS(dstruc.time))
                            temp[0,*]=DOUBLE(dstruc.time)
                            IF not keyword_set(pdc) THEN BEGIN
                                    temp[1,*]=DOUBLE(dstruc.flux)
                                    ;back=dstruc.SAP_BKG

                            ENDIF ELSE BEGIN
                                    temp[1,*]=DOUBLE(dstruc.pdc_flux)
                            ENDELSE
                            roll_label=strtrim(dstruc.quarter,2)
                            roll_all=roll_all+strtrim(dstruc.quarter,2)+' '                                
                     ENDELSE
              ENDELSE
              data_org = temp(0:1,*)      ;  We use only the 2 first columns
 ;            data_org_no_bin = temp(0:1,*)      ;  We use only the 2 first columns
 ;             data_org = data_org_no_bin
              ;--------------------------
              ; --- Checking Binarity
              ;--------------------------
;plot,data_org(0,*),data_org(1,*),/yn,tit=roll_label
;stop,'KADACS'
              IF BIN_FLAG THEN BEGIN
                out_bin = remove_transits(data_org, Binary.Period, Binary.Time_Inic, Binary.PWidth, Binary.SWidth, Binary.Separation)
                data_org = out_bin 
              ENDIF  

              ;--------------------------
              ; ---CHECKING BIT STATUS
              ;--------------------------
              b_flag=  32 + 64 + 4096 ; Reaction wheel desaturation event; Argabrightening; 
              
              IF roll_label eq '12' or roll_label eq '16' THEN b_flag=bit_flag
              status_flag = dstruc.sap_quality  ; Array with the status of each point 
              a = WHERE(status_flag eq 393216.00)   ; TO avoid problem in the flags
              IF a(0) gt -1 THEN status_flag(a) = 0.0                       
              remove_bad_status, data_org, status_flag, b_flag, OUT_STATUS, status_out
              data_org = OUT_STATUS

              gd=WHERE(finite(data_org(1,*)) eq 1)  ; Indexes of good points
  ;            IF BIN_FLAG THEN data_org_no_bin = data_org_no_bin(*,gd)
              If silence eq 0 THEN plot,data_org(0,*),data_org(1,*),/yn
              in = [data_org(0,gd)-data_org(0,gd(0)),data_org(1,gd)]
              np_file = n_elements(data_org(0,*))
              If silence eq 0 THEN PRINT,'Q',j,' Length=',data_org(0,np_file-1)-data_org(0,0)
              time_inic(j) = data_org(0,0)
              out = in
              ;-----
              ; DIGITAL=0
              IF keyword_set(pol_fit_segment) THEN BEGIN
                  If pol_fit_segment lt 48 THEN BEGIN
                      a = polfit(out(0,*),out(1,*),pol_fit_segment,fit)  
                  ENDIF ELSE BEGIN
                      fit=trismooth(out,pol_fit_segment)       
                  ENDELSE  
                  out(1,*)=out(1,*)/fit*mean(fit)                     
  ;                IF BIN_FLAG THEN data_org_no_bin(1,gd) = data_org_no_bin(1,gd)/fit*mean(fit)
              ENDIF
              data_org(1,gd)=out(1,*)
           
              IF silence eq 0 THEN plot,data_org(0,*),data_org(1,*),/yn
       
              IF j eq 0 THEN BEGIN
                  data   = data_org
                  STATUS = STATUS_FLAG
                  median_first = median(data_org(1,*))
              ENDIF ELSE BEGIN
                  temp1  = status
                  STATUS = [temp1,STATUS_FLAG]
                  temp = data
                  np1  = N_ELEMENTS(temp(0,*))
                  np2  = N_ELEMENTS(data_org(0,*))
                  data = DBLARR(2,np1+np2)
                  ;--------------------------------------
                  ; Level corrections
                  ;--------------------------------------
                  IF njump ge 1 THEN BEGIN
                      np_dt   = 29.4244*60./86400.
                      cadence = 'LC'
                      IF keyword_set(public) THEN BEGIN
                                IF dstruc.obsmode eq 'short cadence' THEN BEGIN
                                        np_dt   = 58.85/86400.
                                        cadence = 'SC'
                                ENDIF                               
                      ENDIF ELSE BEGIN
                                IF median(data_org(0,1:*)-data_org(0,0:*))*24*60. lt 25 THEN BEGIN
                                        np_dt   = 58.85/86400.
                                        cadence = 'SC'
                                ENDIF        
                      ENDELSE      

                      mm1 = moment(reform(data_org(1,*)),/nan,sdev=sigma_org1)
                        
                      ;STOP,'before_levels'
                      sigma_org1 = 10000
                        
                      If pol_fit_segment lt 48 THEN BEGIN
                              IF sigma_org1 gt 7000 THEN BEGIN
                                   ; print,'level'
 ;                                   IF BIN_FLAG THEN BEGIN
 ;                                     level,temp,data_org,data2_out,out,cadence
 ;                                     data_org_no_bin(1,gd) = 
 ;                                   ENDIF
                                    level,temp,data_org,data2_out,out,cadence   ;     ,/fit  ,/draw
                              ENDIF ELSE BEGIN                          
                                   ; print,'level2'
                                    level_2,temp,data_org,data2_out,out,cadence,/draw  ;/draw   ;concatenate the data by leveling the quarters but using as reference                                                                                                                        ; the points of the second file after start_fit_days
                              ENDELSE
                                 data_org=data2_out
                      ENDIF ELSE BEGIN
                                  data_org(1,*)=data_org(1,*)/median(data_org(1,*))*median_first
                      ENDELSE
                  ENDIF
                  ;-------------------------------------
                 
                  data(*,0:np1-1)=temp
                  data(*,np1:*)=data_org
              ENDELSE                                                             
        ENDFOR   
;STOP
        indx=sort(data(0,*))
        data1=data
        data1(0,*)=data(0,indx)
        data1(1,*)=data(1,indx)
        data=data1
;STOP
        ;------Checking negative fluxes
        mini=min(data(1,*),/nan)
        IF mini le 0 then data(1,*) = data(1,*) + abs(mini)+1.0
        ;IF pixel THEN read_kepler_fits,std_name_files(0),dstruc_std,htab0=htabKepler
        IF n_elements(htabKepler)  gt 1 THEN htab0=htabKepler
;plot,data(0,*),data(1,*),/yn,tit=roll_label
;stop,'KADACS1'
RETURN
END

;----------------------------------------------------------------
;   READ KEPLER PUBLIC FITS FILES
;----------------------------------------------------------------
PRO read_kepler_fits,infile,data,pixel=pixel,htab0=htab0   ;, infile,data,roll_label,no_nan=no_nan,type=type

;infile='/Users/rgarcia/DATA/KEPLER/Solar_like/LONG_CADENCE/PIXELS/kplr007341231-2009350155506_llc.fits'
cadence=1625.3514d0
bjdref=0.d0
;fits_info,infile,n_ext=N_ext,/silent
;IF N_ext ge 1 THEN BEGIN
            
             fits_read,infile,tab0,htab0,exten_no=0
             fits_read,infile,tab1,htab1,exten_no=1
             bjdrefi=double(sxpar(htab0,'BJDREFI'))
             bjdreff=double(sxpar(htab0,'BJDREFF'))
             bjdref=bjdrefi+bjdreff

            IF NOT KEYWORD_SET(pixel) THEN BEGIN
                     bjdrefi=double(sxpar(htab0,'BJDREFI'))
                     bjdreff=double(sxpar(htab0,'BJDREFF'))
                     bjdref=bjdrefi+bjdreff

                    test=sxpar(htab0,'FILEVER')
                    FILEVERSION=test        ; If >2.1 PDCSAP_FLUX is PDC_MAP
                    IF DOUBLE(strtrim(test,2)) ge 2.0 THEN version=2
                    IF test EQ 0 THEN version=1
                    table=tab1
                    IF version EQ 1 THEN BEGIN
                            ;STOP,'SOFTWARE FILE VERSION !, WHAT SHOULD I DO ????, (STOP)'
                             quarter=sxpar(htab1,'QUARTER')
                             module=sxpar(htab1,'MODULE')
                             output=sxpar(htab1,'OUTPUT')
                             channel=sxpar(htab1,'CHANNEL')
                             res=MRDFITS(infile,1,header,columns=['cadence_number','barytime','ap_raw_flux','ap_raw_err'],/silent)          
                             lc_cad_o=res.cadence_number
                             lc_date_o=res.barytime
                             lc_flux_o=res.ap_raw_flux / 1625.3514d0  ;convert to e-/s
                             lc_err_o=res.ap_raw_err / 1625.3514d0  ;convert to e-/s
                    ENDIF

                    IF version EQ 2 THEN BEGIN
                             quarter=sxpar(htab1,'QUARTER')
                             module=sxpar(htab1,'MODULE')
                             output=sxpar(htab1,'OUTPUT')
                             channel=sxpar(htab1,'CHANNEL')
                             KEPLERID=sxpar(htab0,'KEPLERID')
                             DATA_REL=sxpar(htab1,'DATA_REL')    ; DATA RELEASE notes
                             KEPMAG=sxpar(htab1,'KEPL_MAG')       ; Kepler Magnitude
                             OBSMODE=sxpar(htab1,'OBSMODE')     ; OBSMODE = 'long cadence'       / observing mode 
                             UNITS_FLUX=sxpar(htab1,'TUNIT4')      ; Units of SAP_FLUX ("e-/s")
                             LC_START=sxpar(htab1,'LC_START')
                             LC_END=sxpar(htab1,'LC_END')
                             ; Other values in the header: LOGG, TEFF
                             bjdrefi=double(sxpar(htab1,'BJDREFI'))
                             bjdreff=double(sxpar(htab1,'BJDREFF'))
                             bjdref=bjdrefi+bjdreff
                             ;timeslice=double(sxpar(htab1,'TIMSLICE')) 
                             ;time_slice_correction = (DOUBLE(0.25) + double(0.62)*double(5.0d0-timeslice))/86400.d0
                             ;res=MRDFITS(infile,1,header,columns=['CADENCENO','TIME','TIMECORR','SAP_FLUX','SAP_FLUX_ERR','PDCSAP_FLUX','PDCSAP_FLUX_ERR','SAP_QUALITY','SAP_BKG','SAP_BKG_ERR'],/silent)
                             res=MRDFITS(infile,1,header,columns=['CADENCENO','TIME','TIMECORR','SAP_FLUX','SAP_FLUX_ERR','PDCSAP_FLUX','PDCSAP_FLUX_ERR','SAP_QUALITY'],/silent)

                             ;PRINT,'Start=',LC_START,' END=',LC_END
                             ; Put NaN in all timing cadences in which the time is less than the first good time
                             temp=res.time
                             a=WHERE(finite(temp) eq 1)
                             b=where(temp(a) lt temp(a(0)))
                             if b(0) gt -1 THEN temp(a(b)) = !values.f_nan                                
                             ;------------------------------------------
                             TIME_BJD=DOUBLE(temp) + bjdref -240d4   ;- res.timecorr + time_slice_correction     
                             FLUX=res.sap_flux  ;/ 1625.3514  ;convert to e-/s
                             FLUX_ERR=res.sap_flux_err  ; / 1625.3514  ;convert to e-/s      
                             PDC_FLUX=res.pdcsap_flux
                             PDC_FLUX_ERR=res.pdcsap_flux_err
                             SAP_QUALITY=res.SAP_QUALITY
                             ;SAP_BKG=res.SAP_BKG
                             ;SAP_BKG_ERR=res.SAP_BKG_ERR
                             
                             DATA={quarter: quarter, module:module, output:output,channel:channel,id: KEPLERID, DATA_REL:DATA_REL,    $
                             KEPMAG:KEPMAG, OBSMODE:OBSMODE, time:TIME_BJD, FLUX: FLUX, FLUX_ERR:FLUX_ERR, PDC_FLUX:PDC_FLUX, PDC_FLUX_ERR:PDC_FLUX_ERR, FILEVERSION:FILEVERSION, SAP_QUALITY:SAP_QUALITY}        ;,SAP_BKG:SAP_BKG,SAP_BKG_ERR:SAP_BKG_ERR}
            ;PRINT,infile
                        ENDIF
            ENDIF ELSE BEGIN    ; PIXEL DATA
                             bjdrefi=double(sxpar(htab1,'BJDREFI'))
                             bjdreff=double(sxpar(htab1,'BJDREFF'))
                             bjdref=bjdrefi+bjdreff
                             quarter=sxpar(htab0,'QUARTER')
                             module=sxpar(htab0,'MODULE')
                             ;output=sxpar(htab1,'OUTPUT')
                             ;channel=sxpar(htab1,'CHANNEL')
                             KEPLERID=sxpar(htab0,'KEPLERID')
                             DATA_REL=sxpar(htab0,'DATA_REL')    ; DATA RELEASE notes
                             KEPMAG=sxpar(htab0,'KEPL_MAG')       ; DATA RELEASE notes
                             OBSMODE=sxpar(htab0,'OBSMODE')     ; OBSMODE = 'long cadence'       / observing mode 
                             ;UNITS_FLUX=sxpar(htab1,'TUNIT4')      ; Units of SAP_FLUX ("e-/s")
                             ;LC_START=sxpar(htab1,'LC_START')
                             ;LC_END=sxpar(htab1,'LC_END')
                              res=MRDFITS(infile,1,header,columns=['TIME','FLUX','FLUX_ERR','FLUX_KEPMASK','FLUX_KEPMASK_ERR','Quality'],/silent)
                            ;  'TIMECORR','SAP_FLUX','SAP_FLUX_ERR','PDCSAP_FLUX','PDCSAP_FLUX_ERR','SAP_QUALITY'],/silent)
                            ; Put NaN in all timing cadences in which the time is less than the first good time
                             temp=res.time
                             a=WHERE(finite(temp) eq 1)
                             b=where(temp(a) lt temp(a(0)))
                             if b(0) gt -1 THEN temp(a(b)) = !values.f_nan     
                             ;------------------------------------------ 
                                 TIME_BJD=DOUBLE(temp) + bjdref -240e4   ;- res.timecorr + time_slice_correction       
                                 FLUX=res.flux  ;/ 1625.3514  ;convert to e-/s ; NEW MASK
                                 FLUX_KEPMASK=res.FLUX_KEPMASK  ; Original Kepler Mask
                                 nnpp=N_elements(flux)
                                 FLUX_err=replicate(0,nnpp)
                                 FLUX_KEPMASK_ERR=replicate(0,nnpp)
                                 SAP_QUALITY=res.QUALITY
                                 IF n_elements(tag_names(res)) gt 3 THEN BEGIN
                                    FLUX_ERR=res.flux_err  ; / 1625.3514  ;convert to e-/s        
                                    FLUX_KEPMASK_ERR=res.FLUX_KEPMASK_ERR
                                 ENDIF ELSE BEGIN
                                   IF obsmode eq 0 THEN Obsmode='long cadence'
                                 ENDELSE
         
                                 DATA={quarter: quarter, module:module,id: KEPLERID, DATA_REL:DATA_REL,    $
                                 KEPMAG:KEPMAG, OBSMODE:OBSMODE, time:TIME_BJD, FLUX: FLUX, FLUX_ERR:FLUX_ERR, FLUX_KEPMASK:FLUX_KEPMASK, FLUX_KEPMASK_ERR:FLUX_KEPMASK_ERR,SAP_QUALITY:SAP_QUALITY} 
            ENDELSE

; ENDIF ELSE BEGIN
;        PRINT,'BAD FILE IN QUARTER'
; ENDELSE
                 
END

PRO read_rg_psd,id,freq,pot,path=path,filter=filter,silent=silent
        If not keyword_set(path) then path='/Users/rgarcia/DATA/KEPLER/RG/CORR/'
        IF not keyword_set(filter) then filter=48.
        wild='*'+id+'*'
        READ_CONCAT_KEPLER,path,wild,data,/silent
        ppm,data,out,dg_pol=filter
        psd,out,freq,pot,ofac=1
        If not keyword_set(silent) THEN BEGIN
            !p.multi=[0,1,3]
            plot,data(0,*),data(1,*),/yn
            plot,out(0,*),out(1,*),/yn
            plot_oo,freq,pot,/yn
        ENDIF
END

;-----------------------------------------------------------------
;  Spikes
;       Do a 2nd sigma clipping using a triangular smooth of 1 day
;-----------------------------------------------------------------
 PRO spikes,data,threshold=threshold,stat_correc=stat_correc,ns=ns,filt=filt
 if not keyword_set(filt) THEN filt=1440
    
    IF not KEYWORD_SET(threshold) THEN threshold=6
    gd=WHERE(finite(data(1,*)) eq 1)
    in=[data(0,gd)-data(0,gd(0)),data(1,gd)]
    time=86400.*reform(data(0,*)-data(0,0))
    dt=60.
    res=reform(data(1,*))
    np_interp=long(max(time)/dt)
    t=lindgen(np_interp)*60.
    res_int=interpol(res(gd),time(gd),t)
    fres_int=smooth(smooth(res_int,filt,/edge_truncate),filt,/edge_truncate)   ; TRIANGULAR SMOOTH
    fres=interpol(fres_int,t,time)
    ffres=1.d6*reform((res(gd)/fres(gd))-1)             
    ;plot,ffres,/yn,col=0
    a=where(abs(ffres) gt threshold *median(abs(ffres)),stat)
    out=reform(data(1,*))
    If stat gt 0 THEN BEGIN
      PRINT,'----------------------------'
      PRINT,'***Extra clipping points:',stat
      PRINT,'----------------------------'
      ask=1
       ;READ,'Do you want to apply this (1=yes)?',ask
       IF ask eq 1 THEN BEGIN
                PRINT,'Doing correction'
               ;stat_correc(2,ns)=stat_correc(2,ns)+stat
               data(1,gd(a))=!values.f_nan   

       ENDIF        
    ENDIF    
 RETURN
 END



PRO test_quarters,kic,out=out

PATH0='/Users/rgarcia/DATA/KEPLER/Solar_like/KASOC_ASCII/Q0*'
PATH1='/Users/rgarcia/DATA/KEPLER/Solar_like/KASOC_ASCII/Q1*'

READ_KEPLER_DIR,path0,files1,id_stars1                    ; Recover the identifiers in Q0*
READ_KEPLER_DIR,path1,files2,id_stars2                    ; Recover the identifiers in Q1*

all_id_stars=[id_stars1,id_stars2]
id_stars=all_id_stars(UNIQ(all_id_stars,sort(all_id_stars)))    ;id without repeating stars



RETURN
END



PRO KADACS
        PRINT,'*****************************************************************************'
        PRINT,' COMPILED: Kepler Asteroseismic Data Correction and Analysis Software (KADACS) package' 
        PRINT,'*****************************************************************************'
RETURN
END

PRO verif_star_quarter,id,quarters
    path_in='/Users/rgarcia/DATA/KEPLER/Solar_like/KASOC_ASCII/Q'
    path_in1='/Users/rgarcia/DATA/KEPLER/Solar_like/KASOC_ASCII/'
    
    qinic = 0
       names = file_basename(file_search(path_in+'*'))
       qfin = LONG(STRMID(names(n_elements(names)-1),1,2))  
    quarters = ''
    FOR i=qinic,n_elements(names)-1 DO BEGIN
       path=path_in1+names(i)+'/'
       name_file=''
      ; STOP
       name_file = file_basename(file_search(path,'kplr*'+strtrim(id,2)+'*.dat'))
       ;print,name_file(0),' ',names(i)
       If name_file(0) ne '' Then quarters=quarters+' '+names(i)
    ;STOP
    ENDFOR
    ;print,quarters
END

PRO stars_in_quarters,id,qinic=qinic,qfin=qfin,q0=q0;,file_out=file_out
    path_in='/Users/rgarcia/DATA/KEPLER/Solar_like/KASOC_ASCII/Q'
    path_out='/Users/rgarcia/WORK/ASTROSIS/KEPLER/SL/LISTS_STARS/'
    IF not keyword_set(q0) THEN q0 = 0
    IF not keyword_set(qinic) THEN BEGIN 
           qinic = 1
           If Keyword_set(q0) THEN qinic = 0
    ENDIF
    IF not keyword_set(qfin) THEN BEGIN
           names = file_basename(file_search(path_in+'*'))
           qfin = LONG(STRMID(names(n_elements(names)-1),1,2))  
    ENDIF
;    IF not keyword_set(file_out) THEN BEGIN
;            file_out = 'LIST_STARS_Q' + STRTRIM(qinic,2) + '_Q' + STRTRIM(Qfin,2)
;    ENDIF   
    ; MAIN LOOP
    count = 0
    FOR i = qinic,qfin DO BEGIN
           IF i le 9 THEN BEGIN
                suffix = '0'+STRTRIM(i,2)
           ENDIF ELSE BEGIN
                suffix = STRTRIM(i,2)
           ENDELSE
           path=path_in+suffix+'*/'
           PRINT,'LOOKING into ',path
           READ_KEPLER_DIR,path,files,id,wild='*.dat'
           IF count eq 0 THEN BEGIN
                    id_stars=id
           ENDIF ELSE BEGIN
                   id_stars=[id_stars,id]
           ENDELSE        
           
           count = count + 1
    ENDFOR
    id=id_stars(UNIQ(id_stars,sort(id))) 
END

PRO window_function, data, out,outd=outd,plot=plot
    gd = WHERE(finite(data(1,*)) eq 1,ngd)
    outw = DBLARR(2,ngd)
    outw(0,*) = data(0,gd)
    outw(1,*) = 1        ; data(1,gd)
    regular_grid,outw,out_reg
    out_reg(0,*)=out_reg(0,*)*86400.
    np=N_ELEMENTS(out_reg(0,*))
    dt=out_reg(0,2)-out_reg(0,1)
   ; a = WHERE(out_reg(1,*) ne 0)
    mom=moment(out_reg(1,*),/double,mean=mn)
    out_reg(1,*) = out_reg(1,*)-mn
    STOP
    fft_sola,np,dt,reform(out_reg(1,*)),freq,pow
    out = DBLARR(2,N_ELEMENTS(freq))
    out(0,*) = freq
    out(1,*) = Pow
    If keyword_set(plot) THEN BEGIN 
         plot_oo,out(0,*)*1e6,out(1,*),xr=[1e-2,300],xst=1
         psd,data,freq,pow
        plot_oo,freq*1e6,pow,xr=[1e-1,300],xst=1,background=255,col=1
        oplot,out(0,*)*1e6,out(1,*),col=4
    ENDIF     
END

pro phase_diagram, time, flux, period, b=b, plot=plot, nlevels=nlevels, coef_smooth=coef_smooth, title=title

  if not keyword_set(nlevels) then nlevels=255
  
  t=time-time[0]
  dt=median(t[1:*]-t[0:*])
  f=flux
  ngd=where(finite(f) ne 1)
  f[ngd]=0.
  Nx=ceil(period/dt)
  Ny=ceil(t[-1]/period)
  b= FLTARR(Nx,Ny)
  
  FOR i=0,Ny-1 DO begin
     ind=where((t ge i*period) and (t lt (i+1)*period))
     b[0:min([n_elements(ind),Nx])-1,i]=f[ind[0:min([n_elements(ind),Nx])-1]]
  endfor

  if keyword_set(coef_smooth) then b=smooth(b, coef_smooth, /edge_truncate)
  
  if keyword_set(plot) then $
     contour, b, indgen(Nx)*dt, indgen(Ny)*period, nlevels=nlevels, $
              /fill, /ys, /xs, xtitle='Time modulo '+strtrim(period,2), ytitle='Time', $
              background=-1, color=1, title=title
  
end

PRO bd_points_kepler,data,out,sz_gaps_max,interpol=interpol
            bd_intp = WHERE(finite(data(1,*)) eq 0)
            gd = WHERE(finite(data(1,*)) eq 1)
            data_gd=data(*,gd)
            gaps=data_gd(0,1:*)-data_gd(0,*)
            ndx_holes = where(gaps gt dt and gaps lt sz_gaps_max)   ; indexes
            IF bd_intp(0) ne -1 THEN BEGIN                      
                  Y_intp = INTERPOL(reform(data(1,gd)),reform(data(0,gd))-data(0,0),reform(data(0,bd_intp))-data(0,0),/spline )   
                 data(1,bd_intp)=Y_intp
            END
END





;------------------------------------------------------------------------------------------
; READ_KADACS
;------------------------------------------------------------------------------------------
;   IDL Routine to read FITS files containing LIGHT CURVES OR PSD 
;------------------------------------------------------------------------------------------
;   EXAMPLE:
;------------------------------------------------------------------------------------------
;           READ LIGHT CURVE with a 1 day limit of the interpolated gaps:
;                            READ_KADACS,file,data,t=t,flux=flux,header=header,wdw=wdw,inpaint_limit=1
;           READ a PSD file:
;                            READ_KADACS,file,data,header=header,\psd,freq=freq,pow=pow
;------------------------------------------------------------------------------------------
;   Parameters:
;               file                  : Name of the input file (including path)
;               data                : 2 dimensional array containing (time,flux) or (Frequency, power) 
;  Optional Parameters:
;               t                       : Array containing Time (for the light curve fits files)
;               flux                  : Array containing the flux (for the light curve fits files)
;               header             : Header of the FITS file
;               wdw                 : Window function (0: No data, 1: Kepler data, 2: Interpolated data)
;               inpaint_limit     : Size of the maximum interpolated gaps (in days)
;               no_interp          : If you do not want any interpolated data
;               psd                   : In case the file to read already contains a PSD
;               freq                  : Array containing the frequencies
;               pow                  : Array containing the power
;-------------------------------------------------------------------------------------------

PRO READ_KADACS,file,data,t=t,flux=flux,header=header,wdw=wdw,inpaint_limit=inpaint_limit,no_interp=no_interp,psd=psd,freq=freq,pow=pow
     IF N_params() LT 1 THEN BEGIN
          PRINT,'Syntax -  READ_KADACS,file,data'
          PRINT,'              [t=t,flux=flux,header=header,wdw=wdw,inpaint_limit=inpaint_limit,no_interp=no_interp,psd=psd,freq=freq,pow=pow]' 
          RETURN
      ENDIF

    data = READFITS(file,header)
    IF not KEYWORD_SET(psd) THEN BEGIN
           wdw = readfits(file,EXTEN_NO=1)
           IF KEYWORD_SET(no_interp) THEN BEGIN
                   IF wdw(0) eq -1 THEN BEGIN
                         PRINT,'There is no information on the inerpolated window function'
                         PRINT,'The program is stopped'
                         PRINT,'--------------------------------------------------'
                         STOP
                    ENDIF    
                    indx = WHERE(wdw eq 2)
                    data(1,indx) = 0.0
           ENDIF ELSE BEGIN
                   IF KEYWORD_SET(inpaint_limit)  THEN BEGIN      ; To limit the length of the interpolated points
                            IF wdw(0) eq -1 THEN BEGIN
                                 PRINT,'There is no information on the inerpolated window function'
                                 PRINT,'The program is stopped'
                                 PRINT,'--------------------------------------------------'
                                 STOP
                            ENDIF    
                            dt = Median(data(0,1:*)-data(0,*))
                            limit_points = Inpaint_limit/dt
                            indx=where(wdw eq 1) ; Indexes of good points
                            gaps = indx(1:*)-indx(*)
                            indx_big_gaps = WHERE(gaps gt limit_points,nn)
                            IF nn gt 0 THEN BEGIN
                                FOR i=0l,nn-1 DO BEGIN
                                    indx1 = indx( indx_big_gaps(i) )+1          ; Low limit of the big gap
                                    indx2 = indx( indx_big_gaps(i) +1)-1      ; High limit of the big gap
                                    data(1,indx1:indx2) = 0.0                        ; Points in the mean gaps are set to zero             ; 
                                ENDFOR
                            ENDIF
                   ENDIF
            ENDELSE      
            gd = WHERE(data(1,*) ne 0.0,nn)
            IF nn gt 0 THEN BEGIN
                mean_value = mean(data(1,gd))                ; Median value of the good points
                data(1,gd)=data(1,gd)-mean_value           ; The time series should have a zero mean
            ENDIF
            t = REFORM(data(0,*))
            flux = REFORM(data(1,*))
    ENDIF ELSE BEGIN        ; IF the file to be read is a PSD
            freq = REFORM(data(0,*))
            pow = REFORM(data(1,*))
    ENDELSE
END

PRO copy_files,path_in, path_out,id1,id2,wild=wild
    ;copy_files,'/Volumes/Data_3To/DATA/KEPLER/GKM_Dwarfs/RAW/','/Volumes/RGB_2Tb/DATA/KEPLER/GKM_Dwarfs/RAW3/',9700032,12984422
    IF strmid(path_in,0,1,/reverse_offset) ne '/' THEN path_in=path_in+'/'
    IF strmid(path_out,0,1,/reverse_offset) ne '/' THEN path_out=path_out+'/'
    ;wild='kplr*.fits'
    IF not KEYWORD_SET(wild) THEN wild='kplr*.fits'
    PRINT,'Reading files in the directory'
    READ_KEPLER_DIR,path_in,files,id,wild=wild
    idd=LONG(id)
    files_all=file_basename(file_search(path_in+wild))
    ndx=where(idd ge id1 and idd le id2)
    PRINT,'START processing'

    If ndx(0) gt -1 THEN BEGIN
        FOR i=0L,n_elements(ndx)-1 DO BEGIN
            PRINT,i,n_elements(ndx),' ',id( ndx(i) )
            a=WHERE(strpos(files_all,strtrim(id(ndx(i)),2)) ne -1)
            If a(0) ne -1 THEN BEGIN
                FOR j=0,n_elements(a)-1 DO file_copy,path_in+files_all(a(j)),path_out,/overwrite			
            ENDIF
        ENDFOR
    ENDIF
END

PRO delete_files,path_in,id1,id2
    IF strmid(path_in,0,1,/reverse_offset) ne '/' THEN path_in=path_in+'/'
    wild='kplr*.fits'
    PRINT,'Reading files in the directory'
    READ_KEPLER_DIR,path_in,files,id,wild=wild
    idd=LONG(id)
    files_all=file_basename(file_search(path_in+wild))
    ndx=where(idd ge id1 and idd le id2)
    If ndx(0) gt -1 THEN BEGIN
        FOR i=0L,n_elements(ndx)-1 DO BEGIN
            PRINT,i,n_elements(ndx),id( ndx(i) )
            a=WHERE(strpos(files_all,strtrim(id(ndx(i)),2)) ne -1)
            If a(0) ne -1 THEN BEGIN
                FOR j=0,n_elements(a)-1 DO file_delete,path_in+files_all(a(j))		
            ENDIF
        ENDFOR
    ENDIF
END

function smooth_lightcurve_sym, time, flux, delta_t, npt=npt
;
; Program that smooth a LC, adding point by symetry at its ends
;
; time:     original timeserie
; flux:     original flux (must be the same size as time!)
; delta_t : size of the smooth window
;
; retuns smth : smoothed LC
;
; Optional
;
; npt: number of points for the smooth
;      if used, time and delta_t are ignored
;
; Written by T.Ceillier, september 2014

  t=time
  f=flux

; Calculate the number of points to add

  if n_elements(npt) eq 0 then begin
     ind_before=where(t le t[0]+delta_t)
     n_before=n_elements(ind_before)
     ind_after=where(t ge t[-1]-delta_t)
     n_after=n_elements(ind_after)
     np_global=max([n_before,n_after])
  endif else begin
     np_global=npt
  endelse

; Find the good starting and endind points

  n_for=ceil(np_global/4.)
  spread_array_before=fltarr(n_for)
  spread_array_after=dblarr(n_for)

  n_elem_f=n_elements(f)

  for i=0,n_for-1 do begin
     
     np=np_global+i

     f_i=f[i:n_elem_f-1-i]

     new_flux=prolongement_vector(f_i, np)
     smt=smooth(smooth(new_flux,np_global,/nan),np_global,/nan)
     
     ind_f_max=n_elements(new_flux)-1
     flux_smt=new_flux-smt

     trans_before=flux_smt[ceil(np*0.5):ceil(np*1.5)]
     trans_after=flux_smt[ind_f_max-ceil(np*1.5):ind_f_max-ceil(np*0.5)]
     
     ind_ok_before=where(finite(trans_before))
     ind_ok_after=where(finite(trans_after))

     spread_array_before[i]=max(trans_before[ind_ok_before])-min(trans_before[ind_ok_before])
     spread_array_after[i]=max(trans_after[ind_ok_after])-min(trans_after[ind_ok_after])

  endfor

  min_before=min(spread_array_before, i_best_before)
  min_after=min(spread_array_after, i_best_after)

  f_best=f[i_best_before:n_elem_f-1-i_best_after]
  np_best=np_global+max([i_best_before,i_best_after])

  new_f_best=prolongement_vector(f_best, np_best)
  smt_global=smooth(smooth(new_f_best,np_global,/nan),np_global,/nan)

  smt=smt_global[np_best-i_best_before:n_elements(smt_global)-1-np_best+i_best_after]

  return, smt

end

pro create_header_Kepler, header_kepler, info_struct, header_kadacs, psd=psd
header_kadacs=header_kepler

;---GENERAL INFO
sxaddpar, header_kadacs, 'ORIGIN', 'CEA & SSI' 
sxaddpar, header_kadacs, 'DATE', info_struct.date, 'date of creation'; info_struct.date
sxaddpar, header_kadacs, 'QUARTERS', info_struct.roll_label, 'quarters observed'; info_struct.roll_label
sxaddpar, header_kadacs, 'PIPELINE ', info_struct.version, 'version of KADACS pipeline'; info_struct.version
sxaddpar, header_kadacs, 'DATATYPE', info_struct.datatype, 'type of data: PDC or KADACS'; info_struct.datatype
sxaddpar, header_kadacs, 'NOCORREC', info_struct.nocorrec, '1 if no correction applied'; info_struct.nocorrec
sxaddpar, header_kadacs, 'PIXELDATA',  info_struct.pixel_type, '1 If the corrections use pixel data'; info_struct.pixel_type
sxaddpar, header_kadacs, 'INPTHRESH',info_struct.inpaint_thres , '[days] size of gaps inpainted (0 if no inpaint)'; info_struct.inpaint_thres

;---DETAILED INFO ON THE CORRECTIONS
sxaddpar, header_kadacs, 'TSFILTER', info_struct.n_days_filt, '[days] filter of the residuals (trismooth)'; info_struct.n_days_filt
sxaddpar, header_kadacs, 'DFILTER1',info_struct.dg_filt_dig , '# scales of the 1st digital filter (0 no filter)'; dg_filt_dig
sxaddpar, header_kadacs, 'DFILTER2', info_struct.dg_filt_dig2, '# scales of the 2nd digital filter';dg_filt_dig2
sxaddpar, header_kadacs, 'THRESDF2',info_struct.filt_dg_thresh , '[sigma] threshold 2nd dig filter (0 no filter)';filt_dg_thresh
sxaddpar, header_kadacs, 'SIGTHRESH',  info_struct.sgm_thre, '[sigma] threshold to crop raw data' ; info_struct.sgm_thre
sxaddpar, header_kadacs, 'SPKTHRESH', info_struct.spikes_thres, '[sigma] threshold to crop the residuals'; info_struct.spikes_thres
sxaddpar, header_kadacs, 'LINPOL', info_struct.lin_resid, 'polynomial deg. to linearize the residuals'; info_struct.lin_resid
sxaddpar, header_kadacs, 'POLFITQ ', info_struct.pol_fit_segment, 'degree of the polynomial fit (redresse)';info_struct.pol_fit_segment
sxaddpar, header_kadacs, 'QEDGES', info_struct.quarter_edge, 'correction in the quarter edges' ;quarter_edge
sxaddpar, header_kadacs, 'INTKNAN', info_struct.interp_kep_time, 'interpolate original inf points' ;interp_kep_time
sxaddpar, header_kadacs, 'FLAGCOR',info_struct.flag , ' SAP_QUALITY flag to remove pointst Q[12,16]'

;---add time, flux, PSD, freq
if keyword_set(psd) then begin
   sxaddpar, header_kadacs, 'TTYPE1', 'FREQUENCY',  'column title: frequency'; info_struct.ttype1
      sxaddpar, header_kadacs, 'TUNIT1', 'muHz', 'column units: microHz'       
      sxaddpar, header_kadacs, 'TFORM1', 'D   ', 'column format: 64-bit floating point' 

      sxaddpar, header_kadacs, 'TTYPE2', 'PSD',  'column title: power spectrum density from KADACS' ; info_struct.ttype2
      sxaddpar, header_kadacs, 'TUNIT2', 'ppm^2/muHz', 'column units: (Parts per Million)^2 per microHz'       
      sxaddpar, header_kadacs, 'TFORM2', 'D   ', 'column format: signed 64-bit floating point'
endif else begin
    sxaddpar, header_kadacs, 'TTYPE1', 'TIME',  'column title: data time stamps'
   sxaddpar, header_kadacs, 'TUNIT1', 'BJD - 2454833', 'column units: barycenter corrected JD'       
   sxaddpar, header_kadacs, 'TFORM1', 'D   ', 'column format: 64-bit floating point'  
   
   sxaddpar, header_kadacs, 'TTYPE2', 'FLUX',  'column title: KADACS processed flux'; info_struct.ttype2
   sxaddpar, header_kadacs, 'TUNIT2', 'ppm','column units: Parts per Million'       
   sxaddpar, header_kadacs, 'TFORM2', 'D   ', 'column format: signed 64-bit floating point'

endelse
   
   ;endif
end

pro create_header_Kepler_ext1, header_kepler, info_struct, header_kadacs
header_kadacs=header_kepler

;header for extension1: the mask
sxaddpar, hdr_ts_ext1, 'TTYPE1', 'MASK',  'column title: MASK'; info_struct.ttype1
   sxaddpar, hdr_ts_ext1, 'TFORM1', 'I    ',  'column format: 0: No data, 1: Kepler data, 2: Inpainted data' ; info_struct.ttype1

;TTYPE = ‘MASK   ‘     /Column title: MASK 			TFORM1  = ‘I       '           / 0: No data, 1: Kepler data, 2: Inpainted data

end

pro create_header_CoRoT, header_CoRoT, info_struct, header_kadacs
header_kadacs=header_CoRoT

;---GENERAL INFO
sxaddpar, header_kadacs, 'ORIG_COR', 'CEA', 'Lab. responsible of corrected files'
sxaddpar, header_kadacs, 'PAP2REF', '2011MNRAS.414L...6G','Paper to be cited'
sxaddpar, header_kadacs, 'DATE_COR', info_struct.date_corr, 'date of creation'; info_struct.date_corr
sxaddpar, header_kadacs, 'PIPE_COR', 'KADACS', 'Pipeline used to correct data'; info_struct.datatype
sxaddpar, header_kadacs, 'VERS_COR ', info_struct.version_CORR, 'version of KADACS pipeline'; info_struct.version


;---DETAILED INFO ON THE CORRECTIONS
sxaddpar, header_kadacs, 'TSFILTER', info_struct.Trifilt, '[days] filter of the residuals (trismooth)'; info_struct.n_days_filt
sxaddpar, header_kadacs, 'SPKTHREH', info_struct.Thresh_spk, '[sigma] threshold to crop the residuals'; info_struct.spikes_thres

end

pro create_header_K2, info_struct, header_kadacs,header_K2=header_k2
IF keyword_set(header_k2) THEN header_kadacs=header_K2

;---GENERAL INFO
sxaddpar, header_kadacs, 'SIMPLE', 'K2 data', 'Written by R.A. Garcia'
;sxaddpar, header_kadacs, 'ORG', 'CEA', 'Written by R.A. Garcia'

sxaddpar, header_kadacs, 'BITPIX', -64, 'Number of bits per data pixel'

sxaddpar, header_kadacs, 'NAXIS', 2, 'Number of data axes'
sxaddpar, header_kadacs, 'NAXIS1', 2, 'Frequency vs. Power'
sxaddpar, header_kadacs, 'NAXIS2', info_struct.ncol, 'Data points'
sxaddpar, header_kadacs, 'EXTEND', 2, 'Light Curve & Window (0:nodata, 1:data, 2: interpolated) '

sxaddpar, header_kadacs, 'ORIG_COR', 'Written by R.A. Garcia', 'Lab. responsible of corrected files: CEA'
sxaddpar, header_kadacs, '2_W_COR', 'Vanderburg & Johnson', '2 Wheel Cor based 2014PASP..126..948V'
sxaddpar, header_kadacs, 'PAP2REF', '2011MNRAS.414L...6G','Paper to be cited'
sxaddpar, header_kadacs, 'MISSION', 'K2', 'NASA Space mission'
sxaddpar, header_kadacs, 'DATE_COR', info_struct.date_corr, 'date of creation'; info_struct.date_corr
sxaddpar, header_kadacs, 'PIPE_COR', 'KADACS', 'Pipeline used to correct data'; info_struct.datatype
sxaddpar, header_kadacs, 'VERS_COR ', info_struct.version_CORR, 'version of KADACS pipeline'; info_struct.version


;---DETAILED INFO ON THE CORRECTIONS
sxaddpar, header_kadacs, 'SPKTHREH', info_struct.Thresh_spk, '[sigma] threshold to crop the residuals'; info_struct.spikes_thres
sxaddpar, header_kadacs, 'SPKRMV', info_struct.bd_spk, '[points] Number of removed points'; info_struct.n_days_filt
sxaddpar, header_kadacs, 'TSFILTER', info_struct.Trifilt, '[days] filter of the residuals (trismooth)'; info_struct.n_days_filt
sxaddpar, header_kadacs, 'INPTHRESH',LONG(info_struct.inpaint_thres) , '[days] Max inpainted gap size  (0 no inpaint)'; info_struct.inpaint_thres

sxaddpar, header_kadacs, 'EPIC', LONG(info_struct.EPIC), 'EPIC number of the star'; EPIC
end


PRO TEST_EXTENSION,path,wild,name_file_out
    files=file_search(path,wild,count=nfiles)
    CLOSE,20
    OPENW,20,name_file_out
    
    FOR i=0L,nfiles-1 DO BEGIN
        fits_info,files(i),textout=textout,n_ext=N_ext,/silent
        IF N_ext lt 1 THEN BEGIN
            PRINT,i,nfiles, 'Bad file= ',file_basename(files(i))
            PRINTF,20,files(i)
        ENDIF
    ENDFOR
    CLOSE,20
END    


;----------------------------------------------------------------
;   READ KEPLER PUBLIC FITS FILES
;----------------------------------------------------------------
PRO read_k2_fits,infile,data,htab0=htab0,htab1=htab1   ;, infile,data,roll_label,no_nan=no_nan,type=type

cadence=1625.3514d0
bjdref=0.d0

fits_read,infile,tab0,htab0,exten_no=0
fits_read,infile,tab1,htab1,exten_no=1
bjdrefi=double(sxpar(htab0,'BJDREFI'))
bjdreff=double(sxpar(htab0,'BJDREFF'))
bjdref=bjdrefi+bjdreff


bjdrefi=double(sxpar(htab1,'BJDREFI'))
bjdreff=double(sxpar(htab1,'BJDREFF'))
bjdref=bjdrefi+bjdreff
quarter=sxpar(htab0,'QUARTER')
module=sxpar(htab0,'MODULE')
;output=sxpar(htab1,'OUTPUT')

;channel=sxpar(htab1,'CHANNEL')
EXTNAME=sxpar(htab0,'EXTNAME')
TARGET=sxpar(htab0,'TARGET')
DATA_REL=sxpar(htab0,'DATA_REL')    ; DATA RELEASE notes
KEPMAG=sxpar(htab0,'KEPMAG')       ; DATA RELEASE notes
OBSMODE=sxpar(htab0,'OBSMODE')     ; OBSMODE = 'long cadence'       / observing mode 
;UNITS_FLUX=sxpar(htab1,'TUNIT4')      ; Units of SAP_FLUX ("e-/s")
;LC_START=sxpar(htab1,'LC_START')
;LC_END=sxpar(htab1,'LC_END')
res=MRDFITS(infile,1,header,columns=['TIME','FLUX_PRF','FLUX_0_00500','FLUXERR_0_00500','FLUX_0_00050','FLUXERR_0_00050'],/silent)
;  'TIMECORR','SAP_FLUX','SAP_FLUX_ERR','PDCSAP_FLUX','PDCSAP_FLUX_ERR','SAP_QUALITY'],/silent)
; Put NaN in all timing cadences in which the time is less than the first good time
temp=res.time
a=WHERE(finite(temp) eq 1)
b=where(temp(a) lt temp(a(0)))
if b(0) gt -1 THEN temp(a(b)) = !values.f_nan     
;------------------------------------------ 
 TIME_BJD=DOUBLE(temp) + bjdref -240e4   ;- res.timecorr + time_slice_correction       
 FLUX_0_00500=res.FLUX_0_00500
 FLUX_0_00050= res.FLUX_0_00050; Original Kepler Mask
 nnpp=N_elements(flux_0_00500)
 FLUX_err=replicate(0,nnpp)
 FLUX_KEPMASK_ERR=replicate(0,nnpp)


 DATA={extname: extname, target: target,quarter: quarter, module:module, KEPMAG: KEPMAG,    $
 OBSMODE:OBSMODE, time:TIME_BJD, FLUX_0_00500: FLUX_0_00500, FLUX_0_00050:FLUX_0_00050} 

                 
END


pro FDRM, dnu, nu_max, Teff, R, M, logg

R = (135.1^2/3090.)*(nu_max/dnu^2)*sqrt(Teff/5777.)

M=(135.1^2/3090.)^2*(nu_max/dnu^2)^2*(nu_max/3090.)*(Teff/5777.)^(3./2.)
;print, 'R =', R
;print, 'M =', M

G=6.67384d-8 ; m^3/g s^-2
Ms=1.98892d33   ; g
Rs=6.955d10  ;cm
Teffs=5777.d0  ;K
logg=alog10(G*nu_max/3090. * Ms/Rs^2 *(teff/teffs)^0.5)


;print, 'R =', R
;print, 'M =', M
;PRINT,'Logg=',logg

;dnu=134.*(numax/3050.)^0.8
;numax=3050.*(dnu/134)^(1./0.8)

; R_solar=6.955 10^8 m
;Mass_solar= 1.98892e30 Kg

end

PRO mycgcolors

WHITE = cgcolor('white')
BLACK = cgcolor('black')
BLUE = cgcolor('BLUE')
BLU2 = cgcolor('Sky Blue')
BLU3 = cgcolor('Cornflower Blue')
BLU4 = cgcolor('ROYAL BLUE')
BLU5= cgcolor('NAVY')

RED = cgcolor('RED')
RED2 = cgcolor('RED2')
RED3 = cgcolor('RED3')
RED4= cgcolor('RED4')
RED5 = cgcolor('RED5')

PINK = cgcolor('Magenta')
PIN2 = cgcolor('PINK')
PIN3 = cgcolor('HOT PINK')
PIN4 = cgcolor('DEEP PINK')

GREEN = cgcolor('GREEN')
GRE2 = cgcolor('LIME GREEN')
GRE3 = cgcolors('Olive Drab')
GRE4 = cgcolor('DARK GREEN')
GRE5 = cgcolors('Olive')

ORG1 = cgcolor('ORG1')
ORG2 = cgcolor('ORG2')
ORG3 = cgcolor('ORG3')
ORG4 = cgcolor('ORG4')
ORG5 = cgcolor('ORG5')
ORG6 = cgcolor('ORG6')

ALMOND = cgcolors('ALMOND')
ALM1 = cgcolor('Bisque')
ALM2 = cgcolor('Moccasin')
ALM3 = cgcolor('Wheat')
ALM4 = cgcolor('Burlywood')
ALM5 = cgcolor('TAN')

Gray = cgcolor('gray')
GRA1 = cgcolor('Light Gray')
GRA2 = cgcolor('Medium Gray')
GRA3 = cgcolor('Slate Gray')
GRA4 = cgcolor('Dark Gray')
GRA5 = cgcolor('Charcoal')

END

FUNCTION readcol_custom_format, columns_numbers, format=fmt
;-----------------------------------------------------------------------------------------------------------
;       Ceillier 2015
;       Example
;       file='APOKASC_Catalog.v7.0.txt'
;       readcol, file, kic_apo, vsini_apo, format=readcol_custom_format([1,71], format='L,D')
;
;        où 1 et 71 sont les numéros des colonnes que tu veux lire (ça commence à 1 et pas 0) et tu peux préciser le format de lecture 
;       (L pour long, D pour double, etc…). Les numéros de colonnes doivent être dans le bon ordre, par contre.
;-----------------------------------------------------------------------------------------------------------
  
  new_num=columns_numbers[UNIQ(columns_numbers, SORT(columns_numbers))]

  if n_elements(new_num) ne n_elements(columns_numbers) then begin
     print, 'Columns numbers must not have duplicates.'
     return, ''
  endif else begin
     if total(new_num-columns_numbers) ne 0 then begin
        print, 'Columns numbers must be monotonous.'
        return, ''
     endif
  endelse

  if N_elements(fmt) GT 0 then begin ;FORMAT string supplied?
     
     if size(fmt,/tname) NE 'STRING' then $
        message,'ERROR - Supplied FORMAT keyword must be a scalar string.'
     
     new_format=strsplit(fmt,',', /extract)
     
     if n_elements(new_format) ne n_elements(new_num) then begin
        print, 'FORMAT keyword must be the same size as the number of columns to read.'
        return, ''
     endif
     
  endif else begin              ;Read everything as floating point
     fmt = 'F'
     if n_elements(new_num) GT 1 then for i = 1,n_elements(new_num)-1 do fmt = fmt + ',F'
     message, 'Format keyword not supplied - All columns assumed floating point',/INF
  endelse
  
  new_format=strsplit(fmt,',', /extract)
  new_format=new_format[UNIQ(columns_numbers, SORT(columns_numbers))]
  
  col_num=[0,columns_numbers]
  
  format_final=''
    
  for i=0, n_elements(new_format)-1 do begin
     n_x=col_num[i+1]-col_num[i]-1
     if n_x gt 0 then begin
        this_string=string(replicate('X,',n_x), format='('+string(n_x)+'A)')+new_format[i]+','
     endif else begin
        this_string=new_format[i]+','
     endelse
     format_final=format_final+this_string     
  endfor
  
  format_final=strmid(format_final,0,strlen(format_final)-1)

  return, format_final
  
END

PRO check_data_release,path_in,file_out
;READ_KEPLER_DIR,path_in,files,id_stars,wild='kplr*_slc.fits',length=9
files=file_search(path_in,'kplr*.fits')

CLOSE,1
openw,1,'data_release.txt'
PRINTf,1,'KIC   Quarter     Season      Data Release'
for i=0L,n_elements(files)-1 DO BEGIN
     fits_read,files(i),tab0,htab0,exten_no=0
     data_rel=sxpar(htab0,'DATA_REL')
     QUARTER=sxpar(htab0,'QUARTER')
     SEASON=sxpar(htab0,'SEASON')   
     id_stars=sxpar(htab0,'KEPLERID')
     printf,1,id_stars,' ',quarter, '  ',season,'  ',data_rel
ENDFOR

CLOSE,1

END

pro backgnd_corr, s, ff2, yback

  region,250,280,s*1e6,ff2,xx,yy ;6000 a 8000 
  noise=median(yy)
  region,2.,150,s*1e6,ff2,x,y     ;6 10000
  xl=alog10(x)
  yl=alog10(y)
  a=poly_fit(xl,yl,3,fit)
  yfit=poly(alog10(s*1e6),a)
  yback=(10^yfit)+noise

  yback1=(10^yfit)
  a=where(yback1 le noise,n)
  if a(0) gt -1 then begin
    b = where(s(a) gt 100e-6)
    if b(0) gt -1 THEN yback(a(b(0)):*)=yback(a(b(0)))
  ENDIF  
  ;!p.multi=0
  ;plot_oo, s*1e6,ff2, xr=[1e-1,300],back=255,col=1,xst=1
  ;oplot, s*1e6, yback1, col=2
  ;oplot,[1e-10,1e10],[noise,noise],col=3
  ;oplot, s*1e6, yback, col=4,thick=2

  ;oplot, s, ff2/yback, col=4
  ;stop

end
;-----------------------------------------------------------
;  Compute the total power of the granulation between two limits substracting the noise
;
;  in -> Power spectrum (freq in muHz, power)
;  gr_pwr-> output 
;  OPTIONAL:
;         frq_limits
;         noise_frq_limit
;-----------------------------------------------------------
PRO Gr_total_power,in,gr_pwr,frq_limits=frq_limits, noise_frq_limits=noise_frq_limits
  IF not Keyword_set(frq_limits) THEN frq_limits = [10,270]
  IF not keyword_set(noise_frq_limits) THEN noise_frq_limits = [276,-1]
  IF noise_frq_limits(1) eq -1 then noise_frq_limits(1) = in(0,-1)




END 

pro region,inic,fin,xori,yori,xmat,ymat,av=av,sgm=sgm, med=med, total_power=total_power,out_metrics=out_metrics
  in = where(xori ge inic(0))
  indx0 = in(0)
  xs = xori(in)
  xy = yori(in)
  in = where(xs lt fin(0))
  xmat = xs(in)
  ymat = xy(in)
  mom  = MOMENT(ymat,sdev=sgm)
  med  = MEDIAN(ymat)
  total_power = TOTAL(ymat)
  av=mom(0)
  out_metrics = DBLARR(4)
  out_metrics(0) = av                             ; Mean
  out_metrics(1) = sgm                            ; Sigma
  out_metrics(2) = med                            ; Median
  out_metrics(3) = total_power                    ; Total_power


  ;print,'media=',av,'sigma=',sgm
  ;a=where(ymat lt 3*sgm+av)
  ;if a(0) ne -1 then sgm1=sigma(ymat(a))
  ;if a(0) ne -1 then print,'media=',total(ymat(a))/n_elements(ymat(a)),'sigma=',sgm
end


; Break-up velocity
;The break-up velocity of a star is an expression that is used to describe the case where the centrifugal force at the equator
;is equal to the gravitational force. For a star to be stable the rotational velocity must be below this value.

;Omega_K = sqrt ( GM/Req^3 )

; angle= asin( Vsini(Km/s) / (2*!pi*R(km)/T(sec)) )

;PRO remove_transits_old,kic,data,period,limits
;FOR j=0,(n_elements(limits)/2.) - 1 Do BEGIN
;    FOR i=0,data(0,-1)/limits(j) DO BEGIN
;             PRINT,KIC, DOUBLE(data(0,0)+limits(j)+period*i),double(data(0,0)+limits(j+1)+period*i)
;    ENDFOR
;ENDFOR
;
;
;END



