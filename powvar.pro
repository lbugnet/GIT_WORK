PRO POWVAR, $
;IN
STAR_TAB_PSD=STAR_TAB_PSD,   FREQ_INIC_GR, FREQ_FIN_GR, FREQ_INIC_GR_NS, FREQ_FIN_GR_NS, ID_STAR=ID_STAR, STAR_PATH_PSD=STAR_PATH_PSD,$
  STAR_PATH_LC=STAR_PATH_LC, kpp=kpp, EPIC=EPIC, CADENCE=CADENCE, MAG_COR=MAG_COR, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE,$
;OUT
OUTPUT_A2Z_1=OUTPUT_A2Z_1


;+
; :Author: Lisa BUGNET

; ---------------------------------------------------------------------------;
;           PROGRAM TO COMPUTE P_VAR FROM PSD
; ---------------------------------------------------------------------------;
; 
; NAME: POWVAR.pro
; 
; CALLING SEQUENCE: 
; 
;  POWVAR, STAR_TAB_PSD=STAR_TAB_PSD, OUTPUT_A2Z_1=OUTPUT_A2Z_1, NS=NS, $
;  FREQ_INIC_GR, FREQ_FIN_GR, FREQ_INIC_GR_NS, FREQ_FIN_GR_NS, ID_STAR=ID_STAR,$
;   STAR_PATH_PSD=STAR_PATH_PSD, CADENCE=CADENCE, STAR_PATH_LC=STAR_PATH_LC, $
;   EPIC=EPIC, kpp=kpp, MAG_COR=MAG_COR, HELP=HELP
;
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;

; --- STAR_TAB_PSD  ----- DBLARR(2) -----------------------------------------;
; --------------------- contains PSD of stars (frequency, POWER) ------------;
; --------------------- computed by COMPUTE_PSD_DATA.pro in A2ZPL.pro -------;
;
; --- FREQ_INIC_GR = initial frequency of data (muHz) -----------------------;
; --- FREQ_FIN_GR  = end frequency of data (muHz) ---------------------------;
; 
; --- FREQ_INIC_GR = initial frequency of noise (muHz) ----------------------;
; ------------------ optionnal if MAG_COR=1  --------------------------------;
; --- FREQ_FIN_GR  = end frequency of noise (muHz) --------------------------;
; ------------------ optionnal if MAG_COR=1 ---------------------------------;
; 
; --- ID_STAR ----- STRING --------------------------------------------------;
; ------------------ from COMPUTE_PSD_DATA.pro ------------------------------;
; 
; --- STAR_PATH_PSD optionnal ----- STRING ----------------------------------;
; ------------------ Its header contains Kepler magnitude -------------------;
; 
; --- STAR_PATH_LC optionnal ----- STRING -----------------------------------;
; ------------------ Doesn't contains Kp, need kpp keyword ------------------;
; 
; --- kpp optionnal ----- DOUBLE --------------------------------------------;
; ----------------- Contains Kepler Magnitude -------------------------------;
; 
; --- EPIC optionnal ----- STRING -------------------------------------------;
; ----------------- star name associated to kpp -----------------------------;
; 
; --- CADENCE ----- STRING --------------------------------------------------;
; ------------------ ='LC' or ='SC' or ='' ----------------------------------;
; 
; --- MAG_COR ---------------------------------------------------------------;
; --------------------- =1 or 0 --------------- (default =0) ----------------;
; ------------------- correction of magnitude or high frequency noise -------;

; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
;
; --- OUTPUT_A2Z_1 contains the POWVAR metric P_var -------------------------;
; --------------------- OUTPUT_A2ZP_1(*,0)=EPIC -----------------------------;
; --------------------- OUTPUT_A2ZP_1(*,1)=P_VAR=DATA-NOISE -----------------;
; --------------------- OUTPUT_A2ZP_1(*,3:6)=DATA ---------------------------;
; --------------------- OUTPUT_A2ZP_1(*,7:10)=NOISE -------------------------;
; ---------------------------------------------------------------------------;

;-
IF keyword_set(help) THEN BEGIN
  doc_library,'POWVAR', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
  RETURN
  ;you have to write POWVAR, /help
ENDIF


;---------------------------------------------------------------------------
;------------------------- CALCUL METRIC OUTPUT_A2Z ------------------------
;---------------------------------------------------------------------------
if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

  if n_elements(STAR_TAB_PSD) ne 0 then begin
    data_arr_freq=STAR_TAB_PSD(0,*)
    data_arr_power=STAR_TAB_PSD(1,*)
  endif

  data_arr=dblarr(2,n_elements(data_arr_freq))
  data_arr(0,*)=data_arr_freq(*)
  data_arr(1,*)=data_arr_power(*)
  freq_org = data_arr(0,*) *1e6        ; in muHz ; enlever le 1e6 si region bug car deja en muHz !!!
  power_arr    = data_arr(1,*)         ; power
  freq_org_ns = data_arr(0,*) *1e6     ; in muHz
  power_arr_ns    = data_arr(1,*)      ; power

 ; if (freq_org(0) gt 1.e6) then print, 'WARNING FREQ TOO BIG, CHANGE UNIT'
  
  IF freq_fin_gr_ns le freq_inic_gr_ns THEN BEGIN
    freq_inic_gr_ns = freq_fin_gr_ns-5.
  ENDIF
  
  

  region, freq_inic_gr_ns, freq_fin_gr_ns, freq_org_ns, power_arr_ns, xmat_ns, ymat_ns, out_metrics=out_metrics_ns ;compiler kadacs si probleme
  region, freq_inic_gr,    freq_fin_gr,    freq_org,    power_arr,    xmat,    ymat,    out_metrics=out_metrics
  
  
  ;------- MAGNITUDE CORRECTION -----------------------------------

if MAG_COR eq 1 then begin
  
  if ((CADENCE eq 'LC') or (CADENCE eq '')) then begin
    if n_elements(star_path_psd) ne 0 then begin
      Kp=sxpar(headfits(STAR_PATH_PSD), 'KEPMAG')
    endif
    if n_elements(star_path_lc) ne 0 then begin
      match, long(id_star), long(EPIC), i1, i2
      kp=kpp(i2)
    endif
    c = 3.46d0 * 10^(0.4d0*(12.-Kp)+8)
    siglower = sqrt( c + 7e6 * max([1,Kp/14.])^4. ) / c
    siglower = siglower * 1d6  ; convert in ppm
    dt = 1./2*(data_arr_freq(n_elements(data_arr_freq)-1.))
    siglower = siglower^2.*2*dt*1.e-6 ;ppm^2/muHz
  endif
  
  if (CADENCE eq 'SC') then begin
    if n_elements(star_path_psd) ne 0 then begin
      Kp=sxpar(headfits(STAR_PATH_PSD), 'KEPMAG')
    endif
    if n_elements(star_path_lc) ne 0 then begin
      match, long(id_star), long(EPIC), i1, i2
      kp=kpp(i2)
    endif
    c = 1.28d0*10^(0.4d0*(12.-Kp)+7.)
    N=49032.
    siglower = 10^6 * ( (c+9.5* 1.d5*(14./Kp)^5.)^(0.5) ) / (c*N^0.5)
    siglower = siglower * 1d6 ; convert in ppm
    dt = 1./2*(data_arr_freq(n_elements(data_arr_freq)-1.))
    siglower = siglower^2.*2*dt*1.e-6 ;ppm^2/muHz
  endif
  
  OUTPUT_A2Z_1=dblarr(11)
  OUTPUT_A2Z_1[0] = double(ID_STAR)
  OUTPUT_A2Z_1[1] = double(out_metrics[0]) - siglower        ; METRIC POWVAR
  OUTPUT_A2Z_1[2] = double(out_metrics[0]-out_metrics_ns[0])
  OUTPUT_A2Z_1[3:6] = double(out_metrics[*])                 ; MEAN SIGNAL
  OUTPUT_A2Z_1[7:10] = double(out_metrics_ns[*])             ; MEAN NOISE
  
endif else begin ;(PAS DE CORRECTION DE MAGNITUDE)

  OUTPUT_A2Z_1=dblarr(11)
  OUTPUT_A2Z_1[0] = double(ID_STAR)
  OUTPUT_A2Z_1[1] = double(out_metrics[0]-out_metrics_ns[0])        ; METRIC POWVAR
  OUTPUT_A2Z_1[2] = double(out_metrics[0]-out_metrics_ns[0])
  OUTPUT_A2Z_1[3:6] = double(out_metrics[*])                 ; MEAN SIGNAL
  OUTPUT_A2Z_1[7:10] = double(out_metrics_ns[*])             ; MEAN NOISE
    
endelse
  ;out_metrics(0) = av                                       ; Mean
  ;out_metrics(1) = sgm                                      ; Sigma
  ;out_metrics(2) = med                                      ; Median
  ;out_metrics(3) = total_power                              ; Total_power



;----------------------------------------------------------------


END