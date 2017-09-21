PRO A2ZPL,$ 
  ;IN
  STAR_PATH_LC_LIST=STAR_PATH_LC_LIST, STAR_LC_LIST=STAR_LC_LIST, STAR_PATH_PSD_LIST=STAR_PATH_PSD_LIST, STAR_KIC_LIST_TXT=STAR_KIC_LIST_TXT, STAR_PATH_KIC_LIST=STAR_PATH_KIC_LIST,$
  FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, MAG_COR=MAG_COR, FREQ_INIC_GR_DEB=FREQ_INIC_GR_DEB, FREQ_FIN_GR=FREQ_FIN_GR, FREQ_INIC_GR_NS=FREQ_INIC_GR_NS, FREQ_FIN_GR_NS=FREQ_FIN_GR_NS,$
  DAY=DAY, CALCUL_SLOPE=CALCUL_SLOPE, PARAMS=PARAMS, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE,$
  ;OUT
  STAR_TAB_PSD=STAR_TAB_PSD, OUTPUT_A2Z=OUTPUT_A2Z, NUMAX_GUESS=NUMAX_GUESS, SOLAR_LIKE=SOLAR_LIKE, SUN=SUN, GOLD=GOLD,  HELP=HELP,$
  ;PYTHON
  N_STARS_TOT=N_STARS_TOT
;+
; :Author: Lisa BUGNET

; ---------------------------------------------------------------------------;
;           MAIN PROGRAM TO COMPUTE P_VAR, SLOPE AND OULIERS
; ---------------------------------------------------------------------------;
; 
; NAME: A2ZPL.pro
; 
; CALLING SEQUENCE: 
; 
;  A2ZPL, STAR_PATH_LC_LIST=STAR_PATH_LC_LIST, STAR_LC_LIST=STAR_LC_LIST, $
;  STAR_PATH_PSD_LIST=STAR_PATH_PSD_LIST, STAR_KIC_LIST_TXT=STAR_KIC_LIST_TXT, $
;  STAR_PATH_KIC_LIST=STAR_PATH_KIC_LIST, FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH,$
;  MAG_COR=MAG_COR, FREQ_INIC_GR_DEB=FREQ_INIC_GR_DEB, FREQ_FIN_GR=FREQ_FIN_GR, $
;  FREQ_INIC_GR_NS=FREQ_INIC_GR_NS, FREQ_FIN_GR_NS=FREQ_FIN_GR_NS, DAY=DAY, $
;  CALCUL_SLOPE=CALCUL_SLOPE, PARAMS=PARAMS,  STAR_TAB_PSD=STAR_TAB_PSD, $
;  OUTPUT_A2Z=OUTPUT_A2Z, NUMAX_GUESS=NUMAX_GUESS, SOLAR_LIKE=SOLAR_LIKE, $
;  SUN=SUN, GOLD=GOLD,  HELP=HELP
;
; EXAMPLES:
;  ;(KEPLER ALL 20 J '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_ALL.sav')
;  Star_PATH_PSD_list=file_search('/Volumes/TEMP/RG_DR25/K???/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_INP', '*COR_PSD_filt_inp.fits')
;  A2Zpl, Star_PATH_PSD_LIST=star_path_psd_list, CALCUL_SLOPE=1
;
;  ;(K2 ALL '/Users/lbugnet/DATA/METRIC/K2/ALL/K2_varlaw_ALL.sav')
;  Star_PATH_LC_list=file_search('/Volumes/TEMP/K2/GAP/JOEL/', '*hipass.rescale')
;  A2Zpl, Star_PATH_LC_LIST=star_path_lc_list, CALCUL_SLOPE=1
;  
;  ;A2ZPL, STAR_KIC_LIST_TXT='/Users/lbugnet/DATA/METRIC/CLUSTER_ENRICO/CRGs_Class_NGC6791.txt', type='KEPLER', file_name_output_a2zp='CLUSTER_ENRICO_CRGs_Class_NGC6791'

; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
;
; --- STAR_PATH_LC_LIST optionnal -------------------------------------------;
; --------------------- contains light curve paths of stars -----------------;
;
; --- STAR_LC_LIST optionnel ------------------------------------------------;
; --------------------- contains light curves of stars ----------------------;
;
; --- STAR_PATH_PSD_LIST optionnal ------------------------------------------;
; --------------------- contains PSD paths of stars -------------------------;
;
; --- STAR_KIC_LIST_TXT optionnal -------------------------------------------;
; --------------------- contains star KIC/EPIC list -------------------------;
;
; --- STAR_PATH_KIC_LIST optionnal ------------------------------------------;
; --------------------- conatins KIC paths of stars -------------------------;
;
; --- STAR_TAB_PSD optionnal ------------------------------------------------;
; --------------------- contains PSD of stars (frequency, POWER) ------------;
;
; --- PARAMS optionnal ------------------------------------------------------;
; --------------------- contains name of data to use ------------------------;
; --------------------- default is ['numax', 'dnu'] -------------------------;
;
; --- FILE_OUTPUT_A2Z_PATH optionnal ----------------------------------------;
; --------------------- contains the path of already-calculed POWVAR --------;
;
; --- MAG_COR ---------------------------------------------------------------;
; --------------------- =1 or 0 --------------- (default =0) ----------------;
; ------------------- correction of magnitude or high frequency noise -------;
;
; --- FREQ_INIC_GR= initial frequency of data -------------------------------;
; --- FREQ_FIN_GR= end frequency of data ------------------------------------;
; --- FREQ_INIC_GR= initial frequency of noise ------------------------------;
; --- FREQ_FIN_GR= end frequency of noise -----------------------------------;
;
; --- DAY contains inpaint '20','55','80' or ' '  Kepler   ------------------;
;
; --- CALCUL_SLOPE=1 for POWVAR slope study ---------------------------------;
;
; --- SOLAR_LIKE optionnal --------------------------------------------------;
; --- add SOLAR_LIKE='SOLAR_LIKE' if super Nyquist of main sequence stars ---;
;
; --- SUN optionnal ---------------------------------------------------------;
; --- add SUN='SUN' if you want to study the Sun ----------------------------;
;
; --- GOLD optionnal --------------------------------------------------------;
; --- set GOLD='GOLD' (3 years data) or GOLD='NO_GOLD' (3 months data) ------;
; 
; 
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
;
; --- STAR_TAB_PSD ----------------------------------------------------------;
; --- contains PSD of stars (frequency (muHz), POWER (ppm^2/muHz)) ----------;
;
; --- OUTPUT_A2Z  ----- DBLARR(*,11) ----------------------------------------;
; --- OUTPUT_A2Z contains the POWVAR metric P_var ---------------------------;
; --------------------- OUTPUT_A2ZP(*,0)=EPIC/KIC ---------------------------;
; --------------------- OUTPUT_A2ZP(*,1)=P_VAR=DATA-NOISE (ppm^2/muHz) ------;
; --------------------- OUTPUT_A2ZP(*,3:6)=DATA -----------------------------;
; --------------------- OUTPUT_A2ZP(*,7:10)=NOISE ---------------------------;
; ---------------------------------------------------------------------------;
;
; --- NUMAX_GUESS contains guesses of numax by P_var ------------------------;
; ---------------------------------------------------------------------------;
; ---------------------------------------------------------------------------; 


;A2ZPL main program call
  ;PREPARE_FILES_A2ZPL [ ]
  ;COMPUTE_PSD_DATA.pro  -> create the PSD  [x]
    ;Light_curve_path_to_SPD [x]
    ;Light_curve_to_PSD [x]
    ;PSD_path_to_PSD [x]
    ;KIC_to PSD [x]
    ;KIC_Path_to_PSD [x]
  ;POWVAR.pro -> compute the metric (PSD-PSDnoise) [x]
  ;SAVE_SLOPE_POWVAR_NUMAX.pro -> compute res, slope of metric [x]
    ;FLAG_BAD_STARS
    ;DETECT_OUTLIERS_POWVAR
  ;VARLAW.pro -> guess numax from metric [x]

;-
;resolve_all, UNRESOLVED=variable, /CONTINUE_ON_ERROR

IF keyword_set(help) THEN BEGIN
  doc_library,'A2ZPL', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
  RETURN
  ;you have to write a2zpl, /help
ENDIF


;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;-------- DATA PSD PREPARATION ---------------------------------------------------------------------------------------------------------------------------------------------------------------;
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

if (n_elements(FREQ_INIC_GR_DEB) eq 0) then FREQ_INIC_GR=0.7   ; 0.7 if 20J, 0.2 if 80J
if n_elements(FREQ_INIC_GR_DEB) ne 0 then FREQ_INIC_GR=FREQ_INIC_GR_DEB
if (n_elements(FREQ_FIN_GR) eq 0) then FREQ_FIN_GR=283         ; muHZ (8500 SOLAR_LIKE)
if (n_elements(FREQ_INIC_GR_NS) eq 0) then FREQ_INIC_GR_NS=275 ; [muHZ]  doesn't matter if MAG_COR=1
if (n_elements(FREQ_FIN_GR_NS) eq 0) then FREQ_FIN_GR_NS=283   ; [muHZ]  doesn't matter if  MAG_COR=1
if n_elements(SOLAR_LIKE) eq 0 then SOLAR_LIKE=''
if n_elements(fill) eq 0 then fill=''
if n_elements(cadence) eq 0 then cadence=''
if n_elements(STAR_PATH_PSD_LIST) ne 0 then begin
  if (STREGEX(STAR_PATH_PSD_LIST(0), 'ALLQ') ne -1) then fill='ALLQ'
  if (STREGEX(STAR_PATH_PSD_LIST(0), 'Start_Q5') ne -1) then fill='Q5'
  if ((STREGEX(STAR_PATH_PSD_LIST(0), 'LC_DR25') ne -1) or (STREGEX(STAR_PATH_PSD_LIST(0), 'NO_GOLD_STANDARD_DR25/LC/OUT'))) then cadence='LC'
  if ((STREGEX(STAR_PATH_PSD_LIST(0), 'SC_PDC_DR25') ne -1) or (STREGEX(STAR_PATH_PSD_LIST(0), 'NO_GOLD_STANDARD_DR25/SC/RESULTS') ne -1)) then cadence='SC'
  if (STREGEX(STAR_PATH_PSD_LIST(0), 'MISS') ne -1) then fill='MISS'
endif
if (n_elements(sun) eq 0) then sun=''
if (n_elements(gold) eq 0) then gold=''
if (n_elements(MAG_COR) eq 0) then MAG_COR=1                   ; Default is corretion
if n_elements(STAR_KIC_LIST_TXT) ne 0 then begin
  if (STREGEX(STAR_KIC_LIST_TXT, 'APOKASC') ne -1) then fill='APOKASC'
  if ((STREGEX(STAR_KIC_LIST_TXT, 'APOKASC') ne -1) and (cadence eq '')) then cadence='LC'
  
endif

CHAMP=''
TYPE=''
PREPARE_FILES_A2ZPL, STAR_PATH_PSD_LIST=STAR_PATH_PSD_LIST, STAR_PATH_LC_LIST=STAR_PATH_LC_LIST, STAR_KIC_LIST_TXT=STAR_KIC_LIST_TXT, STAR_PATH_KIC_LIST=STAR_PATH_KIC_LIST, FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, $
   TYPE=TYPE, CHAMP=CHAMP, N_STARS_TOT=N_STARS_TOT, PATH_OUTPUT=PATH_OUTPUT


;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;-------- SKIP POWVAR IF OUTPUT_A2ZP ALREADY EXISTS (n_elements(FILE_OUTPUT_A2Z_PATH) eq 1) --------------------------------------------------------------------------------------------------;
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

if n_elements(FILE_OUTPUT_A2Z_PATH) eq 1 then goto, OUTPUT_OK

;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;-------- MAIN POWVAR CALCULATION ------------------------------------------------------------------------------------------------------------------------------------------------------------;
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

if n_elements(STAR_PATH_LC_LIST) ne 0 then STAR_PATH_LC=STAR_PATH_LC_LIST(0)
if n_elements(STAR_PATH_PSD_LIST) ne 0 then STAR_PATH_PSD=STAR_PATH_PSD_LIST(0)
if n_elements(STAR_PATH_KIC_LIST) ne 0 then STAR_PATH_KIC=STAR_PATH_KIC_LIST(0)
if n_elements(STAR_KIC_LIST_TXT) ne 0 then begin
  readcol, STAR_KIC_LIST_TXT, STAR_KIC_LIST, stringSKIP='#', format='L', /silent
  STAR_KIC=STAR_KIC_LIST(0)
endif
COMPUTE_PSD_DATA, STAR_PATH_LC=STAR_PATH_LC, STAR_LC=STAR_LC, STAR_PATH_PSD=STAR_PATH_PSD, STAR_KIC=STAR_KIC, STAR_PATH_KIC=STAR_PATH_KIC,$
  TYPE=TYPE, CHAMP=CHAMP, STAR_TAB_PSD=STAR_TAB_PSD, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT
  OUTPUT_A2Z = DBLARR(N_STARS_TOT,11)
  ID_STARS=[]
  INIC=0L
  
  ;---------------
  ; Loop by star
  ;---------------
  KEPTAB=[]
  AVV=[]
  FEHH=[]
  HMAGG=[]
  RADIUSS=[]
  TEFFF=[]
FOR ns = INIC, n_stars_tot-2 DO BEGIN
print, ns
  if (n_elements(STAR_PATH_PSD_LIST) ne 0) then begin
    STAR_PATH_PSD=STAR_PATH_PSD_LIST(ns)
  endif
  if (n_elements(STAR_KIC_LIST_TXT) ne 0) then begin
   readcol, STAR_KIC_LIST_TXT, STAR_KIC_LIST, stringSKIP='#', format='L', /silent
   STAR_KIC=STAR_KIC_LIST(ns)
  endif
  if (n_elements(STAR_PATH_LC_LIST) ne 0) then begin
    STAR_PATH_LC=STAR_PATH_LC_LIST(ns)
  endif
  if (n_elements(STAR_LC_LIST) ne 0) then begin
     STAR_LC=STAR_LC_LIST(ns)
  endif
  if (n_elements(STAR_PATH_KIC_LIST) ne 0) then begin
    STAR_PATH_KIC=STAR_PATH_KIC_LIST(ns)
  endif

  COMPUTE_PSD_DATA, STAR_PATH_LC=STAR_PATH_LC, STAR_LC=STAR_LC, STAR_PATH_PSD=STAR_PATH_PSD, STAR_KIC=STAR_KIC, STAR_PATH_KIC=STAR_PATH_KIC,$
    TYPE=TYPE, CHAMP=CHAMP, STAR_TAB_PSD=STAR_TAB_PSD,  ID_STAR=ID_STAR, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT

  POWVAR, STAR_TAB_PSD=STAR_TAB_PSD, OUTPUT_A2Z_1=OUTPUT_A2Z_1, FREQ_INIC_GR, FREQ_FIN_GR, FREQ_INIC_GR_NS, FREQ_FIN_GR_NS, ID_STAR=ID_STAR, STAR_PATH_PSD=STAR_PATH_PSD, $
    CADENCE=CADENCE, STAR_PATH_LC=STAR_PATH_LC, EPIC=EPIC, kpp=kpp, MAG_COR=MAG_COR, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, KEPMAG=KEPMAG,AV=AV, TEFF=TEFF, FEH=FEH, RADIUS=RADIUS, HMAG=HMAG
    
  ID_STARS=[ID_STARS, ID_STAR]

  OUTPUT_A2Z[ns,*]=OUTPUT_A2Z_1
  KEPTAB=[KEPTAB, KEPMAG]
  TEFFF=[TEFFF, TEFF] 
  FEHH=[FEHH, FEH] 
  RADIUSS=[RADIUSS,RADIUS] 
  HMAGG=[HMAGG, HMAG]
  AVV=[AVV,AV]
  if (ns mod 50) eq 0 then save, file=PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+SUN+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(FREQ_INIC_GR,1)+'_'+'output_numax_all'+'.sav', $
    OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, KEPTAB , AVV; sauvegarde de secours

ENDFOR


;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------ SLOPE ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

OUTPUT_OK: ; if OUTPUT_A2ZP ALREADY EXISTS (n_elements(FILE_OUTPUT_A2Z_PATH) eq 1)

if n_elements(CALCUL_SLOPE) eq 0 then CALCUL_SLOPE=1
if n_elements(PARAMS) eq 0 then PARAMS=['numax','dnu'];, 'numax_savita', 'dnu_savita'] ; PAR DEFAULT ON CALCULE LES DEUX
save_slope_powvar_numax, OUTPUT_A2Z=OUTPUT_A2Z, XX=XX, YY=YY, TYPE=TYPE, RES=RES, THRESHOLD=THRESHOLD, SLOPE_FIT=SLOPE_FIT, CHAMP=CHAMP, SOLAR_LIKE=SOLAR_LIKE, fill=fill, $
  CALCUL_SLOPE=CALCUL_SLOPE, cadence=cadence, FREQ_INIC_GR=FREQ_INIC_GR, GOLD=GOLD,FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, PARAMS=PARAMS, PATH_OUTPUT=PATH_OUTPUT


;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------ GUESS NUMAX FROM OUTPUT_A2Z -----------------------------------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

varlaw, TYPE=TYPE, CHAMP=CHAMP, OUTPUT_A2Z=OUTPUT_A2Z, NUMAX_GUESS=NUMAX_GUESS, NG_MAX=NG_MAX, NG_MIN=NG_MIN, DAY=DAY, res=res, slope_fit=slope_fit, threshold=threshold, xx=xx, yy=yy, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------------ SAVE OUTPUT_A2Z FROM POWVAR -----------------------------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

save, file=PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+SUN+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(FREQ_INIC_GR,1)+'_'+'output_numax_all'+'.sav', OUTPUT_A2Z, KEPTAB, AVV, TEFFF, FEHH, RADIUSS,HMAGG,NUMAX_GUESS, NG_MAX, NG_MIN


;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;
;------------- SOLAR_LIKE CASE ----------------------------------------------------------------------------------------------------------------------------------------------------------------;
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------;

if SOLAR_LIKE eq 'SOLAR_LIKE' then begin
  output_a2zp=output_a2z
  restore, PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
  match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
  output_resize_b=output_a2zp[i2,*]
  ks=long(output_a2zp[*,0])
  ks(i2)=-1
  w=where(ks ne -1)
  out=output_a2zp[w,*]
  kic_s=kic_s(i1)
  xx=alog10(numax(i1))
  yy=alog10(output_a2zp[i2,1])
  index_bad_zero = WHERE( finite(xx) lt 0.1)
  if (index_bad_zero(0) ne -1) then begin
    remove, index_bad_zero, xx, yy, kic_s, output_resize_b
  endif
  ee=where(finite(yy) lt 0.01)
  if (ee(0) ne -1) then begin
    remove, ee, xx, yy, kic_s, output_resize_b
  endif
  match, round(output_a2zp[i2,0]), round(output_resize_b), ind1, ind2, count=nn
  output_resize=output_a2zp[i2(ind1), *]
  res_solar = poly_fit(xx,yy,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
  threshold_ok=1*stddev(yy)
  pp=plot(10^xx, 10^yy, xtitle='(numax a2z KEPLER SOLARLIKE)', xlog=1,ylog=1,  ytitle='(metric)',symbol="D", SYM_FILLED=1, linestyle="none")
  pp1=plot(10^xx, 10^(res(1)*alog10(10^xx)+res(0)), /overplot, color='dark magenta', name='POWVAR KEPLER')
  pp1.thick=2
  rr=sort(xx)
  poly=polygon([10^xx(rr),10^reverse(xx(rr))], [10^(res(1)*alog10(10^xx(rr))+res(0)-threshold),reverse(10^(res(1)*alog10(10^xx(rr))+res(0)+threshold))], target=pp, /DATA,FILL_BACKGROUND=1,  $
    FILL_COLOR="dark magenta", FILL_TRANSPARENCY=60, TRANSPARENCY=60)
  ;X = [min(10^xx), max(10^xx), max(10^xx), min(10^xx)] & Y = [10^(res(1)*alog10(min(10^xx))+res(0)-threshold), 10^(res(1)*alog10(max(10^xx))+res(0)-threshold),$
  ; 10^(res(1)*alog10(max(10^xx))+res(0)+threshold), 10^(res(1)*alog10(min(10^xx))+res(0)+threshold)]
  pp2=plot(10^xx, 10^(res_solar(0)+(res_solar(1))*(xx)),xlog=1, ylog=1,/overplot, name='POWVAR '+'SOLARLIKE '+cadence+' '+fill+' '+strtrim(freq_inic_gr,1))
  pp2.thick=2
  poly=polygon([10^xx(rr),reverse(10^xx(rr))], [10^(res_solar(1)*alog10(10^xx(rr))+res_solar(0)+threshold_ok),reverse(10^(res_solar(1)*alog10(10^xx(rr))+res_solar(0)-threshold_ok))],target=pp,$
      /DATA, /FILL_BACKGROUND, FILL_COLOR="black", TRANSPARENCY=60, FILL_TRANSPARENCY=60)
  ll=LEGEND(TARGET=[pp1,pp2], POSITION=[1000, 900], $
  /DATA, /AUTO_TEXT_COLOR)
  pp.save, PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/'+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'slope'+'.png'

  save, file=PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.sav', OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN,$
     res_solar, threshold_ok, xx,yy
  close,2
  openw,2,PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.txt'
  close,2
  openw,2, PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/'+CHAMP+GOLD+cadence+'_'+fill+'_'+strtrim(freq_inic_gr,1)+'_'+'output_numax_all'+'.txt', /append
  printf,2, '#KIC K2 C7, PSD VARIABILITY, numax_guess, max_numax_guess, min_numax_guess , res_solar, threshold_solar'
  for ii=0, n_elements(NUMAX_GUESS)-1 do printf, 2, OUTPUT_A2Z[ii,0], OUTPUT_A2Z[ii,1], NUMAX_GUESS(ii), NG_MAX(ii), NG_MIN(ii), res_solar(0), res_solar(1), threshold_ok
  close,2
endif

;if STREGEX(STAR_KIC_LIST_TXT, 'CLUSTER_ENRICO') ne -1 then begin
;  ;readcol, '/Users/lbugnet/bin/path_'+strcompress(TYPE)+'.cfg', path, format='A'
;  close,2
;  openw,2,'/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/'+FILE_NAME_OUTPUT_A2ZP+'.txt'
;  close,2
;  openw,2, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/'+FILE_NAME_OUTPUT_A2ZP+'.txt', /append
;  printf,2, '#KIC K2 C7, PSD VARIABILITY '
;  for ii=0, n_elements(NUMAX_GUESS)-1 do printf, 2, OUTPUT_A2Z[ii,0], OUTPUT_A2Z[ii,1]
;  close,2
;
;endif

STOP

END