PRO save_slope_powvar_numax,$
;IN
 OUTPUT_A2Z=OUTPUT_A2Z,  TYPE=TYPE, $
 CHAMP=CHAMP , SOLAR_LIKE=SOLAR_LIKE , CALCUL_SLOPE=CALCUL_SLOPE, FREQ_INIC_GR=FREQ_INIC_GR, FILL=FILL, $
 cadence=cadence, GOLD=GOLD, FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, PARAMS=PARAMS, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, $
 PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE,$
;OUT
 RES=RES, THRESHOLD=THRESHOLD, SLOPE_FIT=SLOPE_FIT ,XX=XX, YY=YY

;+
; :Author: Lisa BUGNET
;
; ---------------------------------------------------------------------------;
;           PROGRAM TO COMPUTE POWVAR SLOPE FROM POWVAR/NUMAX/DNU DATA
; ---------------------------------------------------------------------------;
;
; NAME: save_slope_powvar_numax.pro
;
; CALLING SEQUENCE:
;
;  save_slope_powvar_numax, OUTPUT_A2Z=OUTPUT_A2Z, XX=XX, YY=YY, TYPE=TYPE, $
;  RES=RES, THRESHOLD=THRESHOLD, SLOPE_FIT=SLOPE_FIT ,$
;  CHAMP=CHAMP , SOLAR_LIKE=SOLAR_LIKE , CALCUL_SLOPE=CALCUL_SLOPE, $
;  FREQ_INIC_GR=FREQ_INIC_GR, FILL=FILL, $
;  cadence=cadence, GOLD=GOLD, FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, $
;  PARAMS=PARAMS, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
; --- OUTPUT_A2Z  ----- DBLARR(*,11) ----------------------------------------;
; --- OUTPUT_A2Z contains the POWVAR metric P_var ---------------------------;
; --------------------- OUTPUT_A2ZP(*,0)=EPIC/KIC ---------------------------;
; --------------------- OUTPUT_A2ZP(*,1)=P_VAR=DATA-NOISE (ppm^2/muHz) ------;
; --------------------- OUTPUT_A2ZP(*,3:6)=DATA -----------------------------;
; --------------------- OUTPUT_A2ZP(*,7:10)=NOISE ---------------------------;
; ---------------------------------------------------------------------------;
;
; --- TYPE ----- STRING -----------------------------------------------------;
; --------------------- contains 'K2' or 'KEPLER' ---------------------------;
;
; --- CHAMP optionnal ----- STRING ------------------------------------------;
; --------------------- contains '3' or '4' or ... if TYPE = 'K2' -----------;
;
; --- SOLAR_LIKE optionnal --------------------------------------------------;
; --- set SOLAR_LIKE='SOLAR_LIKE' if super Nyquist of main sequence stars ---;
; 
; --- FILL optionnal --------------------------------------------------------;
; --------------------- contains 'ALLQ' or 'Q5' for GOLD AND NO_GOLD STARS --;
; 
; --- cadence optionnal -----------------------------------------------------;
; --------------------- contains 'LC' or 'SC' for KEPLER stars --------------;
; 
; --- GOLD optionnal --------------------------------------------------------;
; --- set GOLD='GOLD' (3 years data) or GOLD='NO_GOLD' (3 months data) ------;
; 
; --- CALCUL_SLOPE =1 for POWVAR slope study --------------------------------;
; 
; --- FREQ_INIC_GR = initial frequency of data ------------------------------;
;
; --- FILE_OUTPUT_A2Z_PATH optionnal ----------------------------------------;
; --------------------- contains the path of already-calculed POWVAR --------;
;
; --- PARAMS optionnal ------------------------------------------------------;
; --------------------- contains name of data to use ------------------------;
; --------------------- default is ['numax', 'dnu'] -------------------------;
;
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
;
; --- RES -------------------------------------------------------------------;
; --------------------- contains (bx+a) a=res(0), b=res(1) ------------------;
; --------------------- regression of slope P_var VS numax/dnu/... ----------;
; 
; --- THRESHOLD -------------------------------------------------------------;
;---------------------- contains distance to the powvar law in sigma -------;
; ---------------------- default value is 1 ---------------------------------;
  
; --- SLOPE_FIT -------------------------------------------------------------;
; ---------------------- contains value of ax+b -----------------------------;
;
; --- XX ----- DBLARR(*) ----------------------------------------------------;
; --------------------- contains alog10(numax/dnu/...) ----------------------;
;
; --- YY ----- DBLARR(*) ----------------------------------------------------;
; --------------------- contains alog10(P_var)=alog10(OUTPUT_A2Z[*,1]) ------;
; ---------------------------------------------------------------------------;
;-

  IF keyword_set(help) THEN BEGIN
    doc_library,'save_slope_powvar_numax', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
    RETURN
    ;you have to write save_slope_powvar_numax, /help
  ENDIF

if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

GLOBAL=''

if CALCUL_SLOPE eq 1 then begin

COMPUTE_SLOPE=1 ; PAR DEFAUT ON CALCULE LA PENTE SUR LES DONNEES
;COMPUTE_SLOPE=0 ;si on veut utiliser la pente totale Kepler (numax)
  if (n_elements(FILE_OUTPUT_A2Z_PATH) eq 1) then begin
    if STREGEX(FILE_OUTPUT_A2Z_PATH, 'sav') ne -1 then restore, FILE_OUTPUT_A2Z_PATH else print, 'output_a2z understood';RESTORE OUTPUT_A2Z DATA
  endif

  if TYPE eq 'KEPLER' then begin
    OUTPUT_A2Zp=OUTPUT_A2Z
    if SOLAR_LIKE eq '' then begin
      ;file 2: Contient les 16000 étoiles dont on connait un numax par A2Z
      restore, PATH_TABLE+'results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
      readcol,  PATH_TABLE+'Vrard_2016_6111_DP.txt', kic_vrard, dnu_vrard, dp_vrard, edp_vrard, q_vrard, m_vrard, em_vrard, alias_vrard, measure_vrard, status_vrard, /silent, format='L,D,D,D,D,D,D,A,A,A', stringskip='#', DELIMITER=';'
      ;match, kic_vrard, kic_s, i1, i2
      ;pp=plot(dnu_vrard(i1), dnu(i2))
    endif else if SOLAR_LIKE eq 'SOLAR_LIKE' then begin
      readcol, PATH_OUTPUT+TYPE+'/SOLAR_LIKE/A2Z_results_goldstd_sorted.txt', KIC_s, flagg, fmin, fmax, numax, enumax, format='L,D,D,D,D,D'
    endif
    
  endif

  if (TYPE eq 'K2') then begin
    OUTPUT_A2Zp=OUTPUT_A2Z
    if (where(file_search(PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest'+'.sav') eq '') eq 0) then begin
      readcol, PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest.txt',kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax,  format='L,D,D,D,D,D,D', /silent
      save, file=PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest'+ '.sav', kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax
    endif else begin
      restore, PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest.sav', /verbose ;kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax ; contient en fait que les champs 4,6 et 7
    endelse
    if CHAMP eq '3' then begin
      restore, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/RESULT_A2zr/RESULTS_ACF_slope_gt_6_lor_fwhmlt035_GOOD_ge42le212_ampge11_gran_metrics_TEST.sav', /verbose ; pour le champ 3
      kic_s=long(OUT_BACK_ACF[*,0])
      dnu=OUT_BACK_ACF[*,3]
      numax=OUT_BACK_ACF[*,2]
      numax2=OUT_BACK_ACF[*,6] 
      readcol, PATH_DATA+'results_SAVITA_RG_K2_C3_Poles_05muHz_dnu.txt', KIC_savi , Dnu_s    , ErrDnu_s ,  fmin_s   ,  fmax_s   ,  numax_s   , Err_nu_max_s,Dnu_estimated_s, /silent, format='L,D,D,D,D,D,D,D'
      ;check good
      readcol, PATH_TABLE+'list_K2_C3_Ok_A2Z_A2ZR.txt', kic_good, numax_good, /silent, format='L'

    endif
  endif

;BEGIN LOOP on PARAMS
if n_elements(CHAMP) eq 0 then CHAMP=''

FOR ii=0, n_elements(PARAMS)-1 do begin
  if (PARAMS[ii] eq 'numax') then numax=numax
  
  if (PARAMS[ii] eq 'vrard_a2z_dnu') and (TYPE eq 'KEPLER') then begin
    match, long(OUTPUT_A2Zp[*,0]), long(kic_s), ind1, ind2
    numax=dnu(ind2)
    kic_s=kic_s(ind2)
    OUTPUT_A2Zp=OUTPUT_A2Zp[ind1,*]
    match, long(OUTPUT_A2Zp[*,0]), long(kic_vrard), i1, i2
    kic_s=kic_vrard(i2)
    OUTPUT_A2Zp1=OUTPUT_a2zp[i1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    ;numax=dnu_vrard(i2)
    numax=numax(i1)
    status=status_vrard(i2)
    m_vrard=m_vrard(i2)
  endif
  
  if (PARAMS[ii] eq 'vrard_dnu') and (TYPE eq 'KEPLER') then begin
    match, long(OUTPUT_A2Zp[*,0]), long(kic_s), ind1, ind2
    numax=dnu(ind2)
    kic_s=kic_s(ind2)
    OUTPUT_A2Zp=OUTPUT_A2Zp[ind1,*]
    stop
    match, long(OUTPUT_A2Zp[*,0]), long(kic_vrard), i1, i2
    kic_s=kic_vrard(i2)
    OUTPUT_A2Zp1=OUTPUT_a2zp[i1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    numax=dnu_vrard(i2)
    status=status_vrard(i2)
    m_vrard=m_vrard(i2)
    stop
  endif
  
  if (PARAMS[ii] eq 'numax_savita') then begin ; ne fonctionne pas si données mauvaises !
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    numax_s=numax_s(ind2)
    numax=numax_s
    kic_s=OUTPUT_a2zp[ind1,0]
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
  endif
  
  if (PARAMS[ii] eq 'dnu_savita') then begin
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    dnu_s=dnu_s(ind2)
    numax=dnu_s 
    kic_s=OUTPUT_a2zp[ind1,0]
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
  endif
  
  if (PARAMS[ii] eq 'dnu') then numax=dnu
  
  if (PARAMS[ii] eq 'numax_verif') then numax=numax2
  
  if (PARAMS[ii] eq 'numax_good') and (CHAMP eq '3') then begin
    match, long(OUTPUT_A2Zp[*,0]), long(kic_good), i1, i2
    kic_s=OUTPUT_A2Zp[i1,0]
    OUTPUT_A2Zp1=OUTPUT_a2zp[i1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    numax=numax_good(i2)   
  endif

  if (PARAMS[ii] eq 'numax_savita_miss') then begin ; les missclassified n'ont pas encore étées calculées par powvar.....
   ;a2zpl, FILE_OUTPUT_A2Z_PATH='no_matter_what', MAG_COR=0, PARAMS=['numax_savita_miss','dnu_savita_miss'], CALCUL_SLOPE=1
   readcol, PATH_TABLE+'Missclassified_Savita.txt',  Full,   P   ,   KIC_savi,  Stage_s, Kpmag_s , Teff_s, Teffr_s ,  dnu_s  , numax_s   , logg_s  , Mass_s,    Rad_s, stringskip='#',  /silent, format='D,D,L,D,D,D,D,D,D,D,D,D'
    restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all_missclassified.sav', /verbose
    OUTPUT_a2zp=output_a2z
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    ;numax_s=numax_s(ind2)
    numax=numax_s(ind2)
    kic_s=OUTPUT_a2zp[ind1,0]
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    COMPUTE_SLOPE=0
  endif

  if (PARAMS[ii] eq 'dnu_savita_miss') then begin ; les missclassified n'ont pas encore étées calculées par powvar.....
    readcol, PATH_TABLE+'Missclassified_Savita.txt',  Full,   P   ,   KIC_savi,  Stage_s, Kpmag_s , Teff_s, Teffr_s ,  dnu_s  , numax_s   , logg_s  , Mass_s,    Rad_s, stringskip='#',  /silent, format='D,D,L,D,D,D,D,D,D,D,D,D'
    restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all_missclassified.sav', /verbose
    OUTPUT_a2zp=output_a2z
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    ;numax_s=numax_s(ind2)
    numax=dnu_s
    kic_s=OUTPUT_a2zp[ind1,0]
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    COMPUTE_SLOPE=0
  endif

  if (PARAMS[ii] eq 'numax_savita_RG_LIST_15470') then begin ; les missclassified n'ont pas encore étées calculées par powvar.....
    readcol, PATH_TABLE+'list_kic_knownRG_DR25_nomisc.txt',  Kic_savi, stringskip='#',  /silent, format='L'
    readcol, PATH_TABLE+'list_param_known_RG_DR25.txt', KIC_RG, kp_s, teff_s, logg_s, dnu_s, numax_s, stringskip='#', /silent, format='L,D,D,D,D,D'
    match, long(kic_savi), long(kic_rg), i1, i2
    numax_s=numax_s(i2)
    readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
    OUTPUT_A2Zp=dblarr(n_elements(output_a2z0), 11)
    OUTPUT_a2zp[*,0]=output_a2z0[*]
    OUTPUT_A2Zp[*,1]=OUTPUT_A2Z1[*]
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    ;numax_s=numax_s(ind2) ne pas mettre sinon faux
    numax=numax_s
    kic_s=kic_savi(ind2)
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    OUTPUT_A2Z=OUTPUT_A2Zp
  endif

  if (PARAMS[ii] eq 'dnu_savita_RG_LIST_15470') then begin ; les missclassified n'ont pas encore étées calculées par powvar.....
    readcol, PATH_TABLE+'list_kic_knownRG_DR25_nomisc.txt',  Kic_savi, stringskip='#',  /silent, format='L'
    readcol, PATH_TABLE+'list_param_known_RG_DR25.txt', KIC_RG, kp_s, teff_s, logg_s, dnu_s, numax_s, stringskip='#', /silent, format='L,D,D,D,D,D'
    match, long(kic_savi), long(kic_rg), i1, i2
    numax_s=dnu_s(i2)
    readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
    OUTPUT_A2Zp=dblarr(n_elements(output_a2z0), 11)
    OUTPUT_a2zp[*,0]=output_a2z0[*]
    OUTPUT_A2Zp[*,1]=OUTPUT_A2Z1[*]
    match, long(OUTPUT_A2Zp[*,0]), long(kic_savi), ind1, ind2
    ;numax_s=numax_s(ind2) ne pas mettre sinon faux
    numax=numax_s
    kic_s=kic_savi(ind2)
    OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
    OUTPUT_A2Zp=OUTPUT_A2Zp1
    OUTPUT_A2Z=OUTPUT_A2Zp
  endif

if (PARAMS[ii] eq 'can_numax') then begin 
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax , skipline=0,  /silent, format='L'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  kic_s=kic(ind2)
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
endif

if (PARAMS[ii] eq 'can_dnu') then begin
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax , dnu,  skipline=1,  /silent, format='L'
  numax=dnu
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Z[*,0]), long(Kic), ind1, ind2
  kic_s=kic(ind2)
  OUTPUT_A2Zp1=OUTPUT_a2z[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
endif
if (PARAMS[ii] eq 'cor_numax') then begin
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax, dnu, skipline=1,  /silent, format='L'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)


endif
if (PARAMS[ii] eq 'cor_dnu') then begin
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax, dnu, skipline=1,  /silent, format='L'
  numax=dnu
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)

endif
if (PARAMS[ii] eq 'oct_numax') then begin
  
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax2, dnu2, numax, skipline=1,  /silent, format='L'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)

endif
if (PARAMS[ii] eq 'oct_dnu') then begin
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax2, dnu2, numax, dnu, skipline=1,  /silent, format='L'
  numax=dnu
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)

endif

if (PARAMS[ii] eq 'syd_numax') then begin
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax2, dnu2, numax3, dnu3, numax, skipline=1,  /silent, format='L'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)

endif
if (PARAMS[ii] eq 'syd_dnu') then begin 
  readcol, PATH_TABLE+'comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax2, dnu2, numax3, dnu3, numax, dnu, skipline=1,  /silent, format='L'
  numax=dnu
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)
endif
if (PARAMS[ii] eq 'dnu_APOKASC_A2ZP') then begin
  restore, '/Users/lbugnet/DATA/TABLES/Results_ACF_MODEL_CCF_REFINE_MLEfit_v1.sav', /verbose
  numax=OUT_PAR_ALL_MLE(4,*)
  kic=LONG(OUT_PAR_ALL_MLE(0,*))
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__7_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  numax=numax(ind2)
  kic_s=kic(ind2)
endif
if (PARAMS[ii] eq 'dnu_APOKASC_A2Z') then begin
  restore, PATH_TABLE+'results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
  numax=dnu
  kic=kic_s
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC__7_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  numax=numax(ind2)
  kic_s=kic(ind2)
endif

GG=1
if GG eq 1 then begin
  ;plot en logg
  restore, '/Users/lbugnet/DATA/TABLES/Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav', /verbose
  Teff=Teff_in
  FDRM, dnu, numax, Teff, R, M, logg
  numax=logg
endif
  
 ;--------------------------------------------------------------------
  ;-------------- COMPUTE SLOPE ---------------------------------------
  ;--------------------------------------------------------------------
if COMPUTE_SLOPE eq 1 then begin
  match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
  output_resize_b=output_a2zp[i2,*]

  ks=long(output_a2zp[*,0])
  ks(i2)=-1
  w=where(ks ne -1)
  out=output_a2zp[w,*]
  outkic=out[*,0]

  kic_s=kic_s(i1)
  xx=alog10(numax(i1))
  yy=alog10(output_a2zp[i2,1])
  ;yy=alog10(output_a2zp[i2,5]+output_a2zp[i2,1]-output_a2zp[i2,3]) ;median
  www=where(xx gt alog10(freq_inic_gr))
  xx_1=xx(www)
  yy_1=yy(www)

  index_bad_zero = WHERE( finite(xx_1) lt 0.01)
  
  if (index_bad_zero(0) ne -1) then begin
    remove, index_bad_zero, xx_1, yy_1, kic_s, output_resize_b
  endif
  
  ee=where(finite(yy_1) lt 0.01)
  
  if (ee(0) ne -1) then begin
    remove, ee, xx_1, yy_1, kic_s, output_resize_b
  endif
  
  match, round(output_a2zp[i2,0]), round(output_resize_b), ind1, ind2, count=nn
  output_resize=output_a2zp[i2(ind1), *]

;------------------------- FIT --------------------------------------
  res = poly_fit(xx_1,yy_1,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
  slope_fit = res(0) + res(1)*xx_1


;------------- IMPROVE METRIC ---------------------------------------
  flag_bad_stars,  RES=RES, OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY_1, SLOPE_FIT=SLOPE_FIT, XX=XX_1, flag=flag, threshold=threshold

  flag_metric=flag
  xx_ok=xx_1(where(flag_metric eq 0))
  yy_ok=yy_1(where(flag_metric eq 0))
  res_ok = poly_fit(xx_ok,yy_ok,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
  slope_fit_ok = res_ok(0) + res_ok(1)*xx_ok

  Residuals_ok = yy_ok-slope_fit_ok
  threshold_ok=1*stddev(yy_ok)

  w_up=where((residuals_ok) gt (threshold_ok))
  w_do=where((residuals_ok) lt (-threshold_ok))

  if w_do(0) ne -1 then begin
    match, xx_ok(w_do), xx, xx_ok1,xx1, count=mm
    flag_metric(xx1)=1
  endif

  if w_up(0) ne -1 then begin
    match, xx_ok(w_up), xx, xx_ok2,xx2, count=mm
    flag_metric(xx2)=2
  endif

  ;;------ ACTIVER SI ON VEUT GARDER QUE LES NUMAX OK ------------------
  ;if w_do(0) ne -1 then begin
  ;  remove, w_do, xx_ok, yy_ok,residuals_ok,slope_fit_ok
  ;endif
  ;if w_up(0) ne -1 then begin
  ;  remove, w_up, xx_ok, yy_ok,residuals_ok,slope_fit_ok
  ;endif
  ;xx=xx_ok
  ;yy=yy_ok
  ;slope_fit=slope_fit_ok
  ;---------------------------------------------------------------------

  res=res_ok
  slope_fit=res(0) + res(1)*xx
  residuals=residuals_ok
  ;threshold= 1*stddev(yy_ok)
  global=''
  
endif else if COMPUTE_SLOPE eq 0 then begin ;We take powvar slope from 15470 RG stars
  global='GLOBAL_LAW'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_numax20J_ALL_KEPLER_LC0.700000_.sav', /verbose ; global law for numax
  if (PARAMS[ii] eq 'numax_savita_miss') then restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_numax_savita_RG_LIST_1547020J_ALL_KEPLER_0.700000_.sav', /verbose
  if (PARAMS[ii] eq 'dnu_savita_miss') then restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_dnu_savita_RG_LIST_1547020J_ALL_KEPLER_0.700000_.sav', /verbose

  match,long(output_a2zp[*,0]),long(kic_s),i2,i1,count=n
  output_resize_b=output_a2zp[i2,*]

  ks=long(output_a2zp[*,0])
  ks(i2)=-1
  w=where(ks ne -1)
  out=output_a2zp[w,*]
  outkic=out[*,0]

  kic_s=kic_s(i1)
  xx=alog10(numax(i1))
  yy=alog10(output_a2zp[i2,1])

  www=where(xx gt alog10(freq_inic_gr))
  xx_1=xx(www)
  yy_1=yy(www)

  index_bad_zero = WHERE( finite(xx_1) lt 0.01)
  
  if (index_bad_zero(0) ne -1) then begin
    remove, index_bad_zero, xx_1, yy_1, kic_s, output_resize_b
  endif
  
  ee=where(finite(yy_1) lt 0.01)
  
  if (ee(0) ne -1) then begin
    remove, ee, xx_1, yy_1, kic_s, output_resize_b
  endif
  
  match, round(output_a2zp[i2,0]), round(output_resize_b), ind1, ind2, count=nn
  output_resize=output_a2zp[i2(ind1), *]

  slope_fit=res(0) + res(1)*xx_1
  flag_bad_stars,  RES=RES, OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY_1, SLOPE_FIT=SLOPE_FIT, XX=XX_1, flag=flag, threshold=threshold

  flag_metric=flag
  xx_ok=xx_1(where(flag_metric eq 0))
  yy_ok=yy_1(where(flag_metric eq 0))
  res_ok = poly_fit(xx_ok,yy_ok,1,yfit=yfit) ;res(0)=b, res(1)=a pour y=ax+b
  slope_fit_ok = res_ok(0) + res_ok(1)*xx_ok

  Residuals_ok = yy_ok-slope_fit_ok
  threshold_ok=1*stddev(yy_ok)

  w_up=where((residuals_ok) gt (threshold_ok))
  w_do=where((residuals_ok) lt (-threshold_ok))

  if w_do(0) ne -1 then begin
    match, xx_ok(w_do), xx, xx_ok1,xx1, count=mm
    flag_metric(xx1)=1
  endif

  if w_up(0) ne -1 then begin
    match, xx_ok(w_up), xx, xx_ok2,xx2, count=mm
    flag_metric(xx2)=2
  endif

  ;;------ ACTIVER SI ON VEUT GARDER QUE LES NUMAX OK ------------------
  ;if w_do(0) ne -1 then begin
  ;  remove, w_do, xx_ok, yy_ok,residuals_ok,slope_fit_ok
  ;endif
  ;if w_up(0) ne -1 then begin
  ;  remove, w_up, xx_ok, yy_ok,residuals_ok,slope_fit_ok
  ;endif
  ;xx=xx_ok
  ;yy=yy_ok
  ;slope_fit=slope_fit_ok
  ;---------------------------------------------------------------------

  res=res_ok
  slope_fit=res(0) + res(1)*xx
  residuals=residuals_ok
  ;threshold= 1*stddev(yy_ok)
endif
  ;--------------------------------------------------------------------
  ;--------- DETECT OUTLIERS ------------------------------------------
  ;--------------------------------------------------------------------
  PARAM=PARAMS[ii]
  flag=dblarr(n_elements(residuals))
  
  ;--- check slope OK ---
    if (abs(res[1]) lt 1) then begin
      if type eq 'K2' then begin
        res[0]=4.7966292
        res[1]=-1.1220903
        threshold=0.36497251
        slope_fit=res(0) + res(1)*xx
        residuals=yy-slope_fit
        print, 'WARNING BAD SLOPE DETECTED'
      endif    
    endif
  ;---------------------
  
  flag_bad_stars,  RES=RES, OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY, SLOPE_FIT=SLOPE_FIT, XX=XX, FLAG=FLAG, threshold=threshold
  detect_outliers_powvar,  OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY, XX=XX, RES=RES, THRESHOLD=THRESHOLD, FLAG=FLAG, SLOPE_FIT=SLOPE_FIT, GLOBAL=GLOABL, TYPE=TYPE, CHAMP=CHAMP, SOLAR_LIKE=SOLAR_LIKE, fill=fill,cadence=cadence, FREQ_INIC_GR=FREQ_INIC_GR, GOLD=GOLD, PARAM=PARAM, status=status, m_vrard=m_vrard, PATH_OUTPUT=PATH_OUTPUT, GG=GG
  print, 'pourcentage out '+PARAMS[ii]+'='
  print, float(n_elements(where(flag eq 1))+n_elements(where(flag eq 2)))/float(n_elements(xx))*100.



  ;--------- SAVE -----------------------------------------------------
  if TYPE eq 'KEPLER' then begin
    save, file=PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/KEPLER_varlaw_'+PARAMS[ii]+'20J_ALL_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_'+fill+'.sav', res, slope_fit, residuals, threshold, xx, yy, flag ; a 20J
  endif else if TYPE eq 'K2' then begin
    save, file=PATH_OUTPUT+''+TYPE+'/C'+CHAMP+'/varlaw_'+PARAMS[ii]+'_C'+CHAMP+'_'+strtrim(freq_inic_gr,1)+'_'+fill+'.sav', res, slope_fit, residuals, threshold, xx, yy, flag ; a 20J
  endif
    
  if type eq 'KEPLER' then begin    
    if (PARAMS[ii] eq 'numax') then begin
    ;;; SAVE OUT A2Z STARS
      close, 2
      openw, 2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/POWVAR_OUT_A2Z_STARS.txt'
      close,2
      openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/POWVAR_OUT_A2Z_STARS.txt', /append
      for ii=0, n_elements(out[*,0])-1 do printf, 2, long(outkic(ii)), long(out[ii,1])
      close,2

    ;;; SAVE A2Z STARS
      close, 2
      openw, 2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/POWVAR_A2Z_STARS.txt'
      close,2
      openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/POWVAR_A2Z_STARS.txt', /append
      for ii=0, n_elements(xx)-1 do printf, 2, long(kic_s(ii)), long(10^yy(ii))
      close,2
    endif
  endif
  
ENDFOR ; END LOOP on PARAMS
    
    
    
;  if TYPE eq 'KEPLER' then begin
;    ;;------------------------- MATCH OUTLIERS ---------------------------------------------------
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt', kic_dnu,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_low_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt', kic_dnu,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_high_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt', kic_dnu, /silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_ok_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    ;;;  NUMAX OK BUT WRONG DNU
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt', kic_numax,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt', kic_dnu1,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu1), ind1, ind2
;    ;print, long(kic_numax(ind1))
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt', kic_dnu2,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu2), ind11, ind22
;    ;print, long(kic_numax(ind11))
;
;
;    close, 2
;    openw, 2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/numax_ok_dnu_a2z_bad.txt'
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/numax_ok_dnu_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind11))-1 do printf, 2, long(kic_numax(ind11(ii)))
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/numax_ok_dnu_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind1))-1 do printf, 2, long(kic_numax(ind1(ii)))
;    close, 2
;
;    ;;;  DNU OK BUT WRONG NUMAX
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_dnu_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt', kic_numax,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt', kic_dnu1,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu1), ind1, ind2
;    ;print, long(kic_numax(ind1))
;    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'detect_outliers_powvar_numax_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt', kic_dnu2,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu2), ind11, ind22
;    ;print, long(kic_numax(ind11))
;
;    close, 2
;    openw, 2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/dnu_ok_numax_a2z_bad.txt'
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/dnu_ok_numax_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind11))-1 do printf, 2, long(kic_numax(ind11(ii)))
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/dnu_ok_numax_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind1))-1 do printf, 2, long(kic_numax(ind1(ii)))
;    close, 2
;    
;  
;  endif else if TYPE eq 'K2' then begin
; 
;    ;;------------------------- MATCH OUTLIERS ---------------------------------------------------
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt', kic_dnu,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_low_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt', kic_dnu,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_high_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt', kic_dnu, /silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt', kic_numax,/silent,format='L'
;
;    match, long(kic_dnu), long(kic_numax), i1, i2
;    print, 'tot_ok_dnu'
;    print, n_elements(kic_dnu)
;    print, 'commun numax a2z'
;    print, n_elements(i1)
;
;    ;;;  NUMAX OK BUT WRONG DNU
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt', kic_numax,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt', kic_dnu1,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu1), ind1, ind2
;    ;print, long(kic_numax(ind1))
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt', kic_dnu2,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu2), ind11, ind22
;    ;print, long(kic_numax(ind11))
;
;
;    close, 2
;    openw, 2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/numax_ok_dnu_a2z_bad.txt'
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+'/C'+CHAMP+'/numax_ok_dnu_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind11))-1 do printf, 2, long(kic_numax(ind11(ii)))
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+'/C'+CHAMP+'/numax_ok_dnu_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind1))-1 do printf, 2, long(kic_numax(ind1(ii)))
;    close, 2
;
;    ;;;  DNU OK BUT WRONG NUMAX
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_dnu_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt', kic_numax,/silent,format='L'
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt', kic_dnu1,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu1), ind1, ind2
;    ;print, long(kic_numax(ind1))
;    readcol, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_numax_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt', kic_dnu2,/silent,format='L'
;    match, long(kic_numax), long(kic_dnu2), ind11, ind22
;    ;print, long(kic_numax(ind11))
;
;    close, 2
;    openw, 2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/dnu_ok_numax_a2z_bad.txt'
;    close,2
;    openw, 2,  PATH_OUTPUT+TYPE+'/C'+CHAMP+'/dnu_ok_numax_a2z_bad.txt', /append
;    for ii=0, n_elements(kic_numax(ind11))-1 do printf, 2, long(out[ii,0]), long(out[ii,1])
;    close, 2 
;  
;  endif
;

;--------------------------------------------------------------------;--------------------------------------------------------------------
;--------------------------------------------------------------------;--------------------------------------------------------------------
;--------------------------------------------------------------------;--------------------------------------------------------------------
;--------------------------------------------------------------------;--------------------------------------------------------------------

endif else begin ; IF CALCUL_SLOPE=0 
  
;  restore, PATH_OUTPUT+TYPE+'/KEPLER_varlaw_20J_ALL.sav';, res, slope_fit, residuals, threshold, xx, yy ; a 20J
;  
;  if TYPE eq 'KEPLER' then begin
;    OUTPUT_A2Zp=OUTPUT_A2Z
;    restore, PATH_TABLE+'results_A2Z_all.sav', /verbose  ;KIC_S, dnu, ednu, fmin, fmax, numax
;    if SOLAR_LIKE eq 'SOLAR_LIKE' then begin
;      if (where(file_search(PATH_OUTPUT+TYPE+'/SOLAR_LIKE/A2Z_results_goldstd_sorted'+'.sav') eq '') eq 0) then begin
;        readcol, PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/A2Z_results_goldstd_sorted.txt', KIC_s, flag, fmin, fmax, numax, enumax,format='D,D,D,D,D,D,D', /silent
;        save, file=PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/A2Z_results_goldstd_sorted.sav', KIC_s, flag, fmin, fmax, numax, enumax
;      endif else begin
;        restore, PATH_OUTPUT+TYPE+'/'+SOLAR_LIKE+'/A2Z_results_goldstd_sorted.sav';, KIC_s, flag, fmin, fmax, numax, enumax
;      endelse
;    endif
;  endif
;
;
;  if TYPE eq 'K2' then begin
;    OUTPUT_A2Zp=OUTPUT_A2Z
;    if (where(file_search(PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest'+'.sav') eq '') eq 0) then begin
;      readcol, PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest.txt',kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax,  format='D,D,D,D,D,D,D', /silent
;      save, file=PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest'+ '.sav', kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax
;    endif else begin
;      restore, PATH_OUTPUT+TYPE+'/ALL/A2Z_results_K2_ALL_Everest.sav', /verbose ;kic_s, numax, err_numax,dnu, err_Dnu, Amax, err_Amax
;    endelse
;  endif
;  
endelse

END