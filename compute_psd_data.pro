PRO COMPUTE_PSD_DATA,$
;IN
 STAR_PATH_LC=STAR_PATH_LC, STAR_LC=STAR_LC, STAR_PATH_PSD=STAR_PATH_PSD, STAR_KIC=STAR_KIC, STAR_PATH_KIC=STAR_PATH_KIC,$
  TYPE=TYPE, CHAMP=CHAMP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE,$
;OUT  
  STAR_TAB_PSD=STAR_TAB_PSD, ID_STAR=ID_STAR, HELP=HELP
;+
; :Author: Lisa BUGNET

; ---------------------------------------------------------------------------;
;                 PROGRAM TO COMPUTE PSD
; ---------------------------------------------------------------------------;
;
; NAME: COMPUTE_PSD_DATA.pro
;
; CALLING SEQUENCE:
;
;   COMPUTE_PSD_DATA, STAR_PATH_LC=STAR_PATH_LC, STAR_LC=STAR_LC, $
;     STAR_PATH_PSD=STAR_PATH_PSD, STAR_KIC=STAR_KIC, STAR_PATH_KIC=STAR_PATH_KIC,$
;     TYPE=TYPE, CHAMP=CHAMP, STAR_TAB_PSD=STAR_TAB_PSD, N_STARS_TOT=N_STARS_TOT, $
;     ID_STAR=ID_STAR
;
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
;
; --- STAR_PATH_LC optionnal ------------------------------------------------;
; --------------------- contains light curve path of the star ---------------;
;
; --- STAR_LC optionnal -----------------------------------------------------;
; --------------------- contains light curve of the star --------------------;

; --- STAR_PATH_PSD optionnal -----------------------------------------------;
; --------------------- contains PSD path------------------------------------;
;
; --- STAR_KIC optionnal ----------------------------------------------------;
; --------------------- contains star KIC/EPIC  -----------------------------;
;
; --- STAR_PATH_KIC optionnal -----------------------------------------------;
; --------------------- contains KIC path -----------------------------------;
;
; --- TYPE ----- STRING -----------------------------------------------------;
; --------------------- contains 'K2' or 'KEPLER' ---------------------------;
;
; --- CHAMP optionnal ----- STRING ------------------------------------------;
; --------------------- contains '3' or '4' or ... if TYPE = 'K2' -----------;

; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
;
; --- ID_STAR ----- STRING --------------------------------------------------;
; ------------------ STAR NAME ----------------------------------------------;
;
; --- STAR_TAB_PSD  ----- DBLARR(2) -----------------------------------------;
; --------------------- contains PSD of stars (frequency, POWER) ------------;
; ---------------------------------------------------------------------------;
;

;-  
 IF keyword_set(help) THEN BEGIN
  doc_library,'compute_psd_data', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
  RETURN
  ;you have to write PREPARE_FILES_A2ZPL, /help
ENDIF

if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

if (n_elements(TYPE) eq 0) then TYPE=' '
if ((n_elements(STAR_PATH_LC) eq 0) and (n_elements(STAR_LC) eq 0) and (n_elements(STAR_PATH_PSD) eq 0) and (n_elements(STAR_PSD) eq 0) and (n_elements(STAR_KIC) eq 0) and (n_elements(STAR_PATH_KIC) eq 0) and (TYPE eq 'K2')) then begin   
    readcol, '/Users/lbugnet/bin/compute_psd_data_k2.CFG', STAR_DATA, format='A', /silent
    STAR_PATH_LC=STAR_DATA(0)
    ;CHAMP=STAR_DATA(1) pas de raison !
endif    
    
if ((n_elements(STAR_PATH_LC) eq 0) and (n_elements(STAR_LC) eq 0) and (n_elements(STAR_PATH_PSD) eq 0) and (n_elements(STAR_PSD) eq 0) and (n_elements(STAR_KIC) eq 0) and (n_elements(STAR_PATH_KIC) eq 0) and (TYPE eq 'KEPLER')) then begin
  readcol, '/Users/lbugnet/bin/compute_psd_data_kepler.CFG', STAR_DATA, format='A', /silent
  STAR_PATH_PSD=STAR_DATA(0)
  print, 'kepler!'
endif    
    

if (n_elements(STAR_PATH_LC) ne 0) then begin

  EXTENSION='txt'
  sav=strpos(STAR_PATH_LC, 'sav')
  if sav ne -1 then EXTENSION='sav'
  fits=strpos(STAR_PATH_LC, 'fits')
  if fits ne -1 then EXTENSION='fits'
  dat=strpos(STAR_PATH_LC, 'dat')
  if dat ne -1 then EXTENSION='dat'
  LIGHT_CURVE_PATH_TO_PSD, STAR_PATH_LC=STAR_PATH_LC, STAR_TAB_PSD=STAR_TAB_PSD, EXTENSION=EXTENSION
  if TYPE eq 'K2' then begin
    if strpos(STAR_PATH_LC, 'ktwo') ne -1 then begin
      ID_STAR=STRMID(file_basename(STAR_PATH_LC), 4, 9)
    endif else begin
      ID_STAR=STRMID(file_basename(STAR_PATH_LC), 20, 9)
    endelse
  endif
  if TYPE eq 'KEPLER' then ID_STAR=STRMID(STAR_PATH_LC, 122,9)
endif

if (n_elements(STAR_LC) ne 0) then begin
  LIGHT_CURVE_TO_PSD, STAR_LC=STAR_LC, STAR_TAB_PSD=STAR_TAB_PSD
  ;;;;;;;;A VOIR SELON FORMAT FICHIER POUR ID_STAR !!!!!
endif

if (n_elements(STAR_PATH_PSD) ne 0) then begin
  EXTENSION='txt'
  sav=strpos(STAR_PATH_PSD, 'sav')
  if sav ne -1 then EXTENSION='sav'
  fits=strpos(STAR_PATH_PSD, 'fits')
  if fits ne -1 then EXTENSION='fits'
  PSD_PATH_TO_PSD, STAR_PATH_PSD=STAR_PATH_PSD, STAR_TAB_PSD=STAR_TAB_PSD, EXTENSION=EXTENSION
  if TYPE eq 'K2' then ID_STAR=STRMID(STAR_PATH_PSD, 60, 9)
  if TYPE eq 'KEPLER' then begin
    if STREGEX(STAR_PATH_PSD, 'polfitseg0_80.0000d') ne -1 then ID_STAR=STRMID(file_basename(STAR_PATH_PSD),4 , 9)
    if STREGEX(STAR_PATH_PSD, 'polfitseg960.000_20.0000d') ne -1 then ID_STAR=STRMID(file_basename(STAR_PATH_PSD),4 , 9)
    if STREGEX(STAR_PATH_PSD, 'polfitseg2880.00_2.00000d') ne -1 then ID_STAR=STRMID(file_basename(STAR_PATH_PSD),4 , 9)
    if STREGEX(STAR_PATH_PSD, PATH_DATA+'Solar_like/GOLD_STANDARDS/SC_PDC_DR25/RESULTS_KADACS_filt_polfitseg2880.00_2.00000d_ppm0_inpaint3_Start_Q5') ne -1 then ID_STAR=STRMID(file_basename(STAR_PATH_PSD),4 , 9)
    if STREGEX(STAR_PATH_PSD, PATH_DATA+'Solar_like/GOLD_STANDARDS/SC_PDC_DR25/RESULTS_KADACS_filt_polfitseg2880.00_2.00000d_ppm0_inpaint3_ALLQ') ne -1 then ID_STAR=STRMID(file_basename(STAR_PATH_PSD),4 , 9)
  endif
endif

if (n_elements(STAR_PATH_KIC) ne 0) then begin
  EXTENSION='txt'
  sav=strpos(STAR_PATH_KIC, 'sav')
  if sav ne -1 then EXTENSION='sav'
  fits=strpos(STAR_PATH_KIC, 'fits')
  if fits ne -1 then EXTENSION='fits'
  KICS_PATH_TO_PSD, STAR_PATH_KIC=STAR_PATH_KIC, STAR_TAB_PSD=STAR_TAB_PSD, TYPE=TYPE, CHAMP=CHAMP, ID_STAR=ID_STAR
endif

if (n_elements(STAR_KIC) ne 0) then begin
  EXTENSION='txt'
  KICS_TO_PSD, STAR_KIC=STAR_KIC, STAR_TAB_PSD=STAR_TAB_PSD, TYPE=TYPE, CHAMP=CHAMP, EXTENSION=EXTENSION, STAR_PATH_PSD=STAR_PATH_PSD
  ID_STAR=long(STAR_KIC)
endif

print, 'TYPE=', type
if TYPE eq 'K2' then print, 'champ', champ
END