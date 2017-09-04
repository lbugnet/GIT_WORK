PRO prepare_files_a2zpl,$
;IN
 STAR_PATH_PSD_LIST=STAR_PATH_PSD_LIST, STAR_PATH_LC_LIST=STAR_PATH_LC_LIST, STAR_KIC_LIST_TXT=STAR_KIC_LIST_TXT, FILE_OUTPUT_A2Z_PATH=FILE_OUTPUT_A2Z_PATH, $
 STAR_PATH_KIC_LIST=STAR_PATH_KIC_LIST, N_STARS_TOT=N_STARS_TOT, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE,$
;OUT
 TYPE=TYPE, CHAMP=CHAMP, HELP=HELP


;+
; :Author: Lisa BUGNET

; ---------------------------------------------------------------------------;
;           PROGRAM TO PREPARE FILES FOR A2ZPL FOR POWVAR.pro
; ---------------------------------------------------------------------------;
;
; NAME: prepare_files_a2zpl.pro
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
; 
; --- STAR_PATH_LC_LIST optionnal -------------------------------------------;
; --------------------- contains light curve paths of stars -----------------;
;
; --- STAR_PATH_PSD_LIST optionnal ------------------------------------------;
; --------------------- contains PSD paths of stars -------------------------;
;
; --- STAR_KIC_LIST_TXT optionnal -------------------------------------------;
; --------------------- contains star KIC/EPIC list -------------------------;
;
; --- STAR_PATH_KIC_LIST optionnal ------------------------------------------;
; --------------------- contains KIC paths of stars -------------------------;
; 
; --- N_STARS_TOT -----------------------------------------------------------;
; --------------------- number of stars in the sample -----------------------;
; 
; 
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
; 
; --- TYPE ----- STRING -----------------------------------------------------;
; --------------------- contains 'K2' or 'KEPLER' ---------------------------;
;
; --- CHAMP optionnal ----- STRING ------------------------------------------;
; --------------------- contains '3' or '4' or ... if TYPE = 'K2' -----------;
; 
; ---------------------------------------------------------------------------;
; 
;-
  
  IF keyword_set(help) THEN BEGIN
    doc_library,'prepare_files_a2zpl', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
    RETURN
    ;you have to write PREPARE_FILES_A2ZPL, /help
  ENDIF
  
if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

path_temp_K2_C6=PATH_DATA+'K2/GAP/JOEL/C6/rescale_C6'
path_temp_K2_C4=PATH_DATA+'K2/GAP/JOEL/C4/rescale_C4'
path_temp_K2_C7=PATH_DATA+'K2/GAP/JOEL/C7/rescale_C7'
path_dd_K2_C6=PATH_OUTPUT+'K2/C6'
path_dd_K2_C4=PATH_OUTPUT+'K2/C4'
path_dd_k2_c7=PATH_OUTPUT+'K2/C7'
path_K2_ALL=PATH_OUTPUT+'K2/ALL'
if (n_elements(STAR_PATH_PSD_LIST) ne 0) then begin
  if n_elements(STAR_PATH_PSD_LIST) eq 1 then begin
    STAR_PATH_PSD_LIST=file_search(STAR_PATH_PSD_LIST )
  endif
  STAR_PATH_PSD=STAR_PATH_PSD_LIST(0)
  n_stars_tot=n_elements(STAR_PATH_PSD_LIST)
  if STREGEX(STAR_PATH_PSD, 'K2') ne -1 then begin 
    TYPE='K2'
    champ=''
    if STREGEX(STAR_PATH_PSD, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_PATH_PSD, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_PATH_PSD, 'C7') ne -1 then champ='7'
    if (file_dirname(STAR_PATH_PSD) eq (path_K2_ALL)) or (STREGEX(STAR_PATH_PSD, 'ALL') ne -1) or (STREGEX(STAR_PATH_PSD, 'C*') eq -1) then champ='ALL'
  endif  
    if (STREGEX(STAR_PATH_PSD, 'KEPLER') ne -1) or (STREGEX(STAR_PATH_PSD, 'kpl') ne -1) or (STREGEX(STAR_PATH_PSD, 'K00') ne -1) or (STREGEX(STAR_PATH_PSD, 'RG_DR25') ne -1) or (STREGEX(STAR_PATH_PSD, 'LC') ne -1) or (STREGEX(STAR_PATH_PSD, 'SC') ne -1)then TYPE='KEPLER'
  print, 'TYPE=', TYPE
endif

if (n_elements(STAR_PATH_LC_LIST) ne 0) then begin
  if n_elements(STAR_PATH_LC_LIST) eq 1 then begin
    STAR_PATH_LC_LIST=file_search(STAR_PATH_LC_LIST )
  endif

  STAR_PATH_LC=STAR_PATH_LC_LIST(0)
  STAR_PATH_LC_LAST=STAR_PATH_LC_LIST(n_elements(STAR_PATH_LC_LIST)-1)
  N_STARS_TOT=n_elements(STAR_PATH_LC_LIST)
  print, 'n_stars_tot', n_stars_tot
 if (STREGEX(STAR_PATH_LC, 'K2') ne -1) or (STREGEX(STAR_PATH_LC, 'ktwo') ne -1) or (STREGEX(STAR_PATH_LC, 'k2') ne -1) then begin 
  TYPE='K2'
  print, TYPE
    if STREGEX(STAR_PATH_LC, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_PATH_LC, 'C6_BEN') ne -1 then champ='6_BEN'
    if STREGEX(STAR_PATH_LC, 'C3') ne -1 then champ='3'
    if STREGEX(STAR_PATH_LC, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_PATH_LC, 'C7') ne -1 then champ='7'
    print, champ

    if (n_elements(STAR_PATH_LC) gt 10000) or (STREGEX(STAR_PATH_LC, 'ALL') ne -1 ) or (STREGEX(STAR_PATH_LC, 'C*') eq -1) or (file_dirname(STAR_PATH_LC) ne file_dirname(STAR_PATH_LC_LAST)) then champ='ALL' 
  endif  
    if (STREGEX(STAR_PATH_LC, 'KEPLER') ne -1) or (STREGEX(STAR_PATH_LC, 'K00') ne -1) then TYPE='KEPLER'

  print, 'TYPE=', TYPE
  print, 'CHAMP=', CHAMP

endif
; non opérationnel
;if (n_elements(STAR_LC_LIST) ne 0) then begin
;  readcol, star_lc_list, KIC, format='A'
;  STAR_LC=STAR_LC_LIST(0,0)
;  n_stars_tot=n_elements(STAR_LC_LIST)
;endif

if (n_elements(FILE_OUTPUT_A2Z_PATH) ne 0) then begin
  if (STREGEX(FILE_OUTPUT_A2Z_PATH, 'KEPLER') ne -1) or (STREGEX(FILE_OUTPUT_A2Z_PATH, 'DR25') ne -1) or STREGEX(FILE_OUTPUT_A2Z_PATH, 'K00') ne -1 then TYPE='KEPLER'
  if (STREGEX(FILE_OUTPUT_A2Z_PATH, 'K2') ne -1) then begin
    TYPE='K2'
    if STREGEX(FILE_OUTPUT_A2Z_PATH, 'C6') ne -1 then champ='6'
    if STREGEX(FILE_OUTPUT_A2Z_PATHT, 'C4') ne -1 then champ='4'
    if STREGEX(FILE_OUTPUT_A2Z_PATH, 'C7') ne -1 then champ='7'
    if (file_dirname(FILE_OUTPUT_A2Z_PATH) eq (path_K2_ALL)) or (STREGEX(FILE_OUTPUT_A2Z_PATH, 'ALL') ne -1) or (STREGEX(FILE_OUTPUT_A2Z_PATH, 'C*') eq -1) then champ='ALL'
  endif
endif

if (n_elements(STAR_KIC_LIST_TXT) ne 0) then begin
  readcol, STAR_KIC_LIST_TXT, STAR_KIC_LIST, STRINGSKIP='#', format='A', /silent
  n_stars_tot=n_elements(STAR_KIC_LIST)
  STAR_KIC=STAR_KIC_LIST(0)
  if (STREGEX(STAR_KIC_LIST_TXT, 'K2') ne -1) then begin
    TYPE='K2'
    if STREGEX(STAR_KIC_LIST_TXT, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_KIC_LIST_TXT, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_KIC_LIST_TXT, 'C7') ne -1 then champ='7'
    if (file_dirname(STAR_KIC_LIST_TXT) eq (path_K2_ALL)) or (STREGEX(STAR_KIC_LIST_TXT, 'ALL') ne -1) or (STREGEX(STAR_KIC_LIST_TXT, 'C*') eq -1) then champ='ALL'
  endif
    if (STREGEX(STAR_KIC_LIST_TXT, 'KEPLER') ne -1) or (STREGEX(STAR_KIC_LIST_TXT, 'DR25') ne -1) or STREGEX(STAR_KIC_LIST_TXT, 'K00') ne -1 then TYPE='KEPLER'

  print, 'TYPE=', TYPE
endif

if (n_elements(STAR_PATH_KIC_LIST) ne 0) then begin
  if n_elements(STAR_PATH_KIC_LIST) eq 1 then begin
    STAR_PATH_KIC_LIST=file_search(STAR_PATH_KIC_LIST)
  endif
  N_STARS_TOT=n_elements(STAR_PATH_KIC_LIST)
  STAR_PATH_KIC=STAR_PATH_KIC_LIST(0)
  if STREGEX(STAR_PATH_KIC_LIST, 'K2') ne -1 then begin 
    TYPE='K2'
    if STREGEX(STAR_PATH_KIC_LIST, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_PATH_KIC_LIST, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_PATH_KIC_LIST, 'C7') ne -1 then champ='7'
    if (file_dirname(STAR_PATH_KIC_LIST) eq (path_K2_ALL)) or (STREGEX(STAR_PATH_KIC_LIST, 'ALL') ne -1) or (STREGEX(STAR_PATH_KIC_LIST, 'C*') eq -1) then champ='ALL'
  endif
    if (STREGEX(STAR_PATH_KIC_LIST, 'KEPLER') ne -1) or (STREGEX(STAR_PATH_KIC_LIST, 'K00') ne -1) then TYPE='KEPLER'

  print, 'TYPE=', TYPE
endif
  
END