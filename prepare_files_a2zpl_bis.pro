PRO prepare_files_a2zpl_bis, STAR_PATH_PSD_LIST=STAR_PATH_PSD_LIST, STAR_PATH_LC_LIST=STAR_PATH_LC_LIST, STAR_KIC_LIST_TXT=STAR_KIC_LIST_TXT, STAR_PATH_KIC_LIST=STAR_PATH_KIC_LIST, TYPE=TYPE, CHAMP=CHAMP, N_STARS_TOT=N_STARS_TOT

path_temp_K2_C6='/Volumes/TEMP/K2/GAP/JOEL/C6/rescale_C6'
path_temp_K2_C4='/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4'
path_temp_K2_C7='/Volumes/TEMP/K2/GAP/JOEL/C7/rescale_C7'
path_dd_K2_C6='/Users/lbugnet/DATA/METRIC/K2/C6'
path_dd_K2_C4='/Users/lbugnet/DATA/METRIC/K2/C4'
path_dd_k2_c7='/Users/lbugnet/DATA/METRIC/K2/C7'
path_K2_ALL='/Users/lbugnet/DATA/METRIC/K2/ALL'
if (n_elements(STAR_PATH_PSD_LIST) ne 0) then begin
  if n_elements(STAR_PATH_PSD_LIST) eq 1 then begin
    STAR_PATH_PSD_LIST=file_search(STAR_PATH_PSD_LIST )
  endif
  STAR_PATH_PSD=STAR_PATH_PSD_LIST(0)
  n_stars_tot=n_elements(STAR_PATH_PSD_LIST)
  if STREGEX(STAR_PATH_PSD, 'K2') ne -1 then begin 
    TYPE='K2'
    if STREGEX(STAR_PATH_PSD, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_PATH_PSD, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_PATH_PSD, 'C7') ne -1 then champ='7'
    if (file_dirname(STAR_PATH_PSD) eq (path_K2_ALL)) or (STREGEX(STAR_PATH_PSD, 'ALL') ne -1) or (STREGEX(STAR_PATH_PSD, 'C*') eq -1) then champ='ALL'
  endif  
    if (STREGEX(STAR_PATH_PSD, 'KEPLER') ne -1) or (STREGEX(STAR_PATH_PSD, 'K00') ne -1) then TYPE='KEPLER'
  print, 'TYPE=', TYPE
endif
stop
if (n_elements(STAR_PATH_LC_LIST) ne 0) then begin
  if n_elements(STAR_PATH_LC_LIST) eq 1 then begin
    STAR_PATH_LC_LIST=file_search(STAR_PATH_LC_LIST )
  endif

  STAR_PATH_LC=STAR_PATH_LC_LIST(0)
  N_STARS_TOT=n_elements(STAR_PATH_LC_LIST)
  print, 'n_stars_tot', n_stars_tot
 if STREGEX(STAR_PATH_LC, 'K2') ne -1 then begin 
  TYPE='K2'
    if STREGEX(STAR_PATH_LC, 'C6') ne -1 then champ='6'
    if STREGEX(STAR_PATH_LC, 'C4') ne -1 then champ='4'
    if STREGEX(STAR_PATH_LC, 'C7') ne -1 then champ='7'
    if (n_elements(STAR_PATH_LC) gt 10000) or (STREGEX(STAR_PATH_LC, 'ALL') ne -1 ) or (STREGEX(STAR_PATH_LC, 'C*') eq -1) then champ='ALL'
  endif  
    if (STREGEX(STAR_PATH_LC, 'KEPLER') ne -1) or (STREGEX(STAR_PATH_LC, 'K00') ne -1) then TYPE='KEPLER'

  print, 'TYPE=', TYPE
  print, 'CHAMP=', CHAMP
  stop
endif
stop
; non opérationnel
;if (n_elements(STAR_LC_LIST) ne 0) then begin
;  readcol, star_lc_list, KIC, format='A'
;  STAR_LC=STAR_LC_LIST(0,0)
;  n_stars_tot=n_elements(STAR_LC_LIST)
;endif

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
    if (STREGEX(STAR_KIC_LIST_TXT, 'KEPLER') ne -1) or STREGEX(STAR_KIC_LIST_TXT, 'K00') ne -1 then TYPE='KEPLER'

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
    if (STREGEX(STAR_PATH_KIC_LIST, 'KEPLER') ne -1) or STREGEX(STAR_PATH_KIC_LIST, 'K00') ne -1 then TYPE='KEPLER'

  print, 'TYPE=', TYPE
endif


END