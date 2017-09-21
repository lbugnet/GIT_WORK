PRO KICS_TO_PSD, STAR_KIC=STAR_KIC, STAR_TAB_PSD=STAR_TAB_PSD, TYPE=TYPE, CHAMP=CHAMP, EXTENSION=EXTENSION, STAR_PATH_PSD=STAR_PATH_PSD

  ;---------------------------------------------------------------------------
  ;------------------------- IN ----------------------------------------------
  ;---------------------------------------------------------------------------
  ;
  ;--- STAR_KIC contient les KIC/EPIC des étoiles dont on veut la PSD --------
  ;--- TYPE contient le type de données: 'K2', 'KEPLER, 'TESS'
  ;--- CHAMP contient le champ d'observation si TYPE='K2', champ=1,2,3,...,18 
  ;---------------------------------------------------------------------------

  ;---------------------------------------------------------------------------
  ;------------------------- OUT ---------------------------------------------
  ;---------------------------------------------------------------------------
  ;
  ;--- STAR_TAB_PSD contient freq/power des etoiles --------------------------
  ;---------------------------------------------------------------------------

  if TYPE eq 'K2' then begin
    EPIC=STAR_KIC
    STAR_PATH_LC='/Volumes/TEMP/K2/GAP/JOEL/C'+strcompress(long(CHAMP))+'/rescale_C'+strcompress(long(CHAMP))+'/hlsp_everest_k2_llc_'+strcompress(long(EPIC))+'-c0'+strcompress(long(CHAMP))+'_kepler_v2.0_lc.txt.clean.res.hipass.rescale'
    LIGHT_CURVE_PATH_TO_PSD, STAR_PATH_LC=STAR_PATH_LC, STAR_TAB_PSD=STAR_TAB_PSD
  endif

  if TYPE eq 'KEPLER' then begin
    KIC=STAR_KIC
    restore, '/Users/lbugnet/DATA/TABLES/LIST_id_RG_DR25.sav', /verbose ;files_arr=chemins d'acces des etoiles du repertoire RGDR25 et ID_STARS_ARR contient les KIC
    match, KIC, ID_STARS_ARR, i_kic, i_id, count=n
    STAR_PATH_PSD=files_arr(i_id)
    KIC=kic(i_kic)
    EXTENSION='fits'
    PSD_PATH_TO_PSD, STAR_PATH_PSD=STAR_PATH_PSD, STAR_TAB_PSD=STAR_TAB_PSD, EXTENSION=EXTENSION
  endif


END