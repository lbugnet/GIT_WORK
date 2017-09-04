PRO DETECT_OUTLIERS_POWVAR,$
;IN
 OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY, XX=XX, RES=RES, THRESHOLD=THRESHOLD, FLAG=FLAG, $
  SLOPE_FIT=SLOPE_FIT,   TYPE=TYPE, CHAMP=CHAMP, SOLAR_LIKE=SOLAR_LIKE, cadence=cadence, $
  fill=fill,FREQ_INIC_GR=FREQ_INIC_GR, GOLD=GOLD, PARAM=PARAM, status=status, m_vrard=m_vrard,$
  HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE


;+
; :Author: Lisa BUGNET
;
; ---------------------------------------------------------------------------;
;           PROGRAM DETECT OUTLIERS STARS FROM POWVAR VS NUMAX/DNU LAW
; ---------------------------------------------------------------------------;
;
; NAME: DETECT_OUTLIERS_POWVAR.pro
;
; CALLING SEQUENCE:
;
;  DETECT_OUTLIERS_POWVAR, OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY, XX=XX, RES=RES,$
;   THRESHOLD=THRESHOLD, FLAG=FLAG, SLOPE_FIT=SLOPE_FIT,   TYPE=TYPE, $
;   CHAMP=CHAMP, SOLAR_LIKE=SOLAR_LIKE, cadence=cadence, fill=fill,$
;   FREQ_INIC_GR=FREQ_INIC_GR, GOLD=GOLD, PARAM=PARAM, status=status,$
;   m_vrard=m_vrard, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, $
;   PATH_TABLE=PATH_TABLE
;
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
; 
; --- OUTPUT_RESIZE  ----- DBLARR(*,11) -------------------------------------;
; --- OUTPUT_RESIZE contains the POWVAR metric P_var for --------------------;
; ------------------------- A2Z stars (-problems) ---------------------------;
; --------------------- OUTPUT_RESIZÉ(*,0)=EPIC/KIC -------------------------;
; --------------------- OUTPUT_RESIZE(*,1)=P_VAR=DATA-NOISE (ppm^2/muHz) ----;
; --------------------- OUTPUT_RESIZE(*,3:6)=DATA ---------------------------;
; --------------------- OUTPUT_RESIZE(*,7:10)=NOISE -------------------------;
; ---------------------------------------------------------------------------;
;
; --- XX ----- DBLARR(*) ----------------------------------------------------;
; --------------------- contains alog10(numax/dnu/...) ----------------------;
;
; --- YY ----- DBLARR(*) ----------------------------------------------------;
; --------------------- contains alog10(P_var)=alog10(OUTPUT_A2Z[*,1]) ------;
; 
; --- RES -------------------------------------------------------------------;
; --------------------- contains (bx+a) a=res(0), b=res(1) ------------------;
; --------------------- regression of slope P_var VS numax/dnu/... ----------;
;
; --- THRESHOLD -------------------------------------------------------------;
; ---------------------- contains distance to the powvar law in sigma -------;
; ---------------------- default value is 1 ---------------------------------;
;
; --- SLOPE_FIT -------------------------------------------------------------;
; ---------------------- contains value of ax+b -----------------------------;
; ---------------------------------------------------------------------------;
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
; --- FREQ_INIC_GR = initial frequency of data (muHz) -----------------------;
;
; --- PARAM -----------------------------------------------------------------;
; --------------------- contains name of data to use ------------------------;
; --------------------- default is 'numax' or 'dnu' -------------------------;
;
; --- m_vrard gives masses for Vrard stars ----------------------------------;
; ---------------------  optionnal, only if PARAm=vrard_dnu... --------------;
; 
; --- STATUS (for Vrard stars) ----------------------------------------------;
; --------------------- 0: RGB, 1: RC1, 2:RC2 -------------------------------;
; ---------------------  optionnal, only if PARAm=vrard_dnu... --------------; 
; 
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
; /////
; ---------------------------------------------------------------------------;
;-
  
  IF keyword_set(help) THEN BEGIN
    doc_library,'detect_outlier_powvar', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
    RETURN
    ;you have to write detect_outlier_powvar, /help
  ENDIF



if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'

 ;--------------------------------------------------------------
 ;------- ROUTINE COMPUTES AND PLOT OUTLIERS FROM METRIC -------
 ;--------------------------------------------------------------
  print, n_elements(numax)
  yy_re=alog10(output_RESIZE[*,1])
  xx_re=alog10(10^xx)
  size=[1,400]
  if  (STREGEX(PARAM , 'dnu') eq 0) then size=[0.1, 40]
  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=1, ylog=1,xr=size, font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="black",linestyle="none", transparency=90);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp5=plot(10^xx, 10^(slope_fit+threshold),xlog=1, ylog=1, color="lime green",linestyle=1 ,/overplot, transparency=99 )
  pp5.thick=2
  pp5=plot(10^xx, 10^(slope_fit), xlog=1, ylog=1,color="orange",linestyle=1 ,/overplot, transparency=99 )
  pp5.thick=2
  pp4=plot(10^xx, 10^(slope_fit-threshold),xlog=1, ylog=1, color="dark magenta",linestyle=1 ,/overplot, transparency=99 )
  pp4.thick=2
  w_low=where(flag eq 1)
  pp1=plot(10^xx_re(w_low), 10^yy_re(w_low),xlog=1, ylog=1,symbol="D", SYM_FILLED=1, color="dark magenta",linestyle="none", name="LOW STARS", /overplot, transparency=50)
  w_high=where(flag eq 2)
  pp2=plot(10^xx_re(w_high), 10^yy_re(w_high),xlog=1, ylog=1,symbol="D", SYM_FILLED=1, color="lime green",linestyle="none", name="HIGH STARS", /overplot, transparency=50)
  ;w3=where(flag eq 3)
  ;pp3=plot(xx_re(w3), yy_re(w3),symbol="D", SYM_FILLED=1, color="orange",linestyle="none", name="RIGHT STARS", /overplot, transparency=50)
  ;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)
  
  if TYPE eq 'KEPLER' then begin
    file_out=PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'.png';+'80J'+'.png';'/Users/lbugnet/DATA/METRIC/metric_numax_A2Z_stars.png'
    pp.save, file_out
    ;SAVE LOW STARS
    w_low=where(flag eq 1)
    close,1
    openw,1,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt';+'80J'+'low_stars.txt'
    close,1
    openw,1, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars.txt', /append ;+'80J'+'low_stars.txt', /append
    for ii=0, n_elements(w_low)-1 do printf,1, long(output_resize[w_low(ii),0]);, M(w_low)
    close,1
    ;SAVE HIGH STARS
    w_high=where(flag eq 2)
    close,2
    openw,2,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(w_high)-1 do printf,2, long(output_resize[w_high(ii),0]);, M(w_high(*))
    close,2
    ;SAVE OK STARS
    w_ok=where(flag eq 0)
    close,2
    openw,2,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'OK_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(w_ok)-1 do printf,2, long(output_resize[w_ok(ii),0]);, M(w_high(*))
    close,2
  endif
   
  if TYPE eq 'K2' then begin
    file_out=PATH_OUTPUT+TYPE+ '/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'.png';+'80J'+'.png';'/Users/lbugnet/DATA/METRIC/metric_numax_A2Z_stars.png'
    pp.save, file_out
    ;SAVE LOW STARS
    outlow=long(output_resize[w_low,0])
    close,1
    openw,1,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt';+'80J'+'low_stars.txt'
    close,1
    openw,1, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_low_stars.txt', /append ;+'80J'+'low_stars.txt', /append
    for ii=0, n_elements(outlow)-1 do printf,1, outlow(ii);, M(w_low)
    close,1
    ;SAVE HIGH STARS
    outhigh=long(output_resize[w_high,0])
    close,2
    openw,2,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_high_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(outhigh)-1 do printf,2, outhigh(ii);, M(w_high(*))
    close,2
    ;SAVE OK STARS
    w_ok=where(flag eq 0)
    outok=long(output_resize[w_ok,0])
    close,2
    openw,2,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+'_OK_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(outok)-1 do printf,2, outok(ii);, M(w_high(*))
    close,2
  endif



if (PARAM eq 'vrard_dnu') or (PARAM eq 'vrard_a2z_dnu') then begin
  
  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=1,xr=[2,30] ,ylog=1, font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="black",linestyle="none", transparency=90);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  w2=where(status eq 2)
  w1=where(status eq 1)  
  w0=where(status eq 0)  
  pp=plot(10^xx(w0), 10^yy(w0),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="blue",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp=plot(10^xx(w1), 10^yy(w1),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="green",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp=plot(10^xx(w2), 10^yy(w2),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="orange",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp.save, PATH_OUTPUT+TYPE+'/'+PARAM+'_RG_3_colors.png'
  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=1, ylog=1,xr=[2,30], font_size=13, xtitle=PARAM+'  $(\mu Hz)$', font_name='Times', rgb_table=33, ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="orange",linestyle="none", transparency=80,vert_colors=  255./(3.79-0.51)*(m_vrard-0.51))
    cb = COLORBAR(POSITION=[0.2,0.75,0.6,0.8], RGB_TABLE=33, RANGE=[0.51,3.79], font_size=10, TEXT_ORIENTATION=1, TITLE='$Mass$')
    pp.save, PATH_OUTPUT+TYPE+'/'+PARAM+'_RG_mass_color_coded.png'
endif

END