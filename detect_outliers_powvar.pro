PRO DETECT_OUTLIERS_POWVAR,$
;IN
 OUTPUT_RESIZE=OUTPUT_RESIZE, YY=YY, XX=XX, RES=RES, THRESHOLD=THRESHOLD, FLAG=FLAG, $
  SLOPE_FIT=SLOPE_FIT,   TYPE=TYPE, CHAMP=CHAMP, SOLAR_LIKE=SOLAR_LIKE, cadence=cadence, $
  fill=fill,FREQ_INIC_GR=FREQ_INIC_GR, GOLD=GOLD, PARAM=PARAM, status=status, m_vrard=m_vrard,$
  HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE, GLOBAL=GLOBAL, GG=GG


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
  xlog=1
  if  ((STREGEX(PARAM , 'numax') eq 0) and (GG ne 1)) then PARAMS=PARAM+'  $(\mu Hz)$'
  if  ((STREGEX(PARAM , 'dnu') eq 0) and (GG ne 1)) then size=[0.1, 40] & xlog=1 & PARAMS=PARAM+'  $(\mu Hz)$'
  if  (GG eq 1)  then size=[1,4] & xlog=0 & PARAMS=PARAM+'   Log(g)'
  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=xlog, ylog=1,xr=size, font_size=13, xtitle=PARAMS,, font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol=".", sym_size=2, SYM_FILLED=1, color="black",linestyle="none", transparency=50);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  ;pp5=plot(10^xx, 10^(slope_fit+threshold),xlog=1, ylog=1, color="lime green",linestyle=1 ,/overplot, transparency=50 )
  ;pp5.thick=2
  ;pp5=plot(10^xx, 10^(slope_fit), xlog=xlog, ylog=1,color="orange",linestyle=1 ,/overplot, transparency=50 )
  ;pp5.thick=2
  ;pp4=plot(10^xx, 10^(slope_fit-threshold),xlog=xlog, ylog=1, color="dark magenta",linestyle=1 ,/overplot, transparency=50 )
  ;pp4.thick=2
  w_low=where(flag eq 1)
  ;pp1=plot(10^xx_re(w_low), 10^yy_re(w_low),xlog=xlog, ylog=1,symbol="D", SYM_FILLED=1, color="dark magenta",linestyle="none", name="LOW STARS", /overplot, transparency=50)
  w_high=where(flag eq 2)
  ;pp2=plot(10^xx_re(w_high), 10^yy_re(w_high),xlog=xlog, ylog=1,symbol="D", SYM_FILLED=1, color="lime green",linestyle="none", name="HIGH STARS", /overplot, transparency=50)
  ;w3=where(flag eq 3)
  ;pp3=plot(xx_re(w3), yy_re(w3),symbol="D", SYM_FILLED=1, color="orange",linestyle="none", name="RIGHT STARS", /overplot, transparency=50)
  ;ll=legend(target=[pp1,pp2], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)
  
  ;--------------------------------------------------------------
  ;------- ecart-type 66% et 95%-------
  ;--------------------------------------------------------------
  wnan=where(10^xx gt 0)
  xxh=xx(wnan)
  outh=output_resize(wnan,*)
  xxmax=where(xxh eq (max(xxh)))
  yyh=yy(wnan)
  n_box=19
  yysort=yyh(sort(yyh))
  xxh=xxh(sort(yyh))
  outh=outh(sort(yyh),*)
  close,1
  openw,1,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'limits.txt'
  close,1
  openw,1, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'limits.txt', /append
  printf,1, '#moyenne', '-95%', '-66$%', '66%', '95%', 'POWVAR'
  close, 1
  count=0
  close, 2
  openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_99.txt'
  close, 2
  openw, 3,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_99.txt'
  close, 3
  openw, 4, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_95.txt'
  close, 4
  openw, 5, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_95.txt'
  close, 5
  openw, 6 , PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_66.txt'
  close, 6
  openw, 7, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_66.txt'
  close, 7
  for ii=0, n_box-2 do begin
    if (ii*300+300) lt n_elements(yysort) then begin
      w1000=where((10^yysort ge 10^yysort(ii*300)) and (10^yysort lt 10^yysort(ii*300+300)))
      mnu1000=mean(10^xxh(w1000))
      wgt=where(10^xxh(w1000) gt mnu1000)
      dimgt=n_elements(wgt)
      posgt=round(dimgt*66./100.)
      posggt=round(dimgt*95./100.)
      posgggt=round(dimgt*99./100.)
      wlt=where(10^xxh(w1000) lt mnu1000)
      dimlt=n_elements(wlt)
      poslt=round(dimlt*33./100.)
      posllt=round(dimlt*5./100.)
      poslllt=round(dimlt*1./100.)
      aa=sort(10^xxh(w1000));(w1000))
      ;middle interval
      mein=mean(10^yysort(ii*300:ii*300+300))
      bin=value_locate(10^yysort(ii*300:ii*300+300), mein)
      ;pp4=plot([10^xxh(w1000(aa(poslt))),10^xxh(w1000(aa(posgt+dimlt)))],[10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=1, ylog=1,linestyle=1 , sym_size=0.8, sym_filled=1, symbol='D', color='blue',/overplot, transparency=9 )
      ;pp4=plot([10^xxh(w1000(aa(posllt))),10^xxh(w1000(aa(posggt+dimlt)))],[10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=1, ylog=1,linestyle=1 , sym_size=0.8, sym_filled=1, symbol='D', color='green',/overplot, transparency=9 )
      pp4=plot([mnu1000, mnu1000], [10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=1, ylog=1, linestyle=1 ,sym_size=0.8, sym_filled=1,symbol='D',color='red', /overplot, transparency=9 )
      
      ;register limits
      openw,1, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'limits.txt', /append 
      printf,1, mnu1000, 10^xxh(w1000(aa(poslllt))),10^xxh(w1000(aa(posllt))), 10^xxh(w1000(aa(poslt))) ,10^xxh(w1000(aa(posgt+dimlt))),10^xxh(w1000(aa(posggt+dimlt))), 10^xxh(w1000(aa(posgggt+dimlt))),10^yysort(ii*300+bin),  format='(f12.2, 2x,f12.2, 2x,f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2)'
      close,1
      
      ;save outliers
      fh95=where(10^xxh(w1000) gt 10^xxh(w1000(aa(posggt+dimlt))))
      fl95=where(10^xxh(w1000) lt 10^xxh(w1000(aa(posllt))))
      fh99=where(10^xxh(w1000) gt 10^xxh(w1000(aa(posgggt+dimlt))))
      fl99=where(10^xxh(w1000) lt 10^xxh(w1000(aa(poslllt))))
      fh66=where(10^xxh(w1000) gt 10^xxh(w1000(aa(posgt+dimlt))))
      fl66=where(10^xxh(w1000) lt 10^xxh(w1000(aa(poslt))))
      outl99=outh(w1000(fl99),0)
      outh99=outh(w1000(fh99),0)
      outl95=outh(w1000(fl95),0)
      outh95=outh(w1000(fh95),0)
      outl66=outh(w1000(fl66),0)
      outh66=outh(w1000(fh66),0)
      
      openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_99.txt', /append
      for jj=0, n_elements(outl99)-1 do printf,2, long(outl99), format='(i10)'
      close, 2
      openw, 3,  PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_99.txt', /append
      for jj=0, n_elements(outh99)-1 do printf,3, long(outh99), format='(i10)'
      close, 3
      openw, 4, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_95.txt',/append
      for jj=0, n_elements(outl95)-1 do printf, 4, long(outl95), format='(i10)'
      close, 4
      openw, 5, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_95.txt',/append
      for jj=0, n_elements(outh95)-1 do printf, 5, long(outh95), format='(i10)'
      close, 5
      openw, 6, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'low_stars_limit_66.txt',/append
      for jj=0, n_elements(outl66)-1 do printf, 6, long(outl66), format='(i10)'
      close, 6
      openw, 7, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'high_stars_limit_66.txt',/append
      for jj=0, n_elements(outh66)-1 do printf, 7, long(outh66), format='(i10)'
      close, 7
      count=count+n_elements(outl66)
      endif
    endfor
    ii=n_box-2
    w1000=where((10^yysort ge 10^yysort(ii*300)) and (10^yysort lt 10^yysort(n_elements(yysort)-1)))
    mnu1000=mean(10^xxh(w1000))
    wgt=where(10^xxh(w1000) gt mnu1000)
    dimgt=n_elements(wgt)
    posgt=round(dimgt*66./100.)
    posggt=round(dimgt*95./100.)
    posgggt=round(dimgt*99./100.)
    wlt=where(10^xxh(w1000) lt mnu1000)
    dimlt=n_elements(wlt)
    poslt=round(dimlt*33./100.)
    posllt=round(dimlt*5./100.)
    poslllt=round(dimlt*1./100.)
    aa=sort(10^xxh(w1000))
    mein=mean(10^yysort(ii*300:n_elements(yysort)-1))
    bin=value_locate(10^yysort(ii*300:n_elements(yysort)-1), mein)
    ;pp4=plot([10^xxh(w1000(aa(poslt))),10^xxh(w1000(aa(posgt+dimlt)))],[10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=xlog, ylog=1,linestyle=1 , sym_size=0.8, sym_filled=1, symbol='D', color='blue',/overplot, transparency=9 )
    ;pp4=plot([10^xxh(w1000(aa(posllt))),10^xxh(w1000(aa(posggt+dimlt)))],[10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=xlog, ylog=1,linestyle=1 , sym_size=0.8, sym_filled=1, symbol='D', color='green',/overplot, transparency=9 )
    pp4=plot([mnu1000, mnu1000], [10^yysort(ii*300+bin),10^yysort(ii*300+bin)],xlog=xlog, ylog=1, linestyle=1 ,sym_size=0.8, sym_filled=1,symbol='D',color='red', /overplot, transparency=9 )

    ;register limits
    ;openw,1, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'limits.txt', /append
    ;printf,1, mnu1000, 10^xxh(w1000(aa(poslllt))), 10^xxh(w1000(aa(posllt))), 10^xxh(w1000(aa(poslt))) ,10^xxh(w1000(aa(posgt+dimlt))),10^xxh(w1000(aa(posggt+dimlt))), 10^xxh(w1000(aa(posgggt+dimlt))),10^yysort(ii*300+bin),  format='(f12.2, 2x,f12.2, 2x,f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2, 2x, f12.2)'
    ;close,1


    readcol, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+'_'+fill+'_'+'20J'+'limits.txt', moy, xm99, xm95, xm66, x66, x95, x99, y
    poly=polygon([xm99, reverse(x99)], [y, reverse(y)],target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=70, TRANSPARENCY=0)
    poly=polygon([xm95, reverse(x95)], [y, reverse(y)],target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="blue", FILL_TRANSPARENCY=40, TRANSPARENCY=0)
    poly=polygon([xm66, reverse(x66)], [y, reverse(y)],target=p, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="aquamarine", FILL_TRANSPARENCY=50, TRANSPARENCY=0)
    
  
  
  
  ;--------------------------------------------------------------
  ;------- histogramme -------
  ;--------------------------------------------------------------
  wnan=where(10^xx gt 0)
  xxh=xx(wnan)
  yyh=yy(wnan)
  wd=where((10^xxh gt 0) and (10^xxh le 100))
  result=histogram(yyh(wd), nbins=200)
  pp=plot(10^yyh(wd(sort(yyh(wd)))), result, xlog=0)
  global=''
  if TYPE eq 'KEPLER' then begin
    file_out=PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'.png';+'80J'+'.png';'/Users/lbugnet/DATA/METRIC/metric_numax_A2Z_stars.png'
    pp.save, file_out
    ;SAVE LOW STARS
    w_low=where(flag eq 1)
    close,1
    openw,1,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'low_stars.txt';+'80J'+'low_stars.txt'
    close,1
    openw,1, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'low_stars.txt', /append ;+'80J'+'low_stars.txt', /append
    for ii=0, n_elements(w_low)-1 do printf,1, long(output_resize[w_low(ii),0]);, M(w_low)
    close,1
    ;SAVE HIGH STARS
    w_high=where(flag eq 2)
    close,2
    openw,2,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'high_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'high_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(w_high)-1 do printf,2, long(output_resize[w_high(ii),0]);, M(w_high(*))
    close,2
    ;SAVE OK STARS
    w_ok=where(flag eq 0)
    close,2
    openw,2,PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'OK_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+ '/'+SOLAR_LIKE+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+SOLAR_LIKE+GLOBAL+'_'+fill+'_'+'20J'+'OK_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(w_ok)-1 do printf,2, long(output_resize[w_ok(ii),0]);, M(w_high(*))
    close,2
  endif
   
  if TYPE eq 'K2' then begin
    file_out=PATH_OUTPUT+TYPE+ '/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'.png';+'80J'+'.png';'/Users/lbugnet/DATA/METRIC/metric_numax_A2Z_stars.png'
    pp.save, file_out
    ;SAVE LOW STARS
    outlow=long(output_resize[w_low,0])
    close,1
    openw,1,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_low_stars.txt';+'80J'+'low_stars.txt'
    close,1
    openw,1, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_low_stars.txt', /append ;+'80J'+'low_stars.txt', /append
    for ii=0, n_elements(outlow)-1 do printf,1, outlow(ii);, M(w_low)
    close,1
    ;SAVE HIGH STARS
    outhigh=long(output_resize[w_high,0])
    close,2
    openw,2,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_high_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_high_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(outhigh)-1 do printf,2, outhigh(ii);, M(w_high(*))
    close,2
    ;SAVE OK STARS
    w_ok=where(flag eq 0)
    outok=long(output_resize[w_ok,0])
    close,2
    openw,2,PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_OK_stars.txt' ;'80J'+'high_stars.txt'
    close,2
    openw,2, PATH_OUTPUT+TYPE+'/C'+CHAMP+'/detect_outliers_powvar_'+PARAM+'_'+TYPE+'_'+CHAMP+'_'+GOLD+cadence+strtrim(freq_inic_gr,1)+GLOBAL+'_OK_stars.txt', /append ;'80J'+'high_stars.txt', /append
    for ii=0, n_elements(outok)-1 do printf,2, outok(ii);, M(w_high(*))
    close,2
  endif


;
;if (PARAM eq 'vrard_dnu') or (PARAM eq 'vrard_a2z_dnu') then begin
;  
;  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=1,xr=[2,30] ,ylog=1, font_size=13, xtitle=PARAMS, font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="black",linestyle="none", transparency=90);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;  w2=where(status eq 2)
;  w1=where(status eq 1)  
;  w0=where(status eq 0)  
;  pp=plot(10^xx(w0), 10^yy(w0),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAMS, font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="blue",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;  pp=plot(10^xx(w1), 10^yy(w1),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAMS, font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="green",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;  pp=plot(10^xx(w2), 10^yy(w2),dim=[800,400],xlog=1, ylog=1, font_size=13, xtitle=PARAMS, font_name='Times', ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="orange",linestyle="none", transparency=90, /overplot);'title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
;  pp.save, PATH_OUTPUT+TYPE+'/'+PARAM+'_RG_3_colors.png'
;  pp=plot(10^xx, 10^yy,dim=[800,400],xlog=1, ylog=1,xr=[2,30], font_size=13, xtitle=PARAMS, font_name='Times', rgb_table=33, ytitle='$POWVAR (ppm^2/\mu Hz)$',symbol="D", SYM_FILLED=1, color="orange",linestyle="none", transparency=80,vert_colors=  255./(3.79-0.51)*(m_vrard-0.51))
;    cb = COLORBAR(POSITION=[0.2,0.75,0.6,0.8], RGB_TABLE=33, RANGE=[0.51,3.79], font_size=10, TEXT_ORIENTATION=1, TITLE='$Mass$')
;    pp.save, PATH_OUTPUT+TYPE+'/'+PARAM+'_RG_mass_color_coded.png'
;endif
;
END