PRO FLAG_BAD_STARS,$
;IN
  RES=RES,  output_resize=output_resize, YY=YY, SLOPE_FIT=SLOPE_FIT,  XX=XX, THRESHOLD=THRESHOLD, $
;OUT
 FLAG=FLAG


;+
; :Author: Lisa BUGNET
; ---------------------------------------------------------------------------;
;           PROGRAM TO FLAG OUTLIERS
; ---------------------------------------------------------------------------;
;  
; NAME: flag_bad_stars.pro  
; 
; CALLING SEQUENCE:
;
;  FLAG_BAD_STARS,  RES=RES,  output_resize=output_resize, YY=YY, $
;  SLOPE_FIT=SLOPE_FIT,  XX=XX,FLAG=FLAG, THRESHOLD=THRESHOLD
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
; 
; --- OUTPUT_RESIZE  ----- DBLARR(*,11) -------------------------------------;
; --- OUTPUT_RESIZE contains the POWVAR metric P_var for --------------------;
;------------------------- A2Z stars (-problems) ---------------------------;
; --------------------- OUTPUT_RESIZÉ(*,0)=EPIC/KIC -------------------------;
; --------------------- OUTPUT_RESIZE(*,1)=P_VAR=DATA-NOISE (ppm^2/muHz) ----;
; --------------------- OUTPUT_RESIZE(*,3:6)=DATA ---------------------------;
; --------------------- OUTPUT_RESIZE(*,7:10)=NOISE -------------------------;
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
; 
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: OUT ---------------------------------;
; ---------------------------------------------------------------------------;
; 
; --- FLAG ------------------------------------------------------------------;
; --------------------- =0 (POWVAR/numax OK), 1 (POWVAR/numax Low) ----------;
; --------------------- or 2 (POWVAR/numax High) ----------------------------;
; ---------------------------------------------------------------------------;
;-

IF keyword_set(help) THEN BEGIN
  doc_library,'flag_bad_stars', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
  RETURN
  ;you have to write flag_bad_stars, /help
ENDIF


Residuals = yy-slope_fit
threshold=1*stddev(yy)
index_bad = WHERE(abs(residuals) ge threshold)
flag=dblarr(n_elements(residuals))
for ii=0,n_elements(residuals)-1 do begin
  
  if ((residuals(ii) le (threshold)) and (10^xx(ii) ge (-threshold))) then begin
    flag(ii)=0
  endif
  ;if ((residuals(ii) le 0.0) and (10^xx(ii) gt 200)) then begin
  ;  flag(ii)=3
  ;endif
  if (residuals(ii) gt (threshold)) then begin
    flag(ii)=2
  endif
  if ((residuals(ii) lt (-threshold))) then begin ;and (10^xx(ii) le 200)) then begin
    flag(ii)=1
  endif

endfor


;STOP,'FLAGS CALC'
END