PRO varlaw, TYPE=TYPE, CHAMP=CHAMP, DAY=DAY, OUTPUT_A2Z=OUTPUT_A2Z, NUMAX_GUESS=NUMAX_GUESS, NG_MAX=NG_MAX, NG_MIN=NG_MIN, GLOBAL=GLOBAL,res=res, slope_fit=slope_fit, threshold=threshold, xx=xx, yy=yy, HELP=HELP, PATH_OUTPUT=PATH_OUTPUT, PATH_DATA=PATH_DATA, PATH_TABLE=PATH_TABLE

;+
;----------------------------------------------------------------------------
;                  CALUCULATE NUMAZ_GUESS FROM POWVAR 
;----------------------------------------------------------------------------
;
; ---------------------------------------------------------------------------;
; ------------------------- PARAMETERS: IN ----------------------------------;
; ---------------------------------------------------------------------------;
;
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
; --- DAY contains inpaint '20','55','80' or ' '  Kepler   ------------------;
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
; --- NUMAX_GUESS contains guesses of numax/dnu... by P_var -----------------;
; --- NG_MAX contains minimum estimation for numax/dnu,... ------------------;  
; --- NG_MAX contains maximum estimation for numax/dnu,... ------------------;
; ---------------------------------------------------------------------------;
;-

IF keyword_set(help) THEN BEGIN
  doc_library,'varlaw', DIRECTORY='/Users/lbugnet/WORK/A2Zp/', PATH='/Users/lbugnet/WORK/A2Zp/'
  RETURN
  ;you have to write varlawl, /help
ENDIF

if n_elements(PATH_OUTPUT) eq 0 then PATH_OUTPUT='~/DATA/METRIC/'
if n_elements(PATH_TABLE) eq 0 then PATH_TABLE='~/DATA/TABLES/'
if n_elements(PATH_DATA) eq 0 then PATH_DATA='/Volumes/TEMP/'


if n_elements(DAY) eq 0 then DAY=' '

if n_elements(res) eq 0 then begin
  if TYPE eq 'KEPLER' then begin 
    if DAY eq '80' then begin
      restore, PATH_OUTPUT+TYPE+'/KEPLER_varlaw_80J_ALL.sav';, res, slope_fit, residuals, threshold, xx, yy
    endif 
    if (DAY eq '20') or (DAY eq ' ') then begin
      restore, PATH_OUTPUT+TYPE+'/KEPLER_varlaw_20J_ALL.sav';, res, slope_fit, residuals, threshold, xx, yy
    endif
  endif
  if TYPE eq 'K2' then begin
    restore, PATH_OUTPUT+TYPE+'/ALL/K2_varlaw_ALL.sav';, res, slope_fit, residuals, threshold ; comprend C4,C6,C7
  endif
endif

numax_guess=dblarr(n_elements(OUTPUT_A2Z[*,1]))
ng_max=dblarr(n_elements(OUTPUT_A2Z[*,1]))
ng_min=dblarr(n_elements(OUTPUT_A2Z[*,1]))

for ii=0, n_elements(OUTPUT_A2Z[*,1])-1 do begin
  numax_guess(ii)=10.^((alog10(output_a2z[ii,1])-res(0))/res(1))
  ng_max(ii)=10.^((alog10(output_a2z[ii,1])-res(0)-threshold)/res(1))
  ng_min(ii)=10.^((alog10(output_a2z[ii,1])-res(0)+threshold)/res(1))
endfor

























END