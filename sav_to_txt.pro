PRO SAV_TO_TXT, PATH=PATH, LABEL=LABEL


if n_elements(label) eq 0 then label=''
restore, PATH+'.sav', /verbose
CMRESTORE, PATH+'.sav', DATA=structure, NAMES=NAMES, PASS_METHOD='STRUCT'

aaa=n_elements(structure(0))-1
aa=tag_names(structure)
close,2
openw,2,PATH+'.txt'
close,2
openw,2, PATH+'.txt', /append
printf,2, '#'+'     '+'KICS'+ '   '+ NAMES(0)+ '   '+ NAMES(1)
;for jj=0, n_elements(NAMES)-1 do begin
;  bb=structure.(aa(jj))
  
  for ii =0, n_elements(NUMAX_GUESS)-1 do printf,2, strtrim(OUTPUT_A2Z[ii,0],1), OUTPUT_A2Z[ii,1], numax_guess(ii)
;  if ii eq 0 then begin
;  for jj=0, n_elements(bb[0,*])-1 do begin
;    
;    printf, 2, bb[*, jj]
;  endfor
;  endif else begin printf, 2, structure.(aa(0)), structure.(aa(1)), structure.(aa(2)), structure.(aa(3))
;  endelse
;endfor
close,2

stop















END