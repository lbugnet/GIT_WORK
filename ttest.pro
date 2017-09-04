ff=file_search('/Volumes/TEMP/K2/GAP/JOEL/C7/rescale_C7/*')
for ii=0, n_elements(ff)-1 do begin
  readcol, ff(ii), time, flux, format='D,D', /silent
  if total(flux) lt 1.0 then print, ff(ii)
endfor 

END