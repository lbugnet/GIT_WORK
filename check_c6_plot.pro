pro check_C7_plot
readcol, '/Users/lbugnet/DATA/METRIC/K2/C7/EPIC_stars_all_bad_numax_K2_C7.txt', epic, format='A,A,A,A,A,A,A,A';, format='(L,L,L,L,L,L,L,L)'
device, true=24, retain=2, decomposed=0

path='/Volumes/TEMP/K2/GAP/JOEL/C7/rescale_C7/'

ind=where(epic ne 0)
epic=epic(ind)

inds=sort(epic)
epic=epic(inds)
!p.multi=[0,3,5]
;nimp, name='test.ps', /paper
for i=0, n_elements(epic)-1 do begin
    filename=file_search(path,'hlsp_everest_k2*'+strtrim(epic(i),1)+'*.rescale',count=nfiles)    
    READCOL,filename(0),time,flux,FORMAT='D,D'
   out=DBLARR(2,n_elements(time))
   out(0,*) = time(*)-time(0)
   out(1,*) = FLUX
   t=reform(out(0,*));*86400.
    res=reform(out(1,*))
    
    ;remove spikes
     spikes,out,threshold=5,filt=2*1440.
     ;in ppm
     out(1,*)=1e6*(out(1,*)/median(out(1,*))-1)
     ;compute PSD
    psd,out,s,ff2,ofac=ofac

    plot_oo,  s*1e6, ff2, xr=[1e-1, 300], xtit='Frequency (!7l!17Hz)', ytit='PSD (ppm!U2!n/!7l!17Hz)', background=254 ,col=1, tit=long(epic(i))
    ;stop
    if (i+1) MOD 15 eq 0 then begin
       ;stop
       save_jpeg,'/Users/lbugnet/DATA/METRIC/K2/C7/plots_C7_outliers_metric_'+strtrim(i,1)+'.jpg' 
       endif

endfor

;fimp

end
