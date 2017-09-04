pro test_s
set_plot, 'X'
window, 0
files_arr=file_search('/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/*')
readcol, files_arr[ns], time,flux, FORMAT='D,D'
;READCOL,'/Volumes/TEMP/K2/GAP/JOEL/C4/rescale_C4/hlsp_everest_k2_llc_210312900-c04_kepler_v2.0_lc.txt.clean.res.hipass.rescale',time, flux,FORMAT='D,D'
out=DBLARR(2,n_elements(time))
out(0,*) = time(*)-time(0)
out(1,*) = flux
;in ppm
out(1,*)=1e6*(out(1,*)-1)
;compute PSD
psd,out,s,ff2,ofac=ofac; s in Hz
;plot
plot_oo, s*1e6, ff2, xr=[1e-1, 300], xtit='Frequency (!7l!17Hz)', ytit='PSD (ppm!U2!n/!7l!17Hz)’
end