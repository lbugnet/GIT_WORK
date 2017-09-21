PRO Keplermag
;PYTHON VERSION
;#KEPLER MAGNITUDE -------------------------------------------------------------
;#path_miss='/Volumes/TEMP/RG_DR25/MISS/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_PSD/*'
;path_miss='/Volumes/TEMP/RG_DR25/K*/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_PSD/*'
;STARS_PATH_PSD=glob.glob(path_miss)
;#idl.STARS_PATH_PSD=STARS_PATH_PSD
;Kp=[]
;for ii in range(len(STARS_PATH_PSD)):
;hdulist = fits.open(STARS_PATH_PSD[ii])
;#print(hdulist[0].header['KEPMAG'])
;KEPMAG=hdulist[0].header['KEPMAG']
;Kp.append(KEPMAG)
;
;import idlsave
;#output_a2z
;#s = idlsave.read('/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all_missclassified.sav')
;s = idlsave.read('/Users/lbugnet/DATA/METRIC/KEPLER/0.700000___output_numax_all.sav')
;idl.s0=s.output_a2z[0,:]
;#idl.s1=s.output_a2z[1,:]
;#numax
;idl.path_tabl='~/DATA/METRIC/KEPLER/SOLAR_LIKE/A2Z_results_goldstd_sorted.txt'#'~/DATA/TABLES/Missclassified_Savita.txt'
;idl.str='#'
;idl.format='L,D,D,D,D,D'#'D,D,L,D,D,D,D,D,D,D,D,D'
;#savi=idl('readcol, path_tabl,  Full,   P   ,   KIC_savi,  Stage_s, Kpmag_s , Teff_s, Teffr_s ,  dnu_s  , numax_s   , logg_s  , Mass_s,    Rad_s, stringskip=str,  /silent, format=format')
;savi=idl('readcol, path_tabl=path_tabl, KIC_s, flagg, fmin, fmax, numax, enumax, format=format')
;#match
;idl('match, long(s0), long(KIC_savi), i1, i2')
;ind.i1=idl.i1
;ind.i2=idl.i2
;numax_s=numax
;#ind=idlsave.read('/Users/lbugnet/DATA/TABLES/miss_save_indice_output_numax.sav')
;#s_inds = np.in1d(s.output_a2z[:,0],savi.kic_s)
;#savi_inds = np.in1d(savi.kic_s, s.output_a2z[:,0])
;#idl('match, long(s.output_a2z[*,0]), long(savi.kic_s), i1, i2, count=nn')
;fig=plt.figure()
;plt.plot(idl.numax_s[ind.i2], s.output_a2z[1,ind.i1], marker='.', linestyle='', color='r')
;ax = fig.add_subplot(111)
;ax.set_xscale("log")
;ax.set_yscale("log")
;for ii in range(len(ind.i1)):
;if Kp[ind.i1[ii]] < 13:
;plt.plot(idl.numax_s[ind.i2[ii]], s.output_a2z[1,ind.i1[ii]], marker='.', linestyle='', color='b')
;plt.show()
;STARS_PATH_PSD=file_search('/Volumes/TEMP/RG_DR25/K*/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/LC_CORR_FILT_PSD/', '*')
;Kp=dblarr(n_elements(stars_path_psd))
;for ii=0, n_elements(STARS_PATH_PSD)-1 do begin
;  Kp[ii]=sxpar(headfits(STARS_PATH_PSD[ii]), 'KEPMAG')
;endfor
;save, file='/Users/lbugnet/DATA/METRIC/KEPLER/KEPMAG.sav', Kp

restore,'/Users/lbugnet/DATA/METRIC/KEPLER/LC__0.700000_output_numax_all.sav', /verbose
;restore,'/Users/lbugnet/DATA/METRIC/KEPLER/LC_MISS_0.700000_output_numax_all.sav', /verbose
PATH_TABLE='~/DATA/TABLES/'
KEPMAG=HMAGG;KEPTAB
restore, PATH_TABLE+'results_A2Z_all.sav', /verbose
match, long(output_a2z[*,0]), long(KIC_s), i1, i2
p=plot(numax(i2), output_a2z[i1,1],dim=[800,400],xlog=1, ylog=1,xr=size, font_size=13, symbol="D", SYM_FILLED=1, rgb_table=reverse(33),vert_colors=255./(16-3)*(KEPMAG(i1)-3),linestyle="none", transparency=90)
hkp=where(KEPMAG(i1) gt 14)
lkp=where(KEPMAG(i1) lt 9)
;pp=plot(numax(i2(hkp)), output_a2z[i1(hkp),1],dim=[800,400],xlog=1, ylog=1,xr=size, font_size=13, symbol="D", SYM_FILLED=1, color="green",linestyle="none", transparency=0, /overplot)
;p=plot(numax(i2(lkp)), output_a2z[i1(lkp),1],dim=[800,400],xlog=1, ylog=1,xr=size, font_size=13, symbol="D", SYM_FILLED=1, color="red",linestyle="none", transparency=0, /overplot)

;Kp des étoiles low

restore, '/Users/lbugnet/DATA/METRIC/KEPLER/KEPLER_varlaw_20J_ALL_KEPLER_LC0.700000_.sav', /verbose
wl=where(flag(i1) eq 1)
Kpl=KEPMAG(i1(wl))
wh=where(flag(i1) eq 2)
Kph=KEPMAG(i1(wh))

stop
END