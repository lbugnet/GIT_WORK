PRO compare_powvar_Kepler_20_80

restore,'/Users/lbugnet/DATA/TABLES/metric_all_stars.sav'
xx20=xx
yy20=yy
res20=res
numax20=output_resize[*,0]
restore, '/Users/lbugnet/DATA/TABLES/metric_all_stars_80j.sav'
numax80=output_resize[*,0]

match, numax20, numax80, in20, in80

;pp1=plot(10^xx20,10^yy20,color='blue', xlog=1, ylog=1,linestyle="none", symbol='D', xtitle='numax a2z', ytitle='Powvar',sym_filled=1,name='Kepler 20J')
;pp2=plot(10^xx,10^yy,color='coral', xlog=1, ylog=1,linestyle="none", symbol='D', sym_filled=0, name="Kepler 80J", /overplot)
;pp=plot(10^xx, 10^(res(1)*xx+res(0)), xlog=1, ylog=1, /overplot, color='red', sym_thick=3);, linestyle='NONE', symbol='.', sym_filled=1)
;pp.thick=3
;pp=plot(10^xx20, 10^(res20(1)*xx20+res20(0)), xlog=1, ylog=1, color='dark blue', /overplot, sym_thick=3);, linestyle='NONE', symbol='.', sym_filled=1)
;pp.thick=3
;ll=legend(TARGET=[pp1,pp2], POSITION=[500,10e8],  /DATA, /AUTO_TEXT_COLOR)
;pp.save, '/Users/lbugnet/DATA/METRIC/KEPLER/compare_powvar_Kepler_20_80.png'

pp3=plot(10^yy20(in20), 10^yy(in80),xlog=1, ylog=1,linestyle="none", symbol='D', xtitle='20J', ytitle='80J')
pp3.save, '/Users/lbugnet/DATA/METRIC/KEPLER/compare_powvar_Kepler_20_80_powvar.png'

stop
END