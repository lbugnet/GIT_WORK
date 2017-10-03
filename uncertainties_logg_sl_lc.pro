pro uncertainties_logg_SL_LC

  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/LC__7_output_numax_all.sav';, OUTPUT_A2Z, NUMAX_GUESS, NG_MAX, NG_MIN, res_solar, threshold_ok

  ;----------- SOLARLIKE confirmées
  readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/Table1_Chaplin.txt', kic_chaplin, numax_chaplin, format='A,D', skipline=29  ; donne kic et numax des etoiles solar_like confirmées
  match, long(output_a2z[*,0]), long(kic_chaplin), ind1, ind2
  numax_chaplin=numax_chaplin(ind2)
  output_chaplin=output_a2z[ind1,1]

  ;----------- APOKASC RG
  readcol, '/Users/lbugnet/DATA/TABLES/comp_results_APOKASC_all2.txt',  Kic, numax1 ,dnu1, numax, dnu, skipline=1,  /silent, format='L'
  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/LC_APOKASC_0.700000_output_numax_all.sav', /verbose
  ;readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/POWVAR_A2Z_STARS.txt', OUTPUT_A2Z0, OUTPUT_A2Z1, format='L,D'
  OUTPUT_A2Zp=dblarr(n_elements(output_a2z), 11)
  OUTPUT_a2zp=output_a2z
  match, long(OUTPUT_A2Zp[*,0]), long(kic), ind1, ind2
  OUTPUT_A2Zp1=OUTPUT_a2zp[ind1,*]
  OUTPUT_A2Z=OUTPUT_A2Zp1
  kic_s=kic(ind2)


  pp1=plot((numax1(ind2)), (OUTPUT_A2Z[*,1]), xr=[1, 10000], yr=[1, 10000], dim=[1200,800],linestyle='none', xlog=1, ylog=1, symbol='D', font_name='Times', font_size=14,sym_filled=1, color='red',sym_size=1.2,  transparency=50, name='RG APOKASC')
  pp2=plot((numax_chaplin), (output_chaplin), linestyle='none', symbol='D', xlog=1, ylog=1,sym_filled=1, color='blue', sym_size=1.2,/overplot, transparency=50, name='SOLAR_LIKE NO GOLD')


  ;;;-------- ADD GOLD STARS (ORANGE) --------------------

  restore, '/Users/lbugnet/DATA/METRIC/KEPLER/SOLAR_LIKE/LC__7_output_numax_all.sav', /verbose
  p=plot(10^xx, 10^yy, xlog=1, ylog=1, symbol="D", SYM_FILLED=1, linestyle="none", color='dark orange',sym_size=1.2,xtitle='$\nu_{max} (\mu Hz)$', ytitle='POWVAR ($ppm^2/\mu Hz$)', name='GOLD STARS', /overplot, transparency=50)













END