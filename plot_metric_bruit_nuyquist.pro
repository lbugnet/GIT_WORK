PRO plot_metric_bruit_nuyquist, out, flag_SN, output_a2zp=output_a2zp
  ;---------------------- PLOT THE GRAPH METRIC VS NOISE --------------------------------

  stars_type_1=0.0
  stars_type_2=1.0
  
  p=plot(output_a2zp[*,7], output_a2zp[*,3],  xlog=1, ylog=1, symbol="D", SYM_FILLED=1, name="All Kepler RED GIANTS",linestyle="none", xtitle='mean(PSDbackground)' , ytitle='mean(PSD)-mean(PSDbackground)')     ; metric en fonction de moyenne du bckgrnd
  p.SYM_COLOR = "black"

  file_out='/Users/lbugnet/DATA/METRIC/metric_vs_noise_OUT_stars.png'

  w1=where(flag_SN eq stars_type_1 )
  p1=plot([out[w1,7],out[w1,7]], [out[w1,3],out[w1,3]],   name="Under Nyquist (UN) OUT STARS" ,symbol="D", SYM_FILLED=1, color="crimson",linestyle="none" ,/overplot)
 help, out[w1,7]
  w2=where(flag_SN eq stars_type_2 )
  p2=plot([out[w2,7],out[w2,7]], [out[w2,3],out[w2,3]],   name="Super Nyquist (SN) OUT STARS" ,symbol="D", SYM_FILLED=1, color="medium aquamarine",linestyle="none" ,/overplot)
  help, out[w2,7]

  ll=legend(target=[p,p1,p2], position=[1.e3,1.e9] , /DATA, /AUTO_TEXT_COLOR)
  p.save, file_out










END