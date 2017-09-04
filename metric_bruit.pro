PRO metric_bruit, output_resize, flag,output_a2zp=output_a2zp
;---------------------- PLOT THE GRAPH METRIC VS NOISE --------------------------------

  stars_type_1=1
  stars_type_2=2
  stars_type_3=3
  p=plot(output_a2zp[*,7], output_a2zp[*,3],  xlog=1, ylog=1, symbol="D", SYM_FILLED=1, name="All A2Z stars",linestyle="none", xtitle='mean(PSDbackground)' , ytitle='mean(PSD)-mean(PSDbackground)')     ; metric en fonction de moyenne du bckgrnd
  p.SYM_COLOR = "black"

  file_out='/Users/lbugnet/DATA/METRIC/metric_vs_noise_savita_stars_16000.png'

  w1=where(flag eq stars_type_1 )
  p1=plot([output_resize[w1,7],output_resize[w1,7]], [output_resize[w1,3],output_resize[w1,3]],   name="LOW STARS" ,symbol="D", SYM_FILLED=1, color="dark magenta",linestyle="none" ,/overplot)
  w2=where(flag eq stars_type_2 )
  p2=plot([output_resize[w2,7],output_resize[w2,7]], [output_resize[w2,3],output_resize[w2,3]],   name="HIGH STARS" ,symbol="D", SYM_FILLED=1, color="lime green",linestyle="none",/overplot)
  w3=where(flag eq stars_type_3 )
  p3=plot( [output_resize[w3,7],output_resize[w3,7]], [output_resize[w3,3],output_resize[w3,3]],   symbol="D",   name="RIGHT STARS" ,SYM_FILLED=1,color="orange", linestyle="none",/overplot)


  ll=legend(target=[p,p1,p2,p3], position=[1.e2,1.e10] , /DATA, /AUTO_TEXT_COLOR)
  p.save, file_out










END