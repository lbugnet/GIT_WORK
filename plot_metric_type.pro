PRO  plot_metric_type, output_resize,yy,xx, res, threshold, flag, slope_fit,  numax

  ;--------------------------------------------------------------
  ;------- ROUTINE COMPUTES AND PLOT NUMAX FROM METRIC ----------
  ;--------------------------------------------------------------
  
  yy_re=alog10(output_resize[*,1])
  xx_re=alog10(numax)
  file_out='/Users/lbugnet/DATA/METRIC/metric_numax_A2Z_stars.png'
  help, slope_fit
  help, output_resize
  pp=plot(xx, yy, yr=[0,8],xtitle='log(numax predicted)', ytitle='log(Metric)',symbol="D", SYM_FILLED=0, color="black",linestyle="none", title="ALL KEPLER RED GIANTS WITH M AND NUMAX KNOWN")
  pp5=plot(xx, slope_fit+threshold, color="lime green",linestyle=1 ,/overplot )
  pp5.thick=3
  pp5=plot(xx, slope_fit, color="slate blue",linestyle=1 ,/overplot )
  pp5.thick=3
  pp4=plot(xx, slope_fit-threshold, color="dark magenta",linestyle=1 ,/overplot )
  pp4.thick=3
  w1=where(flag eq 1)
  pp1=plot(xx_re(w1), yy_re(w1),symbol="D", SYM_FILLED=1, color="dark magenta",linestyle="none", name="LOW STARS", /overplot)
  w2=where(flag eq 2)
  pp2=plot(xx_re(w2), yy_re(w2),symbol="D", SYM_FILLED=1, color="lime green",linestyle="none", name="HIGH STARS", /overplot)
  w3=where(flag eq 3)
  pp3=plot(xx_re(w3), yy_re(w3),symbol="D", SYM_FILLED=1, color="orange",linestyle="none", name="RIGHT STARS", /overplot)
  ll=legend(target=[pp1,pp2,pp3], position=[2.3,7.5] , /DATA, /AUTO_TEXT_COLOR)
  pp.save, file_out

  



















END