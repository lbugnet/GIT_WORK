PRO  metric_predict_numax, out, res, threshold, flag_SN=flag_SN

;--------------------------------------------------------------
;------- ROUTINE COMPUTES AND PLOT NUMAX FROM METRIC ----------
;--------------------------------------------------------------

  yy_out=alog10(out[*,1])
  numax_predicted_out=10^((yy_out-res(0))/res(1))
  xx_out=alog10(numax_predicted_out)
  yerror=dblarr(n_elements(out[*,1]))
  yerror(*)=0
  xerror=dblarr(n_elements(out[*,1]))
  xerror(*)=abs(threshold/sin(res(1)))
  w_SN=where(xx_out gt (2+alog10(3)))                     ; indices des étoiles superNuyquists
  w_SM=where(xx_out lt (alog10(0.7)))                     ; indices des étoiles supermassives
  file_out='/Users/lbugnet/DATA/METRIC/numax_predicted_non_A2Z_stars.png'
  
  ;p = ERRORPLOT(10^xx_out, 10^yy_out, 10^xerror, 10^yerror)
  ;p.color="gray"
  pl=plot(10^xx_out, 10^yy_out, xtitle='$\nu_{max} prédit par POWVAR$', dim=[500,400],  xr=[1e-3,1e6], yr=[1e-2,3e8], sym_size=1.5, ytitle='$POWVAR$',symbol="D",  font_name='Times', font_size=12, ylog=1, xlog=1, SYM_FILLED=1, color="orange",transparency=50,linestyle="none");, title="Metric predict $\nu_{max}$ for 6817 non-A2Z Kepler stars")
 xx=10^xx_out
  ;rr=sort(xx)
  ;polyg=polygon([xx(rr),reverse(xx(rr))], [10^(res(1)*alog10(xx(rr))+res(0)-threshold),reverse(10^(res(1)*alog10(xx(rr))+res(0)+threshold))], target=pl, /DATA,FILL_BACKGROUND=1,  FILL_COLOR="crimson", FILL_TRANSPARENCY=80, TRANSPARENCY=90)
  ;p= ERRORPLOT(10^xx_out, 10^yy_out, xerror, yerror, xlog=1, ylog=1, /overplot)
ppp=plot([10^(2+alog10(3)),10^(2+alog10(3))], [10^(-2),10^10],transparency=0, /overplot )
  ;ppp.color="medium aquamarine"
  ppp=plot(10^xx_out(w_SN), 10^yy_out(w_SN), symbol="D", SYM_FILLED=1, color="indigo",linestyle="none" , sym_size=1.5,transparency=50, /overplot)
 ppp=plot(10^xx_out(w_SM), 10^yy_out(w_SM), symbol="D", SYM_FILLED=1,color="medium spring green",linestyle="none" ,  sym_size=1.5,transparency=50,/overplot)
    ppp.save, file_out

  flag_SN=dblarr(n_elements(out[*,1]))
  flag_SN(*)=0
  flag_SN(w_SN)=1
  
  flag_SM=dblarr(n_elements(out[*,1]))
  flag_SM(*)=0
  flag_SM(w_SM)=1
  save, file='/Users/lbugnet/DATA/METRIC/numax_predicted_non_A2Z_stars.sav', numax_predicted_out, xerror, flag_SN, flag_SM, xx_out, yy_out, out
  stop
  close,5
  openw,5,'/Users/lbugnet/DATA/METRIC/numax_predicted_non_A2Z_stars.txt'
  close,5
  openw,5,'/Users/lbugnet/DATA/METRIC/numax_predicted_non_A2Z_stars.txt', /append
  printf,5, string(out[*,0]), numax_predicted_out[*]
  close,5


help, string(out[*,0]), numax_predicted_out[*]
















END