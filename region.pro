
pro region,inic,fin,xori,yori,xmat,ymat,av=av,sgm=sgm, med=med, total_power=total_power,out_metrics=out_metrics
  in = where(xori ge inic(0))
  indx0 = in(0)
  xs = xori(in)
  xy = yori(in)
  in = where(xs lt fin(0))
  xmat = xs(in)
  ymat = xy(in)
  mom  = MOMENT(ymat,sdev=sgm)
  med  = MEDIAN(ymat)
  total_power = TOTAL(ymat)
  av=mom(0)
  out_metrics = DBLARR(4)
  out_metrics(0) = av                             ; Mean
  out_metrics(1) = sgm                            ; Sigma
  out_metrics(2) = med                            ; Median
  out_metrics(3) = total_power                    ; Total_power


  ;print,'media=',av,'sigma=',sgm
  ;a=where(ymat lt 3*sgm+av)
  ;if a(0) ne -1 then sgm1=sigma(ymat(a))
  ;if a(0) ne -1 then print,'media=',total(ymat(a))/n_elements(ymat(a)),'sigma=',sgm
end
