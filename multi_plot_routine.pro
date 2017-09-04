pro multi_plot_routine, stars_type, numax, files_arr, res, threshold, flag, kic_s, xx, yy, M

  jj=2
  kk=1
 ind=stars_type
 
 p1=plot([1,1],[1,1], ylog=1, xlog=1,dimension=[2000,1000] , color='r')
 ;xsz = 6000
 ;ysz = 1000
 ;WINDOW,xsize=xsz,ysize=ysz
 ;PLOT,[0,0],[0,0],background=255

 IF IND EQ 1 THEN BEGIN
  TYPE='LOW'
  file_mkdir, '/Users/lbugnet/DATA/METRIC/LOW'
  path='/Users/lbugnet/DATA/METRIC/LOW/'
 ENDIF
 IF IND EQ 2 THEN BEGIN
   TYPE='HIGH'
   file_mkdir, '/Users/lbugnet/DATA/METRIC/HIGH'
   path='/Users/lbugnet/DATA/METRIC/HIGH/'
 ENDIF
 IF IND EQ 3 THEN BEGIN
  TYPE='RIGHT'
  file_mkdir, '/Users/lbugnet/DATA/METRIC/RIGHT'
  path='/Users/lbugnet/DATA/METRIC/RIGHT/'
 ENDIF
 IF IND EQ 0 THEN BEGIN
   TYPE='OK'
   file_mkdir, '/Users/lbugnet/DATA/METRIC/OK'
   path='/Users/lbugnet/DATA/METRIC/OK/'
 ENDIF
    
    
  w1=where(flag eq stars_type)
  a=readfits(string(files_arr(w1(0))), /silent)
  ;[6000,1000]
  p1 = plot(a(0,*)*1.d6,smooth(a(1,*),5,/edge_truncate), layout=[5,4,1],dimension=[2000,1000],axis_style=1, ylog=1, xlog=1,xr=[0.5,300],xst=1, yr=[10,0.9*max(a(1,*))], yst=1, title= type+' stars: KIC='+ string(kic_s(w1(0))))
  p1 = plot([numax(w1(0)),numax(w1(0))],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='r')
  numax_t=10^((yy(w1(0))-res(0))/res(1))
  numax_t_min=10^((yy(w1(0))-res(0)+threshold)/res(1))
  numax_t_max=10^((yy(w1(0))-res(0)-threshold)/res(1))
  p1 = plot([numax_t, numax_t],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='b')
  p1 = plot([numax_t_min, numax_t_min],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
  p1 = plot([numax_t_max, numax_t_max],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
  numax_t=dblarr(n_elements(numax)-1)
  numax_t_min=dblarr(n_elements(numax)-1)
  numax_t_max=dblarr(n_elements(numax)-1)
  
  
  FOR ii=0,n_elements(M)-1 do begin
    
    if ((flag(ii) eq stars_type ) and ( (ii) ne (w1(0)))) then begin
      a=readfits(string(files_arr(ii)), /silent)

      if ((jj le 20) and (jj le n_elements(w1))) then begin
        p1 = plot(a(0,*)*1.d6,smooth(a(1,*),5,/edge_truncate), layout=[5,4,jj], ylog=1, xlog=1,xr=[0.5,300],xst=1, yr=[10,0.9*max(a(1,*))], yst=1, title=type +' stars: KIC='+ string(kic_s(ii)), /current)
        p1 = plot([numax(ii),numax(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='r',axis_style=1)
        numax_t(ii)=10.^((yy(ii)-res(0))/res(1))
        numax_t_min(ii)=10^((yy(ii)-res(0)+threshold)/res(1))
        numax_t_max(ii)=10^((yy(ii)-res(0)-threshold)/res(1))
        p1 = plot([numax_t(ii), numax_t(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='b',axis_style=1)
        p1 = plot([numax_t_min(ii), numax_t_min(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g',axis_style=1)
        p1 = plot([numax_t_max(ii), numax_t_max(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g',axis_style=1)
        if ((jj eq 20) or (jj eq n_elements(w1))) then begin
          path_kk=strcompress(path+'plot_metric_numax_'+type+'_stars_panel_'+strtrim(kk)+'.jpg', /remove_all)
          p1.save, path_kk
          p1.close
        endif
        jj=jj+1
      endif else begin
        ;stop
        p1 = plot(a(0,*)*1.d6,smooth(a(1,*),5,/edge_truncate), layout=[5,4,1], ylog=1, xlog=1,xr=[0.5,300],xst=1, yr=[10,0.9*max(a(1,*))], yst=1, dimension=[2000,1000], title='low stars: KIC='+ string(kic_s(ii)))
        p1 = plot([numax(ii),numax(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='r',axis_style=1)
        numax_t(ii)=10.^((yy(ii)-res(0))/res(1))
        numax_t_min(ii)=10^((yy(ii)-res(0)+threshold)/res(1))
        numax_t_max(ii)=10^((yy(ii)-res(0)-threshold)/res(1))
        p1 = plot([numax_t(ii), numax_t(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='b',axis_style=1)
        p1 = plot([numax_t_min(ii), numax_t_min(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g',axis_style=1)
        p1 = plot([numax_t_max(ii), numax_t_max(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g',axis_style=1)
        jj=2    ;; compteur remis a zero pour une nouvelle image
        kk=kk+1 ;; compteur sur le nombre d'images
      endelse

    endif
    
  ENDFOR


END