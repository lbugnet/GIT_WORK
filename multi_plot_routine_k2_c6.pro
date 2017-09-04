pro multi_plot_routine_K2_C6, numax,  res, threshold,xx,yy, files_arr, path_e, kic

  jj=1
  kk=1
 ;p1=plot([1,1],[1,1], ylog=1, xlog=1,dimension=[2000,1000] , color='r')

  FOR ii=0,n_elements(files_arr)-1 do begin
      
      readcol, files_arr[ii], time,flux, FORMAT='D,D' ; time et flux contient le nombre de points du spectre de l'etoile (environ 3085), files_arr contient 6352 etoiles
      out=DBLARR(2,n_elements(time))
      out(0,*) = time(*)-time(0)
      out(1,*) = flux
      ;in ppm
      out(1,*)=1e6*(out(1,*)-1)
      ;compute PSD
      psd,out,s,ff2,ofac=ofac; s in Hz
      a=dblarr(2,n_elements(s))
    
      a(0,*)=s*1.d6
      a(1,*)=ff2

      if ((jj le 20) and (jj le n_elements(numax))) then begin
        p1 = plot(a(0,*),smooth(a(1,*),5,/edge_truncate), layout=[5,4,jj], ylog=1, xlog=1,xr=[1,500],xst=1, yr=[10,0.9*max(a(1,*))], yst=1 ,color='grey', title=' stars: KIC='+ string(kic(ii)),dimension=[2000,1000], /current)
        p1 = plot([numax(ii),numax(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='b',axis_style=1)
        numax_t_min=10.^(alog10(numax(ii))+(threshold)/res(1))
        numax_t_max=10.^(alog10(numax(ii))-(threshold)/res(1))
        p1 = plot([numax_t_min, numax_t_min],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
        p1 = plot([numax_t_max, numax_t_max],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
        Thrust_freq = 47.2281
        for j=0,6 do p1=plot([(Thrust_freq*j),(Thrust_freq*j)],[0.01,100000000], /overplot, linestyle=2, color='dark blue');,lin=1,col=7,thick=3
        if (jj eq 20) then begin
          path_kk=strcompress(path_e+strtrim(kk)+'.jpg', /remove_all)
          p1.save, path_kk
          p1.close
        endif
        jj=jj+1
      endif else begin
       
        p1 = plot(a(0,*),smooth(a(1,*),5,/edge_truncate), layout=[5,4,1], ylog=1, xlog=1,xr=[1,500],xst=1, yr=[10,0.9*max(a(1,*))],color='grey', yst=1, title=' stars: KIC='+ string(kic(ii)), dimension=[2000,1000])
        p1 = plot([numax(ii),numax(ii)],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='b',axis_style=1)
        numax_t_min=10.^(alog10(numax(ii))+(threshold)/res(1))
        numax_t_max=10.^(alog10(numax(ii))-(threshold)/res(1))
        p1 = plot([numax_t_min, numax_t_min],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
        p1 = plot([numax_t_max, numax_t_max],[1,max(a(1,*))], ylog=1, xlog=1, /overplot, color='g')
        Thrust_freq = 47.2281
        for j=0,6 do p3=plot([(Thrust_freq*j),(Thrust_freq*j)],[0.01,100000000], /overplot, linestyle=2, color='dark blue');,lin=1,col=7,thick=3
        jj=2    ;; compteur remis a zero pour une nouvelle image
        kk=kk+1 ;; compteur sur le nombre d'images
        ;stop
      endelse

    
  ENDFOR





END