PRO K2_miss_check, OUTPUT_A2Z=OUTPUT_A2Z

if CHAMP eq 'ALL' then begin
  restore, '/Users/lbugnet/DATA/METRIC/K2/ALL/K2_varlaw_ALL.sav', /verbose;, res, slope_fit, residuals, threshold, xx, yy
endif else begin
  ;file 1: toutes les metriques des K2 C4
  restore, '/Users/lbugnet/DATA/METRIC/K2/C'+CHAMP+'/K2_varlaw_'+CHAMP+'.sav', /verbose;, res, slope_fit, residuals, threshold, xx, yy ;;;; 
  
endelse
  ;file 2: miss_check
  READCOL, '/Users/lbugnet/DATA/METRIC/K2/C4/list_K2_C4_miss_check.txt', KIC_miss1, dnu_r, numax_r, dnu_b, numax_b, dnu_y, numax_y , FORMAT='A,D,D,D,D,D,D', /silent


output_a2zp=output_a2z
  match, long(KIC_miss1), long(output_a2zp[*,0]), i_miss, i_o, count=n


  ;---- determine right numax

  good_numax=strarr(n_elements(i_miss),3)
  for ii=0, n_elements(i_miss)-1 do begin
    if ((numax_b(i_miss(ii)) lt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)-threshold)/res(1))) and (numax_b(i_miss(ii)) gt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)+threshold)/res(1)))) then begin
      good_numax(ii,0)='ok_b' ; marque les etoiles bien calculées
    endif else begin
      good_numax(ii,0)='bad_b'
    endelse
    if ((numax_r(i_miss(ii)) lt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)-threshold)/res(1))) and (numax_r(i_miss(ii)) gt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)+threshold)/res(1)))) then begin
      good_numax(ii,1)='ok_r'
    endif else begin
      good_numax(ii,1)='bad_r'
    endelse
    if ((numax_y(i_miss(ii)) lt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)-threshold)/res(1))) and (numax_y(i_miss(ii)) gt 10^((alog10(output_a2zp[i_o(ii),1])-res(0)+threshold)/res(1)))) then begin
      good_numax(ii,2)='ok_y'
    endif else begin
      good_numax(ii,2)='bad_y'
    endelse
  endfor

  ;print, good_numax

  ;------------- PLOT --------------------

  ;---Benoit
  ;pp=plot(alog10(numax_b(i_miss)), alog10(output_a2zp[i_o,1]), symbol='*', linestyle="none", xtitle='alog10(numax_b)', ytitle="PSDmoy")
  ;pp=plot(alog10(numax_b(i_miss)), res(1)*alog10(numax_b(i_miss))+res(0), /overplot, color='magenta')
  ;pp.save, '/Users/lbugnet/DATA/METRIC/K2/C4/check_b.png'

  ;---Rafa
  ;ppp=plot(alog10(numax_r(i_miss)), alog10(output_a2zp[i_o,1]), symbol='*', linestyle="none", xtitle='alog10(numax_r)', ytitle="PSDmoy")
  ;ppp=plot(alog10(numax_r(i_miss)), res(1)*alog10(numax_r(i_miss))+res(0), /overplot, color='magenta')
  ;ppp.save, '/Users/lbugnet/DATA/METRIC/K2/C4/check_r.png'

  ;---Yvonne
  ;pppp=plot(alog10(numax_y(i_miss)), alog10(output_a2zp[i_o,1]), symbol='*', linestyle="none", xtitle='alog10(numax_y)', ytitle="PSDmoy")
  ;pppp=plot(alog10(numax_y(i_miss)), res(1)*alog10(numax_y(i_miss))+res(0), /overplot, color='magenta')
  ;pppp.save, '/Users/lbugnet/DATA/METRIC/K2/C4/check_y.png'

  ;---- ALL
  wnb=where(good_numax(*,0) ne 'ok_b')
  wnr=where(good_numax(*,1) ne 'ok_r')
  wny=where(good_numax(*,2) ne 'ok_y')
  wb=where(good_numax(*,0) eq 'ok_b')
  wr=where(good_numax(*,1) eq 'ok_r')
  wy=where(good_numax(*,2) eq 'ok_y')
  p=plot((numax_b(i_miss)), (output_a2zp[i_o,1]), symbol='o', ylog=1, xlog=1,linestyle="none", xtitle='(numax_b)', ytitle="PSDmoy", color='dark violet', name='COR')
  p=plot([(numax_b(i_miss(wnb))), (numax_b(i_miss(wnb)))], [(output_a2zp[i_o(wnb),1]),(output_a2zp[i_o(wnb),1])], symbol='o', SYM_FILLED=1, linestyle="none", xtitle='numax_b', ytitle="PSDmoy", color='dark violet', name='COR', /overplot)
  pp=plot((numax_r(i_miss)), (output_a2zp[i_o,1]), symbol='o', linestyle="none", xtitle='(numax_r)', ytitle="PSDmoy", color='lime green',name='A2ZR',/overplot)
  pp=plot([(numax_r(i_miss(wnr))), (numax_r(i_miss(wnr)))], [(output_a2zp[i_o(wnr),1]),(output_a2zp[i_o(wnr),1])], symbol='o', SYM_FILLED=1, linestyle="none", xtitle='numax', ytitle="PSDmoy", color='lime green', name='A2ZR', /overplot)
  ppp=plot((numax_y(i_miss)), (output_a2zp[i_o,1]), symbol='o', linestyle="none", xtitle='(numax_y)', ytitle="PSDmoy",color='orange',name='BHAM', /overplot)
  ppp=plot([(numax_y(i_miss(wny))), (numax_y(i_miss(wny)))], [(output_a2zp[i_o(wny),1]),(output_a2zp[i_o(wny),1])], symbol='o', SYM_FILLED=1, linestyle="none", xtitle='numax', ytitle="PSDmoy", color='orange', name='BHAM', /overplot)
  pppp=plot(numax_b(i_miss(where(numax_b(i_miss) ne -9999))), 10^(res(1)*alog10(numax_b(i_miss(where(numax_b(i_miss) ne -9999))))+res(0)), /overplot, color='grey', name='Métrique K2')
  pppp.thick=3
  p1=plot((numax_b(i_miss)), 10^(res(1)*alog10(numax_b(i_miss))+res(0)+threshold), /overplot, color='grey')
  p1=plot((numax_b(i_miss)), 10^(res(1)*alog10(numax_b(i_miss))+res(0)-threshold), /overplot, color='grey')
  Thrust_freq = 47.2281
  for j=0,6 do p3=plot([(Thrust_freq*j),(Thrust_freq*j)],[0.01,100000000], /overplot, linestyle=2, color='dark blue');,lin=1,col=7,thick=3

  ll=legend(target=[p,pp,ppp,pppp], position=[2,7] , /DATA, /AUTO_TEXT_COLOR)
  pppp.save, '/Users/lbugnet/DATA/METRIC/K2/C4/check_all.png'
  pppp.close


  ;-----SAVE
  close, 1
  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/bad_b_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/bad_b_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wnb)))-1 do printf,1,KIC_miss1(i_miss(wnb(ii))), strcompress(good_numax(wnb(ii),0) +'='),   numax_b(i_miss(wnb(ii))), $
    strcompress(good_numax(wnb(ii),1)+'='), numax_r(i_miss(wnb(ii))), strcompress(good_numax(wnb(ii),2)+'='), numax_y(i_miss(wnb(ii))), $
    10.^((alog10(output_a2zp[i_o(wnb(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wnb(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1
  ; contient KIC des etoiles pour lesquelles benoit est mauvais puis les numax de b,r,y , et le guess numax et la metric power de l'etoile
  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/good_b_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/good_b_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wb)))-1 do printf,1,KIC_miss1(i_miss(wb(ii))), strcompress(good_numax(wb(ii),0) +'='),   numax_b(i_miss(wb(ii))), strcompress(good_numax(wb(ii),1)+'='), numax_r(i_miss(wb(ii))), strcompress(good_numax(wb(ii),2)+'='), numax_y(i_miss(wb(ii))), 10.^((alog10(output_a2zp[i_o(wb(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wb(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1
  ; contient KIC des etoiles pour lesquelles benoit est bon puis les numax de b,r,y , et le guess numax et la metric power de l'etoile

  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/bad_r_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/bad_r_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wnr)))-1 do printf,1,KIC_miss1(i_miss(wnr(ii))), strcompress(good_numax(wnr(ii),0) +'='),   numax_b(i_miss(wnr(ii))), strcompress(good_numax(wnr(ii),1)+'='), numax_r(i_miss(wnr(ii))), strcompress(good_numax(wnr(ii),2)+'='), numax_y(i_miss(wnr(ii))), 10.^((alog10(output_a2zp[i_o(wnr(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wnr(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1
  ; contient KIC des etoiles pour lesquelles rafa est mauvais puis les numax de b,r,y , et le guess numax et la metric power de l'etoile

  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/good_r_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/good_r_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wr)))-1 do printf,1,KIC_miss1(i_miss(wr(ii))), strcompress(good_numax(wr(ii),0) +'='),   numax_b(i_miss(wr(ii))), strcompress(good_numax(wr(ii),1)+'='), numax_r(i_miss(wr(ii))), strcompress(good_numax(wr(ii),2)+'='), numax_y(i_miss(wr(ii))), 10.^((alog10(output_a2zp[i_o(wr(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wr(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1

  ; contient KIC des etoiles pour lesquelles rafa est bon puis les numax de b,r,y , et le guess numax et la metric power de l'etoile
  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/bad_y_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/bad_y_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wny)))-1 do printf,1,KIC_miss1(i_miss(wny(ii))), strcompress(good_numax(wny(ii),0) +'='),   numax_b(i_miss(wny(ii))), strcompress(good_numax(wny(ii),1)+'='), numax_r(i_miss(wny(ii))), strcompress(good_numax(wny(ii),2)+'='), numax_y(i_miss(wny(ii))), 10.^((alog10(output_a2zp[i_o(wny(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wny(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1
  ; contient KIC des etoiles pour lesquelles yvonne est mauvais puis les numax de b,r,y , et le guess numax et la metric power de l'etoile

  openw,1,'/Users/lbugnet/DATA/METRIC/K2/C4/good_y_K2_C4.txt', WIDTH=90
  close,1
  OpenW, 1, '/Users/lbugnet/DATA/METRIC/K2/C4/good_y_K2_C4.txt',  /append
  for ii=0, n_elements(KIC_miss1(i_miss(wy)))-1 do printf,1,KIC_miss1(i_miss(wy(ii))), strcompress(good_numax(wy(ii),0) +'='),   numax_b(i_miss(wy(ii))), strcompress(good_numax(wy(ii),1)+'='), numax_r(i_miss(wy(ii))), strcompress(good_numax(wy(ii),2)+'='), numax_y(i_miss(wy(ii))), 10.^((alog10(output_a2zp[i_o(wy(ii)),1])-res(0))/res(1)), output_a2zp[i_o(wy(ii)),1] , FORMAT='(a10, 3x,a6,3x, D9.2,3x, a6,3x,D9.2, 3x, a6, 3(D12.2,3x))'
  close, 1
  ; contient KIC des etoiles pour lesquelles yvonne est bon puis les numax de b,r,y , et le guess numax et la metric power de l'etoile

  ;--------- NO MODES STARS ----------------------

  kic_nm_r=KIC_miss1(i_miss(wnr)) ; on prend toutes les étoiles mauvaises de r
  irb=where( good_numax(wnr, 0) eq 'ok_b' )
  iry=where( good_numax(wnr, 2) eq 'ok_y' )

  kic_nm_r(irb)=-1 ; on enleve toutes les étoiles bonnes de y
  kic_nm_r(iry)=-1 ; on enleve toutes les étoiles bonnes de b

  remove, where(kic_nm_r eq -1), kic_nm_r, numax_r(i_miss(wnr)) , output_a2zp[i_o(wnr),1]

  ; il ne reste que les etoiles avec mauvais numax dans les 3 cas

  close,12
  openw,12,'/Users/lbugnet/DATA/METRIC/K2/C4/EPIC_stars_all_bad_numax_K2_C4.txt', WIDTH=80
  close,12
  OpenW, 12, '/Users/lbugnet/DATA/METRIC/K2/C4/EPIC_stars_all_bad_numax_K2_C4.txt',  /append
  for ii=0, n_elements(kic_nm_r)-1 do printf, 12, kic_nm_r(ii)
  close, 12


  stop












END