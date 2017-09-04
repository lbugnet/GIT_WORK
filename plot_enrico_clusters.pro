pro PLOT_ENRICO_CLUSTERS


restore, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO_CRGs_Class_NGC6811.sav', /verbose
out6811=output_a2z
ng6811=numax_guess


restore, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO_CRGs_Class_NGC6791.sav', /verbose
out6791=output_a2z
ng6791=numax_guess


restore, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO_CRGs_Class_NGC6819.sav', /verbose
out6819=output_a2z
ng6819=numax_guess

restore,'/Users/lbugnet/DATA/TABLES/metric_all_stars.sav'; xx, yy, res, slope_fit, output_a2zp, output_resize,  out ;=(stars not in common) 



readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/Corsaro_CRGs_Global_Parameters_Corr.txt', KIC  , Teff   , Teffsig ,  FeH , FeHsig   , nuMax  , nuMaxsig , DeltaNu ,DeltaNusig,STRINGSKIP='#', format='D'

match, long(out6791), long(KIC), id1, ind2
numax6791=nuMax(ind2)
match, long(out6811), long(KIC), id1, ind2
numax6811=nuMax(ind2)
match, long(out6819), long(KIC), id1, ind2
numax6819=nuMax(ind2)

pp1=plot(numax6811(*), long(out6811[*,1]), DIM=[800,400], name='Cluster 6811', margin=[0.15,0.02,0.05,0.02],xlog=1, ylog=1,SYM_SIZE=2, font_name='Times', linestyle="none",symbol=5, font_size=15, SYM_FILLED=1, color='green',  ytitle='POWVAR',xtextpos=1, xr=[20,180],yr=[80,10^4])
pp2=plot(numax6791(*), long(out6791[*,1]), name='Cluster 6791',  symbol=6, xlog=1, ylog=1,linestyle="none", SYM_FILLED=1,SYM_SIZE=2, color='blue',/overplot)
pp3=plot(numax6819(*), long(out6819[*,1]), name='Cluster 6819',  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=1, SYM_SIZE=2, color='red',/overplot)
pp11=plot(numax6811(*), long(out6811[*,1]), DIM=[800,400], name='Cluster 6811', margin=[0.15,0.02,0.05,0.02],xlog=1, ylog=1,SYM_SIZE=2, font_name='Times', linestyle="none",symbol=5, font_size=15, SYM_FILLED=0, color='black',  ytitle='POWVAR',xtextpos=1, xr=[20,180],yr=[80,10^4], /overplot)
pp21=plot(numax6791(*), long(out6791[*,1]), name='Cluster 6791',  symbol=6, xlog=1, ylog=1,linestyle="none", SYM_FILLED=0,SYM_SIZE=2, color='black',/overplot)
pp31=plot(numax6819(*), long(out6819[*,1]), name='Cluster 6819',  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=0, SYM_SIZE=2, color='black',/overplot)

;pp=plot(numax, 10^(res(1)*alog10(numax)+res(0)), /overplot)
leg = LEGEND(TARGET=[pp1,pp2, pp3], POSITION=[160,9000],  /DATA, /AUTO_TEXT_COLOR)

pp3.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/plot_numaxe_powvar.png'

stop
;------------ FIT -----------------------

out=dblarr(N_elements(out6791[*,0])+N_elements(out6811[*,0])+N_elements(out6819[*,0]), n_elements(out6791[0,*]))
out[0:29,*]=out6791[0:29,*]
out[30:35,*]=out6811[0:5,*]
out[36:59,*]= out6819[0:23,*]

numax=dblarr(N_elements(numax6791[*,0])+N_elements(numax6811[*,0])+N_elements(numax6819[*,0]), n_elements(numax6791[0,*]))
numax[0:29,*]=numax6791[0:29,*]
numax[30:35,*]=numax6811[0:5,*]
numax[36:59,*]= numax6819[0:23,*]

match, long(out[*,0]), long(KIC), i1, i2, count=nn
out=out[i1,*]
numax=numax[i1]
temp=Teff(i2)
METAL=feh(i2)
readcol, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/Corsaro_CRGs_Global_Parameters_Corr_DScorr_130317_solarCorr_RadIncl.txt', KIC2, dnu2 , numax2 , feh2      , teff2 ,  mass_asfgrid , radius_asfgrid, STRINGSKIP='#', format='D'

match, long(out[*,0]), long(KIC2), in1, in2, count=nn
out=out[in1,*]
numax=numax[in1]
temp=Teff[in1]
METAL=feh[in1]
mass=mass_asfgrid(in2) ; in solar mass

order=sort(numax)

X=alog(numax/3147.0)
Y=alog(mass)
W=(METAL)
A = [1.17, -1.33, -0.40, 0.38]
Z=alog(out[*,1])
;Z = A[0] + A[1]*X + A[2]*Y + A[3]*W
XYW=[[X],[Y],[W]]

yfit = CURVEFIT(XYW, Z, weights, A,sigma,  FUNCTION_NAME='mymulticurvefit')
pp3=plot(numax(order), exp(A[0] + A[1]*X(order) + A[2]*Y(order) + A[3]*W(order)),   xlog=1, ylog=1, color='black', /overplot)
;t1 = TEXT(45, 6000,  'FIT POWVAR' , /DATA, FONT_SIZE=15, font_name='Times')
t2 = TEXT(22 ,6000,  '(a)' , /DATA, FONT_SIZE=15, font_name='Times')
pp3.thick=2
;pp3.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/fit_cluster_enrico.png'

lnBmM=A[0]
s=A[1]
t=A[2]
u=A[3]
stop

p=plot( numax[0:29], out[[0:29],1]*mass[0:29]^(-t)*exp(-u*METAL[0:29]), DIM=[800,400], xr=[20,160], margin=[0.15,0.02,0.05,0.02],font_name='Times', SYM_SIZE=2,  yr=[100, 3000],xlog=1, rgb_table=33, ylog=1, ytitle='$ POWVAR M^{-t} e^{-u*[Fe/H]}$  (ppm)', xtextpos=1,font_size=15,linestyle="none",symbol=6, SYM_FILLED=1, vert_colors=255./(5200.-4000.)*(temp[0:29]-4000), color='black')
p=plot( numax[30:53], out[[30:53],1]*mass[30:53]^(-t)*exp(-u*METAL[30:53]),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=1, SYM_SIZE=2, rgb_table=33,vert_colors=255./(5200.-4000.)*(temp[30:53]-4000), /overplot)
p=plot( numax[54:59], out[[54:59],1]*mass[54:59]^(-t)*exp(-u*METAL[54:59]),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=1, rgb_table=33, SYM_SIZE=2,vert_colors=255./(5200.-4000.)*(temp[54:59]-4000), /overplot)
p=plot( numax[0:29], out[[0:29],1]*mass[0:29]^(-t)*exp(-u*METAL[0:29]), DIM=[800,400], xr=[20,160], margin=[0.15,0.02,0.05,0.02],font_name='Times', SYM_SIZE=2,  yr=[100, 3000],xlog=1,  ylog=1, ytitle='$ POWVAR M^{-t} e^{-u*[Fe/H]}$  (ppm)', xtextpos=1,font_size=15,linestyle="none",symbol=6, SYM_FILLED=0,  color='black', /overplot)
p=plot( numax[30:53], out[[30:53],1]*mass[30:53]^(-t)*exp(-u*METAL[30:53]),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=0, SYM_SIZE=2, color='black', /overplot)
p=plot( numax[54:59], out[[54:59],1]*mass[54:59]^(-t)*exp(-u*METAL[54:59]),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=0,  SYM_SIZE=2,color='black', /overplot)
cb = COLORBAR(POSITION=[0.2,0.15,0.6,0.2], RGB_TABLE=33, RANGE=[4000,5200], TEXT_ORIENTATION=1, TITLE='$T_{eff} (K) $')
t1 = TEXT(50, 2000,  '$\nu_{max}$ effect' , /DATA, FONT_SIZE=15, font_name='Times')
t2 = TEXT(22, 2000,  '(b)' , /DATA, FONT_SIZE=15, font_name='Times')
p.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/numax_effect_4b_cluster_enrico.png'


p1=plot( numax[0:29], out[[0:29],1]*mass[0:29]^(-t), xr=[20,160], DIM=[800,400],yr=[100, 3000], xlog=1, margin=[0.15,0.02,0.05,0.02], font_name='Times',SYM_SIZE=2,rgb_table=reverse(33), vert_colors=255./(0.45)*(metal[0:29]+0.1), ylog=1, font_size=15, ytitle='$POWVAR M^{-t}$',xtextpos=1,linestyle="none",symbol=6, SYM_FILLED=1);, vert_color=255-((0.35+0.1)/(0.002.*256.))*(metal[0:29]+0.1)/0.002)
p1=plot( numax[30:53], out[[30:53],1]*mass[30:53]^(-t),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=1, SYM_SIZE=2,rgb_table=reverse(33),vert_colors=255./(0.45)*(metal[30:53]+0.1), /overplot)
p1=plot( numax[54:59], out[[54:59],1]*mass[54:59]^(-t),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=1, rgb_table=reverse(33),SYM_SIZE=2,vert_colors=255./(0.45)*(metal[54:59]+0.1), /overplot)
p1=plot( numax[0:29], out[[0:29],1]*mass[0:29]^(-t), xr=[20,160], DIM=[800,400],yr=[100, 3000], xlog=1, margin=[0.15,0.02,0.05,0.02], font_name='Times',SYM_SIZE=2,  ylog=1, font_size=15, ytitle='$POWVAR M^{-t}$',xtextpos=1,linestyle="none",symbol=6, SYM_FILLED=0, color='black',/overplot);, vert_color=255-((0.35+0.1)/(0.002.*256.))*(metal[0:29]+0.1)/0.002)
p1=plot( numax[30:53], out[[30:53],1]*mass[30:53]^(-t),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=0, SYM_SIZE=2, color='black',/overplot)
p1=plot( numax[54:59], out[[54:59],1]*mass[54:59]^(-t),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=0, SYM_SIZE=2,color='black', /overplot)
cc = COLORBAR(POSITION=[0.2,0.15,0.6,0.2], RGB_TABLE=reverse(33), RANGE=[-0.1,0.35], TEXT_ORIENTATION=1, TITLE='[Fe\H]')
t1 = TEXT(40, 2000,  '$\nu_{max}$ and [Fe/H] effect' , /DATA, FONT_SIZE=15, font_name='Times')
t2 = TEXT(22, 2000,  '(c)' , /DATA, FONT_SIZE=15, font_name='Times')
p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/numax_and_metal_effect_4c_cluster_enrico.png'


p1=plot( numax[0:29], out[[0:29],1]*exp(-u*METAL[0:29]), DIM=[800,470], xr=[20,160],yr=[100, 3000],xlog=1, xshowtext=1,font_name='Times',SYM_SIZE=2, margin=[0.15,0.15,0.05,0.01],ylog=1, xtitle='$\nu_{max}$   $(\mu Hz)$', ytitle='$POWVAR e^{-u*[Fe/H]}$  (ppm)',linestyle="none", font_size=15,symbol=6, SYM_FILLED=1, rgb_table=62,vert_colors=255./(3.2-0.8)*(mass[0:29]-0.8))
p1=plot( numax[30:53], out[[30:53],1]*exp(-u*METAL[30:53]),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=1, rgb_table=62,SYM_SIZE=2,vert_colors=255./(3.2-0.8)*(mass[30:53]-0.8), /overplot)
p1=plot( numax[54:59], out[[54:59],1]*exp(-u*METAL[54:59]),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=1, rgb_table=62,SYM_SIZE=2,vert_colors=255./(3.2-0.8)*(mass[54:59]-0.8), /overplot)
p1=plot( numax[0:29], out[[0:29],1]*exp(-u*METAL[0:29]), DIM=[800,470], xr=[20,160],yr=[100, 3000],xlog=1, xshowtext=1,font_name='Times',SYM_SIZE=2, margin=[0.15,0.15,0.05,0.01],ylog=1, color='black',xtitle='$\nu_{max}$   $(\mu Hz)$', ytitle='$POWVAR e^{-u*[Fe/H]}$  (ppm)',linestyle="none", font_size=15,symbol=6, SYM_FILLED=0, /overplot)
p1=plot( numax[30:53], out[[30:53],1]*exp(-u*METAL[30:53]),  xlog=1, ylog=1,linestyle="none",symbol=24, SYM_FILLED=0, SYM_SIZE=2,color='black', /overplot)
p1=plot( numax[54:59], out[[54:59],1]*exp(-u*METAL[54:59]),  xlog=1, ylog=1,linestyle="none",symbol=5, SYM_FILLED=0, SYM_SIZE=2,color='black', /overplot)
cd = COLORBAR(POSITION=[0.2,0.25,0.6,0.3], RGB_TABLE=62, RANGE=[0.8,3.2], TEXT_ORIENTATION=1, TITLE='Mass (solar mass)')
t1 = TEXT(40, 2000,  '$\nu_{max}$ and Mass effect' , /DATA, FONT_SIZE=15, font_name='Times')
t2 = TEXT(22, 2000,  '(d)' , /DATA, FONT_SIZE=15, font_name='Times')
p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/numax_and_mass_effect_4d_cluster_enrico.png'

; calcul mean
mm1=mean(mass[0:29])
mm2=mean( mass[30:53])
mm3=mean(mass[54:59])
mo1=mean(out[[0:10],1]*exp(-u*METAL[0:29])^(-t)*(numax[0:10]/3147.0)^(-s))*11
mo1p=mean(out[[12:29],1]*exp(-u*METAL[12:29])^(-t)*(numax[12:29]/3147.0)^(-s))*18
mo1=(mo1+mo1p)/30.
mo2=mean(out[[30:36],1]*exp(-u*METAL[30:36])*(numax[30:36]/3147.0)^(-s))*7
mo2p=mean(out[[38:53],1]*mass[38:53]^(-t)*(numax[38:53]/3147.0)^(-s))*16
mo2=(mo2+mo2p)/(23.)
mo3=mean(out[[54:59],1]*exp(-u*METAL[54:59])*(numax[54:59]/3147.0)^(-s))
p1=plot( mass[0:29], out[[0:29],1]*exp(-u*METAL[0:29])^(-t)*(numax[0:29]/3147.0)^(-s) ,DIM=[600,400], yr=[0,8],  rgb_table=33,font_name='Times',SYM_SIZE=2,vert_colors= 255./(170.-20.)*(numax[0:29]-20.), SYM_THICK=2, font_size=15, xtitle='$M/M_{\odot}$', ytitle='$POWVAR e^{-u*[Fe]/[H]} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol=6, SYM_FILLED=1, color='blue')
p1=plot( mass[30:53], out[[30:53],1]*exp(-u*METAL[30:53])*(numax[30:53]/3147.0)^(-s), linestyle="none",symbol=24, SYM_SIZE=2,SYM_FILLED=1, sym_color='black', SYM_THICK=3,rgb_table=33,vert_colors= 255./(170.-20.)*(numax[30:53]-20.), /overplot)
p1=plot( mass[54:59], out[[54:59],1]*exp(-u*METAL[54:59])*(numax[54:59]/3147.0)^(-s), linestyle="none",symbol=5, SYM_SIZE=2,SYM_FILLED=1,  SYM_THICK=2,rgb_table=33,vert_colors=255./(170.-20.)*(numax[54:59]-20.), /overplot)
p1=plot( mass[0:29], out[[0:29],1]*exp(-u*METAL[0:29])^(-t)*(numax[0:29]/3147.0)^(-s) ,DIM=[800,470], yr=[0,8],  font_name='Times',SYM_SIZE=2, color='black', font_size=15, xtitle='$M/M_{\odot}$', ytitle='$POWVAR e^{-u*[Fe]/[H]} (\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol=6, SYM_FILLED=0, /overplot)
p1=plot( mass[30:53], out[[30:53],1]*exp(-u*METAL[30:53])*(numax[30:53]/3147.0)^(-s), linestyle="none",symbol=24, SYM_SIZE=2,SYM_FILLED=0,   color='black',/overplot)
p1=plot( mass[54:59], out[[54:59],1]*exp(-u*METAL[54:59])*(numax[54:59]/3147.0)^(-s), linestyle="none",symbol=5, SYM_SIZE=2,SYM_FILLED=0,  color='black', /overplot)
p1=plot([mm1,mm1],[mo1,mo1], symbol='star', sym_size=5, sym_filled=1, color='red' ,/overplot)
p1=plot([mm2,mm2],[mo2,mo2], symbol='star', sym_size=5, sym_filled=1, color='red' ,/overplot)
p1=plot([mm3,mm3],[mo3,mo3], symbol='star', sym_size=5, sym_filled=1, color='red' ,/overplot)
p1=plot([mm1,mm1],[mo1,mo1], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black' ,/overplot)
p1=plot([mm2,mm2],[mo2,mo2], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black' ,/overplot)
p1=plot([mm3,mm3],[mo3,mo3], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black' ,/overplot)
ce = COLORBAR(POSITION=[0.4,0.8,0.9,0.85], RGB_TABLE=33, RANGE=[20,170], TEXT_ORIENTATION=1, TITLE='$\nu_{max} (\mu Hz)$')
t1 = TEXT(2, 0.8,  'Mass effect' , /DATA, FONT_SIZE=15, font_name='Times')
p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/mass_effect_6_cluster_enrico.png'

;;calcul mean
m1=mean(out[[0:10],1]*mass[0:10]^(-t)*(numax[0:10]/3147.0)^(-s))*11
m1p=mean(out[[12:29],1]*mass[12:29]^(-t)*(numax[12:29]/3147.0)^(-s))*18
m1=(m1+m1p)/30.
m2=mean(out[[30:36],1]*mass[30:36]^(-t)*(numax[30:36]/3147.0)^(-s))*7
m2p=mean(out[[38:53],1]*mass[38:53]^(-t)*(numax[38:53]/3147.0)^(-s))*16
m2=(m2+m2p)/(23.)
m3=mean(out[[54:59],1]*mass[54:59]^(-t)*(numax[54:59]/3147.0)^(-s))
p1=plot( metal[0:29], out[[0:29],1]*mass[0:29]^(-t)*(numax[0:29]/3147.0)^(-s) , DIM=[600,400],yr=[0,10], rgb_table=33,font_name='Times',SYM_SIZE=2,vert_colors=255./(170.-20.)*(numax[54:59]-20.),  xtitle='[Fe/H]',  font_size=15, ytitle='$POWVAR  M^{-t}$ $(\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol=6, SYM_FILLED=1, color='blue')
p1=plot( metal[30:53], out[[30:53],1]*mass[30:53]^(-t)*(numax[30:53]/3147.0)^(-s), linestyle="none",symbol=24,SYM_SIZE=2, SYM_FILLED=1, color='red', rgb_table=33,vert_colors=255./(170.-20.)*(numax[54:59]-20.),/overplot)
p1=plot( metal[54:59], out[[54:59],1]*mass[54:59]^(-t)*(numax[54:59]/3147.0)^(-s), linestyle="none",symbol=5,SYM_SIZE=2, SYM_FILLED=1, color='green', rgb_table=33,vert_colors=255./(170.-20.)*(numax[54:59]-20.),/overplot)
p1=plot( metal[0:29], out[[0:29],1]*mass[0:29]^(-t)*(numax[0:29]/3147.0)^(-s) , DIM=[800,470],yr=[0,10], font_name='Times',SYM_SIZE=2,  xtitle='[Fe/H]',  font_size=15, ytitle='$POWVAR  M^{-t}$ $(\nu_{max}/ \nu_{max \odot})^{-s}$  (ppm)',linestyle="none",symbol=6, SYM_FILLED=0, color='black', /overplot)
p1=plot( metal[30:53], out[[30:53],1]*mass[30:53]^(-t)*(numax[30:53]/3147.0)^(-s), linestyle="none",symbol=24,SYM_SIZE=2, SYM_FILLED=0, color='black', /overplot)
p1=plot( metal[54:59], out[[54:59],1]*mass[54:59]^(-t)*(numax[54:59]/3147.0)^(-s), linestyle="none",symbol=5,SYM_SIZE=2, SYM_FILLED=0, color='black',/overplot)
p1=plot([metal[0],metal[0]], [m1,m1], symbol='star', sym_size=5, sym_filled=1, color='red' ,/overplot)
p1=plot([metal[30],metal[30]], [m2,m2], symbol='star', sym_size=5, sym_filled=1, color='red' ,/overplot)
p1=plot([metal[54],metal[54]], [m3,m3], symbol='star', sym_size=5, sym_filled=1, color='red', /overplot)
p1=plot([metal[0],metal[0]], [m1,m1], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black' ,/overplot)
p1=plot([metal[30],metal[30]], [m2,m2], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black' ,/overplot)
p1=plot([metal[54],metal[54]], [m3,m3], symbol='star', sym_size=5, sym_filled=0, thick=2, color='black', /overplot)
cf = COLORBAR(POSITION=[0.4,0.8,0.9,0.85], RGB_TABLE=33, RANGE=[20,170], TEXT_ORIENTATION=1, TITLE='$\nu_{max} (\mu Hz)$')
t1 = TEXT(0.2, 0.8,  'Metallicity effect' , /DATA, FONT_SIZE=15, font_name='Times')
p1.save, '/Users/lbugnet/DATA/METRIC/KEPLER/CLUSTER_ENRICO/metal_effect_7_cluster_enrico.png'


stop
END