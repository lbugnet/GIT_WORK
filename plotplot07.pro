p=plot(10^xx, 10^yy, xlog=1, ylog=1, transparency=90, symbol='D', sym_filled=1, linestyle='none', sym_size=1, xr=[8,200], yr=[200,5000], color='black', xtitle='$\nu_{max} (\mu Hz)$', ytitle='$POWVAR (ppm^2/ \mu Hz)$', font_size=13, font_name='Times')
p=plot(10^xx, 10^slope_fit, color='blue violet', /overplot, thick=3, transparency=50)
t2 = TEXT(50, 350,  'Second Clump ?' , /DATA, FONT_SIZE=14, font_name='Times', color='red')
t2 = TEXT(27, 1800,  'RC' , /DATA, FONT_SIZE=14, font_name='Times', color='red')
t2 = TEXT(80, 500,  'RGB' , /DATA, FONT_SIZE=14, font_name='Times', color='red')
p.save, '/Users/lbugnet/DATA/METRIC/KEPLER/zoom.png'