kicc=[213382552, 213383603, 213387880, 213383300, 213388347, 213424804, 213453916, 213531008, 213517464, 213543697, 213551173, 213557419, 213582639, 213600514, 213717948,  213842667, 213819287, 213984563, 214611136, 214689818, 214707342, 214708790, 214712730, 214712971, 214738063, 214739660,  214768984, 214773237, 214799621,214950207,  214810782,214811423,  214841777 , 214890940, 214891734, 214915680, 214922346,  214936674, 214954235, 214957349, 214960550, 214966911, 214999205, 215012004, 215012124,215013553, 215016867, 215022195, 215053785, 215076815, 215088259,  215099704, 215121497, 215131703, 215137144, 215149274, 215155802, 215160377, 215170578, 215197935, 215224181, 215361454,215364388, 215373606, 215390455, 215411991, 215429885, 215768568, 215781342, 216324938, 216426902, 216454198, 216616961,216557256,216677726,    216758133, 216787270, 216847177, 217086535 ]
freq=[30, 20, 30, 150, 25, 30, 40 , 30, 30,  30, 45, 20, 45,  20, 10,  25, 180,   35, 0000000, 30, 20, 20, 10, 10,45, 30,  15, 25, 25 ,     70, 10, 15,  25,  25, 20, 15,  10, 20,  15, 25,  30, 30, 20, 25, 30, 10,10, 25,  10,  30, 15 ,     30, 10, 25, 10, 30 ,30,  10,  40 ,20, 10,  30, 25, 35,   15, 30, 30,  25, 30,30, 10,  40,  30, 30, 25,       25, 25, 25,  40]

close,2
openw,2,'/Users/lbugnet/DATA/METRIC/K2/C7/K2_C7_outliers_modes.txt' 
close,2
openw,2, '/Users/lbugnet/DATA/METRIC/K2/C7/K2_C7_outliers_modes.txt', /append 
printf,2, '#Stars in K2 C7 with modes but according to metric no modes'
close,2
openw,2, '/Users/lbugnet/DATA/METRIC/K2/C7/K2_C7_outliers_modes.txt', /append
for ii=0, n_elements(kicc)-1 do printf, 2, kicc(ii), freq(ii)
close,2
