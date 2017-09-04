PRO missing_mass_stars, kic,kic_s, aa, bb
  ;--------------------------------------------------------------------
  ;-------------- SAVING MISSING MASS STARS ---------------------------
  ;--------------------------------------------------------------------

  ks=kic_s
  ks(bb)=-1
  w=where(ks ne -1)

  close,5
  openw,5,'/Users/lbugnet/DATA/TABLES/missing_stars_s.txt'
  close,5
  openw,5, '/Users/lbugnet/DATA/TABLES/missing_stars_s.txt', /append
  printf,5, 'Stars KIC in results_A2Z_all.sav (132 stars in 16398) that are missing in Q1_17_closeout_starproperties_final_DR25_MAthur_Catalogue.sav'
  close,5
  openw,5, '/Users/lbugnet/DATA/TABLES/missing_stars_s.txt', /append
  printf,5, kic_s(w)
  close,5













END