PRO missing_stars, kic1,kic2, aa, bb, outval, path, title,w=w
  ;--------------------------------------------------------------------
  ;-------------- SAVING MISSING MASS STARS ---------------------------
  ;--------------------------------------------------------------------

  ks=kic2
  ks(bb)=-1
  w=where(ks ne -1)

  close,5
  openw,5,path
  close,5
  openw,5, path, /append
  printf,5, title
  close,5
  openw,5, path, /append
  printf,5, outval(w)
  close,5













END