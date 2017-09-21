PRO copy_files_in_directory, files_dir_in=files_dir_in, directory_out=directory_out, file_kic_path=file_kic_path


if n_elements(files_dir_in) eq 0 then files_dir_in= '/Volumes/TEMP/RG_DR25/MISS/RESULTS_KADACS_COARSE_CheckSTATUS_filt_polfitseg960.000_20.0000d_ppm0_inpaint20/FIGS'
if (STREGEX(file_KIC_PATH, 'low') NE -1) AND (STREGEX(files_dir_in, 'MISS') NE -1) THEN BEGIN
  if n_elements(directory_out) eq 0 then directory_out='/Users/lbugnet/DATA/METRIC/KEPLER/COPY_FILES/LOW_MISS/'
  
  
  Readcol, file_kic_path, kic, flag, format='L' ;fichier avec kic (long)
  Read_kepler_dir,files_dir_in,files,id,wild=‘kplr*.jpg’,length=9

  Match,LONG(kic),long(id),i1,i2,count=np



  count=0
  count2=0
  If (np gt 0) and (n_elements(flag) ne -1) THEN BEGIN
    FOR i=0l,np-1 DO BEGIN
      if long(flag(i1(i))) eq 4 then begin
        print,i,' ',np
        directory_out4=directory_out+'HIGH_NOISE/'
        FILE_COPY, files(i2(i)), directory_out4
        count=count+1
      endif else begin
        print,i,' ',np
        directory_out3=directory_out+'UNKNOWN/'
        FILE_COPY, files(i2(i)), directory_out3
        count2=count2+1
      endelse
    ENDFOR
  ENDIF

ENDIF
if (STREGEX(file_KIC_PATH, 'high') NE -1) AND (STREGEX(files_dir_in, 'MISS') NE -1) THEN BEGIN
  if n_elements(directory_out) eq 0 then directory_out='/Users/lbugnet/DATA/METRIC/KEPLER/COPY_FILES/HIGH_MISS/'



Readcol, file_kic_path, kic, flag, format='L' ;fichier avec kic (long)
Read_kepler_dir,files_dir_in,files,id,wild=‘kplr*.jpg’,length=9

Match,LONG(kic),long(id),i1,i2,count=np


  
count=0
count2=0
If (np gt 0) and (n_elements(flag) ne -1) THEN BEGIN
  FOR i=0l,np-1 DO BEGIN
    if long(flag(i1(i))) eq 1 then begin
      print,i,' ',np
      directory_out4=directory_out+'ACTIVITY/'
      FILE_COPY, files(i2(i)), directory_out4
      count=count+1
    endif else begin
      print,i,' ',np
      directory_out3=directory_out+'UNKNOWN/'
      FILE_COPY, files(i2(i)), directory_out3
      count2=count2+1
    endelse
  ENDFOR
ENDIF
ENDIF 

if (STREGEX(file_KIC_PATH, 'APOKASC') NE -1) and ((STREGEX(file_KIC_PATH, 'low') NE -1)) and ((STREGEX(file_KIC_PATH, '95') NE -1)) THEN BEGIN
  if n_elements(directory_out) eq 0 then directory_out='/Users/lbugnet/DATA/METRIC/KEPLER/COPY_FILES/APOKASC/LOW95/'
  Readcol, file_kic_path, kic,  format='L' ;fichier avec kic (long)
  Read_kepler_dir,files_dir_in,files,id,wild=‘kplr*.jpg’,length=9
  Match,LONG(kic),long(id),i1,i2,count=np
  count=0
  If (np gt 0) THEN BEGIN
    FOR i=0l,np-1 DO BEGIN
        FILE_COPY, files(i2(i)), directory_out
        count=count+1
    ENDFOR
  ENDIF
ENDIF

if (STREGEX(file_KIC_PATH, 'APOKASC') NE -1) and ((STREGEX(file_KIC_PATH, 'high') NE -1)) and ((STREGEX(file_KIC_PATH, '95') NE -1)) THEN BEGIN
  if n_elements(directory_out) eq 0 then directory_out='/Users/lbugnet/DATA/METRIC/KEPLER/COPY_FILES/APOKASC/HIGH95/'
  Readcol, file_kic_path, kic,  format='L' ;fichier avec kic (long)
  Read_kepler_dir,files_dir_in,files,id,wild=‘kplr*.jpg’,length=9
  Match,LONG(kic),long(id),i1,i2,count=np
  count=0
  If (np gt 0) THEN BEGIN
    FOR i=0l,np-1 DO BEGIN
      FILE_COPY, files(i2(i)), directory_out
      count=count+1
    ENDFOR
  ENDIF
ENDIF
stop
END