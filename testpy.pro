pro testpy, aa, bb, aout=aout, baout=baout
resolve_all, UNRESOLVED=variable, /CONTINUE_ON_ERROR
aa=aa+1
bb=aa+4
print, aa
print, bb
;cc=12
;ab=aa
;ba=bb
aout=aa
baout=bb
testpyy, aout=aout
print, 'ok return'
end