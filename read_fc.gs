'reinit'
'set fwrite J:\ncep_cfs_hgt500\fc_1999.grd'
'set gxout fwrite'
number=1
while(number<=1460)
'open H:\cfs_hindcast\1999\prs\prsDly_('number').ctl'
'set lev 500'
'set t 1 45'
'set x 1 360'
'set y 1 181'
'd hgt500mb'
'close 1'
number=number+1
endwhile
'disable fwrite'
;
