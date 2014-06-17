'reinit'
*ifc for forecast
*itrue for true value at 0Z
*'set fwrite H:/modelerror/errors.grd'
ierror=9
while(ierror<=45)
*Max for itrue is 1457, 0Z31Dec
ifc=1
ifcmax=1457-(ierror-1)*4
say ifcmax
*Store seperately
'set gxout fwrite'
'set fwrite H:/modelerror/error'ierror'.grd'
  while(ifc<=ifcmax)
  itrue=ifc+(ierror-1)*4
say ifc
say itrue
  'open J:/cfs_hindcast/1999/prs/prsDly_('ifc').ctl'
  'open J:/cfs_hindcast/1999/prs/prsDly_('itrue').ctl'
  'set x 1 360'
  'set y 1 181'
  'set lev 500'
  'd hgt500mb.1(t='ierror')-hgt500mb.2(t=1)'
  'close 2'
  'close 1'
  ifc=ifc+4
  endwhile
ierror=ierror+1
say ierror
'disable fwrite'
say (ifcmax-1)/4
endwhile
;
