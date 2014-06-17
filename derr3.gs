*The second day of forecast
*The third day in cfsr datasets
'reinit'
'open error3.ctl'
iday=1
while(iday<=363)
  'set grads off'
  'set grid off'
  'set gxout shaded'
  'set x 1 360'
  'set y 1 181'
  'set lev 500'
  'set t 'iday
  'd err' 
  'cbarn'
  'draw title Day2 of forecast-true'
  'printim H:\modelError\error3\'iday'.png white'
  'c'
  iday=iday+1
endwhile
'set grads off'
'set grid off'
'd ave(err,t=1,t=363)'
'cbarn'
'draw title Day2 of forecast-true'
'printim H:\modelError\error3\err3ave.png white'
;
