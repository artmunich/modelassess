*This gs is written to display global averaged forecast RMSE!
*The result will be a 1 dimensional curve with each point stands for 
*global averaged RMSE of that time.
*-----------------------------------------
'reinit'
'open errRMSE.ctl'

'set x 1'
'set y 1'
'set lev 500'
'set t 1 44'

'set grid off'
'set grads off'

'set xaxis 1 45 5'
'set annot 1 4'
'set xlopts 1 5 0.15'
'set ylopts 1 5 0.15'
*'draw xlab Forecast lead time (Day)'
*'draw ylab RMSE (m)'
'set string 1 bc 5'
'set strsiz 0.18 0.20'
'draw string 5.9 0.15 Forecast Lead Time ( Day )'

'set string 1 bc 5 90'
'set strsiz 0.18 0.20'
'draw string 1.3 4.3 RMS error ( m )'

'd tloop(aave(err,x=1,x=360,y=1,y=181))'
'set font 1'
'draw title Global averaged forecast RMSE'

*Here sd is area averaged standard deviation
*sd=84.5 for global;93.0 for NH; 74.7 for SH
*AR stands for attractor radius, ar=sqrt(2)*sd
sd=84.5
ar=sd*1.414
'define dummy1='sd''
'set ccolor 2'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'd dummy1'

'define dummy2='ar''
'set ccolor 3'
'set cthick 10'
'set cstyle 2'
'set cmark 0'
'd dummy2'

*'printim rmse.globave.1d.png x1000 y800 white'
;

