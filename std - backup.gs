*********************************************************************
*  |variables uesd in this script:
*  |yr:     Every year
*  |mean:   Mean value of the variable shtfl.
*  |sd:     Standard deviation of one year.
*----------------------------------------------------------
*Author: Wayne Huai @ SYSU
*Based on CFSR and NCEP data.
*Date: Apr 22, 2012
*Last modified: Jun 16, 2014
******************************************
'reinit'
'open 26_eof.ctl'
'set mpdset hires'
yr=1985
while(yr<=2010)
*******************************************************
*Standard deviation will be caculated
'set lat 0 50.5'
'set lon 119.5 179.5'
*shtfl means surface latent heat flux
'define mean=ave(shtfl,time=00Z01JAN'yr',time=00Z31DEC'yr')'
'define sd=sqrt(ave(pow(shtfl-mean,2),time=00Z01JAN'yr',time=00Z31DEC'yr'))'
*********************************************************
*This is a skill to plot a shaded map with contour on it 
'set grads off'
'set gxout shaded'
'd sd'
'cbarn'
'set gxout contour'
'set cthick 5'
'd sd'
'draw title Standard Deviation of 'yr' '
'printim D:\sd\4213\'yr'.png x1000 y800 white'
'c'
yr=yr+1
endwhile
;

