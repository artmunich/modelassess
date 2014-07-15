!To calculate standard deviation of CFSR true values!
!In this program, annual cycle is removed while in GrADS script it stays which results the value to be unrealistic.
!Written by: Xiaowei Huai
!Last modified: 4/Jul/2014
!----------------------------------------------
program main
implicit none
integer,parameter::nx=360,ny=181,nt=17520,nyear=12,ntime=365*4,nx1=72,ny1=37
integer::i,j,k,l,irec,iyear,t,it
real(kind=4)::cfsr(nt),climate(ntime),climate1,anom(nt)
real(kind=4)::sdc(nx,ny)
real(kind=4)::ax,sx,vx

!-------Input data
!Open true values of hgt500mb which is 4 times a day from 1999 to 2010.
open(100,file='true_origin.grd',form='unformatted',access='direct',recl=1)

!Grid by grid
do j=1,ny
    do i=1,nx
    write(*,*)i,j
        !Raw series of a given grid point
        do k=1,nt
            irec=(k-1)*nx*ny+i*j
            read(100,rec=irec)cfsr(k)
        end do

        !!Calculate the climate cycle
        climate1=0.
        do t=1,ntime
            do iyear=1,nyear
                it=(iyear-1)*ntime+t
                climate1=climate1+cfsr(it)
            end do !nyear
            climate(t)=climate1/real(nyear)
        end do !ntime

        !Remove annual cycle. Anomaly is obtained.
        do k=1,nt
            l=mod(k,ntime)   
            if(l==0)then
                l=nt
                anom(k)=cfsr(k)-climate(l)
            else
                anom(l)=cfsr(k)-climate(l)
            end if
        end do 

        !Standard deviation is calculated.
        call stdev(nt,anom,ax,sx,vx)
        sdc(i,j)=sx

    end do  !nx
end do  !ny

close(100)

!Write standard deviation of 500hPa geopotential height from CFSR true values.
open(200,file='sdc_true.grd',form='unformatted',access='direct',recl=nx*ny)
write(200,rec=1)((sdc(i,j),i=1,nx),j=1,ny)
close(200)


end program

!====================================================================================
!-----*----------------------------------------------------6---------7--
!Computing the mean ax, standard deviation sx
!  and variance vx of a series x(i) (i=1,...,n).
!input: n and x(n)
!  n: number of raw series
!  x(n): raw series
!output: ax, sx and vx
!  ax: the mean value of x(n)
!  sx: the standard deviation of x(n)
!  vx: the variance of x(n)
!By Dr. LI Jianping, May 6, 1999.
!Revised by Dr. Huai Xiaowei, Jun 21, 2014.
subroutine stdev(n,x,ax,sx,vx)
implicit none
integer::n,i
real::ax,sx,vx,x(n)

ax=0.
vx=0.
sx=0.

do i=1,n
    ax=ax+x(i)
end do
ax=ax/float(n)

do i=1,n
  vx=vx+(x(i)-ax)**2
end do
vx=vx/float(n)
sx=sqrt(vx)

return
end subroutine
!===========================================

!=================================
!===interpolation,if you don't want to calculate the error growth of every grid =================
subroutine grid(m,n,mul,h1,h2)
implicit none
integer::m,n,mul
real::h1(m,n),h2(m/mul,((n-1)/mul)+1)
integer::ix,iy,i,j
iy=0
do j=1,n,mul
	iy=iy+1
	ix=0
	do i=1,m,mul
	ix=ix+1
	h2(ix,iy)=h1(i,j)
	end do
end do
return
end subroutine
!===============================

!===================================================
!Year cycle must be eliminated!
!The difference between year cycle and climate average must be noticed.
!=====================================================
!calculate the climate cycle. The raw series is x(m).
!input:    
!---m:the number of the time series
!---x(m):the series
!---nt:the cycle
!output:      
!---y(nt)     
subroutine clim_cycle(m,nt,x,y)
implicit none
integer::m,nt,i,j
integer::flag(nt)
real::x(m)    
real::y(nt)
real::summ
summ=0.0
flag=0
y=0.
do i=1,m
        j=mod(i,nt)
        if(j==0)then
        j=nt
        flag(j)=flag(j)+1
        y(j)=y(j)+x(i)
        else
        flag(j)=flag(j)+1
        y(j)=y(j)+x(i)
        endif
end do
do j=1,nt
        y(j)=y(j)/real(flag(j))
end do
return
end subroutine

!calculate the anomaly
!input:
!---m:the number of the time series
!---x(m):the series
!---nt:the cycle
!---y(nt)
!output:
!---out(m)anomaly series for removing the annual cycle
subroutine anomaly(m,nt,x,y,xout)
implicit none
integer::m,nt,i,j,flag
real::x(m),y(nt)
real::xout(m)
real::summ
summ=0.0
flag=0
do i=1,m
        j=mod(i,nt)   
        if(j==0)then
        j=nt
        xout(i)=x(i)-y(j)
        else
       xout(i)=x(i)-y(j)
        end if
end do
return
end subroutine
