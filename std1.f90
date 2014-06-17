!To calculate standard deviation of time series of 500hPa geopotential height at time 00Z!
!----------------------------------------------------------------------------------------
!Variables:
!   nx,ny: Grid number of each dataset, including CFSR and NCEP Reanalysis 1;
!   nt: Total time length of each dataset;
!   nday: Number of days of 500hPa hgt at time 00Z;
!   c0,r1: Time series of 500hPa hgt at 0Z for CFSR and NCEP R1 respectively.
!--------------------------------------------------------------------------------
!If you store data as r1(nx,ny,nt), this will lead to relocation truncated to fit error.
!It means that the size of array is too huge to store, not yet to manipulate.
!A bad example is like this:

!In order to prevent a huge array, sd is calculated point by point.
!-------------------------------------------------------------------------------------
!Author: Xiaowei Huai
!Last modified: Jun 17, 2014
!----------------------------------------------------------------------------
program main
implicit none
integer,parameter::nxc=360,nyc=181,ntc=17520,ndayc=4380 !CFSR
integer,parameter::nxr1=144,nyr1=73,ntr1=17532,ndayr1=4383 !NCEP Reanalysis 1
integer::i,j,k,irec
real(4)::c0(nxc,nyc,ndayc),r1(nxr1,nyr1,ndayr1)
real(4)::sdc0(nxc,nyc),sdr1(nxr1,nyr1),std

open(100,file='cfsr_hgt0.grd',form='unformatted',access='direct',recl=nxc*nyc)
open(200,file='hgt0.grd',form='unformatted',access='direct',recl=nxr1*nyr1)

do k=1,ndayc
    read(100,rec=k)((c0(i,j,k),i=1,nxc),j=1,nyc)
end do
print*,c0(3,50,23)
do k=1,ndayr1
    read(200,rec=k)((r1(i,j,k),i=1,nxr1),j=1,nyr1)
end do

close(100)
close(200)

call sd(c0,nxc,nyc,ndayc,sdc0)
call sd(r1,nxr1,nyr1,ndayr1,sdr1)

open(300,file='sdc.grd',form='unformatted',access='direct',recl=nxc*nyc*4)
write(300,rec=1)((sdc0(i,j),i=1,nxc),j=1,nyc)
close(300)
open(400,file='sdr1.grd',form='unformatted',access='direct',recl=nxr1*nyr1*4)
write(400,rec=1)((sdr1(i,j),i=1,nxr1),j=1,nyr1)
close(400)

end program

!------------------------------------------------------------------
!To calculate standard deviation
!Input:
!   x(nt): raw data series;
!Output:
!   std: standard deviation of series x(nt).
!--------------------------------------------
subroutine sd(x,nx,ny,nt,stdev)
implicit none
integer::i,j,k,nx,ny,nt
real(4)::ave,std
real(4)::x(nx,ny,nt),stdev(nx,ny)

do j=1,ny
    do i=1,nx
        !Average value for one point
        ave=0.0
        do k=1,nt
            ave=ave+x(i,j,k)
        end do
        ave=ave/float(nt)

        !Standard deviation of the series
        std=0.0
        do k=1,nt
            std=std+(x(i,j,k)-ave)**2
        end do
        std=sqrt(std/float(nt))
        stdev(i,j)=std
    end do
end do
end subroutine

