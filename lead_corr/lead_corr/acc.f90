!This program is written to evaluate the correlation skill between the original daily forecasts against the total field of observations.
!The so-called space correlation is used, i.e. ACC.
!For easy access, take 1999 for a trial.
!Time is from Jan 1, 1999 to Dec 31, 2010.
!Some useful relations: 365*4=1460;12*365=4380;12*365*4=17520.
!----------------------------------------------------------------------------------------
program main
implicit none
integer,parameter::nx1=360,ny1=181,nx_new=72,ny_new=37,inter=45,np=1460,npr=1280   !nt=17520
real,parameter::lat0=-90,interval=2.5
real::h_model(nx1,ny1),h_ini(nx1,ny1)
real::h_model_out(nx_new,ny_new,inter),h_ini_out(nx_new,ny_new,inter)
real::h_new(nx_new,ny_new,inter)
real::lat(ny_new),acc(inter,npr),r,acc_ave(inter)
integer::i,j,k
integer::ini1,ini2
integer::flag,iob

do j=1,ny_new
	lat(j)=lat0+interval*(j-1)
end do

open(100,file='H:\ncep_cfs_h500_rawdata\fc1999.grd',form='unformatted',access='direct',recl=nx1*ny1) !Raw data
open(200,file='H:\cfsrhgt500\cfsr_hgt_1999.grd',form='unformatted',access='direct',recl=nx1*ny1) !!True values

flag=0
iob=1
do j=1,npr !!1460 Forecasts viewed as points minus 45 days, that will be 1460-45*4=1280 points, these will be really used.
	flag=flag+1
	ini1=(j-1)*inter+1
	ini2=iob
	write(*,*)ini1,ini2
    !------for one forecast i.e. for one point-----------------------------------------
	do i=0,inter-1
		read(100,rec=ini1+i)h_model(:,:)
		read(200,rec=ini2+i*4)h_ini(:,:)
		call grid(nx1,ny1,5,h_model(:,:),h_model_out(:,:,i+1))
		call grid(nx1,ny1,5,h_ini(:,:),h_ini_out(:,:,i+1))
	end do
    do i=1,inter
    call correlation_space(nx_new,ny_new,h_model_out(:,:,i),h_ini_out(:,:,i),lat,r)
    acc(i,j)=r
    end do
    !-----------------------------------------------------------------------------------		
	iob=iob+1
end do

!To calculate mean acc
acc_ave=0.0
do i=1,inter
    do j=1,npr
        acc_ave(i)=acc_ave(i)+acc(i,j)
    end do
    acc_ave(i)=acc_ave(i)/float(npr)
end do

open(10,file='H:\lead_corr\acc.txt') !!Store correlation value
write(10,*)acc_ave
close(10)



end program

!-----*----------------------------------------------------6---------7--
!     For the correlation coefficient r between two series 
!       x(i) and y(i), where i=1,...,n.
!     input: n,x(n),y(n)
!       n: number of time series
!       x(n): raw series 
!       y(n): raw series 
!     output: r
!       r: space correlation coefficient between x and y
!     By Dr. Wang Jincheng, July 28, 2010.
subroutine correlation_space(nlon,nlat,x,y,lat,r)
   implicit none
   real,parameter::pi=3.1415926
   integer::nlon,nlat
   real   ::x(nlon,nlat),y(nlon,nlat)
   real   ::lat(nlat)
   integer::i,j,k
   real   ::sxy
   real   ::ax,sx,vx
   real   ::ay,sy,vy
   real   ::r
   
   call meanvar_space(nlon,nlat,lat,x,ax,sx,vx)
   call meanvar_space(nlon,nlat,lat,y,ay,sy,vy)
   sxy=0.0
   do i=1,nlon
      do j=1,nlat
         sxy=sxy+(x(i,j)-ax)*(y(i,j)-ay)*cos(lat(j)/180.0*pi)
     enddo
   enddo

   r=sxy/(sx*sy)
   return

end

!-----*----------------------------------------------------6---------7--
!     Computing the mean ax, standard deviation sx
!       and variance vx of a series x(i) (i=1,...,n).
!     input: n and x(n)
!       n: number of raw series
!       x(n): raw series
!     output: ax, sx and vx
!       ax: the mean value of x(n)
!       sx: the standard deviation of x(n)
!       vx: the variance of x(n)
!     By Dr. LI Jianping, May 6, 1998.
subroutine meanvar_space(nlon,nlat,lat,x,ax,sx,vx)
   implicit none
   real,parameter::pi=3.1415926
   integer::nlon,nlat
   real   ::lat(nlat)
   real   ::x(nlon,nlat)
   real   ::ax,sx,vx
   integer::i,j,k
   ax=0.

   do i=1,nlon
      do j=1,nlat
         ax=ax+x(i,j)  
      enddo
   enddo
   ax=ax/float(nlon*nlat)
   vx=0.
   sx=0.	  
   do i=1,nlon
      do j=1,nlat
         vx=vx+(x(i,j)-ax)**2*cos(lat(j)/180.0*pi)
     enddo
   enddo
   sx=sqrt(vx)
   return
end

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