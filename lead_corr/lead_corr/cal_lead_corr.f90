!This program is written to evaluate the correlation skill between the original daily forecasts against the total field of observations.
!For easy access, take 1999 for a trial.
!Time is from Jan 1, 1999 to Dec 31, 2010.
!Some useful relations: 365*4=1460;12*365=4380;12*365*4=17520.
!----------------------------------------------------------------------------------------
program main
implicit none
integer,parameter::nx1=360,ny1=181,nlead=45
real::r,lead_corr(nx1,ny1,nlead)
real,allocatable::h_model(:,:,:),h_ini(:,:,:)
integer::i,j,k,l,nfc
integer::ini1,ini2,flag


open(100,file='H:\ncep_cfs_h500_rawdata\fc1999.grd',form='unformatted',access='direct',recl=nx1*ny1) !Raw data
open(200,file='H:\cfsrhgt500\cfsr_hgt_1999.grd',form='unformatted',access='direct',recl=nx1*ny1) !!True values
open(10,file='H:\lead_corr\lead_corr_field.grd',form='unformatted',access='direct',recl=nx1*ny1) !!Store correlation value

nfc=1460
flag=1
do i=0,nlead-1

allocate(h_model(nx1,ny1,nfc))
allocate(h_ini(nx1,ny1,nfc))

    do j=1,nfc
        ini1=(j-1)*nlead
        ini2=i*4+1
        write(*,*)ini1,ini2
        read(100,rec=i+1+ini1)h_model(:,:,j)
        read(200,rec=ini2)h_ini(:,:,j)
    end do
    
    do k=1,ny1
        do l=1,nx1
            call correlation(nfc,h_model(l,k,:),h_ini(l,k,:),r)
            lead_corr(l,k,i+1)=r
        end do
    end do
    write(10,rec=flag)lead_corr(:,:,i+1)
nfc=nfc-4
flag=flag+1

deallocate(h_model)
deallocate(h_ini)

end do

close(10)
close(100)
close(200)

end program


!=============calculate the correlation between forecast and observation========================
!-----*----------------------------------------------------6---------7--
!For the correlation coefficient r between two series x(i) and y(i), where i=1,...,n.
!input: n,x(n),y(n)
!  n: number of time series
!  x(n): raw series 
!  y(n): raw series 
!output: r
!  r: correlation coefficient between x and y
!By Dr. LI Jianping, January 5, 2000.
!Revised by Dr. Huai Xiaowei, Jun 21, 2014
subroutine correlation(n,x,y,r)
implicit none
integer::i,n
real::x(n),y(n),r,ax,sx,vx,ay,sy,vy,sxy

call meanvar(n,x,ax,sx,vx)
call meanvar(n,y,ay,sy,vy)

sxy=0.
do i=1,n
    sxy=sxy+(x(i)-ax)*(y(i)-ay)
end do

sxy=sxy/float(n)
r=sxy/(sx*sy)

return
end subroutine
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
subroutine meanvar(n,x,ax,sx,vx)
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
