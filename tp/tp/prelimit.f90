!--------------------------------------------------------------------------------------------
!To assess predictability limit of CFSR.
!The saturation value is sqrt(2) times standard deviation of a variable.
!When increased to the saturation value, interpolation is used to determine the limit.
!---------------------------------------------------------------------------------------
!@Wayne Huai
!-------------------------------------------------------------------------------------------
program main
implicit none
integer,parameter::nx1=360,ny1=181,nt=45,nx_interp=72,ny_interp=37
integer::i,j,k
real::sdc(nx1,ny1),sdc_interp(nx_interp,ny_interp),ar(nx_interp,ny_interp),rmse(nx_interp,ny_interp,nt)
real::tpsd(nx_interp,ny_interp),tpar(nx_interp,ny_interp)

!Open standard deviation file of CFSR first;
!Interpolation is used to fit rmse field;
!AR stands for attractor radius which is potential limit of predictability!
!--------------------------
open(100,file='H:\variance_fortran\sd_cfsr.grd',form='unformatted',access='direct',recl=nx1*ny1)
read(100,rec=1)sdc(:,:)
close(100)

call grid(nx1,ny1,5,sdc,sdc_interp)
ar=1.414*sdc_interp

open(4500,file='H:\variance_fortran\sdc_interp.grd',form='unformatted',access='direct',recl=nx_interp*ny_interp)
write(4500,rec=1)sdc_interp
close(4500)


!Open model rmse of hgt 500
!----------------------------------
open(200,file='H:\variance_fortran\errorrmse_field_ave.grd',form='unformatted',access='direct',recl=nx_interp*ny_interp)
do k=1,nt
    read(200,rec=k)rmse(:,:,k)
end do
close(200)

do j=1,ny_interp
  do i=1,nx_interp
    call pre_limit(nt,rmse(i,j,:),tpsd(i,j),sdc_interp(i,j))
    call pre_limit(nt,rmse(i,j,:),tpar(i,j),ar(i,j))
  end do
end do

open(300,file='H:\variance_fortran\tpsd.grd',form='unformatted',access='direct',recl=nx_interp*ny_interp)
write(300,rec=1)tpsd
close(300)


open (2000,file='H:\variance_fortran\tpsd.txt')
write(2000,*)tpsd
close(2000)

open(400,file='H:\variance_fortran\tpar.grd',form='unformatted',access='direct',recl=nx_interp*ny_interp)
write(400,rec=1)tpar
close(400)

open (3000,file='H:\variance_fortran\tpar.txt')
write(3000,*)tpar
close(3000)

pause

end program

!-------Subroutine process: determining the limit of predictability from the error increasing series.
subroutine pre_limit(nt,x,tlength,satur)
implicit none
integer::nt,i
real::x(nt),tlength
real::satur

do i=1,nt-1
    if(x(i)<=satur .and. x(i+1)>=satur)then
    tlength=((x(i+1)-satur)*i+(satur-x(i))*(i+1))/(x(i+1)-x(i)) 
    else 
    tlength=nt
    end if
end do   
return
end subroutine

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