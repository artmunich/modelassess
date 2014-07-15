program main
implicit none
integer,parameter::nx=72,ny=37,nt=17340,inter=45,i_south_20=15,i_north_20=23 !half hemisphere is from 20-90; tropical area is from 0 to 20.
integer,parameter::nhalf=15,ntrop=9 !for each half hemisphere, there are nhalf grid points; for tropical area, there are ntrop grid points.
real,parameter::lat0=-90.0,interval=5,latn=0
integer::i,j
real::sd(nx,ny),sd_nh(nx,nhalf),sd_sh(nx,nhalf),sd_trop(nx,ntrop)
real::sd_ga,sd_na,sd_sa,sd_ta   !global/north/south/tropical average
real::lat(ny),rmseall(nx,ny,inter),rmse_global_ave(inter),rmse_nh_ave(inter),rmse_sh_ave(inter),rmse_trop_ave(inter)

!latitude Global; north hemisphere; south hemisphere
do j=1,ny
	lat(j)=lat0+interval*(j-1)
end do

!Area averaged standard deviation-------
open(10,file='H:\variance_fortran\sd_true\sdc_true_interp.grd',form='unformatted',access='direct',recl=nx*ny)
read(10,rec=1)sd
sd_sh(:,:)=sd(:,1:i_south_20)
sd_nh(:,:)=sd(:,i_north_20:ny)
sd_trop(:,:)=sd(:,i_south_20:i_north_20)

call rmse_space(nx,ny,sd,lat,sd_ga) !global
call rmse_space(nx,nhalf,sd(:,i_north_20:ny),lat(i_north_20:ny),sd_na) !north hemisphere
call rmse_space(nx,nhalf,sd(:,1:i_south_20),lat(1:i_south_20),sd_sa)  !south hemisphere
call rmse_space(nx,ntrop,sd(:,i_south_20:i_north_20),lat(i_south_20:i_north_20),sd_ta)  !tropical

close(10)

open(20,file='H:\variance_fortran\sd_true\sd_weighted_ave.txt')
write(20,*)'Global averaged sd is ',sd_ga
write(20,*)'Northern hemisphere averaged sd is ',sd_na
write(20,*)'Southern hemisphere averaged sd is ',sd_sa
write(20,*)'Tropical averaged sd is ',sd_ta

close(20)
!*****************************************************************************************************

open(100,file='H:\variance_fortran\errorrmse_field_ave.grd',form='unformatted',access='direct',recl=nx*ny*inter)
read(100,rec=1)rmseall
close(100)

do i=1,inter
    call rmse_space(nx,ny,rmseall(:,:,i),lat,rmse_global_ave(i))
    call rmse_space(nx,nhalf,rmseall(:,i_north_20:ny,i),lat(i_north_20:ny),rmse_nh_ave(i))
    call rmse_space(nx,nhalf,rmseall(:,1:i_south_20,i),lat(1:i_south_20),rmse_sh_ave(i))
    call rmse_space(nx,ntrop,rmseall(:,i_south_20:i_north_20,i),lat(i_south_20:i_north_20),rmse_trop_ave(i))
end do

open(200,file='H:\variance_fortran\rmse_global_ave.grd',form='unformatted',access='direct',recl=inter)
write(200,rec=1)rmse_global_ave
close(200)

open(300,file='H:\variance_fortran\rmse_nh_ave.grd',form='unformatted',access='direct',recl=inter)
write(300,rec=1)rmse_nh_ave
close(300)

open(400,file='H:\variance_fortran\rmse_sh_ave.grd',form='unformatted',access='direct',recl=inter)
write(400,rec=1)rmse_sh_ave
close(400)

open(500,file='H:\variance_fortran\rmse_trop_ave.grd',form='unformatted',access='direct',recl=inter)
write(500,rec=1)rmse_trop_ave
close(500)

end program

!========================SUBROUTINE===========================
!!calculate the space_rmse
!input:
!   nlon,nlat: grid number
!   x(nlon,nlat): variable
!   lat(nlon): latitude
!output:
!   rmse: a real number which is  
subroutine rmse_space(nlon,nlat,x,lat,rmse)
implicit none
real,parameter::pi=3.1415926
integer::nlon,nlat
real::x(nlon,nlat)
real::lat(nlon)
integer::i,j,k
real::total_weight
real::rmse
rmse=0.0
total_weight=0.0
do i=1,nlon
	do j=1,nlat
		rmse=rmse+x(i,j)**2*cos(lat(j)/180.0*pi)
		total_weight=total_weight+cos(lat(j)/180.0*pi)
	end do
end do
rmse=sqrt(rmse/total_weight)
return
end subroutine
