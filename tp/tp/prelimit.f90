!--------------------------------------------------------------------------------------------
!To assess predictability limit of CFSR.
!The saturation value is sqrt(2) times standard deviation of a variable.
!When increased to the saturation value, interpolation is used to determine the limit.
!---------------------------------------------------------------------------------------
!@Wayne Huai
!-------------------------------------------------------------------------------------------
program main
implicit none
integer,parameter::nx=360,ny=181,nt=44
integer::i,j,k
real::sdc(nx,ny),ar(nx,ny),errrmse(nx,ny,nt),tpsd(nx,ny),tpar(nx,ny)


open(100,file='H:\stdev\sdc.grd',form='unformatted',access='direct',recl=nx*ny)
read(100,rec=1)sdc(:,:)
ar(:,:)=1.414*sdc(:,:)
close(100)
print*,sdc(3,5)

open(200,file='H:\modelError\errall\rmse\errRMSE.grd',form='unformatted',access='direct',recl=nx*ny)
do k=1,nt
    read(200,rec=k)errrmse(:,:,k)
end do
close(200)
print*,errrmse(2,4,5)

do j=1,ny
  do i=1,nx
    call pre_limit(nt,errRMSE(i,j,:),tpsd(i,j),sdc(i,j))
    call pre_limit(nt,errRMSE(i,j,:),tpar(i,j),ar(i,j))
  end do
end do

open(300,file='H:\modelError\errall\rmse\tpsd.grd',form='unformatted',access='direct',recl=nx*ny)
write(300,rec=1)tpsd(:,:)
close(300)

open(400,file='H:\modelError\errall\rmse\tpar.grd',form='unformatted',access='direct',recl=nx*ny)
write(400,rec=1)tpar(:,:)
close(400)
end program

!-------Subroutine process: determining the limit of predictability from the error increasing series.
subroutine pre_limit(nt,x,tlength,satur)
implicit none
integer::nt,i
real::x(nt),tlength
real::satur

do i=1,nt-1
    if(x(i).le.satur .and. x(i+1).ge.satur)then
    tlength=((x(i+1)-satur)*i+(satur-x(i))*(i+1))/(x(i+1)-x(i))
    else 
    tlength=nt
    end if
end do   
return
end subroutine