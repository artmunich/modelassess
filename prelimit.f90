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
real::sdc(nx,ny),satur(nx,ny),errrmse(nx,ny,nt),tp(nx,ny)


open(100,file='H:\stdev\sdc.grd',form='unformatted',access='direct',recl=nx*ny)
read(100,rec=1)sdc(:,:)
satur(:,:)=sqrt(2)*sdc(:,:)
close(100)

open(200,file='H:\modelError\errall\rmse\errRMSE.grd',form='unformatted',access='direct',recl=nx*ny)
do i=k,nt
    read(200,rec=i)errrmse(:,:,k)
end do
close(200)

do j=1,ny
  do i=1,nx
    call pre_limit(nt,sdc(i,j,:),tp(i,j),satur(i,j))
  end do
end do

open(300,file='H:\modelError\errall\rmse\tp.grd',form='unformatted',access='direct',recl=nx*ny)
write(300,rec=1)tp(:,:)
close(300)

end program

!-------Subroutine process: determining the limit of predictability from the error increasing series.
subroutine pre_limit(nt,x,tlength,satur)
implicit none
integer::nt,i
real::x(nt),tlength
real::satur

do i=1,nstep
    if(x(i).le.satur.and.x(i+1).ge.satur)then
        goto 111
    else 
    tlength=nt
    end if
end do
111   tlength=((x(i+1)-satur)*i+(satur-x(i))*(i+1))&
    	 /(x(i+1)-x(i))
return
end subroutine