!This program is written to calculate the error evlovement of CFSR compared with true value(0Z).
!In this program error is specified by RMSE (Root Mean Square Error).
!Time is from Jan 1, 1999 to Dec 31, 2010.
!365*4=1460;12*365=4380;12*365*4=17520.
!----------------------------------------------------------------------------------------
program main
implicit none
integer,parameter::nx1=360,ny1=181,nx_new=72,ny_new=37,nt=1460,inter=45,np=1460    !nt=17520
real::h_model(nx1,ny1),h_ini(nx1,ny1)
real::h_model_out(nx_new,ny_new,inter),h_ini_out(nx_new,ny_new,inter)
real::h_new(nx_new,ny_new,inter)
integer::i,j,k
integer::ini1,ini2
integer::flag,iob
character(len=4)::year(12)
character(20)::fname
character(2)::a="fc"
character(4)::b=".grd"

data year/'1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010'/

	open(200,file='H:\cfsrhgt500\cfsr_hgt.grd',form='unformatted',access='direct',recl=nx1*ny1)
	open(10,file='H:\modermse\errorfield_abs.grd',form='unformatted',access='direct',recl=nx_new*ny_new*inter)

flag=0
iob=1

do k=1,12
	write(*,*)k
	!pause
	fname=a//year(k)//b
	open(100,file='H:\ncep_cfs_h500\'//trim(fname)//'',form='unformatted',access='direct',recl=nx1*ny1)
	do j=1,np
		flag=flag+1
		ini1=(j-1)*inter+1
		ini2=iob
		write(*,*)ini1,ini2

		do i=0,inter-1
			read(100,rec=ini1+i)h_model(:,:)
			read(200,rec=ini2+i*4)h_ini(:,:)
			call grid(nx1,ny1,5,h_model(:,:),h_model_out(:,:,i+1))
			call grid(nx1,ny1,5,h_ini(:,:),h_ini_out(:,:,i+1))
		end do
			call errorvar(nx_new,ny_new,inter,h_model_out,h_ini_out,h_new)
			write(10,rec=flag)h_new
			
		iob=iob+1
	end do
	close(100)
end do
close(200)

end program


!=============calculate the error growth of each sample========================
!Input:
!   m,n,nt: Lengths of each dimension of h1(m,n,nt) and h2(m,n,nt);
!   h1,h2: Original 3 dimensional data array.
!Output:
!   hnew: Absoulute differential value.
!--------------------------------------------------------------------------------
subroutine errorvar(m,n,nt,h1,h2,hnew)
implicit none
integer::m,n,nt
integer::i,j,ix,iy
real::h1(m,n,nt),h2(m,n,nt)
real::hnew(m,n,nt)
real::s1(nt),s2(nt),diff(nt)
real::r,sum
iy=0
do j=1,n
	do i=1,m
	diff=0.
	s1(:)=h1(i,j,:)
	s2(:)=h2(i,j,:)
	diff=abs(s1-s2)           !*(s1-s2)
	hnew(i,j,:)=diff(:)
    end do
end do
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
