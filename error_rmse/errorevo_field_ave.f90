program main
implicit none
integer,parameter::nx=72,ny=37,nt=17340,inter=45
integer::i,j,t
real::h(nx,ny,inter),hall(nx,ny,inter)

	open(200,file='H:\variance_fortran\errorfield_abs.grd',form='unformatted',access='direct',recl=nx*ny*inter)
		!open(200,file='I:\fb\ncepout\ncepout.grd',form='unformatted',access='direct',recl=nx*ny)

	do t=1,nt
		write(*,*)t
		read(200,rec=t)h
		hall=hall+h*h/real(nt)
		!write(*,*)hall(10,10,:)
		!pause
	end do
	close(200)

	open(100,file='H:\variance_fortran\errorrmse_field_ave.grd',form='unformatted',access='direct',recl=nx*ny*inter)
	write(100,rec=1)sqrt(hall)
	close(100)

	write(*,*)hall(25,25,:)

	end
