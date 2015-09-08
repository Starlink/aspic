C
C   SEARCHPIX IN [CRB.ASP]
C
C   15 DECEMBER 1986
c
C   Searches a .BDF frame for pixel-values within a 
C   given range
C
	subroutine searchpix
	integer idim(2)
	include 'interim(FMTPAR)'
	include 'interim(ERRPAR)'
	print *,' '
	print *,'Welcome to SEARCHPIX  15 December 1986'
	print *,'--------------------------------------'
	print *,' '
	print *,'The program prints out the coordinates of pixels'
	print *,'having values between user-supplied PIXLO and PIXHI'
	print *,'The program will prompt you for:'
	print *,'   IMAGE  Name of .BDF file (e.g. FLATV)'
	print *,'   PIXLO  low value (e.g. 0)'
	print *,'   PIXHI  high value (e.g. 1000)'
	print *,' '
	call rdimag('IMAGE',fmt_r,2,idim,ndim,ipoint,istat)
	call find_pixel(%val(ipoint),idim(1),idim(2))
	end
C
	subroutine find_pixel(array,nx,ny)
	real array(nx,ny)
	character*1 ans
	logical bit
	bit=.true.
	do while (bit)
	call rdkeyr('PIXLO',.false.,1,vallo,iactels,istat)
	call rdkeyr('PIXHI',.false.,1,valhi,iactels,istat)
	do j=1,ny
		do i=1,nx
		if(array(i,j).le.valhi.and.array(i,j).ge.vallo) then
		write(*,101) i,j,array(i,j)
101		format(' Pixel at ',i3,i4,' has value =',f15.5)
		endif
	enddo
	enddo
	call cnpar('PIXLO',istat)
	call cnpar('PIXHI',istat)
	print *,'Search again?'
	read(*,102) ans
102	format(a1)
	if(ans.ne.'y'.and.ans.ne.'Y') bit=.false.
	enddo
	end
