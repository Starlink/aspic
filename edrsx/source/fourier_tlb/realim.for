      subroutine realim(a,b,m,n)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts data held in modulus and phase form to complex form
*       (seperate real and imaginary parts).
*
*SOURCE
*       REALIM.FOR in FOURIER.TLB
*
*       The input complex data is overwritten by the power and phase
*       data. "Power" is here used to refer to the modulus of the
*	complex value, and phase is arctan(imaginary/real) in
*	radians.
*
*ARGUMENTS
*   INPUTS:
*	a(m,n)	real	"Power" = modulus of complex value
*	b(m,n)	real	"Phase" = arctan(imag/real) in radians
*	m	integer	No. of pixel per line of the FFT
*	n	integer	No. of lines in the FFT
*   OUTPUTS:
*	a(m,n)	real	Real part of the FFT
*	b(m,n)	real	Imaginary part of the FFT
*
*SUBROUTINES CALLED
*       none
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/6/88
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer   m,n
      real      a(m,n),b(m,n)

*
* DECLARE LOCAL VARIABLES
*
      real      im      ! Imaginary part of complex vector
      integer   j       ! Pixel count
      integer   k       ! Line count
      real      modul   ! Modulus of complex vector
      real      phase   ! Phase value complex vector
      real      rl      ! Real part of complex vector

*
* JUST DO IT
*
      do k=1,n
         do j=1,m

            modul=a(j,k)
            phase=b(j,k)

            rl=modul*cos(phase)
            im=modul*sin(phase)

            a(j,k)=rl
            b(j,k)=im

         enddo
      enddo

*
* FINISH
*
      end
