      subroutine powpha(a,b,m,n)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Converts data held in complex form (seperate real and imaginary
*       parts) to power and phase form.
*
*SOURCE
*       POWPHA.FOR in FOURIER.TLB
*
*METHOD
*       The input complex data is overwritten by the power and phase
*       data. "Power" is here used to refer to the modulus of the
*	complex input value, and phase is arctan(imaginary/real) in
*	radians.
*
*ARGUMENTS
*   INPUTS:
*	a(m,n)	real	Real part of the FFT
*	b(m,n)	real	Imaginary part of the FFT
*	m	integer	No. of pixel per line of the FFT
*	n	integer	No. of lines in the FFT
*   OUTPUTS:
*	a(m,n)	real	"Power" = modulus of complex value
*	b(m,n)	real	"Phase" = arctan(imag/real) in radians
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
      real      im      ! Imaginary value
      integer   j       ! Pixel count
      integer   k       ! Line count
      real      phase   ! Phase value
      real      power   ! Power value
      real      rl      ! Real value

*
* JUST DO IT
*
      do k=1,n
         do j=1,m

            rl=a(j,k)
            im=b(j,k)

            power=sqrt(rl*rl+im*im)

            if(im.ne.0.or.rl.ne.0) then
               phase=atan2(im,rl)
            else
               phase=0
            endif

            a(j,k)=power
            b(j,k)=phase

         enddo
      enddo

*
* FINISH
*
      end
