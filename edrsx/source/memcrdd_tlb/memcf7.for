      subroutine memcf7(fwhm,output,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	To produce a image holding a Gaussain in wrap-around format 
*       with centre at pixel (1,1). The total data sum is such that 
*       the FFT will have a real value of 1 at zero frequency.
*
*SOURCE
*       MEMCF7.FOR in MEMCRDD.TLB
*
*ARGUMENTS       
*   INPUTS:
*       fwhm    real            The FWHM of the Gaussian in arc-mins.
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       output(ME_nps,ME_nls) real Output image.
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/ME_COM/,/ZZ_COM/
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
*       D.S. Berry (MAVAD::DSB) 5/9/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE IRAS MISSION PARAMETERS
*
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...
*
      include '(B0_COM)'
      include '(ME_COM)'
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr
      real	fwhm,output(B0_nps,B0_nls)

*
* DECLARE LOCAL VARIABLES
*
      real	a2	! Normalising factor for exponential argument
      real	factor	! Factor which makes total data sum in output correct
      integer	i	! Pixel counter.
      integer	j	! Line counter.
      real	k	! Constant term used in calculating a2
      real	outval	! An output pixel value.
      real	sum	! Total data sum in output image.
      real	x	! Gaussian coordinate centred on (0,0)
      real	x2	! x squared
      integer	xlim	! Max value of x before wrap-around occurs
      real	y	! Gaussian coordinate centred on (0,0)
      real	y2	! y squared
      integer	ylim    ! Max value of y before wrap-around occurs

      parameter (k=0.360674)

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF NO FILTERING IS REQUIRED, SET OUTPUT VALUES TO ZERO AND RETURN
*
      if(fwhm.le.0.0) then
         do j=1,B0_nls
            do i=1,B0_nps
               output(i,j)=0.0
            enddo
         enddo
         goto 999
      endif

*
* CALCULATE THE CONSTANT REQUIRED IN THE EXPONENTIAL TERM
*
      a2=k*(fwhm/ZZ_psz)**2

*
* SET UP UPPER LIMITS OF BOTTOM LEFT QUADRANT OF THE OUTPUT IMAGE
*
      xlim=int(B0_nps/2.0)
      ylim=int(B0_nls/2.0)

*
* CREATE THE IMAGE. (I,J) ARE IMAGE COORDINATES (1-ME_NPS, 1-ME_NLS), 
* (X,Y) ARE COORDINATES CENTRED ON THE PEAK OF THE GAUSSIAN AT (0,0).
*
      sum=0.0

      do j=1,B0_nls

         y=j-1
         if(y.gt.ylim) y=y-B0_nls
         y2=real(y*y)
         
         do i=1,B0_nps

            x=i-1
            if(x.gt.xlim) x=x-B0_nps
            x2=real(x*x)

            outval=exp(-(x2+y2)/a2)

            output(i,j)=outval
            sum=sum+outval

         enddo

      enddo

*
* MODIFY THE OUTPUT TO HAVE A TOTAL DATA SUM EQUAL TO THE SQUARE ROOT OF
* THE NUMBER OF PIXELS. THIS ENSURES THAT THE ZERO FREQUENCY PIXEL OF 
* THE FFT HAS A VALUE OF UNITY (SEE DOCUMENTATION FOR NAG FFT ROUTINES).
*

      factor=sqrt(real(B0_nps*B0_nls))/sum

      do j=1,B0_nls
         do i=1,B0_nps
            output(i,j)=output(i,j)*factor
         enddo
      enddo

*
* FINISH
*
  999 continue

      end
