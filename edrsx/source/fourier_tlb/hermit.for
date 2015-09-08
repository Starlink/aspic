      subroutine hermit(r,i,m,n,h,ilevel)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       To produce a Hermitian FFT image from seperate images holding 
*	real and imaginary parts of the FFT.
*
*SOURCE
*       HERMIT.FOR in FOURIER.TLB
*
*METHOD
*	The "untangling" process described in routine FFTFOR is 
*	reversed. Two seperate estimates of each pixel value in the 
*	Hermitian image are produced from the two input image. These
*	estimates should always be the same if the input real and 
*	imaginary images correspond to a purely real image in image
*	space. If the estimates differ significantly then the user
*	has done something to the FFT which results in it no longer
*	being the FFT of a real image. If this is the case then
*	FOURIER can not be used to invert the FFT. The user is
*	told what the maximum deviation is between any two estimates
*	of the same pixel in the Hermitian FFT.
*
*ARGUMENTS
*   INPUTS:
*	r(m,n)	real	Input image holding real data
*	i(m,n)	real	Input image holding imaginary data
*	m	integer	No. of pixels per line in the inputs
*	m	integer	No. of lines in the inputs
*	ilevel	integer	User information level
*   OUTPUTS:
*	h(m,n)	real	Output Hermitian FFT
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	      wruser
*       EDRS:
*	      lbgone
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
      integer   m,n,ilevel
      real      r(m,n),i(m,n),h(m,n)

*
* DECLARE LOCAL VARIABLES
*
      integer   fm      ! Reflection constant for j axis
      integer   fn      ! Reflection constant for k axis
      integer   ierr    ! Error status
      integer   j       ! j axis loop count
      integer   jlim    ! Upper limit of j loop
      integer   k       ! k axis loop count
      integer   klim    ! Upper limit of k loop
      real      maxdev  ! Maximum deviation of individual estimates
      logical   meven   ! True if m is even
      integer   mp1h    ! (M+1)/2
      integer   mp3h    ! (M+3)/2
      logical   neven   ! True if n is even
      integer   np1h    ! (N+1)/2
      integer   np3h    ! (N+3)/2
      character prbuf*50! Buffer for text output
      real      v1      ! First estimate
      real      v2      ! Second estimate

*
* GET UPPER LIMITS FOR J AND K LOOPS
*
      if(mod(n,2).eq.0) then
         neven=.true.
         klim=n-1
      else
         neven=.false.
         klim=n
      endif

      if(mod(m,2).eq.0) then
         meven=.true.
         jlim=m-1
      else
         meven=.false.
         jlim=m
      endif

*
* CALCULATE OFTEN USED CONSTANTS
*
      mp1h=(m+1)/2
      mp3h=(m+3)/2
      fm=2*mp1h
      np1h=(n+1)/2
      np3h=(n+3)/2
      fn=2*np1h

*
* DO LOWEST ROW IN TOP LEFT QUADRANT
*
      maxdev=0
      h(mp1h,np1h)=r(mp1h,np1h)

      do j=mp3h,jlim

         v1=r(j,np1h)
         v2=r(fm-j,np1h)
         h(j,np1h)=0.5*(v1+v2)
         maxdev=max(maxdev,abs(v2-v1))

         v1=i(fm-j,np1h)
         v2=-i(j,np1h)
         h(fm-j,np1h)=0.5*(v1+v2)
         maxdev=max(maxdev,abs(v2-v1))

      enddo

      if(meven) h(m,np1h)=r(m,np1h)

*
* NOW DO THE REMAINING ROWS IN THE TOP LEFT QUADRANT WHICH HAVE MATCHING
* ROWS IN THE BOTTOM LEFT AND RIGHT QUADRANTS. THIS WILL LEAVE ONE LEFT
* OVER IF N IS EVEN
*
      do k=np3h,klim

         v1=r(mp1h,k)
         v2=r(mp1h,fn-k)
         h(mp1h,k)=0.5*(v1+v2)
         maxdev=max(maxdev,abs(v2-v1))

         v1=i(mp1h,fn-k)
         v2=-i(mp1h,k)
         h(mp1h,fn-k)=0.5*(v1+v2)
         maxdev=max(maxdev,abs(v2-v1))

         do j=mp3h,jlim

            v1=0.5*(r(j,k)+r(fm-j,k))
            v2=0.5*(r(j,fn-k)+r(fm-j,fn-k))
            h(j,k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

            v1=0.5*(r(j,fn-k)-r(fm-j,fn-k))
            v2=0.5*(r(fm-j,k)-r(j,k))
            h(fm-j,fn-k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

            v1=0.5*(i(fm-j,k)-i(j,k))
            v2=0.5*(i(fm-j,fn-k)-i(j,fn-k))
            h(fm-j,k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

            v1=0.5*(-i(fm-j,k)-i(j,k))
            v2=0.5*(i(fm-j,fn-k)+i(j,fn-k))
            h(j,fn-k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

         enddo

         if(meven) then

            v1=r(m,k)
            v2=r(m,fn-k)
            h(m,k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

            v1=-i(m,k)
            v2=i(m,fn-k)
            h(m,fn-k)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

         endif

      enddo

*
* IF N IS EVEN DO THE REMAINING UNMATCHED TOP ROW OF THE TOP LEFT
* QUADRANT
*
      if(neven) then

         h(mp1h,n)=r(mp1h,n)

         do j=mp3h,jlim

            v1=r(j,n)
            v2=r(fm-j,n)
            h(j,n)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

            v1=-i(j,n)
            v2=i(fm-j,n)
            h(fm-j,n)=0.5*(v1+v2)
            maxdev=max(maxdev,abs(v2-v1))

         enddo

         if(meven) h(m,n)=r(m,n)

      endif

*
* IF REQUIRED, DISPLAY MAXIMUM DEVIATION OF ESTIMATES
*
      if(ilevel.ge.3) then
         call wruser('  Maximum difference between estimates of the',
     :               ierr)
         write(prbuf,10) maxdev
 10      format('  same Fourier component is ',G13.6)
         call lbgone(prbuf(29:))
         call wruser(prbuf,ierr)
      endif

*
* FINISH
*
      end
