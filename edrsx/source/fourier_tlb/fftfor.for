      subroutine fftfor(data,m,n,wim1,wim1t,wim2,untang,ilevel,
     :                  ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Does a forward FFT of a real image. The output FFT is stored
*	with the zero frequency pixel at the centre of the image, in
*	either Hermitian form, or real and imaginary form depending 
*	on the value of argument "untang".
*
*SOURCE
*       FFTFOR.FOR in FOURIER.TLB
*
*METHOD
*	This subroutine is based on the single precision NAG 1D 
*	Hermitian FFT routine, C06FAE. The rows of the input image are 
*	transformed, and stored back in the input image. The image is 
*	then transposed, and each row transformed again. The image is 
*	finally transposed again, and at the same time the quadrants
*	are swapped around to put the zero frequency pixel at the
*	centre of the output image.
*
*	If the user wants seperate real and imaginary parts to the FFT,
*	then the hermitian transform is untangled.
*
*ARGUMENTS
*   INPUTS:
*	data(m,n)  real	    Input (purely real) image
*	m	   integer  No. of pixels per line in input image
*	n	   integer  No. of lines in input image
*	wim1(m,n)  integer  Work space
*	wim1t(n,m) integer  Work space to hold transposed images.
*       wim2(m,n)  integer  work space
*       untang	   logical  If .true. then the output Hermitian image is
*			    untangled into seperate real and imaginary
*			    parts.
*       ilevel	   integer  User information level.
*   OUTPUTS:
*       data(m,n)  real	    Output Hermitian FFT
*	wim1(m,n)  integer  Imaginary part of FFT
*       wim2(m,n)  integer  Real part of FFT
*       ierr       integer  Error status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	      wruser,transp
*       NAG (single precision):
*	      c06fae
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
      integer   m,n,ierr,ilevel
      real      data(m,n),wim1t(n,m),wim1(m,n),wim2(m,n)
      logical   untang

*
* DECLARE LOCAL VARIABLES
*
      real      aa      ! Real part of FFT of (real part of FFT of DATA)
      real      ab      ! Real part of FFT of (imag. part of FFT of DATA)
      real      ba      ! Imag. part of FFT of (real part of FFT of DATA)
      real      bb      ! Imag. part of FFT of (imag. part of FFT of DATA)
      integer   fm      ! 2*mp1h
      integer   fn      ! 2*np1h
      integer   freq    ! No. of lines betweeen progress reports
      integer	istat	! Local error status
      integer   j       ! X axis (pixel) counter
      integer   jlim    ! Upper limit of j in untangling section
      integer   k       ! Y axis (line) counter
      integer   klim    ! Upper limit of k in untangling section
      logical   meven   ! True if M is even
      integer   mhp1    ! M/2 + 1
      integer   mhp2    ! M/2 + 2
      integer   mm1     ! M - 1
      integer   mm1h    ! (M-1)/2
      integer   mp1h    ! (M+1)/2
      integer   mp3h    ! (M+3)/2
      logical   neven   ! True if N is even
      integer   nhp1    ! N/2 + 1
      integer   nhp2    ! N/2 + 2
      integer   nm1     ! N - 1
      integer   nm1h    ! (N-1)/2
      integer   np1h    ! (N+1)/2
      integer   np3h    ! (N+3)/2
      character prbuf*80! Buffer for output to terminal screen

*
* SET UP FREQUENTLY USED CONSTANTS
*
      freq=50
      mm1=m-1
      nm1=n-1
      mm1h=(m-1)/2
      mp1h=(m+1)/2
      mp3h=(m+3)/2
      nm1h=(n-1)/2
      np1h=(n+1)/2
      np3h=(n+3)/2
      mhp1=m/2+1
      mhp2=mhp1+1
      nhp1=n/2+1
      nhp2=nhp1+1
      fn=2*np1h
      fm=2*mp1h

      if(mod(n,2).eq.0) then
         neven=.true.
         klim=nm1
      else
         neven=.false.
         klim=n
      endif

      if(mod(m,2).eq.0) then
         meven=.true.
         jlim=mm1
      else
         meven=.false.
         jlim=m
      endif

*
* GIVE INFORMATIONAL MESSAGE IF REQUIRED
*
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         call wruser('  Doing forward transformation',istat)
         call wruser(' ',istat)
      endif

*
* TRANSFORM EACH ROW OF THE INPUT IMAGE. THE REAL AND IMAGINARY RESULTS
* ARE STORED BACK IN THE INPUT IMAGE ROW IN HERMITIAN FORM (SEE NAG
* MANUAL). ARRAY WIM1 IS HERE USED AS TEMPORARY WORK SPACE.
*
      ierr=0
      do k=1,n

         call c06fae(data(1,k),m,wim1,ierr)
         if(ierr.ne.0) goto 999

         if(ilevel.ge.2) then
            if(mod(k,freq).eq.0.or.k.eq.n) then
               write(prbuf,10) k
  10           format('   ',I5,' rows transformed')
               call wruser(prbuf,istat)
            endif
         endif

      enddo

*
* TRANSPOSE THE ARRAY TO THE NxM ARRAY STORED IN WIM1T. 
*
      call transp(data,m,n,wim1t)

*
* TRANSFORM EACH ROW OF THE TRANSPOSED IMAGE (THE SAME AS EACH COLUMN
* OF THE UNTRANSPOSED IMAGE), AGAIN STORING THE OUTPUT IN HERMITIAN 
* FORM. ARRAY "DATA" IS USED AS WORKSPACE.
*
      do j=1,m

         call c06fae(wim1t(1,j),n,data,ierr)
         if(ierr.ne.0) goto 999

         if(ilevel.ge.2) then
            if(mod(j,freq).eq.0.or.j.eq.m) then
               write(prbuf,20) j
  20           format('   ',I5,' columns transformed')
               call wruser(prbuf,istat)
            endif
         endif

      enddo

*
* SWAP QUADRANTS ROUND SO THAT ZERO FREQUENCIES ARE AT CENTRE
* TRANSPOSING IMAGE BACK TO ORIGINAL ORIENTATION IN THE PROCESS.
* ARRAY "DATA" WILL THEN HOLD THE FINAL HERMITIAN OUTPUT
*
      do j=1,mhp1

         do k=1,nhp1
            data(mm1h+j,nm1h+k)=wim1t(k,j)
         enddo

         do k=nhp2,n
            data(mm1h+j,k-nhp1)=-wim1t(k,j)
         enddo

      enddo

      do j=mhp2,m

         do k=1,nhp1
            data(j-mhp1,nm1h+k)=-wim1t(k,j)
         enddo

         do k=nhp2,n
            data(j-mhp1,k-nhp1)=wim1t(k,j)
         enddo

      enddo

*
* THE NEXT SECTION WILL UNTANGLE THE HERMITIAN FORM INTO STRAIGHT-
* FORWARD REAL AND IMAGINARY IMAGES. IF THE USER DOESN'T REQUIRE THESE,
* THEN SKIP THE NEXT SECTION.
*
      if(untang) then

*
* THE REAL AND IMAGINARY IMAGES ARE FORMED BY LINEAR COMBINATIONS OF 4
* IMAGES AA,AB,BA AND BB. THESE ARE THE REAL AND IMAGINARY PARTS OF THE
* FOURIER TRANSFORM OF EACH COLUMN OF THE REAL AND IMAGINARY PARTS OF THE
* FOURIER TRANSFORM OF EACH LINE. THESE IMAGES HAVE SYMETRY SO THAT ONLY
* ONE QUADRANT OF EACH NEED BE RETAINED, AND THE 2D HERMITIAN TRANSFORM
* OBTAINED ABOVE CONSISTS OF ONE QUADRANT FROM EACH IMAGE PACKED
* TOGETHER. THE UNPACKING IS DONE BY WORKING THROUGH THE TOP RIGHT
* QUADRANT CO-ORDINATES AND REFLECTING ABOUT BOTH AXES TO GET THE OTHER
* QUADRANTS. ARRAY WIM2 WILL HOLD THE REAL IMAGE AND ARRAY WIM1 WILL HOLD
* THE IMAGINARY IMAGE. 
*
* FIRST DO LOWEST LINE IN TOP RIGHT QUADRANT (THIS IS AN AXIS OF SYMETRY)
*
         wim1(mp1h,np1h)=0
         wim2(mp1h,np1h)=data(mp1h,np1h)

         do j=mp3h,jlim
            wim2(j,np1h)=data(j,np1h)
            wim2(fm-j,np1h)=data(j,np1h)
            wim1(j,np1h)=-data(fm-j,np1h)
            wim1(fm-j,np1h)=data(fm-j,np1h)
         enddo

         if(meven) then
            wim1(m,np1h)=0
            wim2(m,np1h)=data(m,np1h)
         endif

*
* NOW DO ALL THE HIGHER LINES IN TURN. IF N IS EVEN THEN THERE WILL BE ONE
* UNMATCHED LINE LEFT AT THE END WHICH WILL BE DEALT WITH SEPERATELY.
*
         do k=np3h,klim

            wim2(mp1h,k)=data(mp1h,k)
            wim2(mp1h,fn-k)=data(mp1h,k)
            wim1(mp1h,k)=-data(mp1h,fn-k)
            wim1(mp1h,fn-k)=data(mp1h,fn-k)

            do j=mp3h,jlim
               aa=data(j,k)
               bb=data(fm-j,fn-k)
               ab=-data(fm-j,k)
               ba=-data(j,fn-k)
               wim2(j,k)=aa-bb
               wim2(fm-j,k)=aa+bb
               wim2(j,fn-k)=aa+bb
               wim2(fm-j,fn-k)=aa-bb
               wim1(j,k)=ab+ba
               wim1(fm-j,k)=-ab+ba
               wim1(j,fn-k)=ab-ba
               wim1(fm-j,fn-k)=-ab-ba
            enddo

            if(meven) then
               wim2(m,k)=data(m,k)
               wim2(m,fn-k)=data(m,k)
               wim1(m,k)=-data(m,fn-k)
               wim1(m,fn-k)=data(m,fn-k)
            endif

         enddo

*
* NOW DEAL WITH ANY UNMATCHED LINE
*
         if(neven) then

            wim2(mp1h,n)=data(mp1h,n)
            wim1(mp1h,n)=0

            do j=mp3h,jlim
               wim2(j,n)=data(j,n)
               wim2(fm-j,n)=data(j,n)
               wim1(j,n)=-data(fm-j,n)
               wim1(fm-j,n)=data(fm-j,n)
            enddo

            if(meven) then
               wim2(m,n)=data(m,n)
               wim1(m,n)=0
            endif

         endif

*
* END OF UNTANGLING SECTION
*
      endif

*
* FINISH
*
  999 continue

      end
