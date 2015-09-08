      subroutine fftrev(work,m,n,data,ilevel,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Does a reverse FFT of a single image which is assumed to be
*	an FFT held in Hermitian form, with the zero frequency pixel at 
*	the image centre. The output is a single real image.
*
*SOURCE
*       FFTREV.FOR in FOURIER.TLB
*
*METHOD
*	Copy the input FFT to a work array to avoid the input being
*	overwritten. Swap the quadrants of the input FFT so that the 
*	zero frequency pixel is at position (1,1). Transpose the image. 
*	Take the inverse FFT of each line storing the transform back in
*	its original place. Transpose the image again and take the 
*	inverse FFT of each line again. This gives the final result.
*
*ARGUMENTS
*   INPUTS:
*       work(n,m) real	   Workspace to hold transposed image
*	m	  integer  No. of pixels per line in the input
*	n	  integer  No. of lines in the input
*       ilevel	  integer  User information level
*       data(m,n)  real	   The input Hermitian FFT image
*   OUTPUTS:
*       data(m,n)  real	   The output real image
*       ierr      integer  Error status: 0 - success
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*	      wruser,transp
*       NAG (single precision):
*	      c06fbe,c06gbe
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
      real      work(n,m),data(m,n)

*
* DECLARE LOCAL VARIABLES
*
      integer   freq    ! No. of lines betweeen progress reports
      integer	istat	! Local status value
      integer   j       ! X axis (pixel) counter
      integer   k       ! Y axis (line) counter
      integer   mhp1    ! M/2 + 1
      integer   mhp2    ! M/2 + 2
      integer   mm1h    ! (M-1)/2
      integer   nhp1    ! N/2 + 1
      integer   nhp2    ! N/2 + 1
      integer   nm1h    ! (N-1)/2
      character prbuf*80! Buffer for output to terminal screen

*
* SET UP REQUENTLY USED CONSTANTS
*
      mhp1=1+m/2
      mhp2=2+m/2
      mm1h=(m-1)/2
      nhp1=1+n/2
      nhp2=2+n/2
      nm1h=(n-1)/2
      freq=50

*
* GIVE INFORMATIONAL MESSAGE IF REQUIRED
*
      if(ilevel.ge.2) then
         call wruser(' ',istat)
         call wruser('  Doing inverse transformation',istat)
      endif

*
* SWAP QUADRANTS ROUND SO THAT ZERO FREQUENCIES ARE AT CORNERS,
* TRANSPOSING THE ARRAY IN THE PROCESS
*
      do j=1,mhp1

         do k=1,nhp1
            work(k,j)=data(mm1h+j,nm1h+k)
         enddo

         do k=nhp2,n
            work(k,j)=-data(mm1h+j,k-nhp1)
         enddo

      enddo

      do j=mhp2,m

         do k=1,nhp1
            work(k,j)=-data(j-mhp1,nm1h+k)
         enddo

         do k=nhp2,n
            work(k,j)=data(j-mhp1,k-nhp1)
         enddo

      enddo

*
* TRANSFORM EACH ROW OF THE TRANSPOSED IMAGE (THE SAME AS EACH COLUMN
* OF THE UNTRANSPOSED IMAGE). THE CONJUGATE IS TAKEN FIRST IN ORDER TO
* GET AN INVERSE TRANSFORMATION. THE RESULTS ARE PURELY REAL AND SO DONT
* NEED TO BE CONJUGATED
*
      if(ilevel.ge.2) call wruser(' ',istat)

      ierr=0
      do j=1,m

         call c06gbe(work(1,j),n,ierr)
         call c06fbe(work(1,j),n,data,ierr)
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
* TRANSPOSE THIS IMAGE BACK TO ITS ORIGINAL ORIENTATION
*
      call transp(work,n,m,data)

*
* TRANSFORM EACH ROW OF THE IMAGE.
*
      do k=1,n

         call c06gbe(data(1,k),m,ierr)
         call c06fbe(data(1,k),m,in,ierr)
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
* FINISH
*
  999 continue

      end
