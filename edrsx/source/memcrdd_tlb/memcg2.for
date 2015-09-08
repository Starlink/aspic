      subroutine memcg2(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates estimates of the variance of correlated noise in
*	the input crdd files.
*
*SOURCE
*       MEMCG2.FOR in MEMCRDD.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	
*   WRITE:
*	/_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              
*       THIS PACKAGE (MEMCRDD.TLB):
*              
*       EDRS:
*              
*       INTERIM:
*              
*STARLINK PARAMETERS
*
*
*VAX SPECIFICS
*       implicit none
*       %val
*       do while
*       REAL*8
*       Trig functions in degrees
*       enddo
*       end of line comments
*       2 byte integer values
*       RTL routines
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/4/90
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

* ... CRDD FILE INFO
      include '(B2_COM)'

* ... SDC INFO
      include '(B6_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

* ... USER PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	a,b
      real	covlim	! Lower coverage value for valid image data
      real	cover	! Coverage value
      integer	crddf	! CRDD file index
      logical	got	! Noise values read from disk files if true
      integer	ipin	! Pointer to input BDF
      integer	ipout	! Pointer to output BDF
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      real	mean	! Mean correlated variance in background areas
      integer	nbac	! No. of background samples
      integer	nlin	! No.of lines in the input BDF
      integer	nsum	! No. of values summed in SUM
      integer	offset	! Offset into an internal file
      integer	size	! Size of the input BDF
      integer	start	! Start position in common array
      real	sum	! Sum of background correlated noise variances
      real	totlo	! A pixel value from the total low res image

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* TELL USER WHAT IS GOING ON
*
      if(ZZ_ilv.ge.4) call wruser('  Calculating correlated noise...',
     :                             istat)

*
* GET RATIO OF POINTING TO FIELD NOISE TO DEFINE SOURCE REGIONS, AND 
* FACTOR TO APPLY TO FINAL CORRELATED SIGMA VALUES
*
      a=2.0
      call getpar('SRCRATIO','REAL',1,0.0,1.0E10,.true.,ival,a,istat)

      b=1.0
      call getpar('CORFACT','REAL',1,0.0,1.0E10,.true.,ival,b,istat)

*
* SET FILE 23 TO 1.0 AND FILE 24 TO THE MEAN SURFACE BRIGHTNESS DATA
* FOR EACH SAMPLE, EXCEPT THAT SOURCE SAMPLES ARE SET TO ZERO IN BOTH
* FILES TO EXCLUDE THEM FROM THE CORRELATED NOISE CALCULATION
*
      nbac=0
      do offset=0,ME_mk-1
         if(ME_st(ME_kb(25)+offset).le.a*ME_st(ME_kb(22)+offset)) then
            ME_st(ME_kb(23)+offset)=1.0
            ME_st(ME_kb(24)+offset)=ME_st(ME_kb(21)+offset)
     :                              /ME_st(B6_sol+offset)
            nbac=nbac+1
         else
            ME_st(ME_kb(23)+offset)=0.0
            ME_st(ME_kb(24)+offset)=0.0
         endif
      enddo

*
* IF NOT MUCH OF THE DATA IS BACKGROUND, THE CORRELATED NOISE WILL
* NOT BE WELL DEFINED. IF LESS THAN 30 % IF BACKGROUND SET ALL 
* CORRELATED NOISE VALUES TO ZERO.
*
      write(*,*) ME_mk-nbac,' source samples rejected from correlated'//
     :                      ' noise calculation'

      if(nbac.lt.0.3*ME_mk) then

         call wrerr('G2ERR2')

         do offset=0,ME_mk-1
            ME_st(ME_kb(28)+offset)=0.0
         enddo

         goto 999

      endif

*
* FORM TOTAL COVERAGE IMAGE IN FILE 2
*
      call tropus(23,2)

*
* FORM TOTAL LOW RES IMAGE IN FILE 2
*
      call tropus(24,20)

      covlim=ZZ_psz*ZZ_psz*0.84616E-7

      do offset=0,ME_mj-1

         cover=ME_st(ME_kb(2)+offset)

         if(cover.gt.covlim) then
            ME_st(ME_kb(2)+offset)=ME_st(ME_kb(20)+offset)/cover
         else
            ME_st(ME_kb(2)+offset)=PR_rin
         endif

      enddo

*
* LOOP ROUND EACH INPUT CRDD FILE
*
      do crddf=1,B2_ncf

*
* FORM AN IMAGE IN FILE 1 CONTAINING THE SQUARED RESIDUALS BETWEEN THE
* LOW RES IMAGE OF JUST THIS ONE CRDD FILE, AND THE TOTAL LOW RES IMAGE.
*
         call memcg0(23,1,crddf)
         call memcg0(24,20,crddf)

         do offset=0,ME_mj-1

            cover=ME_st(ME_kb(1)+offset)
            if(cover.gt.covlim) then

               totlo=ME_st(ME_kb(2)+offset)
               if(totlo.ne.PR_rin) then

                  ME_st(ME_kb(1)+offset)=(totlo-
     :                                 ME_st(ME_kb(20)+offset)/cover)**2

               else

                  ME_st(ME_kb(1)+offset)=0.0

               endif

            else

               ME_st(ME_kb(1)+offset)=0.0

            endif
   
         enddo

*
* CREATE A DATA SET (IN FILE 28) BY SAMPLING THE SQUARED RESIDUALS
* USING OPUS
*
         call memcg1(1,28,crddf)

      enddo

*
* LOOP ROUND ALL SAMPLES, MULTIPLYING FILE 28 BY THE SOLID ANGLES TO 
* GET VALUES IN JY**2, AND SCALING THE FINAL VARIANCE VALUES BY THE 
* SQUARE OF THE REQUESTED SIGMA FACTOR.
*
      sum=0.0
      nsum=0
      b=b*b

      do offset=0,ME_mk-1
         ME_st(ME_kb(28)+offset)=ME_st(ME_kb(28)+offset)
     :                           *ME_st(B6_sol+offset)*b

*
* SET INVALID ALL SOURCE SAMPLES (POINTING VARIANCE GREATER THAN TWICE 
* THE FIELD VARIANCE)
*
         if(ME_st(ME_kb(25)+offset).gt.a*ME_st(ME_kb(22)+offset)) then
            ME_st(ME_kb(28)+offset)=PR_rin

*
* FIND THE MEAN VARIANCE OF THE BACKGROUND SAMPLES
*
         else
            sum=sum+ME_st(ME_kb(28)+offset)
            nsum=nsum+1
         endif

      enddo

*
* REPLACE ALL INVALID VARIANCES WITH THE MEAN VARIANCE VALUE, 
*
      if(nsum.gt.0) then
         mean=sum/nsum
      else
         call wrerr('G2ERR2')
         mean=0.0
      endif

      do offset=0,ME_mk-1
         if(ME_st(ME_kb(28)+offset).eq.PR_rin) then
            ME_st(ME_kb(28)+offset)=mean
         endif
      enddo

*
* FINISH
*
  999 continue

      end
