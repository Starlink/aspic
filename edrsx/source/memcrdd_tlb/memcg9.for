      subroutine memcg9(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Expand image.
*	
*
*SOURCE
*       MEMCG9.FOR in MEMCRDD.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*
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
*       D.S. Berry (MAVAD::DSB) /10/89
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

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real*8	b
      real*8	fun3
      integer	istat	! Temporary status value
      real	maxval
      real	minval
      real	newval
      integer	nbad
      integer	ngood
      real*8	g8_a	! 
      integer	offset
      real*8	pixval
      real	sum1

      COMMON /g8_COM/ g8_a

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999


      if(G8_a.eq.PR_rin) goto 999

               
      maxval=-1.0E32
      do offset=0,ME_mj
         maxval=max(maxval,abs(ME_st(ME_kb(1)+offset)))
      enddo
      write(*,*) ' Values before expansion:'
      write(*,*) '   Max pixel value : ',maxval

      sum1=0.0
      maxval=-1.0E32
      minval=1.0E32
      ngood=0
      nbad=0

      b=0.84616D-7*G8_a*dble(ZZ_psz)**2

      do offset=0,ME_mj

         pixval=ME_st(ME_kb(1)+offset)
         newval=fun3(pixval*b)/b
         ME_st(ME_kb(1)+offset)=newval

         if(newval.ne.PR_rin) then
            maxval=max(maxval,newval)
            minval=min(minval,newval)
            sum1=sum1+newval
            ngood=ngood+1
         else
            nbad=nbad+1
         endif

      enddo

      write(*,*) ' Values after compression:'
      write(*,*) '   (',nbad,' bad pixels generated)'
      write(*,*) '   Max pixel value : ',maxval
      write(*,*) '   Min pixel value : ',minval
      write(*,*) '   Mean pixel value: ',sum1/ngood

*
* FINISH
*
  999 continue

      end


      function fun3(arg)
      include '(PR_DEC)'
      real*8 fun1,arg,tmp1,tmp2
      if(1.0D0-arg.gt.0.0) then
         fun3=0.5D0*log( (1.0D0+arg)/(1.0D0-arg) )
      else
         fun3=dble(PR_rin)
      endif

      end
 
