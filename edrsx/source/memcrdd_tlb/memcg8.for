      subroutine memcg8(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Compress data and accuracies.
*	
*
*SOURCE
*       MEMCG8.FOR in MEMCRDD.TLB
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

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real*8	acc
      real*8	cmp
      real*8	data
      real*8	fun1
      real*8	fun2
      integer	istat	! Temporary status value
      integer	ival
      real	maxcmp
      real	maxdat
      real	mindat
      real	maxsig
      real	minsig
      real	newacc
      real	newdat
      real	newsig
      real*8	G8_a	! 
      integer	offset
      real	sum1
      real	sum2

      COMMON /G8_COM/ G8_a

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

      maxcmp=1.0
      call getpar('MAXCOMP','REAL',1,0.012,1.0,.true.,ival,maxcmp,istat)

      if(maxcmp.ge.1.0) then
         G8_a=PR_rin
         goto 999
      endif

      G8_a=sqrt(2.0/maxcmp-2.0)
      do while(abs(cmp-maxcmp).gt.0.001)
         G8_a=fun1(G8_a)/maxcmp
      enddo
               
      maxdat=-1.0E32
      do offset=0,ME_mk
         maxdat=max(maxdat,abs(ME_st(ME_kb(21)+offset)))
      enddo
      write(*,*) ' Values before compression:'
      write(*,*) '   Max data : ',maxdat

      G8_a=G8_a/maxdat

      sum1=0.0
      maxdat=-1.0E32
      mindat=1.0E32
      sum2=0.0
      maxsig=-1.0E32
      minsig=1.0E32

      do offset=0,ME_mk

         data=ME_st(ME_kb(21)+offset)
         acc=ME_st(ME_kb(22)+offset)
         newdat=fun1(data*G8_a)/G8_a
         newacc=acc*fun2(data*G8_a)
         ME_st(ME_kb(21)+offset)=newdat
         ME_st(ME_kb(22)+offset)=newacc

         maxdat=max(maxdat,newdat)
         mindat=min(mindat,newdat)
         sum1=sum1+newdat

         newsig=1.0/newacc
         maxsig=max(maxsig,newsig)
         minsig=min(minsig,newsig)
         sum2=sum2+newsig

      enddo

      write(*,*) ' Values after compression:'
      write(*,*) '   Max data : ',maxdat
      write(*,*) '   Min data : ',mindat
      write(*,*) '   Mean data: ',sum1/ME_mk

      write(*,*) '   Max sigma : ',maxdat
      write(*,*) '   Min sigma : ',mindat
      write(*,*) '   Mean sigma: ',sum1/ME_mk

*
* FINISH
*
  999 continue

      end


      function fun1(arg)
      real*8 fun1,arg,tmp1,tmp2
      tmp1=exp(arg)
      tmp2=1.0d0/tmp1
      fun1=(tmp1-tmp2)/(tmp1+tmp2)
      end
 
      function fun2(arg)
      real*8 fun2,arg,tmp1,tmp2
      tmp1=exp(arg)
      tmp2=1.0d0/tmp1
      fun2=0.25D0*(tmp1+tmp2)**2
      end
 
