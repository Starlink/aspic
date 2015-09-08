      subroutine comprs(in,out,size,rinval,maxcmp,maxdat,a)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE
*	COMPRS.FOR in UTILITIES.TLB
*--------------------------------------------------------
      implicit none

      integer	size
      real	in(size),out(size),rinval,maxcmp,maxdat,a

*
* DECLARE LOCAL VARIABLES
*
      real	data
      integer	i

*
* IF A HAS ALREADY BEEN CALCULATED, DO NOT RECALCULATE IT.
*
      if(a.eq.rinval) then

*
* IF MAXIMUM COMPRESSION IS OUTSIDE THE LIMITS, PERFORM NO COMPRESSION.
*
         if(maxcmp.le.0.0.or.maxcmp.ge.1.0) then
            a=rinval
            goto 999
         endif

*
* FIND VALUE OF "a" WHICH GIVES THE REQUESTED MAXIMUM COMPRESSION FOR 
* THE MAXIMUM DATA VALUE
*
         a=abs(maxdat)*(maxcmp**2)
               
      endif

*
* COMPRESS THE DATA 
*
      do i=1,size
         data=in(i)
         if(data.ne.rinval) then
            out(i)=sqrt(a*abs(data))*sign(1.0,data)
         else
            out(i)=rinval
         endif
      enddo

*
* FINISH
*
  999 continue

      end






      subroutine expand(in,out,size,rinval,a)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*SOURCE
*	EXPAND.FOR in UTILITIES.TLB
*--------------------------------------------------------
      implicit none

      integer	size
      real	in(size),out(size),rinval,a

*
* DECLARE LOCAL VARIABLES
*
      real	data
      integer	i

*
* IF NO COMPRESSION WAS PERFORMED, DONT DO ANY EXPANSION.
*
      if(a.eq.rinval) goto 999

*
* EXPAND THE DATA 
*
      do i=1,size
         data=in(i)
         if(data.ne.rinval) then
            out(i)=sign(1.0,data)*data*data/a
         else
            out(i)=rinval
         endif
      enddo

*
* FINISH
*
  999 continue

      end
