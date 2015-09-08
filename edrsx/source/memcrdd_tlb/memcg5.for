      subroutine memcg5(in,out,m,n,rinval)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates the reciprocal of the Fourier transform of a
*       symetric function (i.e. transform must be purely real).
*	The input and output are both in transposed Hermitian form.
*
*SOURCE
*       MEMCG5.FOR in MEMCRDD.TLB
*
*METHOD
*	All a bit complicated to describe in a comment! I suppose
*	I will have to write a document about it some time. One
*	point to note is that the same array can be specified as
*	an input and the output at the same time with no ill effects.
*       
*ARGUMENTS       
*   INPUT:
*	in	real	Input FFT
*	m	integer	No. of pixels per line of the original image
*	n	integer	No. of lines in the original image
*	rinval	real	Value to store if denominator is zero
*   OUTPUTS:
*	out	real	Output holding reciprocal of the input
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
*       D.S. Berry (MAVAD::DSB) 12/6/90
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	m,n
      real	in(n,m),out(n,m),rinval

*
* DECLARE LOCAL VARIABLES
*
      real	aa
      integer	aalin
      integer	aapix
      integer	ablin
      integer	abpix
      integer	balin
      integer	bapix
      real	bb
      integer	bblin
      integer	bbpix
      real	den
      integer	j
      integer   jlim
      integer	k
      integer	klim
      logical   meven
      logical	neven

*
* THIS CODE WILL HAVE TO BE COMMENTED SOME DAY
*
      if(mod(n,2).eq.0) then
         neven=.true.
         klim=n/2-1
      else
         neven=.false.
         klim=n/2
      endif

      if(mod(m,2).eq.0) then
         meven=.true.
         jlim=m/2-1
      else
         meven=.false.
         jlim=m/2
      endif


      aa=in(1,1)
      if(aa.ne.0.0) then
         out(1,1)=1.0/aa		! k=0 and j=0
      else
         out(1,1)=rinval	
      endif


      aalin=1
      balin=1
      do k=1,klim			! j=0

         aapix=k+1
         bapix=n-k+1

         aa=in(aapix,aalin)

         if(aa.ne.0.0) then
            out(aapix,aalin)=1.0/aa
         else
            out(aapix,aalin)=rinval
         endif

         out(bapix,balin)=0.0

      enddo         

      if(neven) then
         aa=in(klim+2,1)
         if(aa.ne.0) then
            out(klim+2,1)=1.0/aa
         else
            out(klim+2,1)=rinval
         endif
      endif



      aapix=1
      abpix=1
      do j=1,jlim			! k=0

         aalin=j+1
         ablin=m-j+1

         aa=in(aapix,aalin)

         if(aa.ne.0) then
            out(aapix,aalin)=1.0/aa
         else
            out(aapix,aalin)=rinval
         endif

         out(abpix,ablin)=0.0

      enddo         

      if(meven) then
         aa=in(1,jlim+2)
         if(aa.ne.0.0) then
            out(1,jlim+2)=1.0/aa
         else
            out(1,jlim+2)=rinval
         endif
      endif


      do k=1,klim				! General j and k
         aapix=k+1
         bbpix=n-k+1
         abpix=aapix
         bapix=bbpix

         do j=1,jlim

            aalin=j+1
            bblin=m-j+1
            ablin=bblin
            balin=aalin

            aa=in(aapix,aalin)
            bb=in(bbpix,bblin)

            den=bb*bb-aa*aa
            if(den.ne.0.0) then
               out(aapix,aalin)= -aa/den
               out(bbpix,bblin)= bb/den
            else
               out(aapix,aalin)= rinval
               out(bbpix,bblin)= rinval
            endif

            out(abpix,ablin)= 0.0
            out(bapix,balin)= 0.0

         enddo

         if(meven) then

            aalin=jlim+2
            balin=aalin

            aa=in(aapix,aalin)

            if(aa.ne.0.0) then
               out(aapix,aalin)= 1.0/aa
            else
               out(aapix,aalin)=rinval
            endif

            out(bapix,balin)= 0.0

         endif

      enddo

      if(neven) then
         aapix=klim+2
         abpix=aapix

         do j=1,jlim

            aalin=j+1
            bblin=m-j+1
            ablin=bblin
            balin=aalin

            aa=in(aapix,aalin)

            if(aa.ne.0.0) then
               out(aapix,aalin)= 1.0/aa
            else
               out(aapix,aalin)= rinval
            endif

            out(abpix,ablin)= 0.0

         enddo

         if(meven) then

            aalin=jlim+2

            aa=in(aapix,aalin)

            if(aa.ne.0.0) then
               out(aapix,aalin)= 1.0/aa
            else
               out(aapix,aalin)=rinval
            endif

         endif
      endif

*
* FINISH
*
      end
