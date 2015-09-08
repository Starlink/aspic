      subroutine memcd0(in1,in2,out,m,n)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Multiplies together two Fourier transforms stored in
*	transposed Hermitian form (see NAG manual chapter C06).
*	The product is also in transposed Hermitian form.
*
*SOURCE
*       MEMCD0.FOR in MEMCRDD.TLB
*
*METHOD
*	All a bit complicated to describe in a comment! I suppose
*	I will have to write a document about it some time. One
*	point to note is that the same array can be specified as
*	an input and the output at the same time with no ill effects.
*       
*ARGUMENTS       
*   INPUT:
*	in1	real	First input FFT
*	in2	real	Second input FFT
*	m	integer	No. of pixels per line of the original image
*	n	integer	No. of lines in the original image
*   OUTPUTS:
*	out	real	Output holding producr of two inputs
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
*       D.S. Berry (MAVAD::DSB) 6/10/89
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	m,n
      real	in1(n,m),in2(n,m),out(n,m)

*
* DECLARE LOCAL VARIABLES
*
      real	aa1
      real	aa2
      integer	aalin
      integer	aapix
      real	ab1
      real	ab2
      integer	ablin
      integer	abpix
      real	ba1
      real	ba2
      integer	balin
      integer	bapix
      real	bb1
      real	bb2
      integer	bblin
      integer	bbpix
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


      out(1,1)=in1(1,1)*in2(1,1)	! k=0 and j=0


      aalin=1
      balin=1
      do k=1,klim			! j=0

         aapix=k+1
         bapix=n-k+1

         aa1=in1(aapix,aalin)
         aa2=in2(aapix,aalin)
         ba1=in1(bapix,balin)
         ba2=in2(bapix,balin)

         out(aapix,aalin)=aa1*aa2-ba1*ba2
         out(bapix,balin)=ba1*aa2+aa1*ba2

      enddo         
      if(neven) out(klim+2,1)=in1(klim+2,1)*in2(klim+2,1)



      aapix=1
      abpix=1
      do j=1,jlim			! k=0

         aalin=j+1
         ablin=m-j+1

         aa1=in1(aapix,aalin)
         aa2=in2(aapix,aalin)
         ab1=in1(abpix,ablin)
         ab2=in2(abpix,ablin)

         out(aapix,aalin)=aa1*aa2-ab1*ab2
         out(abpix,ablin)=ab1*aa2+aa1*ab2

      enddo         
      if(meven) out(1,jlim+2)=in1(1,jlim+2)*in2(1,jlim+2)


      do k=1,klim
         aapix=k+1
         bbpix=n-k+1
         abpix=aapix
         bapix=bbpix

         do j=1,jlim

            aalin=j+1
            bblin=m-j+1
            ablin=bblin
            balin=aalin

            aa1=in1(aapix,aalin)
            aa2=in2(aapix,aalin)

            bb1=in1(bbpix,bblin)
            bb2=in2(bbpix,bblin)

            ab1=in1(abpix,ablin)
            ab2=in2(abpix,ablin)

            ba1=in1(bapix,balin)
            ba2=in2(bapix,balin)

            out(aapix,aalin)= aa1*aa2+bb1*bb2-ba1*ba2-ab1*ab2
            out(bbpix,bblin)= aa1*bb2+bb1*aa2+ba1*ab2+ab1*ba2
            out(abpix,ablin)=-ba1*bb2+ab1*aa2+aa1*ab2-bb1*ba2
            out(bapix,balin)= ba1*aa2-ab1*bb2+aa1*ba2-bb1*ab2

         enddo
         if(meven) then

            aalin=jlim+2
            balin=aalin

            aa1=in1(aapix,aalin)
            aa2=in2(aapix,aalin)

            ba1=in1(bapix,balin)
            ba2=in2(bapix,balin)

            out(aapix,aalin)= aa1*aa2-ba1*ba2
            out(bapix,balin)= ba1*aa2+aa1*ba2

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

            aa1=in1(aapix,aalin)
            aa2=in2(aapix,aalin)

            bb1=0
            bb2=0

            ab1=in1(abpix,ablin)
            ab2=in2(abpix,ablin)

            ba1=0
            ba2=0

            out(aapix,aalin)= aa1*aa2-ab1*ab2
            out(abpix,ablin)= ab1*aa2+aa1*ab2

         enddo
         if(meven) then

            aalin=jlim+2

            aa1=in1(aapix,aalin)
            aa2=in2(aapix,aalin)

            out(aapix,aalin)= aa1*aa2

         endif
      endif

*
* FINISH
*
      end
