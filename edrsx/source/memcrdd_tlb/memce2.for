      subroutine memce2(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Creates a new default model.
*
*SOURCE
*       MEMCE2.FOR in MEMCRDD.TLB
*
*METHOD
*	The new model is a linear combination of the old model and
*	the current reconstruction. The proportions of each of these
*	two images in the new model varies from pixel to pixel.
*	Pixels which have a high entropy in the current reconstruction
*	are determined by the current reconstruction, where as pixels
*	with a low entropy are determined by the old model.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gt2diw
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,memcc6,memcc7,memcd0
*              
*STARLINK PARAMETERS
*	MODELOUT	The output image to hold the new model
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/2/90
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

* ... FITS DESCRIPTORS FOR OUTPUT IMAGE
      include '(B0_COM)'

* ... POINTERS TO INTERNAL FILES
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
      real	b	! Background pixel value
      real	f	! Reconstruction pixel value
      real	gain	! A speed at which the model is allowed to change
      integer	istat	! Local status value
      integer	ival	! Dummy integer argument
      real	m	! Model pixel value
      integer	nused	! No. of good entropy values found.
      integer	offset	! Offset into an image
      real	s	! Entropy pixel value
      real	smax	! Maximum entropy per pixel
      real	smean	! Mean entropy per pixel
      real	stot	! Total entropy

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* SEE IF A NEW MODEL IS REQUIRED. IF NOT, QUIT.
*
      call gt2diw('MODELOUT',102,.true.,B0_nps,B0_nls,ival,istat)
      if(istat.ne.0) goto 999

*
* IF REQUIRED TELL THE USER WHATS HAPPENING
*
      if(ZZ_ilv.ge.3) call wruser('  Creating new model...',istat)

*
* CREATE THE (MINUS) ENTROPY IMAGE IN FILE 2
*
      smax=0.0
      stot=0.0
      nused=0

      do offset=0,ME_mj-1

         f=ME_st(ME_kb(1)+offset)
         m=ME_st(ME_kb(20)+offset)

         if(f.gt.0.and.m.gt.0) then
            s=-(f-m-f*log(f/m))
            nused=nused+1
         else
            s=0.0
         endif

         ME_st(ME_kb(2)+offset)=s
         smax=max(smax,s)
         stot=stot+s

      enddo

*
* IF THE TOTAL ENTROPY IS ZERO, THEN THE MODEL IS LEFT AS IT IS
*
      if(stot.gt.0.0) then


*
* CALCULATE A FACTOR WHICH CAUSES THE MODEL TO CHANGE MORE SLOWLY IF
* THE ENTROPY OF ALL PIXELS ARE SIMILAR
* 
         smean=stot/real(nused)
         gain=max(1.0/real(nused),1.0-smean/smax)

*
* FORM THE NEW MODEL, ADDING ON THE BACKGROUND
*
         do offset=0,ME_mj-1
   
            f=ME_st(ME_kb(1)+offset)
            m=ME_st(ME_kb(20)+offset)
            s=ME_st(ME_kb(2)+offset)
            b=ME_st(B6_bac+offset)
            ME_st(ME_kb(20)+offset)=m+gain*(f-m)*s/smax+b
   
         enddo

      endif

*
* OUTPUT THE NEW MODEL TO DISK
*
      call memca2('MODELOUT',20,.true.,istat)

*
* FINISH
*
  999 continue

      end
