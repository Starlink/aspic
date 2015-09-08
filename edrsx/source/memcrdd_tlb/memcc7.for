      subroutine memcc7(inv,in,out,work,npix,nlin,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Takes inverse or forward Fourier transform of an Hermitian
*	input.
*
*SOURCE
*       MEMCC7.FOR in MEMCRDD.TLB
*
*METHOD
*	This version uses the NAG Hermitian FFT routine C06FBE.
*       
*ARGUMENTS
*	inv		logical    TRUE if inverse required
*       in(nlin,npix)	real	   Input image (Transposed Hermitian)
*	work(nlin,npix) real	   Work space
*	nlin		integer	   No. of lines in the input image
*	npix		integer	   No. of pixel per line
*	ierr		integer	   Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	out(npix,nlin)	real	   Output 
*       ierr    	integer    Exit status: 0 - success
*
*SUBROUTINES CALLED
*       NAG:
*              c06fae,c06gbe
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npix,nlin,ierr
      real	in(nlin,npix),out(npix,nlin),work(nlin,npix)
      logical	inv

*
* DECLARE LOCAL VARIABLES
*
      integer	lin
      integer	pix

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* COPY THE INPUT TO THE WORKING SPACE TO AVOID THE INPUT BEING 
* OVERWRITTEN
*
      do pix=1,npix
         do lin=1,nlin
            work(lin,pix)=in(lin,pix)
         enddo
      enddo

*
* TRANSFORM EACH COLUMN OF THE TRANSPOSED FFT. CONJUGATE IS TAKEN FIRST
* TO GET THE INVERSE TRANSFORMATION IF REQUIRED. NB, CONJUGATION IS 
* NOT REQUIRED ON OUTPUT SINCE OUTPUT IS PURELY REAL. ARRAY "OUT" IS
* USED AS WORKING SPACE
*
      ierr=0
      do pix=1,npix
         if(inv) call c06gbe(work(1,pix),nlin,ierr)
         call c06fbe(work(1,pix),nlin,out,ierr)
         if(ierr.ne.0) goto 999
      enddo

*
* TRANSPOSE THE ARRAY "WORK" AND STORE IN THE ARRAY "OUT"
*
      do lin=1,nlin
         do pix=1,npix
            out(pix,lin)=work(lin,pix)
         enddo
      enddo

*
* TRANSFORM EACH ROW OF THE OUTPUT ARRAY. 
*
      do lin=1,nlin
         if(inv) call c06gbe(out(1,lin),npix,ierr)
         call c06fbe(out(1,lin),npix,work,ierr)
         if(ierr.ne.0) goto 999
      enddo

*
* FINISH
*
  999 continue

      end
