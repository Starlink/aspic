      subroutine memcc6(inv,in,out,work,npix,nlin,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Takes Fourier tranform of a real image. 
*
*SOURCE
*       MEMCC6.FOR in MEMCRDD.TLB
*
*METHOD
*	This version uses the NAG Hermitian FFT routine C06FAE
*       
*ARGUMENTS       
*   INPUTS:
*	inv		logical    TRUE if inverse required
*       in(npix,nlin)	real	   Input image
*	work(npix,nlin) real	   Work space
*	nlin		integer	   No. of lines in the input image
*	npix		integer	   No. of pixel per line
*	ierr		integer	   Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	out(nlin,npix)	real	   Output FFT (Transposed Hermitian)
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
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	npix,nlin,ierr
      real	in(npix,nlin),out(nlin,npix),work(npix,nlin)
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
* COPY THE INPUT IMAGE TO A TEMPORARY WORKING ARRAY TO AVOID THE INPUT
* IMAGE BEING OVERWRITTEN
*
      do lin=1,nlin
         do pix=1,npix
            work(pix,lin)=in(pix,lin)
         enddo
      enddo

*
* TRANSFORM EACH ROW OF THE WORK ARRAY. THE REAL AND IMAGINARY RESULTS
* ARE STORED BACK IN THE WORK ARRAY IN HERMITIAN FORM (SEE NAG MANUAL).
* ARRAY OUT IS USED AS WORK SPACE. IF INVERSE IS REQUIRED THEN 
* CONJUGATION IS PERFORMED ON THE OUTPUT ONLY, SINCE INPUT IS PURELY
* REAL.
*
      ierr=0
      do lin=1,nlin
         call c06fae(work(1,lin),npix,out,ierr)
         if(inv) call c06gbe(work(1,lin),npix,ierr)
         if(ierr.ne.0) goto 999
      enddo

*
* TRANSPOSE THE WORK ARRAY AND STORE IT IN ARRAY "OUT"
*
      do lin=1,nlin
         do pix=1,npix
            out(lin,pix)=work(pix,lin)
         enddo
      enddo

*
* TRANSFORM EACH ROW OF THE TRANSPOSED IMAGE (THE SAME AS EACH COLUMN
* OF THE UNTRANSPOSED IMAGE), AGAIN STORING THE OUTPUT IN HERMITIAN FORM
*
      do pix=1,npix
         call c06fae(out(1,pix),nlin,work,ierr)
         if(inv) call c06gbe(out(1,pix),nlin,ierr)
         if(ierr.ne.0) goto 999
      enddo

*
* FINISH
*
  999 continue

      end
