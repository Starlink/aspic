      subroutine imszud(fname,npix,nlines,nim)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO UPDATE THE DESCRIPTOR ITEMS IN A 2 OR 3D IMAGE FRAME WHICH
*       SPECIFY THE IMAGE DIMENSIONS
*
*METHOD
*       INSERT THE REQUIRED VALUES, ALLOWING FOR EITHER 2D OR 3D
*       IMAGES
*
*ARGUMENTS
*       FNAME (IN)
*       CHARACTER*(*)
*               THE PARAMETER NAME OF THE FRAME TO BE UPDATED
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF EACH IMAGE
*       NIM (IN)
*       INTEGER
*               THE NUMBER OF IMAGE PLANES (1 FOR A 2D IMAGE)
*
*STARLINK PARAMETERS
*       THE FRAME SPECIFIED IN THE ARGUMENT 'FNAME' IS ACCESSED BY
*       THIS ROUTINE
*
*CALLS
*       THIS PACKAGE:
*               PTDSCR
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      character fname*(*),cval*1
 
*
* DETERMINE NUMBER OF IMAGE DIMENSIONS
*
      naxis=2
 
      if(nim.gt.1)naxis=3
 
*
* INSERT DESCRIPTOR VALUES
*
      call ptdscr(fname,'NAXIS','INTEGER',naxis,rval,cval,ierr)
      call ptdscr(fname,'NAXIS1','INTEGER',npix,rval,cval,ierr)
      call ptdscr(fname,'NAXIS2','INTEGER',nlines,rval,cval,ierr)
 
      if(naxis.eq.3)call ptdscr(fname,'NAXIS3','INTEGER',nim,rval,cval
     : ,ierr)
 
*
* REMOVE NAXIS3 IF IMAGE IS 2D
*
 
      if(naxis.eq.2)call dldscr(fname,'NAXIS3',istat)
 
      end
 
 
 
