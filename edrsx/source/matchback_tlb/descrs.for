      subroutine descrs(image,ipin,crval1,crval2,crpix1,crpix2,cdelt1,
     :                  cdelt2,crota1,naxis1,naxis2,bscale,bzero,blank,
     :                  pixsiz,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Gets a pointer to an image, and the associated descriptor 
*	values required by routine MATCHB, from an image with 
*	parameter name IMn where n is given in argument 'image'.
*
*SOURCE
*       DESCRS.FOR in MATCHBACK.TLB
*
*ARGUMENTS       
*   INPUTS:
*	image		integer		Image counter
*   OUTPUTS:
*	crval1		real		FITS descriptor CRVAL1
*	crval2		real		FITS descriptor CRVAL2
*	crpix1		integer		FITS descriptor CRPIX1
*	crpix2		integer		FITS descriptor CRPIX2
*	cdelt1		real		FITS descriptor CDELT1
*	cdelt2		real		FITS descriptor CDELT2
*	crota1		real		FITS descriptor CROTA1
*	naxis1		integer		FITS descriptor NAXIS1
*	naxis2		integer		FITS descriptor NAXIS2
*	bscale		real		FITS descriptor BSCALE
*	bzero		real		FITS descriptor BZERO
*	blank		integer		FITS descriptor BLANK
*	ipin		integer		Pointer to input image
*	pixsiz		real		Pixel size in square degrees
*       ierr            integer         Error status
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,rdimds
*       EDRS:
*              lbgone,wrerr,gtdscr
*              
*STARLINK PARAMETERS
*	IM1-IM20	The input images
*	DIFFSIZE(error)	Accessed if not all image shave the same pixel 
*			size
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 31/1/88
*-------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      integer	image,crpix1,crpix2,naxis1,naxis2,blank,ierr,ipin
      real	crval1,crval2,cdelt1,cdelt2,crota1,bscale,bzero,pixsiz
      
*
* DECLARE LOCAL VARIABLES
*
      character cval*1	! Dummy character argument
      integer	istat	! Internal error status
      integer	ival	! Dummy integer argument
      character	pname*4	! Parameter name associated with the image
      real	rval	! Dummy real argument

*
* CONSTRUCT PARAMETER NAME
*
      write(pname,20) image
   20 format('IM',I2)
      call lbgone(pname(3:))

*
* MAP THE DATA FROM THE BDF FILE
*
      call gt2dir(pname,104,.true.,naxis1,naxis2,ipin,ierr)
      if(ierr.ne.0) goto 999
      
*
* GET STANDARD IRAS IMAGE POSITIONAL DESCRIPTORS
*
      call rdimds(pname,.true.,crval1,crval2,crpix1,crpix2,cdelt1,
     :            cdelt2,crota1,cval,ierr)
      if(ierr.ne.0) goto 999

*
* CALCULATE PIXEL SIZE, AND CHECK ITS THE SAME AS THE LAST PIXEL SIZE, 
* IF THIS IS NOT THE FIRST IMAGE
*
      if(image.eq.1) then
         pixsiz=cdelt1*cdelt2
      else if(cdelt1*cdelt2.ne.pixsiz) then
         call wrerr('DIFFSIZ')
         ierr=99
         goto 999
      endif

*
* SET UP DEFAULT VALUES FOR BSCALE, BZERO AND BLANK, THEN GET THEIR
* TRUE VALUES
*
      bscale=1.0
      bzero=0.0
      blank=-999999
      call gtdscr(pname,'BSCALE','REAL',ival,bscale,cval,ierr)
      call gtdscr(pname,'BZERO','REAL',ival,bzero,cval,ierr)
      call gtdscr(pname,'BLANK','INTEGER',blank,rval,cval,ierr)

*
* FINISH
*
  999 continue

      end
