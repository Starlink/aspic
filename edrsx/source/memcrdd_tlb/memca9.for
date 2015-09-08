      subroutine memca9(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Reads in a trial sky image, from which simulated data is
*	to be generated.
*
*SOURCE
*       MEMCA9.FOR in MEMCRDD.TLB
*
*METHOD
*	Get a pointer to an EDRS format image. Get the associated
*	descriptors and store in common. Set up the FITS descriptors
*	to those of the input image, and store the pixel size.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   WRITE:
*	/A9_COM/,
*		A9_bsc	Scale factor from input image
*		A9_bze	Zero offset from input image
*		A9_inv	Flag for invalid values in input image
*		A9_pin  Pointer to input image
*		A9_nls  No. of lines in input image
*		A9_nps  No. of pixels per line in input image
*	/B0_COM/,
*		B0_nps  No. of pixels per line in image
*	        B0_nls  No. of lines in image
*		B0_fit	FITS descriptors
*	/ZZ_COM/,
*		ZZ_psz  Size of a square pixel in arcmins
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gt2dir,rdimds
*       EDRS:
*              gtdscr,wrerr
*
*STARLINK PARAMETERS
*	SKYIN		The input image to use as a trial sky image
*	A9ERR1(error)	Accessed if the input image does not have square
*			pixels.
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/11/89
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

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(A9_COM)'

* ... FITS DESCRIPTORS
      include '(B0_COM)'

* ... PARAMETER VALUES (NORMALLY SPECIFIED BY THE USER)
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      character cval*1	! Dummy character argument
      real	dl1	! CDELT1: Size of a pixel along "pixel" axis 
      real	dl2	! CDELT2: Size of a pixel along "line" axis
      character instrm	! Instrument or program which produced the image
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	px1	! CRPIX1: Pixel no. of reference pixel
      integer	px2	! CRPIX2: Line no. of reference pixel
      real	rot	! CROTA1: Orientation of image w.r.t. north
      real	rval	! Real dummy argument
      character	units*30! String describing physical units of sky image
      real	vl1	! CRVAL1: RA of reference pixel
      real	vl2	! CRVAL2: DEC of reference pixel
      

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* GET A POINTER TO THE INPUT IMAGE
*
      call gt2dir('SKYIN',102,.false.,A9_nps,A9_nls,A9_pin,ierr)

*
* IF SUCCESFUL, READ IN DESCRIPTORS
*
      if(ierr.eq.0) then

         A9_bsc=1.0
         A9_bze=0.0
         A9_inv=-100000

         call gtdscr('SKYIN','BSCALE','REAL',ival,A9_bsc,cval,istat)
         call gtdscr('SKYIN','BZERO','REAL',ival,A9_bze,cval,istat)
         call gtdscr('SKYIN','INVAL','INTEGER',A9_inv,rval,cval,istat)

         call rdimds('SKYIN',.false.,vl1,vl2,px1,px2,dl1,dl2,rot,
     :                instrm,ierr)

*
* CHECK PIXELS ARE SQUARE
*
         if(dl1.ne.dl2) then
            call wrerr('A9ERR1')
            ierr=1
            goto 999
         endif

*
* CHECK WHAT UNITS THE INPUT IMAGE IS IN. IF NOT JY/ST, TELL USER THAT
* IT WILL BE ASSUMED THAT INPUT IMAGE IS IN JY/ST
*
         call gtdscr('SKYIN','BUNIT','CHARACTER',ival,rval,units,istat)
         if(istat.ne.0.or.index(units,'JY/ST').eq.0) then
            call wruser('*** Assuming image data values are in Jy/St',
     :                   istat)
         endif

*
* FIND NEXT LARGER LEGAL IMAGE SIZES
*
         call imsize(A9_nps,B0_nps,ierr)
         if(ierr.ne.0) goto 999

         call imsize(A9_nls,B0_nls,ierr)
         if(ierr.ne.0) goto 999

*
* COPY FITS DESCRIPTORS TO COMMON 
*
         B0_fit(1)=vl1
         B0_fit(2)=vl2
         B0_fit(3)=real(px1+(B0_nps-A9_nps)/2)
         B0_fit(4)=real(px2+(B0_nls-A9_nls)/2)
         B0_fit(5)=dl1
         B0_fit(6)=dl2
         B0_fit(7)=rot

*
* STORE PIXEL SIZE IN ARCMINS, IN COMMON
*
         ZZ_psz=dl1*60.0

      endif

*
* FINISH
*
  999 continue

      end
