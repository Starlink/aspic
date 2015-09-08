      subroutine rdimds(image,null,crval1,crval2,crpix1,crpix2,cdelt1,
     :                  cdelt2,crota1,instrm,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Returns the descriptors describing the position on the sky of
*       an image produced by CRDIMAGE
*
*SOURCE
*       RDIMDS.FOR in UTILITIES.TLB
*
*ARGUMENTS
*   INPUTS
*       IMAGE   character The parameter name poinint to the input image
*       NULL    logical If True the a null value is OK, otherwise it is
*                       an error
*   OUTPUTS
*       CRVAL1  real    The RA of the ref pixel in degrees
*       CRVAL2  real    The DEC of the ref pixel in degrees
*       CRPIX1  integer The cross scan pixel location of the ref pixel,
*                       increasing west.
*       CRPIX2  integer The in scan pixel location of the ref pixel.
*       CDELT1  real    The cross scan size of a pixel in degrees of arc
*       CDELT2  real    The in scan size of a pixel in degrees of arc
*       CROTA1  real    Angle anti-clockwise from north to -ve in scan axis
*                       in degrees.
*       INSTRM  character The INSTRUME descriptor. Indicates if the image
*                       was originally created by COMBINE or CRDIMAGE.
*       IERR    integer Status return. 0 - Success.
*                                    1-5 - INTERIM errors (See SUN4)Reported)
*                                     10 - Too many bad values given for frame
*                                     11 - Pixels have zero size(reported)
*                                     -1 - Image was produced by COMBINE
*                                          but CROTA1 was not zero
*
*USED BY
*       IRASCOEF,IRASSHIFT,DRAWSCAN
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*               GT2DID,WRERR,PTDERR
*       EDRS:
*               GTDSCR
*
*STARLINK PARAMETERS
*       'IMAGE'/read/   The name of the image is contained in character variable
*                       IMAGE
*       PIXNOSIZ/error/ Accessed if the pixels are of zero size
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 25/8/87
*---------------------------------------------------------------------------
*
      implicit none
*
* DECLARE ARGUMENTS
*
      real      crval1,crval2,cdelt1,cdelt2,crota1
      integer   ierr,crpix1,crpix2
      character image*(*),instrm*(*)
      logical   null
*
* DECLARE LOCAL VARIABLES
*
      character cval*1  ! Dummy character argument
      integer   i       ! Implicit loop count
      integer   ival    ! Dummy integer argument
      real      rval    ! dummy real argument
      real      temp    ! Temporary storage for real values
*
* GET AN INPUT IMAGE WITHOUT MAPPING THE DATA
*
      call gt2did(image,null,ierr)
      if(ierr.ne.0) goto 999
*
* GET THE NECESSARY DESCRIPTORS FROM THE IMAGE
*
      call gtdscr(image,'CRPIX1','REAL',ival,temp,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CRPIX1')
         goto 999
      endif
      crpix1=int(temp)
      call gtdscr(image,'CRPIX2','REAL',crpix2,temp,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CRPIX2')
         goto 999
      endif
      crpix2=int(temp)
      call gtdscr(image,'CRVAL1','REAL',ival,crval1,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CRVAL1')
         goto 999
      endif
      call gtdscr(image,'CRVAL2','REAL',ival,crval2,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CRVAL2')
         goto 999
      endif
      call gtdscr(image,'CROTA1','REAL',ival,crota1,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CROTA1')
         goto 999
      endif
      call gtdscr(image,'CDELT1','REAL',ival,cdelt1,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CDELT1')
         goto 999
      endif
      call gtdscr(image,'CDELT2','REAL',ival,cdelt2,cval,ierr)
      if(ierr.ne.0) then
         call ptderr('CDELT2')
         goto 999
      endif
      call gtdscr(image,'INSTRUME','CHARACTER',ival,rval,instrm,ierr)
      if(ierr.ne.0) then
         call ptderr('INSTRUME')
         goto 999
      endif
*
* CHECK THAT IF IMAGE WAS PRODUCED BY COMBINE, CROTA1 VALUE IS ZERO
* IF IT IS, THEN CHANGE IT TO 180 TO BE CONSISTENT WITH CROTA1 FROM
* CRDIMAGE
*
      If(index(instrm,'COMBINE').ne.0) then
         if(crota1.eq.0) then
            crota1=180
*
* IF IT'S NOT, THEN SET THE ERROR FLAG, BUT DON'T REPORT IT
*
         else
            ierr=-1
         endif
      endif
*
* CHECK THAT THE PIXELS ARE NOT OF ZERO SIZE
*
      if(cdelt1*cdelt2.eq.0) then
         call wrerr('PIXNOSIZ')
         ierr=11
         goto 999
      endif
*
* IF ALL DESCRIPTORS OBTAINED OK THEN SET ERROR STATUS TO ZERO
*
      ierr=0
*
* FINISH
*
 999  continue
      end
