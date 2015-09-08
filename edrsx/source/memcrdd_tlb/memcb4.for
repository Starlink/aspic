      subroutine memcb4(crota,npixs,nlins,fits,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Set up default FITS descriptor values for output image.
*	
*SOURCE
*       MEMCB4.FOR in MEMCRDD.TLB
*
*METHOD
*	The frame is generated with north upwards.
*
*	SURVEY mode:
*          The RA, DEC and scan orientation at the start and end of each
*       scan is calculated by routines BOREST and SCAN. These RA and DEC
*       values are converted to pixel coords within a temporary frame in
*       which the reference pixel is (0,0) and is located at the start
*       of the first CRDD file. These pixel coords are used to calculate
*       the coords of the corners of the scans, and the max and min x
*	and y coords are found. The reference pixel of the final frame
*	is placed in the middle of the smallest box enclosing all the 
*	scans, plus a blank margin. The orientation of the output frame
*	is given by the calling routine. If it is set invalid (=PR_rin)
*	than a value is chosen such that the mean scan direction is 
*	vertical. This will hopefully minimise the size of the output 
*	image, and thus the CPU required to deconvolve it.
*
*	AO mode:
*	   The AO footprints reference point is used as the reference 
*	point for the FITS descriptors, and is placed in the middle of
*	the frame. A blank margin is added round the edge. The output 
*	frame always has north upwards.
*       
*ARGUMENTS       
*   INPUTS:
*	crota	real	        The angle from north to the -ve y 
* 				axis, measured clockwise, in degrees.
*				If this lies outside the range +360
*				to -360 degrees, then a value is used
*				which minimizes the image size
*       ierr    integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       npixs	integer		No. of pixels per line of the image
*       nlins	integer		No. of lines in the image
*	fits(7)	real		Fits descriptors (see routine MEMCB0)
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A7_COM/,/B2_COM/,/ZZ_COM/,/AO_COM/
*   READ/WRITE:
*	/BORE/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              borest,scan,rdtoxy,xytord,
*       THIS PACKAGE (MEMCRDD.TLB):
*              aolims (source attached)
*       EDRS:
*              wrerr
*              
*STARLINK PARAMETERS
*	B4ERR1(error) Accessed if the data covers no area
*
*VAX SPECIFICS
*       implicit none
*       %val
*       Trig functions in degrees
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 28/9/89
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

* ... AO DSCRIPTOR VALUES
      include 'UTILITIES(AO_COM)'

* ... PSF INFORMATION
      include '(A7_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      real	crota,fits(7)
      integer	npixs,nlins,ierr

*
* DECLARE LOCAL PARAMETERS
*
      integer down    ! Index for down-stream sample offsets
      real    invang  ! Flag for invalid angles
      real    scanhw  ! Half width of focal plane in arcmins
      integer up      ! Index for up-stream sample offsets

      parameter	  (down   =       1,
     :             invang = -999999,
     :             scanhw =    15.5,
     :             up     =       2)


*
* DECLARE LOCAL VARIABLES
*
      real    anglef(PR_crd)  ! Scan angle of first sample
      real    anglel(PR_crd)  ! Scan angle of last sample
      real    cossum	      ! Sum of cosines of all scan angles
      real    dec	      ! DEC of reference pixel
      real    decf(PR_crd)    ! DEC of the the boresight at first sample
      real    decl(PR_crd)    ! DEC of the the boresight at last sample
      integer file	      ! Input CRDD file counter
      integer istat	      ! Temporary error status
      real    marpix	      ! Width of margin in pixels
      real    ra	      ! RA of reference pixel
      real    raf(PR_crd)     ! RA of the the boresight at first sample
      real    ral(PR_crd)     ! RA of the the boresight at last sample
      integer samphi	      ! Sample no. which would put boresight at
			      ! the position of the last detector sample
      integer samplo	      ! Sample no. which would put boresight at
			      ! the position of the first detector sample
      integer sampof(IR_bns,2)! Table of offsets from boresight to
			      ! detectors (in units of samples)
      real    sinsum	      ! Sum of sines of all scan angles
      real    ucrota	      ! Value used for CROTA
      real    x1,x2	      ! X coords of corners of current scan
      real    xmax,xmin	      ! Limits of X coords in temporary frame
      real    xx	      ! X position within temporary frame
      real    y1,y2	      ! Y coords of corners of current scan
      real    ymax,ymin	      ! Limits of Y coords in temporary frame
      real    yy	      ! Y position within temporary frame

*
* SET UP LOCAL DATA
*
      data    sampof  /  31,  13,  -4,  17,
     :                  -40, -59, -42, -29/

*
* CHECK THE INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*-----------------------------------------------------------
* FIRST DEAL WITH SURVEY CRDD FILES.....
*
      if(ZZ_typ.eq.'SURVEY') then

*
* INITIALISE SUM OF ANGLE FUNCTIONS USED TO DETERMINE THE ORIENTATION
* OF THE IMAGE FRAME
*
         sinsum=0.0
         cossum=0.0

*
* CALCULATE THE RA, DEC AND SCAN ORIENTATION OF THE START AND END OF
* EACH CRDD FILE
*
         do file=1,B2_ncf

*
* SET UP COMMON BLOCK /BORE/ HOLDING LEAST SQUARES LINEAR FITS TO 
* BORESIGHT POINTING DATA
*
            call borest(B2_frm(file),ierr)
            if(ierr.ne.0) goto 999

*
* CALCULATE WHAT THE SAMPLE NUMBER WOULD HAVE TO BE TO PUT THE BORESIGHT
* AT THE POSITION OF THE FIRST SAMPLE OF THE MOST UP-SCAN DETECTOR
*
            samplo=1+sampof(B2_bnd,up)

*
* CALL SCAN TO CALCULATE THE RA, DEC AND SCAN ANGLE OF THE MOST 
* UP-SCAN (EARLIEST) SAMPLE
*
            call scan(samplo,B2_frm(file),B2_bnd,anglef(file),raf(file),
     :                decf(file))

*
* CALCULATE WHAT THE SAMPLE NUMBER WOULD HAVE TO BE TO PUT THE BORESIGHT
* AT THE POSITION OF THE LAST SAMPLE OF THE MOST DOWN-SCAN DETECTOR
*
            samphi=B2_nys(file)+sampof(B2_bnd,down)

*
* CALL SCAN TO CALCULATE THE RA, DEC AND SCAN ANGLE OF THE MOST 
* DOWN-SCAN (LATEST) SAMPLE
*
            call scan(samphi,B2_frm(file),B2_bnd,anglel(file),ral(file),
     :                decl(file))

*
* INCREMENT SUMS OF ANGLE FUNCTIONS
*
            sinsum=sinsum+sind(anglef(file))+sind(anglel(file))
            cossum=cossum+cosd(anglef(file))+cosd(anglel(file))

*
* LOOP ROUND FOR NEXT CRDD FILE
*
         enddo

*
* IF AN INVALID VALUE WAS PROVIDED FOR CROTA, CALCULATE A NEW VALUE
* WHICH MINIMIZES THE SIZE OF THE OUTPUT FRAME (CROTA IS THE ANGLE FROM 
* NORTH TO THE -VE Y AXIS, MEASURED CLOCKWISE)
*
         if(crota.lt.-360.0.or.crota.gt.360.0) then
            ucrota=180.0-atan2d(sinsum,cossum)
         else
            ucrota=crota
         endif

*
* A TEMPORARY FRAME IS DEFINED IN WHICH THE REFERENCE PIXEL IS (0,0) 
* AND CORRESPONDS TO THE FIRST SAMPLE IN THE FIRST CRDD FILE
*
         fits(1)=raf(1)
         fits(2)=decf(1)
         fits(3)=0
         fits(4)=0
         fits(5)=ZZ_psz/60.0
         fits(6)=ZZ_psz/60.0
         fits(7)=ucrota

*
* INITIALISE THE EXTREMAL VALUES REQUIRED TO COVER ALL THE DATA IN THIS
* TEMPORARY FRAME
*
         xmax=-1.0e32
         xmin=1.0e32
         ymax=-1.0e32
         ymin=1.0e32

*
* LOOP ROUND EACH SCAN TO GET THE EXTREMAL POSITIONS IN THE TEMPORARY
* FRAME
*
         do file=1,B2_ncf

*
* CALCULATE THE TEMPORARY FRAME COORDS OF THE FIRST SAMPLE
*
            call rdtoxy(xx,yy,fits(1),fits(2),int(fits(3)),
     :                  int(fits(4)),fits(5),fits(6),
     :                  fits(7),raf(file),decf(file))

*
* CALCULATE THE COORDS OF THE CORNERS OF THE SCAN AT THIS END
*
            x1=xx+scanhw*cosd(ucrota+anglef(file))/ZZ_psz
            x2=xx-scanhw*cosd(ucrota+anglef(file))/ZZ_psz
            y1=yy+scanhw*sind(ucrota+anglef(file))/ZZ_psz
            y2=yy-scanhw*sind(ucrota+anglef(file))/ZZ_psz

*
* UPDATE THE X AND Y LIMITS WITHIN THE TEMPORARY FRAME
*
            xmax=max(xmax,x1,x2)
            xmin=min(xmin,x1,x2)
            ymax=max(ymax,y1,y2)
            ymin=min(ymin,y1,y2)

*
* DO THE SAME FOR THE LAST SAMPLE
*
            call rdtoxy(xx,yy,fits(1),fits(2),int(fits(3)),
     :                  int(fits(4)),fits(5),fits(6),
     :                  fits(7),ral(file),decl(file))

            x1=xx+scanhw*cosd(ucrota+anglel(file))/ZZ_psz
            x2=xx-scanhw*cosd(ucrota+anglel(file))/ZZ_psz
            y1=yy+scanhw*sind(ucrota+anglel(file))/ZZ_psz
            y2=yy-scanhw*sind(ucrota+anglel(file))/ZZ_psz

            xmax=max(xmax,x1,x2)
            xmin=min(xmin,x1,x2)
            ymax=max(ymax,y1,y2)
            ymin=min(ymin,y1,y2)

*
* GO ROUND FOR NEXT CRDD FILE
*
         enddo

*-----------------------------------------------------------
* NOW DEAL WITH AO CRDD FILES.....
*
      else

*
* FIND MIN AND MAX VALUES OF PIXEL AND ROW NUMBERS ASSUMING PIXEL (0,0)
* IS AT REFERENCE POSITION
*
         xmax=-1.0e32
         xmin=1.0e32
         ymax=-1.0e32
         ymin=1.0e32
      
         do file=1,B2_ncf
            call aolims(%val(B2_pin(file)),B2_nys(file),B2_nde(file),
     :                  B2_bpx(file),ZZ_psz,xmin,xmax,ymin,ymax)
         enddo

*
* A TEMPORARY FRAME IS DEFINED IN WHICH THE REFERENCE PIXEL IS (0,0) 
*
         fits(1)=AO_ra(1)
         fits(2)=AO_dec(1)
         fits(3)=0
         fits(4)=0
         fits(5)=ZZ_psz/60.0
         fits(6)=ZZ_psz/60.0
         fits(7)=180.0

*-----------------------------------------------------------
* FOR BOTH AO AND SURVEY CRDD FILES....
*
      endif

*
* CHECK THERE IS SOME VALID DATA COVERAGE
*
      if(xmax.lt.-1.0e31) then
         call wrerr('B4ERR1',istat)
         ierr=1
         goto 999
      endif

*
* INCREMENT THE FRAME LIMITS BY THE MARGIN WIDTH IN PIXELS
*
      marpix=A7_mar/ZZ_psz
      xmax=xmax+marpix
      xmin=xmin-marpix
      ymax=ymax+marpix
      ymin=ymin-marpix

*
* CALCULATE SIZE OF THE FRAME WHICH JUST COVERS ALL THE DATA
*
      npixs=int(xmax-xmin+2.0)
      nlins=int(ymax-ymin+2.0)

*
* FIND RA AND DEC OF MID PIXEL IN FINAL FRAME
*
      xx=xmin+npixs/2.0-1.0
      yy=ymin+nlins/2.0-1.0
      call xytord(xx,yy,fits(1),fits(2),
     :            int(fits(3)),int(fits(4)),fits(5),fits(6),
     :            fits(7),ra,dec)

*
* SET UP THE REMAINING FINAL FRAME DESCRIPTORS
*
      fits(1)=ra
      fits(2)=dec
      fits(3)=npixs/2.0
      fits(4)=nlins/2.0

*
* FINISH
*
  999 continue

      end



C-----------------------------------------------------------------------
      subroutine aolims(data,nys,nde,inval,pixsiz,xmin,xmax,ymin,ymax)
      implicit  none
      integer	nys,nde,data(nys,nde,3),inval,det,samp
      real	pixsiz,xmin,xmax,ymin,ymax,pix,lin

      do det=1,nde
         do samp=1,nys
            if(data(samp,det,1).ne.inval) then
               pix=-data(samp,det,3)/(pixsiz*60.0)
               lin=data(samp,det,2)/(pixsiz*60.0)
               xmax=max(xmax,pix)
               xmin=min(xmin,pix)
               ymax=max(ymax,lin)
               ymin=min(ymin,lin)
            endif
         enddo
      enddo

      end
