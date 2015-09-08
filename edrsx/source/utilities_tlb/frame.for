      subroutine frame(mode,ipincr,frmid,ncrddf,band,pixsiz,crota,
     :                 crdata,npix,nlin,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       Determines the parameters describing an image frame which will
*       just contain the data stored in a series of survey or AO CRDD 
*       files.
*
*SOURCE
*       FRAME.FOR in UTILITIES.TLB
*
*METHOD
*	SURVEY mode:
*       It as assumed that the descriptors from each CRDD file are 
*       contained in the common block declared in module DS_COM.
*          The RA, DEC and scan orientation at the start and end of each
*       scan is calculated by routines BOREST and SCAN. These RA and DEC
*       values are converted to pixel coords within a temporary frame in
*       which the reference pixel is (0,0) and is located at the start
*       of the first CRDD file. These pixel coords are used to calculate
*       the coords of the corners of the scans, and the max and min x
*	and y coords are found. The reference pixel of the final frame
*	is placed in the middle of the smallest box enclosing all the 
*	scans.
*	   The orientation of the final frame is given by the calling
*	routine in argument CROTA. However, if CROTA is given the value
*	-999999.0, then the frame is oriented so that the mean of the
*	individual scan orientations is parallel to the y axis.

*	AO mode:
*	   To be written.
*       
*ARGUMENTS       
*   INPUTS:
*	mode	    character 'AO' or 'SURVEY'
*	ipincr(ncrddf)integer Pointers to input data files
*       frmid(ncrddf) integer A list of descriptor frame identifiers
*			      used to index the descriptor arrays
*     			      held in module DS_COM
*       ncrddf	      integer The number of CRDD files to be included
*       band	      integer The IRAS band no. (1-4) of the CRDD
*       pixsiz	      real    The required pixel size in arcmins
*       crota	      real    The required frame orientation (degrees)
*			      equal to the CRDIMAGE CROTA1 descriptor.
*   OUTPUTS:
*       crota	      real    The value of CROTA1 actually used
*       crdata(7)     real    The frame descriptors (see below)
*	npix	      integer The no. of pixel per line in the o/p frame
*	nlin	      integer The no. of lines in the o/p frame
*       ierr          integer Error status: 0 -success
*
* NB, The input array CRDATA should contain the following:
*    crdata(1) The RA of the sky image ref pixel in degrees (CRVAL1)
*    crdata(2) The DEC of the sky image ref pixel in degrees (CRVAL2)
*    crdata(3) The x axis pixel location of the sky image ref pixel
*							     (CRPIX1)
*    crdata(4) The y axis pixel location of the sky image ref pixel.
*							     (CRPIX2)
*    crdata(5) The cross scan size of a pixel in degrees of arc (CDELT1)
*    crdata(6) The in scan size of a pixel in degrees of arc (CDELT2)
*    crdata(7) Angle clockwise from north to -ve in scan axis in 
*                                                 degrees.    (CROTA1)
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              borest,scan,rdtoxy,xytord
*	THIS PACKAGE (source attached to this module):
*		aolims
*
*VAX SPECIFICS
*       implicit none
*       Trig functions in degrees
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 4/5/89
*-------------------------------------------------------------------
*
      implicit none

*
* DECLARE PARAMETERS
*
      integer down    ! Index for down-stream sample offsets
      real    invang  ! Flag for invalid angles
      integer maxinp  ! Maximum number of CRDD files allowed
      real    scanhw  ! Half width of focal plane in arcmins
      integer up      ! Index for up-stream sample offsets

      parameter	  (down   =       1,
     :             invang = -999999,
     :             maxinp =      30,
     :             scanhw =    15.5,
     :             up     =       2)

*
* INCLUDE COMMON BLOCKS HOLDING IRAS MISSION PARAMETERS (VARIABLES
* CALLED IR_xxx),DESCRIPTORS FROM SURVEY CRDD FILE (VARIABLES CALLED 
* DS_xxx), DESCRIPTORS FROM AO CRDD FILE (VARIABLES CALLED AO_xxx), 
* AND INFO ABOUT DETECTORS (VARIABLES CALLED DT_xxx).
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DS_COM)'
      include 'UTILITIES(AO_COM)'
      include 'UTILITIES(DT_DAT)'

*
* DECLARE ARGUMENTS
*
      integer   ncrddf,ierr,frmid(ncrddf),band,npix,nlin,ipincr(ncrddf)
      real      pixsiz,crota,crdata(7)
      character mode*(*)
      
*
* DECLARE LOCAL VARIABLES
*
      real    anglef(maxinp)  ! Scan angle of first sample
      real    anglel(maxinp)  ! Scan angle of last sample
      real    cossum	      ! Sum of cosines of all scan angles
      real    dec	      ! DEC of reference pixel
      real    decf(maxinp)    ! DEC of the the boresight at first sample
      real    decl(maxinp)    ! DEC of the the boresight at last sample
      integer file	      ! Input CRDD file counter
      real    ra	      ! RA of reference pixel
      real    raf(maxinp)     ! RA of the the boresight at first sample
      real    ral(maxinp)     ! RA of the the boresight at last sample
      integer samphi	      ! Sample no. which would put boresight at
			      ! the position of the last detector sample
      integer samplo	      ! Sample no. which would put boresight at
			      ! the position of the first detector sample
      integer sampof(IR_bns,2)! Table of offsets from boresight to
			      ! detectors (in units of samples)
      real    sinsum	      ! Sum of sines of all scan angles
      real    x1,x2	      ! X coords of corners of current scan
      real    xmax,xmin	      ! Limits of X coords in temporary frame
      real    xx	      ! X position within temporary frame
      real    y1,y2	      ! Y coords of corners of current scan
      real    ymax,ymin	      ! Limits of Y coords in temporary frame
      real    yy	      ! Y position within temporary frame

      data    sampof  /  31,  13,  -4,  17,
     :                  -40, -59, -42, -29/


      WRITE(*,*) 'Entering FRAME'

*
* INITIALISE THE ERROR STATUS TO THE SUCCESS VALUE
*
      ierr=0

*
* CHECK THAT THERE IS SUFFICIENT INTERNAL WORK SPACE TO STORE ALL THE
* REQUIRED DATA
*
      if(ncrddf.gt.maxinp.or.ncrddf.le.0) then
         ierr=1000
         goto 999
      endif

*-----------------------------------------------------------
* FIRST DEAL WITH SURVEY CRDD FILES.....
*
      if(mode.eq.'SURVEY') then

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
         do file=1,ncrddf

*
* SET UP COMMON BLOCK /BORE/ HOLDING LEAST SQUARES LINEAR FITS TO 
* BORESIGHT POINTING DATA
*
            call borest(frmid(file),ierr)
            if(ierr.ne.0) goto 999

*
* CALCULATE WHAT THE SAMPLE NUMBER WOULD HAVE TO BE TO PUT THE BORESIGHT
* AT THE POSITION OF THE FIRST SAMPLE OF THE MOST UP-SCAN DETECTOR
*
            samplo=1+sampof(band,up)

*
* CALL SCAN TO CALCULATE THE RA, DEC AND SCAN ANGLE OF THE MOST 
* UP-SCAN (EARLIEST) SAMPLE
*
            call scan(samplo,frmid(file),band,anglef(file),raf(file),
     :                decf(file))

*
* CALCULATE WHAT THE SAMPLE NUMBER WOULD HAVE TO BE TO PUT THE BORESIGHT
* AT THE POSITION OF THE LAST SAMPLE OF THE MOST DOWN-SCAN DETECTOR
*
            samphi=DS_nys(frmid(file))+sampof(band,down)

*
* CALL SCAN TO CALCULATE THE RA, DEC AND SCAN ANGLE OF THE MOST 
* DOWN-SCAN (LATEST) SAMPLE
*
            call scan(samphi,frmid(file),band,anglel(file),ral(file),
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
* IF THE CALLING ROUTINE PROVIDED AN INVALID VALUE FOR THE FRAME
* ORIENTATION, FORM THE TRIGONOMETRIC MEAN OF THE SCAN ANGLES AND USE
* THAT AS THE FRAME ORIENTATION
*
         if(crota.eq.invang) crota=180-atan2d(sinsum,cossum)

*
* A TEMPORARY FRAME IS DEFINED IN WHICH THE REFERENCE PIXEL IS (0,0) 
* AND CORRESPONDS TO THE FIRST SAMPLE IN THE FIRST CRDD FILE
*
         crdata(1)=raf(1)
         crdata(2)=decf(1)
         crdata(3)=0
         crdata(4)=0
         crdata(5)=pixsiz/60.0
         crdata(6)=pixsiz/60.0
         crdata(7)=crota

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
         do file=1,ncrddf

*
* CALCULATE THE TEMPORARY FRAME COORDS OF THE FIRST SAMPLE
*
            call rdtoxy(xx,yy,crdata(1),crdata(2),int(crdata(3)),
     :                  int(crdata(4)),crdata(5),crdata(6),
     :                  crdata(7),raf(file),decf(file))

*
* CALCULATE THE COORDS OF THE CORNERS OF THE SCAN AT THIS END
*
            x1=xx+scanhw*cosd(crota+anglef(file))/pixsiz
            x2=xx-scanhw*cosd(crota+anglef(file))/pixsiz
            y1=yy+scanhw*sind(crota+anglef(file))/pixsiz
            y2=yy-scanhw*sind(crota+anglef(file))/pixsiz

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
            call rdtoxy(xx,yy,crdata(1),crdata(2),int(crdata(3)),
     :                  int(crdata(4)),crdata(5),crdata(6),
     :                  crdata(7),ral(file),decl(file))

            x1=xx+scanhw*cosd(crota+anglel(file))/pixsiz
            x2=xx-scanhw*cosd(crota+anglel(file))/pixsiz
            y1=yy+scanhw*sind(crota+anglel(file))/pixsiz
            y2=yy-scanhw*sind(crota+anglel(file))/pixsiz

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
      
         do file=1,ncrddf
            call aolims(%val(ipincr(file)),AO_nys(frmid(file)),
     :                  AO_nde(frmid(file)),AO_bpx(frmid(file)),pixsiz,
     :                  xmin,xmax,ymin,ymax)
         enddo

*
* BUT A 5 ARCMIN BORDER ROUND THE DATA TO ALLOW FOR THE SIZE OF A
* DETECTOR
*
         xmin=xmin-5.0/pixsiz
         xmax=xmax+5.0/pixsiz
         ymin=ymin-5.0/pixsiz
         ymax=ymax+5.0/pixsiz

*
* A TEMPORARY FRAME IS DEFINED IN WHICH THE REFERENCE PIXEL IS (0,0) 
*
         crdata(1)=AO_ra(1)
         crdata(2)=AO_dec(1)
         crdata(3)=0
         crdata(4)=0
         crdata(5)=pixsiz/60.0
         crdata(6)=pixsiz/60.0
         crdata(7)=180.0

*-----------------------------------------------------------
* FOR BOTH AO AND SURVEY CRDD FILES....
*
      endif

*
* CHECK THERE IS SOME VALID DATA COVERAGE
*
      if(xmax.lt.-1.0e31) then
         call wruser('*** No valid data coverage',ierr)
         ierr=111
         goto 999
      endif

*
* CALCULATE SIZE OF THE FRAME WHICH JUST COVERS ALL THE DATA
*
      npix=int(xmax-xmin+2.0)
      nlin=int(ymax-ymin+2.0)

*
* FIND RA AND DEC OF MID PIXEL IN FINAL FRAME
*
      xx=xmin+npix/2.0-1.0
      yy=ymin+nlin/2.0-1.0
      call xytord(xx,yy,crdata(1),crdata(2),
     :            int(crdata(3)),int(crdata(4)),crdata(5),crdata(6),
     :            crdata(7),ra,dec)

*
* SET UP THE REMAINING FINAL FRAME DESCRIPTORS
*
      crdata(1)=ra
      crdata(2)=dec
      crdata(3)=npix/2.0
      crdata(4)=nlin/2.0

*
* FINISH
*
  999 continue
      write(*,*) 'Leaving FRAME'
      write(*,*) ' '

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
