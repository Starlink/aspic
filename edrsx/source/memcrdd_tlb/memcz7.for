      subroutine memcz7
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations 
*	required to produce blurred CRDD.
*
*SOURCE
*       MEMCZ7.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   READ:
*	/B2_COM/,/ME_COM/
*   WRITE:
*	/ZZ_COM/
*		ZZ_cro - Clockwise angle from north to -ve Y axis
*		ZZ_deg - Deglitching strength required (0.0 - 1.0)
*		ZZ_fld - Noise level in background regions given by user
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_k   - Reciprocal of signal to noise ratio
*		ZZ_psz - Pixel size given by user
*		ZZ_typ - Data type 'AO' or 'SURVEY'
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca4,memca5,memca7,memca8,memcb0,memcb2,memcc5
*       EDRS:
*              getpar
*
*STARLINK PARAMETERS
*	CROTA   - Clockwise angle from north to -ve Y axis in degrees
*	DATATYPE- Data type 'AO' or 'SURVEY'
*	DEGLIT  - Deglitching effectiveness (0.0 - 1.0)
*	ILEVEL	- Amount of non-MEMSYS3 information to display
*	LORES	- The low resolution output image
*	PIXSIZE	- Pixel size given by user
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 1/11/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... MEMSYS3 INFO
      include '(ME_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      real	defpix(4)! Default pixel size in arcmins for each band
      integer	ierr	! Inherited status value
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	ltext	! Length of user-selected text string
      integer	ncomm	! Position of selected string within option list
      real	rval	! Dummy real argument

*
* INITIALISE DATA
*
      data    defpix/0.25, 0.25, 0.5, 1.0/

*
* SET ERROR STATUS TO SUCCESS VALUE (MOST MEMC ROUTINES USE "INHERITED
* STATUS" ERROR SYSTEM)
*
      ierr=0

*
* SEE HOW MUCH INFORMATION USER WANTS DISPLAYED ON THE SCREEN
*
      ZZ_ilv=2
      call getpar('ILEVEL','INTEGER',1,1.0,5.0,.true.,ZZ_ilv,rval,istat)

*
* SEE IF USER WANTS TO USE AO CRDD OR SURVEY CRDD
*
      ncomm=2
      call gtstrn('DATATYPE',.true.,'AO,SURVEY.',1,ncomm,ZZ_typ,ltext,
     :             istat)

*
* GET POINTERS TO THE INPUT CRDD FILES AND READ IN THEIR DESCRIPTORS
*
      if(ZZ_typ.eq.'AO') then
         call memca5(ierr)
      else
         call memca6(ierr)
      endif

*
* COPY DESCRIPTORS FOR BOTH AO AND SURVEY DATA TO A COMMON LOCATION
*
      call memcb2(ierr)

*
* SEE HOW DATA SAMPLES ARE TO BE DIVIDED INTO GROUPS
*
      call memca8(ierr)

*
* IF AN ERROR OCCURED IN ANY OF THE ABOVE CALLS, ABORT IMMEDIATELY
*
      if(ierr.ne.0) goto 999

*
* GET OUTPUT PIXEL SIZE IN ARCMINS
*
      ZZ_psz=defpix(B2_bnd)
      call getpar('PIXSIZE','REAL',1,0.0,1.0E6,.true.,ival,ZZ_psz,istat)

*
* GET IMAGE ORIENTATION (CLOCKWISE ANGLE FROM NORTH TO -VE Y AXIS, IN
* DEGREES). A VALUE OF OUTSIDE THE RANGE +/-360 CAUSES A
* VALUE CALCULATED IN ROUTINE MEMCB4 TO BE USED.
*
      ZZ_cro=-9999.0

*
* SET UP FITS PARAMETERS DESCRIBING OUTPUT IMAGE FRAME (SIZE, ETC)
*
      call memcb0(ierr)
      if(ierr.ne.0) goto 999

*
* SET FIELD NOISE TO ZERO
*
      ZZ_fld=0.0

*
* GET FWHM OF GAUSSIAN TO SMOOTH THE CRDD WITH (IN ARCMINS). A NEGATIVE
* VALUE USES A DEFAULT VALUE CALCULATED LATER ON TO BE USED.
*
      ZZ_cfl=ZZ_psz
      call getpar('FILTER','REAL',1,-1.0E6,1.0E6,.true.,ival,ZZ_cfl,
     :             istat)

      if(ZZ_cfl.eq.0.0) then
         call wrerr('Z7ERR1')
         goto 999
      endif

*
* GET EFFECTIVENESS OF DEGLITCHING REQUIRED. 0.0 MEANS NO 
* DEGLITCHING, 1.0 MEANS FULL DEGLITCHING. NB, A VALUE OF 1.0 WILL
* REMOVE POINT SOURCES AS WELL AS GLITCHES!
*
      ZZ_deg=0.3
      call getpar('DEGLIT','REAL',1,0.0,1.0,.true.,ival,ZZ_deg,istat)

*
* CALL MEMCG6 TO SET UP THE DATA STRUCTURES REQUIRED TO PRODUCE THE
* BLURRED CRDD.
*
      call memcg6(ierr)

*
* OUTPUT THE BLURRED CRDD TO DISK
*
      call memca2('CRDOUT',21,.false.,ierr)

*
* FINISH
*
  999 continue

      end
