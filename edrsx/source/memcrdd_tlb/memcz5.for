      subroutine memcz5
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations 
*	required to produce a high resolution image using MEMSYS3.
*
*SOURCE
*       MEMCZ5.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   READ:
*	/B2_COM/
*   WRITE:
*	/ZZ_COM/
*		ZZ_aim - Target value of omega for MaxEnt procedure
*		ZZ_cfl - Size of data filter (in arcmins)
*		ZZ_cro - Value for CROTA descriptor (orientation of image)
*		ZZ_deg - Degliching strength required (0.0 - 1.0)
*		ZZ_fld - Noise level in background regions given by user
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_ins - In scan pointing error in arcmins
*		ZZ_k   - Reciprocal of signal to noise ratio
*		ZZ_lev - Amount of MEMSYS3 diagnostics to display
*		ZZ_mth - MEMSYS3 method; COMBINATION, CLASSIC,
*                        HISTORIC, NONAUTO or MCM
*		ZZ_nit - Max. no. of MEM iterations to perform
*		ZZ_ntr - Max. no.of trials to perform when producing
*                        noise estimates due to  pointing
*		ZZ_pnt - Pointing error in arcmins
*		ZZ_psf - Fractional error in the used PSFs
*		ZZ_psz - Pixel size given by user
*		ZZ_rat - Convergence rate limit
*		ZZ_sig - Noise to add to simulated data
*		ZZ_stf - Name of stop file
*		ZZ_tol - Tolerance required of MEMSYS3 calculations
*		ZZ_typ - Data type 'AO' or 'SURVEY'
*		ZZ_xs  - Cross scan pointing error in arcmins
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn,gtfile
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,memca5,memca6,memca7,memca8,memcb0,memcb1,
*	       memcb2
*       EDRS:
*              getpar
*       INTERIM:
*              ctoi,rdkeyc
*
*STARLINK PARAMETERS
*	AIM 	- Target value of omega for MaxEnt procedure
*	CROTA   - Clockwise angle from north to -ve y axis, in degrees.
*	DATATYPE- Data type 'AO' or 'SURVEY'
*	DEGLIT  - Deglitching effectiveness (0.0 - 1.0)
*	FIELD	- Noise level in background regions given by user
*	FILTER	- Size of gaussian ICF, in arcmins.
*	HIRES	- The High resolution output image
*	ILEVEL	- Amount of non-MEMSYS3 information to display
*	INSCAN  - In scan pointing error in arcmins
*	K	- Reciprocal of ignal to noise ratio
*	LEVEL	- Amount of MEMSYS3 diagnostics to display
*	LOGFILE - Name of text file to receive MEMSYS3 diagnostics
*	METHOD	- MEMSYS3 method
*	NITER	- Max. no. of MEM iterations to perform
*	NTRIAL	- No. of trials to use when calculating noise due to pointing
*	PIXSIZE	- Pixel size given by user
*	PSFERR  - Fractional error in PSF values
*	RATE	- Convergence rate limit
*	STOPFILE- Name of file which will cause MEMCRDD to stop
*	TOL	- Tolerance required of MEMSYS3 calculations
*	XSCAN   - Cross scan pointing error in arcmins
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE IRAS MISSION PARAMETERS AND FOCAL PLANE DATA
*
      include 'UTILITIES(IR_PAR)'
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      real	defpix(4)! Default pixel size in arcmins for each band
      integer	ierr	! Inherited status value
      real	inscan(4)! Default in scan pointing errors (arcmins)
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      real	k(4)    ! Default values for fractional error in solid angle 
      character	logfil*40! Name of file to receive MEMSYS3 diagnostics
      integer	ltext	! Length of user-selected text string
      integer	ncomm	! Position of selected string within option list
      real	rval	! Dummy real argument
      character text*10	! Text of selected option
      real	xscan(4)! Default cross scan pointing errors (arcmins)

*
* INITIALISE DATA
*
      data    defpix/0.25, 0.25, 0.50, 1.00/,
     :        inscan/0.05, 0.05, 0.10, 0.10/,
     :         xscan/0.20, 0.20, 0.25, 0.25/,
     :             k/0.01, 0.01, 0.03, 0.04/

*
* SET ERROR STATUS TO SUCCESS VALUE (MOST MEMC ROUTINES USE "INHERITED
* STATUS" ERROR SYSTEM)
*
      ierr=0

*
* GET THE NAME OF A "STOP FILE" FOR USE IN BATCH PROCESSES (THE PROGRAM
* CHECK TO SEE IF THIS FILE EXISTS AT VARIOUS POINTS AND EXITS NEATLY
* IF IT DOES)
*
      ZZ_stf='NONE'
      call rdkeyc('STOPFILE',.true.,1,ZZ_stf,ival,istat)

*
* SEE HOW MUCH INFORMATION USER WANTS DISPLAYED ON THE SCREEN
*
      ZZ_ilv=1
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
* GET POINTER TO A PSF STACK AND READ IN ITS DESCRIPTORS
*
      call memca7(ierr)

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
      call getpar('CROTA','REAL',1,-1.0E32,1.0E32,.true.,ival,ZZ_cro,
     :             istat)

*
* SET UP FITS PARAMETERS DESCRIBING OUTPUT IMAGE FRAME (SIZE, ETC)
*
      call memcb0(ierr)
      if(ierr.ne.0) goto 999

*
* GET REMAINING PARAMETERS....
*
* METHOD - CLASSIC OR HISTORIC MAXENT. "COMBINATION" MODE MIXES 
* CLASSIC-AUTOMATIC AND HISTORIC CALLS TO PRODUCE CLASSIC
* RESULTS IN MUCH LESS TIME THAN USING PURELY CLASSIC CALLS.
* NONAUTO IS CLASSIC WITHOUT AUTOMATIC NOISE SCALING. MCM IS NOT 
* MAXENT AT ALL, BUT IS A VERY SIMPPLE "MAXIMUM CORRELATION" METHOD.
*
      ncomm=2
      call gtstrn('METHOD',.true.,'COMBINATION,CLASSIC,HISTORIC,'//
     :            'NONAUTO,MCM.',1,ncomm,ZZ_mth,ltext,istat)

*
* NTRIAL - SPECIFIES MAX NO OF TRIALS TO BE PERFORMED WHEN PRODUCING
* NOISE ESTIMATES DUE TO POINTING
*
      ZZ_ntr=1000
      call getpar('NTRIAL','INTEGER',1,0.0,1.0E6,.true.,ZZ_ntr,rval,
     :             istat)

*
* XSCAN - CROSS SCAN POINTING ERROR IN ARCMINS
*
      ZZ_xs=xscan(B2_bnd)
      call getpar('XSCAN','REAL',1,0.0,1.0E6,.true.,ival,ZZ_xs,istat)

*
* INSCAN - IN SCAN POINTING ERROR IN ARCMINS
*
      ZZ_ins=inscan(B2_bnd)
      call getpar('INSCAN','REAL',1,0.0,1.0E6,.true.,ival,ZZ_ins,istat)

*
* PSFERR - FRACTIONAL ERROR IN SUPPLIED PSF VALUES
*
      ZZ_psf=0.1
      call getpar('PSFERR','REAL',1,0.0,1.0E6,.true.,ival,ZZ_psf,istat)

*
* AIM - SPECIFIES TERMINATION CRITERION
*
      ZZ_aim=1.0
      call getpar('AIM','REAL',1,0.0,1.0E6,.true.,ival,ZZ_aim,istat)
*
* AIM - SPECIFIES TERMINATION CRITERION
*
      ZZ_aim=1.0
      call getpar('AIM','REAL',1,0.0,1.0E6,.true.,ival,ZZ_aim,istat)
*
* FILTER - SPECIFIES SIZE OF GAUSSIAN INTRINSIC CORRELATION FUNCTION.
*
      ZZ_cfl=0.0
      call getpar('FILTER','REAL',1,0.0,1.0E6,.true.,ival,ZZ_cfl,
     :            istat)

*
* RATE: MAX STEP SIZE BETWEEN ITERATIONS. THE ABSOLUTE VALUE OF ZZ_RAT
* IS USED, BUT IF THE GIVEN VALUE IS NEGATIVE, THEN THE USED VALUE MAY
* BE CHANGED IF THE "TEST" DIAGNOSTIC IS PARTICULARLY HIGH OR LOW.
*
      ZZ_rat=1.0
      call getpar('RATE','REAL',1,-1.0E6,1.0E6,.true.,ival,ZZ_rat,istat)

*
* LEVEL: AMOUNT OF MEMSYS3 DIAGNOSTIC INFORMATION TO DISPLAY. IF ANY
*        DIAGNOSTICS ARE REQUIRED OPEN A FILE ON UNIT 10
*
      ncomm=1
      call gtstrn('LEVEL',.true.,'0,1,2,3,10,11,12,13,20,21,22,23.',1,
     :            ncomm,text,ltext,istat)
      call ctoi(text,ZZ_lev,istat)

      if(ZZ_lev.gt.0) then
         logfil='SYS$OUTPUT'
         call gtfile('LOGFILE',PR_uni,.true.,'NEW',logfil,istat)
         if(istat.ne.0) ZZ_lev=0
      endif

*
* NITER: THE MAXIMUM NUMBER OF ITERATIONS OF THE MEM ALGORITHM TO 
* PERFORM
*
      ZZ_nit=20
      call getpar('NITER','INTEGER',1,0.0,1.0E6,.true.,ZZ_nit,rval,
     :             istat)

*
* K: FRACTIONAL ERROR IN SOLID ANGLE ESTIMATES
*
      ZZ_k=k(B2_bnd)
      call getpar('K','REAL',1,-1.0E6,1.0E6,.true.,ival,ZZ_k,istat)

*
* FIELD: NOISE LEVEL IN BACKGROUND REGIONS. A VALUE OF ZERO CAUSES 
* INTERNALLY CALCULATED ESTIMATES OF THE FIELD NOISE TO BE USED.
*
      ZZ_fld=0.0
      call getpar('FIELD','REAL',1,0.0,1.0E20,.true.,ival,ZZ_fld,istat)

*
* DEGLIT: EFFECTIVENESS OF DEGLITCHING REQUIRED. 0.0 MEANS NO 
* DEGLITCHING, 1.0 MEANS FULL DEGLITCHING. NB, A VALUE OF 1.0 WILL
* REMOVE POINT SOURCES AS WELL AS GLITCHES!
*
      ZZ_deg=0.3
      call getpar('DEGLIT','REAL',1,0.0,1.0,.true.,ival,ZZ_deg,istat)

*
* TOL: MEMSYS3 TOLERANCE
*
      ZZ_tol=0.1
      call getpar('TOL','REAL',1,0.0,1.0E6,.true.,ival,ZZ_tol,istat)

*
* CALL MEMCB1 TO SET UP THE DATA STRUCTURES REQUIRED BY MEMSYS3 AND THEN
* TO ACTIVATE THE MAIN MEMSYS3 ROUTINE, MEM3, AND PRODUCE INTERMEDIATE
* OUTPUT FILES.
*
      call memcb1(ierr)

*
* PRODUCE FINAL OUTPUT IMAGE
*
      call memca2('HIRES',1,.false.,ierr)

*
* FINISH
*
  999 continue

      end
