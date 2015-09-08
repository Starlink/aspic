      subroutine memcz3
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations 
*	required to produce simulated IRAS data.
*
*SOURCE
*       MEMCZ3.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   WRITE:
*	/ZZ_COM/
*		ZZ_deg - Deglitching strength required (0.0 - 1.0)
*		ZZ_fld - Noise level to add to simulated data
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_ins - In scan pointing error in arcmins
*		ZZ_typ - Data type 'AO' or 'SURVEY'
*		ZZ_xs  - Cross scan pointing error in arcmins
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca0,memca5,memca7,memca8,memca9,memcb0,memcb2,memcd9
*       EDRS:
*              getpar
*
*STARLINK PARAMETERS
*	DATATYPE- Data type 'AO' or 'SURVEY'
*	DEGLIT  - Deglitching effectiveness (0.0 - 1.0)
*	FIELD	- Noise level to be added to data
*	ILEVEL	- Amount of non-MEMSYS3 information to display
*	INSCAN  - In scan pointing error in arcmins
*	XSCAN   - Cross scan pointing error in arcmins
*
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
* INCLUDE COMMON BLOCKS HOLDING ...

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      integer	ierr	! Inherited status value
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      integer	ltext	! Length of user-selected text string
      integer	ncomm	! Position of selected string within option list
      real	rval	! Dummy real argument

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
* GET SIGMA OF FIELD NOISE IS TO BE ADDED TO THE SIMULATED DATA
*
      ZZ_fld=0.0
      call getpar('FIELD','REAL',1,0.0,1.0E30,.true.,ival,ZZ_fld,istat)

*
* XSCAN - CROSS SCAN POINTING ERROR IN ARCMINS
*
      ZZ_xs=0.0
      call getpar('XSCAN','REAL',1,0.0,1.0E6,.true.,ival,ZZ_xs,istat)

*
* INSCAN - IN SCAN POINTING ERROR IN ARCMINS
*
      ZZ_ins=0.0
      call getpar('INSCAN','REAL',1,0.0,1.0E6,.true.,ival,ZZ_ins,istat)

*
* SEE IF USER WANTS TO USE AO CRDD OR SURVEY CRDD
*
      ncomm=2
      call gtstrn('DATATYPE',.true.,'AO,SURVEY.',1,ncomm,ZZ_typ,ltext,
     :             istat)

*
* GET EFFECTIVENESS OF DEGLITCHING REQUIRED. 0.0 MEANS NO 
* DEGLITCHING, 1.0 MEANS FULL DEGLITCHING. NB, A VALUE OF 1.0 WILL
* REMOVE POINT SOURCES AS WELL AS GLITCHES!
*
      ZZ_deg=0.3
      call getpar('DEGLIT','REAL',1,0.0,1.0,.true.,ival,ZZ_deg,istat)

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
* READ IN THE TRIAL SKY IMAGE
*
      call memca9(ierr)

*
* CALL MEMCA0 TO SET UP THE DATA STRUCTURES REQUIRED TO PRODUCE THE
* SIMULATED DATA
*
      call memca0(ierr)

*
* CALL MEMCD9 TO CALCULATE THE SIMULATED DATA AND OUTPUT IT TO DISK
*
      call memcd9(ierr)

*
* FINISH
*
  999 continue

      end
