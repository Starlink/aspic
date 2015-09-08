      subroutine memcz6
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations 
*	required to destripe IRAS data.
*
*SOURCE
*       MEMCZ6.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   READ
*	/B2_bnd/,/B6_COM/,/ME_COM/
*   WRITE:
*	/ZZ_COM/
*		ZZ_deg - Deglitching strength required (0.0 - 1.0)
*		ZZ_fld - Noise level in background regions given by user
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_typ - Data type 'AO' or 'SURVEY'
*		ZZ_nit - No. of destriping iterations
*		ZZ_psz - Size of worki image pixels
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn,gtwork
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,memca5,memca6,memca7,memca8,memcb0,memcb2,memce6,
*	       memce7
*       EDRS:
*              getpar
*
*STARLINK PARAMETERS
*	DATATYPE- Data type 'AO' or 'SURVEY'
*	ILEVEL	- Amount of non-MEMSYS3 information to display
*	NITER	- No. of destriping iterations
*	PIXSIZE - Pixel size for work image
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 19/12/89
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
* INCLUDE COMMON BLOCKS HOLDING ...

* ... CRDD FILE INFO
      include '(B2_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 INFORMATION
      include '(ME_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      logical	break	! Interupt flag
      real	defpix(4)! Default pixel sizes for work image
      integer	ierr	! Inherited status value
      integer	ipw	! Pointer to real work space
      integer	istat	! Temporary status value
      integer	iter	! Current destriping iteration
      integer	ival	! Dummy integer argument
      integer	ltext	! Length of user-selected text string
      integer	ncomm	! Position of selected string within option list
      integer	offset	! Offset into a data set
      real	rval	! Dummy real argument

      data	defpix/0.25,0.25,0.5,1.0/
      common	/CONTROLC/ break

*
* SET ERROR STATUS TO SUCCESS VALUE (MOST MEMC ROUTINES USE "INHERITED
* STATUS" ERROR SYSTEM)
*
      ierr=0

*
* SEE HOW MUCH INFORMATION USER WANTS DISPLAYED ON THE SCREEN
*
      ZZ_ilv=1
      call getpar('ILEVEL','INTEGER',1,0.0,3.0,.true.,ZZ_ilv,rval,istat)

*
* SEE IF USER WANTS TO USE AO CRDD OR SURVEY CRDD
*
      ncomm=2
      call gtstrn('DATATYPE',.true.,'AO,SURVEY.',1,ncomm,ZZ_typ,ltext,
     :             istat)

*
* GET NO. OF DESTRIPING ITERATIONS TO PERFORM
*
      ZZ_nit=3
      call getpar('NITER','INTEGER',1,1.0,1.0E6,.true.,ZZ_nit,rval,
     ;             istat)

*
* DISPLAY CALCULATED NOISE LEVELS
*
      ZZ_fld=0.0

*
* EXTREME DEGLITCHING IS USED TO REMOVE POINT SOURCES.
*
      ZZ_deg=1.0

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
* GET PIXEL SIZE AND FITS DESCRIPTORS FOR WORKING IMAGE
*
      ZZ_psz=defpix(B2_bnd)
      call getpar('PIXSIZE','REAL',1,0.0,1.0E6,.true.,ival,ZZ_psz,istat)

      call memcb0(ierr)
      
*
* CALL MEMCE6 TO SET UP THE DATA STRUCTURES REQUIRED TO PRODUCE THE
* DESTRIPED DATA
*
      call memce6(ierr)
      if(ierr.ne.0) goto 999

*
* CALL MEMCE7 TO PERFORM REQUIRED NO. OF DESTRIPING ITERATIONS
*
      call gtwork('WORK','REAL',IR_dts*PR_crd,ipw,ierr)
      do iter=1,ZZ_nit
         call memce7(%val(ipw),ierr)
         if(break) goto 10
      enddo

  10  continue

*
* CONVERT THE DATA STORED INTERNALLY IN JY/ST TO JY
*
      do offset=0,ME_mk-1
         ME_st(ME_kb(21)+offset)=ME_st(ME_kb(21)+offset)*
     :                           ME_st(B6_sol+offset)
      enddo

*
* PRODUCE OUTPUT DESTRIPED CRDD FILES
*
      call memca2(' ',21,.false.,ierr)

*
* FINISH
*
  999 continue

      end
