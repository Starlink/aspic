      subroutine memcz1
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations
*	required to continue a previous run of memcrdd based on the 
*	information contained within an "analysis" file.
*
*SOURCE
*       MEMCZ1.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B1_COM/,/B2_COM/,/D3_COM/,/ME_COM/
*   WRITE:
*	/ZZ_COM/
*		ZZ_aim - Target value of omega for MaxEnt procedure
*		ZZ_ilv - Amount of non-MEMSYS3 information to display
*		ZZ_lev - Amount of MEMSYS3 diagnostics to display
*		ZZ_mth - MEMSYS3 method; COMBINATION, CLASSIC or HISTORIC
*		ZZ_nit - Max. no. of MEM iterations to perform
*		ZZ_rat - Convergence rate limit
*		ZZ_stf - Name of stop file
*		ZZ_tol - Tolerance required of MEMSYS3 calculations
*	/B1_COM/,
*		B1_it	 The next MEM3 iteration to perform
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              gtstrn,gtfile,wruser
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca2,memcc0,memcc2,memcd5,memcd8,memce2,memcf5,memce5,
*	       memcf9
*       EDRS:
*              getpar,lbgone
*       INTERIM:
*              ctoi,rdkeyc
*
*STARLINK PARAMETERS
*	AIM 	- Target value of omega for MaxEnt procedure
*	HIRES	- The High resolution output image
*	ILEVEL	- Amount of non-MEMSYS3 information to display
*	LEVEL	- Amount of MEMSYS3 diagnostics to display
*	LOGFILE - Name of text file to receive MEMSYS3 diagnostics
*	METHOD	- MEMSYS3 method
*	NITER	- Max. no. of MEM iterations to perform
*	RATE	- Convergence rate limit
*	STOPFILE- Name of file which will cause MEMCRDD to stop
*	TOL	- Tolerance required of MEMSYS3 calculations
*
*VAX SPECIFICS
*       implicit none
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 27/9/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING ...

* ... THE FITS DESCRIPTORS OF THE OUTPUT IMAGE
      include '(B0_COM)'

* ... THE NEXT ITERATION NUMBER
      include '(B1_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... OFFSET ADDED TO DATA
      include '(D3_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      integer	crddf	! CRDD file index
      integer	ierr	! Inherited status value
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      character	logfil*40! Name of file to receive MEMSYS3 diagnostics
      integer	ltext	! Length of user-selected text string
      integer	memrun	! Indicates if a previous run of MEMSYS3 is
			! being continued, or a new one started.
      integer	method	! MEMSYS3 "method" number (See MEMSYS3 manual)
      integer	ncomm	! Position of selected string within option list
      character	prbuf*80! Buffer for screen output
      real	rval	! Dummy real argument
      character text*10	! Text of selected option

*
* SET ERROR STATUS TO SUCCESS VALUE (MOST MEMC ROUTINES USE "INHERITED
* STATUS" ERROR SYSTEM)
*
      ierr=0

*
* GET AN ANALYSIS FILE FROM THE USER AND COPY THE INFORMATION IT 
* CONTAINS BACK INTO THE COMMON BLOCKS FROM WHICH IT CAME. THIS 
* PUTS MEMCRDD BACK INTO THE STATE IT WAS IN AT THE END OF THE RUN 
* WHICH GENERATED THE ANALYSIS FILE.
*
      call memcd8(ierr)
      if(ierr.ne.0) goto 999

*
* TELL THE USER A BIT ABOUT THE PREVIOUS RUN (IRRESPECTIVE OF ILEVEL)
*
      call wruser(' ',istat)
      
      prbuf=' Continuing deconvolution of the following '//ZZ_typ//
     :      ' CRDD files:'
      call lbgone(prbuf(44:))
      call wruser(prbuf,istat)

      do crddf=1,B2_ncf
         call wruser('   '//B2_nam(crddf),istat)
      enddo

      write(prbuf,5) ME_mk
   5  format('  ( ',I10,' usable samples)')
      call lbgone(prbuf(5:))
      call wruser(prbuf,istat)
      call wruser(' ',istat)
      
      write(prbuf,10) ZZ_psz
  10  format('  Output image pixels are ',F5.2,' arcmins square')
      call lbgone(prbuf(27:))
      call wruser(prbuf,istat)

      write(prbuf,20) B0_nps,B0_nls
  20  format('  Output image size is ',I5,'x',I5,' pixels')
      call lbgone(prbuf(30:))
      call lbgone(prbuf(24:))
      call wruser(prbuf,istat)

      write(prbuf,30) B0_fit(1)/15
  30  format('  RA of centre of output image is ',G13.6,' Hours')
      call lbgone(prbuf(35:))
      call wruser(prbuf,istat)

      write(prbuf,40) B0_fit(2)
  40  format('  DEC of centre of output image is ',G13.6,' degrees')
      call lbgone(prbuf(36:))
      call wruser(prbuf,istat)

      write(prbuf,50) B1_it-1
  50  format('  ',I8,' iterations completed')
      call lbgone(prbuf(3:))
      call wruser(prbuf,istat)

*
* NOW ALLOW THE USER TO CHANGE CERTAIN SELECTED PARAMETERS VALUES. THE
* DEFAULT VALUES ARE THE VALUES READ FROM THE ANALYSIS FILE. THE 
* OTHER MEMCRDD PARAMETERS CANNOT BE CHANGED FROM THE VALUES THEY HAD 
* IN THE PREVIOUS RUN
*--------------------------------------------------------------------
*
* GET THE NAME OF A "STOP FILE" FOR USE IN BATCH PROCESSES (THE PROGRAM
* CHECK TO SEE IF THIS FILE EXISTS AT VARIOUS POINTS AND EXITS NEATLY
* IF IT DOES)
*
      call rdkeyc('STOPFILE',.true.,1,ZZ_stf,ival,istat)

*
* SEE HOW MUCH INFORMATION USER WANTS DISPLAYED ON THE SCREEN
*
      call getpar('ILEVEL','INTEGER',1,1.0,5.0,.true.,ZZ_ilv,rval,istat)

*
* AIM - SPECIFIES TERMINATION CRITERION
*
      call getpar('AIM','REAL',1,0.0,1.0E6,.true.,ival,ZZ_aim,istat)

*
* RATE: MAX STEP SIZE BETWEEN ITERATIONS
*
      call getpar('RATE','REAL',1,-1.0E6,1.0E6,.true.,ival,ZZ_rat,istat)

*
* LEVEL: AMOUNT OF MEMSYS3 DIAGNOSTIC INFORMATION TO DISPLAY. IF ANY
*        DIAGNOSTICS ARE REQUIRED OPEN A FILE ON UNIT 10
*
      ncomm=1
      if(ZZ_lev.eq.1) ncomm=2
      if(ZZ_lev.eq.2) ncomm=3
      if(ZZ_lev.eq.3) ncomm=4
      if(ZZ_lev.eq.10) ncomm=5
      if(ZZ_lev.eq.11) ncomm=6
      if(ZZ_lev.eq.12) ncomm=7
      if(ZZ_lev.eq.13) ncomm=8
      if(ZZ_lev.eq.20) ncomm=9
      if(ZZ_lev.eq.21) ncomm=10
      if(ZZ_lev.eq.22) ncomm=11
      if(ZZ_lev.eq.23) ncomm=12

      call gtstrn('LEVEL',.true.,'0,1,2,3,10,11,12,13,20,21,22,23.',1,
     :             ncomm,text,ltext,istat)
      call ctoi(text,ZZ_lev,istat)

      if(ZZ_lev.gt.0) then
         logfil='SYS$OUTPUT'
         call gtfile('LOGFILE',PR_uni,.true.,'NEW',logfil,istat)
         if(istat.ne.0) ZZ_lev=0
      endif

*
* NITER: THE MAXIMUM NUMBER OF ITERATIONS OF THE MEM ALGORITHM TO 
* PERFORM. THIS INCLUDES ITERATIONS PERFORMED IN THE PREVIOUS RUN.
*
      call getpar('NITER','INTEGER',1,0.0,1.0E6,.true.,ZZ_nit,rval,
     :             istat)

*
* TOL: MEMSYS3 TOLERANCE
*
      call getpar('TOL','REAL',1,0.0,1.0E6,.true.,ival,ZZ_tol,istat)

*
* IF A COMBINATION OF CLASSIC AND HISTORIC MEMSYS3 IS BEING USED, 
* CALL MEMCD5 TO PROGRESS CALLING MEMSYS3 IN CLASSIC MODE EVERY FEW 
* ITERATIONS. 
*
      if(ZZ_mth.eq.'COMBINATION') then
         call memcd5(B1_it,.false.,ierr)

      else

*
* SET UP MEMSYS3 METHOD NUMBER
*
         if(ZZ_mth.eq.'CLASSIC') then
            method=12
         else
            method=10
         endif

*
* CALL MEMCC2 TO INITIATE MEMSYS3. SINCE THE CURRENT FUNCTION IS TO
* CONTINUE A HIGH RES RUN, ENTER MEMSYS3 WITH THE ITERATION NO. AT ONE 
* MORE THAN THE FINAL ITERATION PERFORMED BY THE PREVIOUS RUN, AND 
* MEMRUN=1 (SEE MEMSYS3 MANUAL). EACH CALL TO MEM3 WITHIN MEMCC2 USES 
* THE SAME METHOD (SET ABOVE).
*
         memrun=1
         call memcc2(B1_it,method,memrun,ierr)
      endif

*
* CREATE AN IMAGE WHICH CAN BE USED AS A BETTER MODEL FOR THE NEXT
* RUN OF MEMCRDD
*
      call memce2(ierr)

*
* CALCULATE THE COVERAGE IMAGE AND STORE IN FILE 2
*
      call memce5(ME_st(ME_kb(2)),.true.,ierr)

*
* IF REQUIRED, CREATE A NEW BACKGROUND IMAGE WHICH CAN BE USED ON THE 
* NEXT RUN OF MEMCRDD
*
      call memcf9(ierr)

*
* CONVERT THE FINAL IMAGE INTO A MORE CONVENIENT FORM BY ADDING THE
* BACKGROUND IMAGE BACK ON AND SETTING ALL PIXELS WITH VERY LOW COVERAGE
* TO AN INVALID VALUE. THE COVERAGE IMAGE IS LEFT IN FILE 2.
*
      call memcf5(ierr)
      call memca2('HIRES',1,.false.,ierr)

*
* FINISH
*
  999 continue

      end
