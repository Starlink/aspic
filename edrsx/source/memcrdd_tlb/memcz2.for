      subroutine memcz2
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Performs the bulk of the interaction with the users environment,
*	then activates a routine to do the numerical calculations
*	required to analyse a previous run of memcrdd based on the 
*	information contained within an "analysis" file. This analysis
*	consists of returning the integrated flux within a user area
*	of the image, with an estimate of the error on the total flux.
*
*SOURCE
*       MEMCZ2.FOR in MEMCRDD.TLB
*
*COMMON USAGE
*   READ:
*	/B0_COM/,/B2_COM/,/B6_COM/,/ME_COM/,/ZZ_COM/
*   WRITE:
*	/ME_COM/,
*	/ZZ_COM/,
*		ZZ_tol	MEMSYS3 tolerance
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,gtbool
*       THIS PACKAGE (MEMCRDD.TLB):
*              memca1,memcd8,memcf6,memcg4,udiag
*       EDRS:
*              wrerr,getpar,lbgone
*
*STARLINK PARAMETERS
*	LOOP		If YES then multiple masks are processed
*	TOL		MEMSYS3 tolerance
*	Z2ERR1(error)	Accessed if MEMASK calculation was truncated
*			giving possible errors in answers.
*	Z2ERR2(error)	Accessed if MEMASK calculation failed giving 
*			errors in answers.
*	Z2ERR3(error)   Accessed if the ANALYSIS file was not generated
*			by a classic run of MEMCRDD.
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

* ... THE FITS DESCRIPTORS OF THE OUTPUT IMAGE
      include '(B0_COM)'

* ... CRDD FILE INFORMATION
      include '(B2_COM)'

* ... POINTERS TO INTERNAL FILES
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(ZZ_COM)'

*
* DECLARE LOCAL VARIABLES
*
      real	backfl	! Integrated background flux within mask 
      integer	crddf	! CRDD file index
      real	drho	! Standard deviation of rho
      integer	ierr	! Inherited status value
      integer	istat	! Temporary status value
      integer	ival	! Dummy integer argument
      logical	loop	! If .true. then multiple masks are processed
      real	pixsol	! Solid angle of a pixel in steradians
      character	prbuf*80! Buffer for screen output
      real	rho	! Total data sum of product of sky and mask
      real	sum	! Total data sum in user supplied mask image
      logical	udia	! Call udiag before doing any other analysis

*
* SET ERROR STATUS TO SUCCESS VALUE (MOST MEMC ROUTINES USE "INHERITED
* STATUS" ERROR SYSTEM)
*
      ierr=0

*
* GET AN ANALYSIS FILE FROM THE USER AND COPY THE INFORMATION IT 
* CONTAINS BACK INTO THE COMMON BLOCKS FROM WHICH IT CAME. 
*
      call memcd8(ierr)
      if(ierr.ne.0) goto 999

*
* TELL THE USER A BIT ABOUT THE PREVIOUS RUN (IRRESPECTIVE OF ILEVEL)
*
      call wruser(' ',istat)
      
      prbuf=' Analysing deconvolution of the following '//ZZ_typ//
     :      ' CRDD files:'
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

      write(prbuf,30) B0_fit(1)/15.0
  30  format('  RA of centre of output image is ',G13.6,' Hours')
      call lbgone(prbuf(35:))
      call wruser(prbuf,istat)

      write(prbuf,40) B0_fit(2)
  40  format('  DEC of centre of output image is ',G13.6,' degrees')
      call lbgone(prbuf(36:))
      call wruser(prbuf,istat)

*
* SEE IF USER WANTS TO CALL UDIAG TO EXAMINE THE INTERNAL WORK FILES
*
      udia=.true.
      call gtbool('UDIAG',.true.,udia,istat)
      if(udia) call udiag('Analysis')

*
* CHECK ANALYSIS FILE WAS GENERATED BY A CLASSIC OR COMBINATION RUN OF
* MEMCRDD
*
      if(ZZ_mth.eq.'HISTORIC') then
         call wrerr('Z2ERR3')
         ierr=1
         goto 999
      endif

*
* CALCULATE SOLID ANGLE OF ONE SKY PIXEL IN STERADIANS
*
      pixsol=ZZ_psz*ZZ_psz*0.84616E-7

*
* GET TOLERANCE VALUE FROM THE USER
*
      call getpar('TOL','REAL',1,0.0,1.0E6,.true.,ival,ZZ_tol,istat)

*
* SEE IF USER WANTS TO PROCESS MULTIPLE MASKS, OR JUST ONE
*
      loop=.false.
      call gtbool('LOOP',.true.,loop,istat)

*
* LOOP ROUND UNTIL NO MORE MASKS ARE GIVEN (IF LOOPING REQUIRED)
*
  50  continue

*
* GET THE NEXT MASK AND PUT IT IN FILE 2
*
      call memca1(2,B0_nps,B0_nls,sum,ierr)

*
* FIND INTEGRATED BACKGROUND FLUX WITHIN THE ANALYSIS MASK
*
      call memcf6(backfl,ME_st(ME_kb(2)),ME_st(B6_bac),pixsol,ierr)

*
* SMOOTH THE MASK WITH THE ICF
*
      call memcg4(2,ierr)

      if(ierr.eq.0) then

*
* CALL MEMSYS3 ROUTINE "MEMASK" TO EVALUATE THE TOTAL DATA SUM WITHIN 
* THE MASK AND THE ERROR ON THE ESTIMATE
*
         call wruser(' ',istat)
         call wruser('  Calling MEMSYS3 inference routine...',istat)
         call memask(0.0,0.0,ZZ_tol,ME_alp,ME_sig,rho,drho,istat)

*
* GIVE A WARNING BASED ON THE VALUE OF ERROR STATUS FROM MEMASK
* (SEE MEMSYS3 MANUAL)
*
         if(istat.eq.1) then
            call wrerr('Z2ERR1')

         else if(istat.ge.2) then
            call wrerr('Z2ERR2')

         endif

*
* DISPLAY THE DATA SUM (ICLUDING BACKGROUND) WITH ERROR (IN JANSKYS)
*
         write(prbuf,60) rho*pixsol+backfl,drho*pixsol
  60     format('  Integrated flux is ',G13.6,' +/- ',G13.6,' Jy')
         call wruser(' ',istat)
         call wruser(prbuf,istat)

*
* DISPLAY THE TOTAL DATA SUM OF THE MASK
*
         write(prbuf,80) sum
  80     format('  Mask has total data sum of ',G13.6)
         call wruser(prbuf,istat)

         call wruser(' ',istat)

*
* GO ROUND FOR NEXT MASK IF REQUIRED
*
         if(loop) goto 50
      endif


*
* FINISH
*
  999 continue

      end
