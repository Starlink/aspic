      subroutine memcc2(iter0,method,memrun,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Activates the main MEMSYS3 routine, MEM3 (or the Maximum 
*       Correlation Method routine, MCM).
*
*SOURCE
*       MEMCC2.FOR in MEMCRDD.TLB
*
*METHOD
*	Loops round calling MEM3 (or MCM) until either:
*	1) Convergence is reached
*	2) Iteration limit reached
*	3) Stop file found
*	4) User interupt (control-c) detected
*
*	If the user intrupts the process, he is given the chance to
*	write out any of the internal files to disk in the
*	form of a BDF file. He is then asked whether to continue the
*	iteration process, or to terminate immediately.
*       
*ARGUMENTS       
*   INPUT:
*	iter0	integer		The iteration no. to start at. This is
*				used if continuing from a previous run.
*	method	integer		The MEMSYS3 method no. to use. A value
*				of -1 causes a simple MCM system to be
*				used instead of MEM.
*	memrun	integer		0 for a new run, 1 to continue an old run
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	iter0	integer		The next iteration no. (for use if
*				a deconvolution is being continued)
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*       /A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,
*	/ME_COM/,/ZZ_COM/,/CONTROLC/ (set in routine CTRLC)
*   WRITE:
*	/ME_COM/,
*		ME_alp	Regularisation constant (see MEMSYS3 manual)
*		ME_bet	Modified regularisation constant (see MEMSYS3 manual)
*		ME_sig  Scale factor applied to noise estimates
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,exists,gtbool
*	THIS PACKAGE (MEMCRDD.TLB):
*	       udiag,memcc6,memcd0,memcf7,memcf8,memcg4
*       EDRS:
*              lbgone,wrerr
*       INTERIM:
*              cnpar,wrkeyr
*
*STARLINK PARAMETERS
*	STOP		True if user wants to stop after an interupt
*       NSCALE(output)  The scale factor applied to noise estimates
*	C2ERR1(error)	Accessed if the stop file is found.
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 6/10/89
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'
      include 'UTILITIES(IR_PAR)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... NUMBER OF SAMPLE GROUPS.
      include '(A8_COM)'

* ... OUTPUT IMAGE DIMENSIONS.
      include '(B0_COM)'

* ... SAMPLE GROUP EXTENTS.
      include '(B5_COM)'

* ... POINTERS TO INTERNAL FILES.
      include '(B6_COM)'

* ... MEMSYS3 COMMON BLOCKS
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	iter0,method,memrun,ierr

*
* DECLARE LOCAL VARIABLES
*
      logical	break	! True if control-c was pressed
      real	chisq	! Chi squared statistic
      real	good	! No. of good measurements
      integer	group	! Group number.
      integer	istat	! Temporary status value
      integer	iter	! Iteration count
      real	omega	! Rescaled termination criterion
      character	prbuf*80! Buffer for screen output
      real	prob	! Log(probability of getting given data)
      real	s	! Entropy of current reconstruction
      integer	start	! First iteration no.
      logical   stop	! True if no more iterations need be done
      real	test	! Indicates accuracy of entropy maximisation

*
* DECLARE COMMON BLOCK HOLDING CONTROL-C BREAK FLAG
*
      common /CONTROLC/ break

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQUIRED, BLUR THE PSFS BY A GAUSSIAN SPOT. THIS ACTS AS AN 
* INTRINSIC CORRELATION FUNCTION IN OPUS AND TROPUS.
*
      if(ZZ_cfl.gt.0.0) then

*
* GENERATE THE SPOT
*
         call memcf7(ZZ_cfl,ME_st(B6_wk1),ierr)

*
* TAKE ITS FFT
*
         call memcc6(PR_fwd,ME_st(B6_wk1),ME_st(B6_wk3),ME_st(B6_wk2),
     :               B0_nps,B0_nls,ierr)

*
* LOOP ROUND EACH USED GROUP, MULTIPLYING THE PSF BY THE GAUSSIAN FFT
*
         do group=1,A8_ngp
            if(B5_lgs(group).ne.0)  then
               call memcd0(ME_st(B6_psf(group)),ME_st(B6_wk3),
     :                     ME_st(B6_psf(group)),B0_nps,B0_nls)
            endif
         enddo

      endif


*
* IF REQUIRED, TELL USER WHAT IS GOING ON
*
      if(ZZ_ilv.ge.2) then
         call wruser(' ',istat)
         if(method.ne.-1) then
            call wruser('Entering MEMSYS3 (V2.0) loop...',istat)
         else
            call wruser('Entering MCM loop...',istat)
         endif
      endif

*
* LOOP ROUND CALLING MEMSYS3 MAIN ROUTINE (OR MCM) UNTIL CONVERGENCE IS
* REACHED, THE USER INTERUPTS THE PROCESS, OR THE MAXIMUM NO. OF 
* ITERATIONS IS REACHED.
*
      start=iter0
      stop=.false.

      do iter=start,ZZ_nit
         if(.not.stop) then

*
* STORE NEXT ITERATION NUMBER (IN CASE THE JOB IS INTERUPTED DURING THIS
* ITERATION)
*
            iter0=iter+1

*
* TELL USER WHICH ITERATION THIS IS
*
            if(ZZ_ilv.ge.2) then
               write(prbuf,10) iter
  10           format('  Iteration ',I4)
               call lbgone(prbuf(13:))
               call wruser(' ',istat)
               call wruser(prbuf,istat)
            endif

*
* IF REQUIRED, CALL MAIN MEMSYS3 ROUTINE "MEM3"
*
            if(method.ne.-1) then
               call mem3(method,ZZ_lev,memrun,1,5,ZZ_aim,abs(ZZ_rat),
     :                   0.0,0.0,ZZ_tol,ME_alp,ME_bet,s,test,chisq,
     :                   ME_sig,prob,good,omega,istat)

*
* OTHERWISE CALL THE MCM ROUTINE.
*
            else
               call mcm(memrun,chisq,istat)
            endif

*
* SEE IF CONVERGENCE IS REACHED. NB, THE CONJUGATE GRADIENT ERROR CODES
* CAN BE IGNORED IF EVERYTHING ELSE INDICATES CONVERGENCE HAS BEEN
* REACHED
*
            if(mod(istat,10000).eq.0) stop=.true.

*
* IF REQUIRED, WRITE OUT MEMCRDD DIAGNOSTICS. THESE ARE IN ADDITION TO
* ANY DIAGNOSTICS WHICH MEM3 MAY HAVE PRODUCED. NB, NORMALISED 
* CHI-SQUARED IS ONLY USEFUL IN HISTORIC MODE.
*
            if(ZZ_ilv.ge.2) then

               if(method.ne.-1) then

                  write(prbuf,20) omega
  20              format('    Current value of OMEGA       : ',G13.6)
                  call wruser(prbuf,istat)               

                  write(prbuf,30) s
  30              format('    Current value of ENTROPY     : ',G13.6)
                  call wruser(prbuf,istat)               

                  write(prbuf,40) test
  40              format('    Current value of TEST        : ',G13.6)
                  call wruser(prbuf,istat)               

                  if(mod(method,10).eq.0) then
                     write(prbuf,50) chisq/ME_mk
  50                 format('    Current value of norm. CHI-SQ: ',G13.6)
                     call wruser(prbuf,istat)               
                  else
                     write(prbuf,51) prob
  51                 format('    Log(probability)             : ',G13.6)
                     call wruser(prbuf,istat)
                  endif

                  write(prbuf,60) ME_ntr
  60              format('    No. of transforms performed  : ',I7)
                  call wruser(prbuf,istat)               
               
                  write(prbuf,70) abs(ZZ_rat)
  70              format('    Value used for parameter RATE: ',G13.6) 
                  call wruser(prbuf,istat)

*
* NOW PRODUCE MCM DIAGNOSTIC
*
               else
                  write(prbuf,50) chisq/ME_mk
                  call wruser(prbuf,istat)               
               endif

            endif

*
* IF CONTINUING A MEM RUN AND THE "TEST" DIAGNOSTIC IS PARTICULARLY 
* HIGH OR LOW, CHANGE RATE TO BRING IT HOPEFULLY BACK INTO THE RANGE 
* 0.5 TO 0.05 
*
            if(method.ne.-1.and.memrun.eq.1.and.ZZ_rat.lt.0.0) 
     :         call memcf8(ZZ_rat,test)

*
* SET MEMRUN TO INDICATE THAT THE NEXT CALL TO MEM3 WILL BE A 
* CONTINUATION OF A PREVIOUS RUN
*
            memrun=1

*
* CHECK TO SEE IF A FILE EXISTS IN THE CURRENT DIRECTORY WITH THE
* NAME OF THE "STOP FILE" GIVEN BY THE USER. IF IT DOES, RETURN THE 
* CURRENT RECONSTRUCTION TO THE USER AND STOP. THIS FACILITY CAN BE USED
* TO STOP A BATCH JOB WHICH IS GOING ON LONGER THAN EXPECTED, WITHOUT
* LOSING ALL THE CPU.
*
            call exists(ZZ_stf,istat)
            if(istat.eq.0) then
               call wrerr('C2ERR1')
               stop=.true.
            endif

*
* INTERACTIVE PROCESSES MAY BE INTERRUPTED BY THE USER. THE BREAK FLAG
* IS SET TRUE WHEN THIS HAPPENS. 
*
            if(break) then

*
* RESET THE BREAK FLAG
*
               break=.false.

*
* CALL UDIAG ROUTINE TO EXAMINING INTERNAL DATA ETC
*
               call udiag(' ')

*
* SEE IF USER WANTS TO STOP THE MEM LOOP
*
               call cnpar('STOP',istat)
               call gtbool('STOP',.true.,stop,istat)

            endif

*
* UPDATE THE ANALYSIS FILE IN CASE THE JOB CRASHES
*
            call memca3(ierr)
               
*
* DO NEXT ITERATION
*
         endif
      enddo


*
* BLUR THE FINAL RECONSTRUCTION WITH THE ICF. NB, THE ANALYSIS FILE 
* CONTAINS THE UN-BLURRED "HIDDEN" RECONSTRUCTION WHICH MUST BE USED IN 
* "ANALYSIS" MODE.
*
      call memcg4(1,ierr)

*
* IF REQUIRED TELL USER WHAT THE NOISE SCALING FACTOR IS
*
      if(ZZ_ilv.ge.2) then

         if(ME_sig.ne.1.0) then
            write(prbuf,80) ME_sig
   80       format('   MEMSYS3 chose to scale noise estimates by ',
     :             G13.6)
            call wruser(prbuf,istat)         
         endif

         if(method.ne.-1) then
            call wruser('Leaving MEMSYS3 (V2.0) loop',istat)
         else
            call wruser('Leaving MCM loop',istat)
         endif

      endif

*
* FINISH
*
  999 continue

      end
