      subroutine memcd5(iter0,init,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Activates the main MEMSYS3 routine, MEM3, using a combination
*	of CLASSIC and HISTORIC to reduce run time.
*
*SOURCE
*       MEMCD5.FOR in MEMCRDD.TLB
*
*METHOD
*	Loops round calling MEM3 until either:
*	1) Convergence is reached
*	2) Iteration limit reached
*	3) Stop file found
*	4) User interupt (control-c) detected
*
*	HISTORIC calls are made with a very low value of AIM to
*	force MEMSYS3 to follow the MAXent trajectory further than
*	it would do normally using HISTORIC mode. These HISTORIC calls
*	are much faster than a CLASSIC call since none of the 
*	probabalistic quantities are calculated.
*
*	The first classic call is made when the value of chi-squared
*	returned by MEM3 indicates that the historic result has been
*	achieved. The next classic call is performed after half the 
*	no. of iterations taken to reach the historic result. From the
*	difference in omega values returned by these first two classic
*	calls, a guess is made as too when a classic call would give
*	an omega half way to the target value. This guess assumes that
*	omega rises linearly with iteration no. after the historic 
*	result is achieved (except that the first iteration after a
*	classic call seems not to contribute much to the progress). 
*	The gap between succesive classic calls is reduced as the 
*	target value of omega is approached.
*	
*	Initially, all classic calls calculate diagnostics only and do
*	not alter the current reconstruction. This is done to avoid
*	disrupting the convergence process. When the gap between classic
*	calls reduces to 1 (i.e. all calls are classic), a single 
*	classic call is made to re-start the convergence (from the 
*	current reconstruction) using classic instead of historic mode.
*	Succesive classic calls then progress the convergence towards
*	the target omega value, as well as calculating the diagnostics.
*
*	If the user intrupts the process, he is given the chance to
*	write out any of the MEMSYS3 internal files to disk in the
*	form of a BDF file. He is then asked whether to continue the
*	iteration process, or to terminate immediately.
*       
*ARGUMENTS       
*   INPUT:
*	iter0	integer		The iteration no. to start at.
*	init	logical		If true then initialise a new MEMSYS3 run
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	iter0	integer		The next iteration no. (for use if
*				the MEM process is to be continued)
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*       /A8_COM/,/B0_COM/,/B5_COM/,/B6_COM/,
*	/ME_COM/,/ZZ_COM/,/CONTROLC/ (set in routine CTRLC)
*   WRITE:
*	/D5_COM/,
*		D5_gap	  No. of historic iterations before the next 
*			  classic iteration
*		D5_nxt	  Iteration at which next classic call will be
*			  made
*		D5_oml	  Rescaled termination criterion after previous
*			  classic call 
*		D5_run    MEMSYS3 argument "MEMRUN". Indicates what MEM3
*			  will do when called.
*		D5_sta*11 Indicates what stage the convergence process
*			  has reached.
*	/ME_COM/,
*		ME_alp	Regularisation constant (see MEMSYS3 manual)
*		ME_bet	Modified regularisation constant (see MEMSYS3 manual)
*		ME_sig  Scale factor applied to noise estimates
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,exists,gtbool
*       THIS PACKAGE (MEMCRDD.TLB):
*              udiag,memcc6,memcd0,memcf7,memcf8,memcg4
*       EDRS:
*              lbgone,wrerr
*       INTERIM:
*              cnpar,wrkeyr
*
*STARLINK PARAMETERS
*	STOP		True if user wants to stop after an interupt
*	D5ERR1(error)	Accessed if the stop file is found.
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

* ... OUTPUT VALUES FROM THIS ROUTINE
      include '(D5_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	iter0,ierr
      logical	init

*
* DECLARE LOCAL VARIABLES
*
      real	aim	! Target value of omega (see memsys3 manual)
      logical	break	! True if control-c was pressed
      real	chisq	! Chi squared statistic
      real	good	! No. of good measurements
      integer	group	! Group number.
      integer	istat	! Temporary status value
      integer	iter	! Iteration count
      integer	method	! MEMSYS3 method no. (see MEMSYS3 manual)
      integer	newgap	! Candidate value for next classic gap
      real	omega	! Rescaled termination criterion
      character	prbuf*80! Buffer for screen output
      real	prob	! Log(probability of getting given data)
      real	s	! Entropy of current reconstruction
      integer	start	! First iteration to perform
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
         call wruser('Entering MEMSYS3 (V2.0) loop...',istat)
      endif

*
* IF A NEW RUN IS BEING INITIALISED, SET THE ITERATION NO. AT WHICH 
* THE NEXT CLASSIC CALL WILL BE MADE TO AN UNREACHABLE NUMBER. ALSO 
* FLAG THAT THE HISTORIC RESULT HAS NOT YET BEEN REACHED, SET THE 
* PREVIOUS VALUE OF OMEGA TO ZERO, AND SET D5_RUN TO INDICATE 
* THAT A NEW MEMSYS3 RUN IS TO BE STARTED
*
      if(init) then
         D5_gap=ZZ_nit
         D5_nxt=iter0+D5_gap
         D5_sta='PREHISTORIC'
         D5_oml=0.0
         D5_run=0
      endif   

*
* LOOP ROUND CALLING MEMSYS3 MAIN ROUTINE UNTIL CONVERGENCE IS REACHED,
* THE USER INTERUPTS THE PROCESS, OR THE MAXIMUM NO. OF ITERATIONS IS 
* REACHED.
*
      stop=.false.
      start=iter0
      do iter=start,ZZ_nit
         if(.not.stop) then

*
* STORE NEXT ITERATION NUMBER
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
* IF THE TIME HAS COME FOR THE NEXT CLASSIC CALL, PREPARE MEM3 FOR 
* CLASSIC METHOD, AND USE SUPPLIED VALUE FOR "AIM". ALSO IF THE VALUE 
* OF OMEGA RETURNED BY THE LAST CLASSIC CALL WAS GREATER THAN ONE, THEN 
* CONTINUE CALLING CLASSIC MEMSYS3 UNTIL OMEGA FALLS TO 1.0. NB, UNLESS
* THE CONVERGENCE HAS GOT VERY CLOSE TO TERMINATION, THEN A 
* "DIAGNOSTICS ONLY" CALL IS MADE IN ORDER TO PREVENT THE CONVERGENCE
* PROCESS FROM BEING DISRUPTED.
*
            if(iter.eq.D5_nxt.or.D5_oml.gt.ZZ_aim) then
               method=12
               aim=ZZ_aim
               if(D5_sta.ne.'CREEP') D5_run=3

*
* OTHERWISE PERFORM ANOTHER HISTORIC CALL
*
            else
               method=10
               aim=0.0
            endif

*
* IF CONTINUING A MEMRUN AND THE "TEST" DIAGNOSTIC IS PARTICULARLY 
* HIGH OR LOW, CHANGE RATE TO BRING IT HOPEFULLY BACK INTO THE RANGE 
* 0.5 TO 0.05
*
            if(iter.gt.start.and.D5_run.eq.1.and.ZZ_rat.lt.0.0) 
     :           call memcf8(ZZ_rat,test)

*
* CALL MEM3
*
            call mem3(method,ZZ_lev,D5_run,1,5,aim,abs(ZZ_rat),0.0,0.0,
     :                ZZ_tol,ME_alp,ME_bet,s,test,chisq,ME_sig,prob,
     :                good,omega,istat)

*
* SEE IF CONVERGENCE IS REACHED. NB, THE CONJUGATE GRADIENT ERROR CODES
* CAN BE IGNORED IF EVERYTHING ELSE INDICATES CONVERGENCE HAS BEEN
* REACHED (HISTORIC CALLS WILL NEVER CONVERGE DUE TO USE OF AIM=0.0).
*
            if(mod(istat,10000).eq.0) stop=.true.

*
* SET D5_RUN TO INDICATE THAT THE NEXT ITERATION WILL CONTINUE A 
* PREVIOUS RUN WITH THE SAME METHOD (CLASSIC OR HISTORIC).
*
            D5_run=1

*
* SEE IF THE HISTORIC RESULT (CHI-SQUARED=NO. OF DATA VALUES) HAS BEEN 
* REACHED. IF IT HAS INDICATE THAT THE NEXT ITERATION WILL BE A CLASSIC 
* ITERATION
*
            if( D5_sta.eq.'PREHISTORIC') then
               if(chisq.le.ME_mk) then
                  D5_nxt=iter+1
                  D5_sta='FIND GAP'
               endif

*
* IF THE FIRST CLASSIC CALL HAS JUST BEEN PERFORMED, SET THE GAP BEFORE
* THE NEXT CLASSIC CALL TO HALF THE NUMBER OF ITERATIONS NEEDED TO 
* REACH THE HISTORIC RESULT
*
            else if(D5_sta.eq.'FIND GAP') then
               D5_gap=max( 1.0, real((iter-start)*0.5)+1.0 )
               D5_sta='SET GAP'

*
* IF THE SECOND CLASSIC CALL HAS JUST BEEN PERFORMED, GUESS HOW MANY
* ITERATIONS WILL PASS BEFORE OMEGA IS HALF WAY TOWARDS THE TARGET
* VALUE. THIS ASSUMES A LINEAR RISE OF OMEGA WITH ITERATION NUMBER, 
* EXCEPT THAT THE ITERATION IMMEDIATELY FOLLOWING A "DIAGNOSTICS ONLY" 
* CLASSIC CALL DOES NOT CHANGE OMEGA.
*
            else if(D5_sta.eq.'SET GAP') then
               if(iter.eq.D5_nxt) then
                  if(omega.gt.D5_oml) then
                     newgap=nint( 1.0 + (D5_gap-1)*
     :                      0.5*(ZZ_aim-omega)/(omega-D5_oml) )
                     D5_gap=max(1,newgap)
                  else
                     D5_gap=1
                  endif
                  D5_sta='RUN'
               endif

*
* IF A SUBSEQUENT CLASSIC CALL HAS JUST BEEN PERFORMED, UPDATE THE GAP
* BEFORE THE NEXT CLASSIC CALL. THIS IS DONE SO THAT AS THE TARGET 
* OMEGA IS APPROACHED THE INTERVAL BETWEEN CLASSIC CALLS BECOMES LESS. 
* ALSO, THE GAP BETWEEN CLASSIC CALLS IS NEVER ALLOWED TO INCREASE.
*
            else if(D5_sta.eq.'RUN') then
               if(iter.eq.D5_nxt) then
                  if(omega.gt.D5_oml) then
                     newgap=(D5_gap-1)*0.5*(ZZ_aim-omega)/(omega-D5_oml)
                     D5_gap=max(1,min(newgap+1,D5_gap))
                  endif
               endif
            endif

*
* IF GAP SINKS TO THE VALUE OF 2 THEN A "DIAGNOSTICS ONLY" CALL FOLLOWED
* BY A HISTORIC CALL IS PERFORMED. THIS IS LESS EFFICIENT (PROBABLY) 
* THEN A SINGLE "NON-DIAGNOSTICS" CLASSIC CALL.
* THEREFORE STOP USING "DIAGNOSTICS ONLY" CLASSIC CALLS AND INDICATE
* THAT MEMSYS3 IS TO BE RE-INITIALISED FOR USING CLASSIC MODE TO 
* PROGRESS THE CONVERGENCE (PREVIOUSLY HISTORIC MODE HAS ALWAYS BEEN 
* USED).
*
            if(D5_gap.le.2.and.D5_sta.ne.'CREEP') then
               D5_sta='CREEP'
               D5_run=2
               D5_gap=1
            endif

*
* IF ANY CLASSIC CALL HAS JUST BEEN PERFORMED, UPDATE THE ITERATION NO.
* AT WHICH THE NEXT CLASSIC CALL WILL BE PERFORMED, AND SAVE THE VALUE
* OF OMEGA
*
            if(iter.eq.D5_nxt) then
               D5_nxt=iter+D5_gap
               D5_oml=omega
            endif

*
* IF REQUIRED, WRITE OUT MEMCRDD DIAGNOSTICS. THESE ARE IN ADDITION TO
* ANY DIAGNOSTICS WHICH MEM3 MAY HAVE PRODUCED. NB, NORMALISED 
* CHI-SQUARED IS ONLY USEFUL IN HISTORIC MODE.
*
            if(ZZ_ilv.ge.2) then

               write(prbuf,20) omega
  20           format('    Current value of OMEGA       : ',G13.6)
               call wruser(prbuf,istat)               

               write(prbuf,30) s
  30           format('    Current value of ENTROPY     : ',G13.6)
               call wruser(prbuf,istat)               

               write(prbuf,40) test
  40           format('    Current value of TEST        : ',G13.6)
               call wruser(prbuf,istat)               

               if(method.eq.10) then
                  write(prbuf,50) chisq/ME_mk
  50              format('    Current value of norm. CHI-SQ: ',G13.6)
                  call wruser(prbuf,istat)               
               endif

               write(prbuf,60) ME_ntr
  60           format('    No. of transforms performed  : ',I7)
               call wruser(prbuf,istat)               

               write(prbuf,70) abs(ZZ_rat)
  70           format('    Value used for parameter RATE: ',G13.6) 
               call wruser(prbuf,istat)

            endif


*
* CHECK TO SEE IF A FILE EXISTS IN THE CURRENT DIRECTORY WITH THE
* NAME OF THE "STOP FILE" GIVEN BY THE USER. IF IT DOES, RETURN THE 
* CURRENT RECONSTRUCTION TO THE USER AND STOP. THIS FACILITY CAN BE USED
* TO STOP A BATCH JOB WHICH IS GOING ON LONGER THAN EXPECTED, WITHOUT
* LOSING ALL THE CPU.
*
               call exists(ZZ_stf,istat)
               if(istat.eq.0) then
                  call wrerr('D5ERR1')
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
* CALL UDIAG ROUTINE TO EXAMINE INTERNAL DATA, ETC
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
* BLUR THE FINAL RECONSTRUCTION WITH THE ICF
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
         call wruser('Leaving MEMSYS3 (V2.0) loop',istat)
      endif

*
* FINISH
*
  999 continue

      end
