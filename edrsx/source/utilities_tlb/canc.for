      subroutine canc(prim,ref,out,error,nsamp,niter,w,hw,u,rinval,
     :                power,ierr)
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Filters the primary input array using the "Correlated data
*	Adaptive Noise Cancelling" (CANC) method, described by
*	Hattingh (Computers & Geosciences vol 14, p467-480, 1988).
*
*SOURCE
*       CANC.FOR in UTILITIES.TLB
*
*METHOD
*       The CANC method aims to extract only those features from the 
*	primary input array which are correlated with features in the
*	reference input array. See the above reference for a detailed
*	account of the algorithm.
*	The primary and reference inputs must contain no invalid pixels.
*       
*ARGUMENTS       
*   INPUT:
*	prim(nsamp)   real	The primary input array.
*	ref(nsamp)    real	The reference input array. This array
*			 	should be substantially flat, with a 
*				background value equal to zero.
*	nsamp	      integer	No. of data samples in each data array.
*	niter	      integer	No. of CANC iterations to perform
*	w(-hw:+hw)    real	Work space to hold the filter
*	hw	      integer	Half length of the filter in samples
*	u 	      real	Convergence factor. If this is equal to
*				zero on entry, then the routine will 
*				calculate and use a new value.
*	rinval	      real	Flag for missing data values
*	ierr	      integer	Inherited status
*	power	      real	A value controlling how much the 
*				algorithm excludes sharp features which
*                               could otherwise cause ringing. The output
*				values corresponding to such features 
*				are set equal to the primary values. A 
*				value of 0 for POWER causes the sharp
*				features to be included in the filtering
*				process. Larger values cause them to be
*			        excluded more. A value of 2.0 is a good
*				place to start.
*   OUTPUTS:
*       out(nsamp)    real	The output filtered data. The 
*				background will match that of the 
*				reference array.
*	error(nsamp)  real	The difference between the primary input 
*				and the output.
*	u	      real	The value actually used for the 
*				convergence factor.
*	ierr	      integer	Error status: 0 - success
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 11/7/90
*-------------------------------------------------------------------
      implicit none

*
* DECLARE ARGUMENTS
*
      integer	nsamp,niter,hw,ierr
      real	prim(nsamp),ref(nsamp),out(nsamp),error(nsamp)
      real	rinval,w(-hw:hw),u,power

*
* DECLARE LOCAL VARIABLES
*
      real	aa	! Value controlling mix of primary and filtered
			! data in output for sharp features.
      real	e2sum	! Sum of squared error values.
      real	errlim	! Limit of valid error values
      real	errrms	! RMS error value.
      real	errval	! Error between filtered ref. data and prim. data
      real	gain	! The current filter gain.
      real	glimit	! Largest safe value for filter gain
      integer	i	! Index into current filter
      integer	iter	! Current CANC iteration number
      real	limit	! Highest safe value for normalised gain
      integer	mxstrt	! Max. no. of times the algorithm can be 
			! re-started with a new u value.
      integer	nerr	! No. of valid error values.
      logical	newu	! True if a new value for u is being found.
      real	nsigma	! Max. no. of sigma expected for valid data
      integer	nstart	! The no. of times the algorithm has been 
			! re-started with a new u value.
      real	outval	! Output data value
      real	prmrms	! RMS value of primary data
      real	prmval	! Primary data value
      real	refrms	! RMS value of reference data
      real	refval	! Reference data value
      real	safe	! Filter gain value considered to be safe to use.
      integer	samp	! Sample counter for referencing data arrays
      real	target	! Target value for the normalised gain.
			! Used if a new value is being found for the 
			! convergence factor, u.
      real	unew	! The new value for the convergence factor
      real	wval	! A filter element value
      real	fact

      parameter	(limit=2.5,  mxstrt=20, target=0.6, nsigma=2.0)

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* FILL THE OUTPUT AND ERROR ARRAYS WITH BLANK VALUES, AND CHECK INPUTS 
* FOR BLANK PIXELS. ALSO FIND THE RMS VALUE OF THE PRIMARY AND 
* REFERENCE DATA ARRAYS.
*
      prmrms=0.0
      refrms=0.0

      do samp=1,nsamp

         out(samp)=rinval
         error(samp)=rinval
         prmval=prim(samp)
         refval=ref(samp)

         if(prmval.eq.rinval.or.refval.eq.rinval) then
            call wrerr('CANCINV')
            ierr=1
            goto 999
         else
            prmrms=prmrms+prmval*prmval
            refrms=refrms+refval*refval
         endif

      enddo

      prmrms=sqrt(prmrms/nsamp)
      refrms=sqrt(refrms/nsamp)

*
* FIND THE LARGEST SAFE VALUE FOR THE GAIN OF THE FILTER. IF
* IT GETS MUCH LARGER THAN THIS THEN THE ALGORITHM WILL PROBABLY 
* DIVERGE AND WOULD CRASH THE PROGRAM WITH A FLOATING OVERFLOW.
* ALSO CALCULATE A VALUE FOR THE GAIN WHICH CAN BE CONSIDERED TO GIVE
* GOOD "SAFE" RESULTS (I.E. LITTLE RINGING). THIS IS USED IF A NEW VALUE
* IS BEING FOUND FOR THE CONVERGENCE FACTOR.
*
      if(refrms.gt.0.0) then
         glimit=limit*prmrms/refrms
         safe=target*prmrms/refrms
      else
         glimit=prmrms
         safe=1.0
      endif

*
* IF THE CONVERGENCE FACTOR IS ZERO ON ENTRY, THEN FIND A FIRST GUESS
* AT A REPLACEMENT VALUE.
*
      u=abs(u)
      if(u.eq.0) then
         if(prmrms*refrms.ne.0) then
            u=0.5/(prmrms*refrms)
         else
            u=1.0
         endif
         newu=.true.
      else
         newu=.false.
      endif            

*
* ARRIVE HERE IF U NEEDS CHANGING DURING THE COURSE OF THE ALGORITHM.
* THE WHOLE ALGORITHM IS STARTED AGAIN WITH THE NEW VALUE OF U.
*
      nstart=0
   10 continue

*
* SET THE FILTER TO ALL ZEROS
*
      do i=-hw,hw
         w(i)=0.0
      enddo

*
* LOOP ROUND, PERFORMING THE REQUIRED NUMBER OF ITERATIONS OF THE 
* CANC ALGORITHM
*
      do iter=1,niter
         e2sum=0.0

*
* THE EXTREME ENDS OF THE DATA ARRAY CANNOT BE FILTERED. THESE ARE LEFT
* BLANK IN THE OUTPUT. LOOP THROUGH ALL THE USABLE SAMPLES IN BETWEEN.
*
         do samp=hw+1,nsamp-hw

*
* CONVOLVE THE REFERENCE INPUT WITH THE CURRENT FILTER AT THE CURRENT
* SAMPLE POSITION. 
*
            outval=0.0
               
            do i=-hw,hw
               outval=outval+w(i)*ref(samp+i)
            enddo

*
* CALCULATE THE ERROR BETWEEN OUTPUT AND PRIMARY INPUT. 
*
            errval=prim(samp)-outval

*
* IF THE ERROR IS HIGHER THAN USUAL (AS OCCURS AT SHARP FEATURES IN
* THE DATA), THEN LET THE OUTPUT BECOME MORE LIKE THE PRIMARY INPUT.
* THIS REDUCES THE IMPACT OF SHARP FEATURES ON THE FILTER AND SUPRESSES
* RINGING.
*
            if(iter.gt.1) then
               if(errval.ne.0.0) then
   
                  aa=nsigma*errrms/abs(errval)
                  if(aa.lt.1.0) then
                     aa=aa**power
                     outval=aa*outval+(1.0-aa)*prim(samp)
                     errval=prim(samp)-outval
                  endif

               endif
            endif


*
* STORE THE OUTPUT AND ERROR VALUES, AND INCREMENT SUMS TO FIND THE 
* RMS ERROR VALUE IN THIS ITERATION.
*
            out(samp)=outval
            error(samp)=errval
            e2sum=e2sum+errval*errval

*
* UPDATE THE ADAPTIVE FILTER, AND FIND ITS NEW GAIN.
*
            gain=0.0

            do i=-hw,hw
               wval=w(i)+2.0*u*errval*ref(samp+i)
               w(i)=wval
               gain=gain+wval*wval
            enddo

         
            gain=sqrt(gain)

*
* IF A NEW VALUE IS BEING FOUND FOR THE CONVERGENCE FACTOR, THEN
* CHECK THAT THE GAIN IS NOT HIGHER THAN A "SAFE" VALUE. IF IT IS, THEN
* REDUCE U AND START AGAIN. THIS "SAFE" GAIN VALUE IS A LOT LESS THAN 
* THE LIMIT AT WHICH THE ALGORITHM MAY BECOME UNSTABLE.
*
            if(newu) then

               if(gain.gt.safe) then

                  nstart=nstart+1
                  if(nstart.le.mxstrt) then

                     unew=u*safe/gain
                     fact=min(1.0, 20.0*exp(-6.0*real(nstart)/mxstrt ) )
                     u=0.95*unew*fact
                     goto 10
                  else
                     call wrerr('CANCEXC')
                     ierr=3
                     goto 999
                  endif
               endif
            endif

*
* IF THE FILTER HAS A GAIN EXCEEDING "MARGIN" TIMES THE REQUIRED GAIN
* (= TYPICAL PRIMARY VALUE/TYPICAL REFERENCE VALUE), THEN THE ALGORITHM
* WILL PROBABLY DIVERGE AND CAUSE A FLOATING OVERFLOW CONDITION. TRAP
* THIS SITUATION NOW.
*
            if(gain.gt.glimit) then
               call wrerr('CANCDIV')
               ierr=2
               goto 999
            endif

*
* DO THE NEXT SAMPLE POSITION
*
         enddo

*
* FORM THE RMS VALUE OF ALL ERROR VALUES, THEN GO THROUGH AND FORM A
* NEW ESTIMATE OF THE RMS ERROR EXCLUDING OUT-LIERS.
*
         errrms=sqrt(e2sum/nsamp)
         errlim=nsigma*errrms

         e2sum=0.0
         nerr=0
         do samp=1,nsamp
            errval=error(samp)
            if(abs(errval).lt.errlim) then
               e2sum=e2sum+errval*errval
               nerr=nerr+1
            endif
         enddo

         errrms=sqrt(e2sum/nerr)

*
* DO THE NEXT ITERATION
*
      enddo

*
* FINISH
*
  999 continue

      end

