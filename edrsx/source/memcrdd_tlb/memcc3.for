      subroutine memcc3(in,out,work,ndet,nsamp,crddf,var,ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Deglitch the input data, and estimate variance of field noise.
*
*SOURCE
*       MEMCC3.FOR in MEMCRDD.TLB
*
*METHOD
*       
*ARGUMENTS       
*   INPUT:
*	in(nsamp,ndet)	integer	Input un-scaled CRDD data
*	work(nsamp,4)   real	Work space
*	ndet		integer	No. of detector data streams in the CRDD files
*	nsamp		integer	No. of samples per detector data stream
*	crddf		integer	Current CRDD file index
*	ierr		integer	Inherited status: 0 - "OK so far"
*   OUTPUTS:
*	out(nsamp,ndet)	real	Output scaled and deglitched data
*	var(ndet)	real	Variance of noise in each detector in Jy**2
*       ierr		integer Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/A8_COM/,/B2_COM/,/ZZ_COM/
*
*SUBROUTINES CALLED
*       THIS PACKAGE (UTILITIES.TLB):
*              wruser,ffrej
*       EDRS:
*              lbgone
*              
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 3/10/89
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
* INCLUDE DATA ABOUT THE IRAS FOCAL PLANE ETC
*
      include 'UTILITIES(DT_DAT)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... DATA DESCRIBING THE SCAN AND DETECTOR GROUPS
      include '(A8_COM)'

* ... DATA DESCRIBING THE CRDD FILES
      include '(B2_COM)'

* ... THE USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ndet,nsamp,in(nsamp,ndet),crddf,ierr
      real	out(nsamp,ndet),work(nsamp,4),var(ndet)

*
* DECLARE LOCAL PARAMETERS
*
      integer	niter 	! No. of rejection iterations to perform when 
			! calculating sigma
      real	nsigma	! Rejection threshold when calculating sigma

      parameter ( niter  = 8,
     :            nsigma = 3.0)

*
* DECLARE LOCAL VARIABLES
*
      integer	boxsiz	! No. of samples covered by the box filter
      integer	count	! No. of aberant samples close to candidate glitch
      integer	det	! Detector no. in cross-scan order
      real      dn(IR_dts,IR_bns)! Relative noise factors for each detector
      integer	index	! Pointer to pixels within the box size
      integer	istat	! Temporary status value
      integer	ival	! Integer data value before scaling
      integer	ngood	! No. of samples not rejected by FFREJ
      real	noise	! Standard deviation of noise in Jy
      integer	nrej	! No. of samples rejected as glitches
      integer	pntsiz	! Approx. no. of in-scan samples in a point source
      real	psfsiz(4)! Approximate in-scan size of a point source in arcmins
      character	prbuf*80! Buffer for screen output
      integer	samp	! Current sample no.
      real	solang	! Function giving solid angle of a detector


      data	psfsiz/ 1.8, 1.8, 3.4, 7.3/

      data      dn/0.83, 1.07, 0.83, 1.07, 1.07, 1.55, 0.83, 0.83,
     :             0.83, 1.07, 0.83, 2.74, 0.83, 1.07, 0.83, 1.07,
     :             0.70, 0.90, 1.10, 1.10, 1.10, 0.00, 1.10, 0.00,
     :             0.90, 0.90, 0.90, 1.10, 1.90, 1.10, 0.70, 0.00,
     :             0.96, 1.10, 0.96, 0.96, 0.96, 0.96, 0.00, 1.10,
     :             1.25, 0.96, 0.96, 0.96, 1.10, 0.96, 0.96, 0.81,
     :             0.95, 1.12, 0.78, 1.29, 1.12, 1.29, 1.29, 0.78,
     :             0.95, 0.78, 0.95, 0.78, 0.95, 1.12, 0.78, 0.00/


*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* INITIALISE NO. OF SAMPLES REJECTED AS GLITCHES
*
      nrej=0

*
* LOOP ROUND EACH DETECTOR
*
      do det=1,ndet

*
* IF THE DETECTOR IS DEAD OR NOT REQUIRED, SET THE VARIANCE NEGATIVE 
* AND PASS ON TO NEXT DETECTOR
*
         if(solang(det,B2_bnd).eq.0.or.A8_dgp(det).eq.0) then
            var(det)=-1.0E6
            goto 30
         endif

*
* IF USER SUPPLIED A FIELD NOISE VALUE, AND DOES NOT REQUIRE DEGLITCHING
* THEN SET THE NOISE VALUE, COPY THE DATA, AND PASS ON TO THE NEXT 
* DETECTOR
*
         if(ZZ_fld.gt.0.0.and.ZZ_deg.eq.0.0) then
            var(det)=(dn(det,B2_bnd)*ZZ_fld)**2
            do samp=1,nsamp
               ival=in(samp,det)
               if(ival.ne.B2_bpx(crddf)) then
                  out(samp,det)=ival*B2_bsc(crddf)+B2_bze(crddf)
               else
                  out(samp,det)=PR_rin
               endif
            enddo
            goto 30
         endif

*
* CALCULATE THE NO. OF SAMPLES IN A POINT SOURCE
*
         pntsiz=nint( psfsiz(B2_bnd)*DT_srt(B2_bnd)/B2_spd(crddf) )

*
* FIELD NOISE ESTIMATION SEEMS TO BE MORE ACCURATE IF A FILTER BIGGER
* THAN A POINT SOURCE IS USED. THIS COULD BE DUE TO THE CORRELATION 
* BETWEEN NOISE VALUES. 
*
         boxsiz=2*pntsiz


*
* CALL FFREJ TO REJECT NON-FIELD SAMPLES AND ESTIMATE THE NOISE IN THE
* REMAINING FIELD SAMPLES. ARRAY OUT IS USED AS WORKSPACE. THE ARRAY
* WORK(1,1)-WORK(NSAMP,1) CONTAINS A COPY OF THE SCALED INPUT DATA WITH
* ABERRANT SAMPLES SET TO THE INVALID FLAG VALUE.
*
         call ffrej(in(1,det),nsamp,1,B2_bpx(crddf),niter,nsigma,3,1,
     :              boxsiz,1,0,B2_bsc(crddf),B2_bze(crddf),noise,0.1,
     :              work(1,1),PR_rin,ngood,out(1,det),work(1,2),
     :              work(1,3),work(1,4))

*
* IF ALL DATA WAS REJECTED, GIVE A MESSAGE, SET THE VARIANCE NEGATIVE,
* AND PASS ON TO THE NEXT DETECTOR
*
         if(ngood.lt.5) then
            var(det)=-1.0E6
            goto 30
         endif

*
* COPY THE SCALED INPUT DATA TO THE OUTPUT. 
*
         do samp=1,nsamp
            ival=in(samp,det)
            if(ival.ne.B2_bpx(crddf)) then
               out(samp,det)=ival*B2_bsc(crddf)+B2_bze(crddf)
            else
               out(samp,det)=PR_rin
            endif
         enddo

*
* STORE NOISE ESTIMATE PRODUCED BY FFREJ OR GIVEN BY THE USER
*
         if(ZZ_fld.gt.0.0) then
            var(det)=(dn(det,B2_bnd)*ZZ_fld)**2
         else
            var(det)=noise**2
         endif

*
* IF DEGLITCHING IS NOT REQUIRED, PASS ON TO NEXT DETECTOR
*
         if(ZZ_deg.le.0.0) goto 30

*
* LOOP THROUGH THE SAMPLES, LOOKING FOR A SAMPLE WHICH WAS REJECTED BY
* FFREJ. SUCH SAMPLES ARE CANDIDATE GLITCHES.
*
         do samp=1,nsamp
            if(work(samp,1).eq.PR_rin) then

*
* WHEN A CANDIDATE GLITCH IS FOUND, SEE HOW MANY OTHER CANDIDATE 
* GLITCHES THERE ARE WITHIN HALF A POINT SOURCE SIZE AWAY FROM IT
*
               count=0
               do index=max(1,samp-pntsiz/2),min(nsamp,samp+pntsiz/2)
                  if(work(index,1).eq.PR_rin) count=count+1
               enddo

*
* IF THERE ARE LESS THAN THE BOX SIZE TIMES THE DEGLICHING FACTOR, THEN
* THIS CANDIDATE IS CONSIDERED TO BE ISOLATED FROM OTHER CANDIDATE
* GLITCHES, BECOMES A CONFIRMED GLITCH AND IS SET INVALID IN THE
* OUTPUT.
*
               if(count.le.ZZ_deg*pntsiz) then
                  out(samp,det)=PR_rin
                  nrej=nrej+1
               endif

            endif
         enddo

*
* DO THE NEXT DETECTOR
*
  30     continue

      enddo

*
* IF REQUIRED, TELL USER THE NOISE LEVELS, ETC
*
      if(ZZ_ilv.ge.4) then

         call wruser(' ',istat)
         call wruser('  Field noise estimates (in Jy):',istat)
   
         do det=1,ndet
   
            if(var(det).gt.0.0) then
               write(prbuf,40) DT_bal(det,B2_bnd),sqrt(var(det))
   40          format('   Detector #',I2,': ',G13.6)
            else
               write(prbuf,50) DT_bal(det,B2_bnd)
   50          format('   Detector #',I2,'    has no usable data')
            endif
            call wruser(prbuf,istat)
   
         enddo

      endif

      if(ZZ_ilv.ge.3) then

         write(prbuf,10) nrej
  10     format('  (',I10,' samples rejected as glitches)')
         call lbgone(prbuf(4:))
         call wruser(prbuf,istat)
         call wruser(' ',istat)

      endif

*
* FINISH
*
  999 continue

      end
