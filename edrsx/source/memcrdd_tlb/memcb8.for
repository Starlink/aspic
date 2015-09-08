      subroutine memcb8(ierr)
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	Calculates an error estimate for each data sample and store the
*	associated accuracies in internal file 22.
*
*SOURCE
*       MEMCB8.FOR in MEMCRDD.TLB
*
*METHOD
*   	   Two options are available; a crude model in which the total
*	sample variance is the sum of the field variance and a factor
*       times the squared sample flux; or a fine model which is based
*	on a  previously generated estimate of the sky, specified by
*       the user.
*
*	In the fine model, the variance of each data sample is made up 
*       from four terms:
*	1) The "field" noise. This is based on the deviation of each
*	   data sample from the local mean value within the samples
*	   detector (after rejection of source). On entry, internal 
*	   file 22 is assumed to hold the variance associated with 
*          these errors.
*       2) The "correlated" noise.
*	3) The noise caused by pointing errors. This is estimated by
*	   simulating data from the given hi-res image with pointing
*	   errors introduced into the data simulation. Each data sample
*	   is simulated many times with different random pointing errors
*	   and the variance of the simulated values used as an estimate
*	   of the data sample variance.
*	4) The errors caused by uncertainties in the PSF maps and 
*	   detector effective solid angle values. It is assumed that
*          the error of a solid angle estimate is a constant fraction
*	   of the solid angle value, and that the error of a PSF pixel
*	   is a constant fraction of the PSF pixel value. The variance
*	   which these errors would introduce if the real sky looked 
*	   like the given sky image is found and added to the other
*	   variances listed above. NB, these errors are calculated
*	   based on an image from which NO background has been removed.
*
*          If the given sky image is smoother than the real sky, then
*	   the fine noise estimates will be too low. On the other hand,
*          if the given image is overfitted, the noise estimates will be
*          too high. To provide some correction for this, the calculated
*	   variance values are not used directly. Instead, a linear 
*	   combination of the calculated values and the values used to
*	   produce the trial sky, is used.
*       
*ARGUMENTS       
*   INPUT:
*	ierr	integer		Inherited status: 0 - "OK so far"
*   OUTPUTS:
*       ierr    integer         Exit status: 0 - success
*
*COMMON USAGE
*   READ:
*	/B6_COM/,/ME_COM/,/ZZ_COM/
*       On entry file 21 is assumed to hold the data before background
*       subtraction, and file 22 the field noise variances. 
*   WRITE:
*       /ME_COM/
*       On exit file 22 holds the data sample accuracies
*
*SUBROUTINES CALLED
*       THIS PACKAGE (MEMCRDD.TLB):
*              memcf2,memcf3,memce9,memcg2,opus
*       THIS PACKAGE (UTILITIES.TLB):
*              strlen,wruser
*	EDRS:
*	       getpar
*	INTERIM:
*	       wrkeyc,frdata,cnpar
*              
*STARLINK PARAMETERS
*	SKYIN
*	NOISE_OK An output parameter which is set to YES when the
*	        input image defines an acceptable noise model.
*
*VAX SPECIFICS
*       implicit none
*       enddo
*       end of line comments
*
*AUTHOR
*       D.S. Berry (MAVAD::DSB) 26/1/90
*-------------------------------------------------------------------
      implicit none

*
* INCLUDE GLOBAL PARAMETER DECLARATIONS
*
      include '(PR_DEC)'

*
* INCLUDE COMMON BLOCKS HOLDING...

* ... POINTERS TO MEMCRDD INTERNAL FILES
      include '(B6_COM)'

* ... POINTERS TO MEMSYS3 INTERNAL FILES AND OTHER MEMSYS3 INFO
      include '(ME_COM)'

* ... USER SUPPLIED PARAMETER VALUES
      include '(ZZ_COM)'

*
* DECLARE ARGUMENTS
*
      integer	ierr

*
* DECLARE LOCAL VARIABLES
*
      real	acc
      real	c0(5)	! Individual chi squared values from input image
      logical	convrg  ! True if noise loop has converged
      real	corr    ! A correlated noise variance value
      character	cval*1  ! Dummy character argument
      real	data	! Input data value
      real	delta	! Correlated noise variance
      real	dres
      real	factor(2)! Correction factors to account for the given sky
			! image not being correctly fitted.
      real	field	! Field variance term
      integer	file22
      real	flux	! Sample flux value (Jy)
      integer   ipvar
      integer	istat	! Local status value
      integer	ival	! Dummy integer argument
      real      maxpnt	! Maximum variance due to pointing
      real	maxpsf	! Maximum variance due to PSF errors
      real	maxsol	! Maximum variance due to solid angle errors
      real	maxfld	! Maximum variance due to field noise
      real      maxcor	! Maximum variance due to "correlated" noise
      real	maxsig	! Maximum sigma value generated
      real	minsig	! Minimum sigma value generated
      integer	nback	! No. of samples deemed to be background samples
      real	newchi
      real	newvar	! Variance after lagging has been performed
      integer	nlin
      integer	nsamp
      integer	offset	! Offset into a data set
      real	oldchi	!
      real	oldvar	! Variance used on last MEMCRDD run
      real	point	! Pointing error variance term
      character	prbuf*80! Screen output buffer
      real	psf	! PSF error variance term
      real	res	! Squared data residual 
      real	rfilt
      real	s1,s2,s3,s4,s5,s6,s7,s8
      real	scale	
      integer	sdc	
      real	sigma	! Standard deviation associated with a data sample
      real	simdat	! Simulated data sample value
      integer	size
      character skynam*30
      real	sol	! Variance due to solid angles errors
      real	speed	! Noise loop convergence speed (0.0 - 1.0)
      integer	strlen	! Function giving used length of a string
      real	sumsig	! Sum of sigma values generated
      real	totvar	! Total variance
      real	wb	! Weight for background samples
      real	weight(4)
      real	ws	! Weight for source samples

*
* CHECK INHERITED STATUS
*
      if(ierr.ne.0) goto 999

*
* IF REQUIRED TELL THE USER WHAT IS HAPPENING
*
      if(ZZ_ilv.ge.4) then
         call wruser('  ',istat)
         call wruser('  Generating noise model...',istat)
      endif

*
* INITIALISE SUMS FOR STATISTICS
*
      maxsig=-1.0E32
      minsig=1.0E32
      sumsig=0.0
      maxpnt=0.0
      maxpsf=0.0
      maxsol=0.0
      maxfld=0.0
      maxcor=0.0

*
* ATTEMPT TO GET A HIGH RESOLUTION IMAGE TO USE AS THE BASIS FOR THE 
* FINE NOISE ESTIMATION. THIS IMAGE IS USUALLY THE RESULT FROM A 
* PREVIOUS RUN OF MEMCRDD, AND MUST CONTAIN THE SAME AREA OF THE SKY 
* AS THE IMAGE BEING PRODUCED BY THIS RUN OF MEMCRDD. THE IMAGE IS 
* STORED IN INTERNAL FILE 1. INVALID PIXELS IN THE IMAGE ARE REPLACED BY
* THE VALUE ZERO.
*
      call memcf3('SKYIN',.true.,ME_st(ME_kb(1)),0.0,skynam,istat)

*
* IF NO IMAGE WAS GIVEN, GENERATE A "CRUDE" MODEL.
*
      if(istat.ne.0) then

*
         do offset=0,ME_mk-1

            field=ME_st(ME_kb(22)+offset)

            flux=ME_st(ME_kb(21)+offset)
            sol=(ZZ_k*flux)**2
            totvar=field+sol

            sigma=sqrt(max(field,totvar))
            ME_st(ME_kb(22)+offset)=1.0/sigma

            maxsig=max(maxsig,sigma)
            minsig=min(minsig,sigma)
            sumsig=sumsig+sigma
            maxsol=max(maxsol,sol)
            maxfld=max(maxfld,field)

         enddo

         skynam='NONE GIVEN'

*
* OTHERWISE, IF AN IMAGE WAS SUPPLIED BY THE USER....
*
      else

*
* READ IN THE IMAGE DESCRIPTOR GIVING THE FACTOR BY WHICH MEMSYS3 SCALED
* THE NOISE VALUES WHEN PRODUCING THE GIVEN IMAGE
*
         scale=1.0
         call gtdscr('SKYIN','SIGMA','REAL',ival,scale,cval,istat)      

*
* MAKE "SCALE" THE VARIANCE FACTOR INSTEAD OF THE SIGMA FACTOR
*
         scale=scale**2

*
* GET A VALUE FROM THE USER WHICH WILL BE USED TO DETERMINE THE
* SPEED AT WHICH THE NOISE LOOP ATTEMPT TO CONVERGE. A SPEED OF ZERO 
* MEANS NO CONVERGENCE, A SPEED OF 1.0 MEANS FAST CONVERGENCE.
*
         speed=0.5
         call getpar('SPEED','REAL',1,0.0,1.0,.true.,ival,speed,istat)

*
* IF REQUIRED, GENERATE THE VARIANCES DUE TO POINTING ERRORS AND STORE 
* IN FILE 25. ALSO STORE SIMULATED DATA IN FILE 27
*      
         if(ZZ_ntr.gt.1.and.(ZZ_xs.gt.0.0.or.ZZ_ins.gt.0.0)) then

            call memcf2(ME_st(ME_kb(1)),ZZ_ins,ZZ_xs,ZZ_ntr,
     :                  ME_st(ME_kb(25)),ME_st(ME_kb(27)),ME_st(B6_wk1),
     :                  ME_st(B6_wk2),ME_st(B6_wk3),ME_st(B6_wk4),
     :                  ME_st(ME_kb(22)),maxpnt,ierr)

*
* IF NO POINTING ERRORS REQUIRED, SET THE POINTING VARIANCES TO 0 AND
* CALL OPUS TO GENERATE THE SIMULATED DATA
*
         else
            do offset=0,ME_mk-1
               ME_st(ME_kb(25)+offset)=0.0
            enddo
            call opus(1,27)
         endif

*
* GENERATE THE VARIANCES DUE TO PSF UNCERTAINTIES AND STORE IN FILE
* 26. NB, FILE 1 IS OVERWRITTEN WITH THE FFT OF THE SKY IMAGE.
*
         call memce9(ME_st(ME_kb(26)),ME_st(ME_kb(27)),ME_st(B6_wk1),
     :               ME_st(B6_wk2),ME_st(B6_wk3),ZZ_psf,ZZ_k,maxsol,
     :               maxpsf,ierr)

*
* SEE IF THE USER WANTS TO SPECIFY A DISK FILE CONTAINING
* CORRELATED NOISE ESTIMATES AND LAST TIMES VARIANCES
*
         call gt2dir('VARIN',204,.true.,nsamp,nlin,ipvar,istat)

*
* IF NO VARIN FILE WAS GIVEN, OR GIVEN FILE CONTAINS NO CORRELATED VARIANCE 
* ESTIMATES, THEN CALCULATE NEW ONES AND PUT THEM IN FILE 28. (NB,
* FILES 23 AND 24 USED AS WORK SPACE).
*
         if(istat.ne.0.or.(istat.eq.0.and.nlin.eq.1)) then
            call memcg2(ierr)
         endif

*
* IF A FILE WAS GIVEN, READ LAST TIMES VARIANCES INTO FILE 23, AND FREE
* THE DISK DATA. IF NO FILE WAS GIVEN SET THE CONVERGENCE SPEED TO 1.0 
* TO CAUSE LAST TIMES VARIANCES TO BE IGNORED
*
         if(istat.eq.0) then
            call rdvar(%val(ipvar),nsamp,nlin,ierr)
            if(ierr.ne.0) goto 999

            call frdata('VARIN',istat)

         else
            speed=1.0
         endif

         call cnpar('VARIN',istat)


*
* QUIT IF ANYTHING HAS GONE WRONG
*
         if(ierr.ne.0) goto 999

*
* CALCULATE INDIVIDUAL CHI SQUAREDS
*
         call chisqs(c0,weight)

*
* INITIALISE SUMS
*
         s1=0
         s2=0
         s3=0
         s4=0
         s5=0
         s6=0
         s7=0
         s8=0

*
* LIMIT THE CHANGE IN USED VARIANCES BETWEEN THIS MEMCRDD RUN AND THAT 
* WHICH GENERATED THE ANALYSIS FILE, BY LAGGING THE CURRENT VARIANCES
*
         do offset=0,ME_mk-1

            field=max(0.0,ME_st(ME_kb(22)+offset))
            psf=max(0.0,ME_st(ME_kb(26)+offset))
            point=max(0.0,ME_st(ME_kb(25)+offset))
            corr=max(0.0,ME_st(ME_kb(28)+offset))

            totvar=corr+field+psf+point
            oldvar=ME_st(ME_kb(23)+offset)*scale
            newvar=speed*totvar+(1.0-speed)*oldvar
            sigma=sqrt(max(1.0E-20,newvar))
  
            ME_st(ME_kb(22)+offset)=1.0/sigma

            data=ME_st(ME_kb(21)+offset)
            simdat=ME_st(ME_kb(27)+offset)
            dres=(data-simdat)**2
            res=dres/newvar
            ws=(psf+point)/totvar
            wb=(field+corr)/totvar
            s1=s1+wb*res
            s2=s2+wb
            s3=s3+ws*res
            s4=s4+ws
            s5=s5+res
            s6=s6+1.0

            if(speed.ne.1.0) then
               res=dres/oldvar
               s7=s7+res
            endif
 
            res=dres/totvar
            s8=s8+res


            maxcor=max(maxcor,corr)
            maxsig=max(maxsig,sigma)
            minsig=min(minsig,sigma)
            sumsig=sumsig+sigma
            maxfld=max(maxfld,field)

         enddo


*
* CALCULATE THE OLD AND NEW CHI SQUARED VALUES, AND CONVERGENCE FLAG
*
         newchi=s5/s6
         c0(5)=s8/s6

         if(s7.gt.0.0) then
            oldchi=s7/s6
   
            if(abs((oldchi-newchi)/(oldchi+newchi)).le.0.05) then
               convrg=.true.
            else
               convrg=.false.
            endif
   
         else
            oldchi=-1.0
            convrg=.false.
         endif

      endif


*
* IF REQUIRED, DISPLAY STATISTICS
*
      if(ierr.eq.0.and.ZZ_ilv.ge.2) then


         call wruser('  Initial maximum noise components (in Jy) ...',
     :                istat)

         write(prbuf,20) sqrt(maxfld)
  20     format('     Detector noise                 : ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,30) sqrt(maxpnt)
  30     format('     Noise due to pointing errors   : ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,40) sqrt(maxpsf)
  40     format('     Noise due to PSF uncertainties : ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,50) sqrt(maxsol)
  50     format('     Noise due to solid angle errors: ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,60) sqrt(maxcor)
  60     format('     Noise due to "correlated" noise: ',G13.6)
         call wruser(prbuf,istat)

         if(skynam.ne.'NONE GIVEN') then
            call wruser(' ',istat)

            write(*,*) ' '
            write(*,*) '  Initial total chi squared for ',
     :                 skynam,' : ',c0(5)
            write(*,*) '  Initial individual chi squareds: '
            write(*,*) '     field     : ',c0(1),
     :                 ' (weight=',weight(1),')'
            write(*,*) '     pointing  : ',c0(2),
     :                 ' (weight=',weight(2),')'
            write(*,*) '     correlated: ',c0(3),
     :                 ' (weight=',weight(3),')'
            write(*,*) '     PSF       : ',c0(4),
     :                 ' (weight=',weight(4),')'


            write(*,*) ' '

            if(oldchi.gt.0.0) then
               write(*,*) ' Old total chi squared     : ',oldchi
            else
               write(*,*) ' Old total chi squared     : UNDEFINED'
            endif
            
            write(*,*) ' New total chi squared     : ',newchi
            write(*,*) '  "  background chi squared: ',s1/s2
            write(*,*) '  "  source chi squared    : ',s3/s4
            write(*,*) ' '

         endif

      endif


      if(ierr.eq.0.and.ZZ_ilv.ge.2) then

         call wruser(' ',istat)
         call wruser('  Statistics for used noise values (in Jy) ...',
     :                istat)

         write(prbuf,90) maxsig
  90     format('     Maximum: ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,100) sumsig/ME_mk
  100     format('     Mean   : ',G13.6)
         call wruser(prbuf,istat)

         write(prbuf,110) minsig
  110    format('     Minimum: ',G13.6)
         call wruser(prbuf,istat)

      endif

*
* IF INPUT AND OUTPUT VARIANCES ARE NEARLY THE SAME, SET
* OUTPUT PARAMETER "NOISE_OK" TO YES. OTHERWISE SET IT TO NO.
*
      if(skynam.ne.'NONE GIVEN'.and.convrg) then
         call wrkeyc('NOISE_OK','YES',1,istat)
      else
         call wrkeyc('NOISE_OK','NO',1,istat)
      endif

*
* PRODUCE A DISK FILE HOLDING THE CORRELATED NOISE ESTIMATES AND THIS 
* TIMES VARIANCES
*
      if(skynam.eq.'NONE GIVEN') then
         nlin=1
      else
         nlin=2
      endif

      call gt2diw('VAROUT',204,.true.,ME_mk,nlin,ipvar,istat)

      if(istat.eq.0) then
         call wrvar(%val(ipvar),ME_mk,nlin)
         call frdata('VAROUT',istat)
      endif
      call cnpar('VAROUT',istat)

* 
*
* FINISH
*
  999 call frdata('SKYIN',istat)
      call cnpar('SKYIN',istat)

      end

C-----------------------------------------------------------------------
      subroutine chisqs(chisq,weight)
      implicit none

      include '(PR_DEC)'
      include '(ME_COM)'

      real	field,point,psf,data,simdat,res,corr,totvar,srp,srd,src,
     :          srf,swp,swd,swc,swf,ssw,cp,cd,cf,cc,chisq(5),
     :          weight(4),wp,wd,wc,wf,sr,sw
      integer	offset

*
* INITIALISE THE SUMS
*
      sr=0.0
      sw=0.0
      srp=0.0
      srd=0.0
      src=0.0
      srf=0.0
      swp=0.0
      swd=0.0
      swc=0.0
      swf=0.0

*
* LOOP ROUND ALL SAMPLES
*
      do offset=0,ME_mk-1

*
* IF THE POINTING ERROR IS LOW ENOUGH...
*
         field=max(0.0,ME_st(ME_kb(22)+offset))
         psf=max(0.0,ME_st(ME_kb(26)+offset))
         point=max(0.0,ME_st(ME_kb(25)+offset))
         corr=max(0.0,ME_st(ME_kb(28)+offset))

         data=ME_st(ME_kb(21)+offset)
         simdat=ME_st(ME_kb(27)+offset)
         totvar=corr+field+point+psf
         res=(data-simdat)**2/totvar

*
* CALCULATE WEIGHTS
*
         wp=(point/totvar)**2
         wd=(psf/totvar)**2
         wc=(corr/totvar)**2
         wf=(field/totvar)**2

*
* INCREMENT THE SUMS
*
         sr=sr+res
         sw=sw+1.0

         srp=srp+wp*res
         swp=swp+wp

         srd=srd+wd*res
         swd=swd+wd

         src=src+wc*res
         swc=swc+wc

         srf=srf+wf*res
         swf=swf+wf

*
* DO NEXT SAMPLE
*
      enddo

*
* CALCULATE RESULTS
*
      if(swf.gt.0.0) then
         chisq(1)=srf/swf
      else
         chisq(1)=1.0E6
      endif

      if(swp.gt.0.0) then
         chisq(2)=srp/swp
      else
         chisq(2)=1.0E6
      endif

      if(swc.gt.0.0) then
         chisq(3)=src/swc
      else
         chisq(3)=1.0E6
      endif

      if(swd.gt.0.0) then
         chisq(4)=srd/swd
      else
         chisq(4)=1.0E6
      endif

      if(sw.gt.0.0) then
         chisq(5)=sr/sw
      else
         chisq(5)=1.0E6
      endif


      ssw=swp+swd+swc+swf
      weight(1)=swf/ssw
      weight(2)=swp/ssw
      weight(3)=swc/ssw
      weight(4)=swd/ssw

      end

C-------------------------------------------------------------------
      subroutine wrvar(var,nsamp,nlin)
      implicit none

      include '(PR_DEC)'
      include '(ME_COM)'

      integer	nsamp,nlin,samp
      real	var(nsamp,nlin),acc,cor

      if(nlin.eq.2) then

         do samp=1,nsamp
            acc=ME_st(ME_kb(22)+samp-1)
            cor=ME_st(ME_kb(28)+samp-1)
            var(samp,1)=1.0/(acc*acc)
            var(samp,2)=cor
         enddo

      else if(nlin.eq.1) then

         do samp=1,nsamp
            acc=ME_st(ME_kb(22)+samp-1)
            var(samp,1)=1.0/(acc*acc)
         enddo

      endif

      end

C---------------------------------------------------------------
      subroutine rdvar(var,nsamp,nlin,ierr)
      implicit none

      include '(PR_DEC)'
      include '(ME_COM)'

      integer	nsamp,nlin,samp,ierr
      real	var(nsamp,nlin)

      if(ierr.ne.0) goto 999

      if(nsamp.ne.ME_mk.or.nlin.gt.2) then
         call wrerr('B8ERR1')
         ierr=1
         goto 999
      endif

      if(nlin.eq.2) then

         do samp=1,nsamp
            ME_st(ME_kb(23)+samp-1)=var(samp,1)
            ME_st(ME_kb(28)+samp-1)=var(samp,2)
         enddo

      else if(nlin.eq.1) then

         do samp=1,nsamp
            ME_st(ME_kb(23)+samp-1)=var(samp,1)
         enddo

      endif

  999 continue

      end
