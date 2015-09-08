      subroutine rffrej(ia,npix,nlines,niter,gamma,ix,iy,ilevel,
     :  sigma,convrg,out,rinval,ngood,rstor,nstor,rline,
     :  nline)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REJECT DEFECTS IN A SUBSTANTIALLY SMOOTH REAL IMAGE
*	(BASED ON FFREJ).
*
*SOURCE
*	RFFREJ.FOR in UTILITIES.TLB
*
*METHOD
*       SMOOTH THE IMAGE USING A RECTANGULAR FILTER AND FORM A NOISE
*       ESTIMATE BASED ON A COMPARISON OF THE ORIGINAL IMAGE WITH THE
*       SMOOTHED VERSION. REJECT PIXELS WHICH DEVIATE FROM THE SMOOTHED
*       VERSION BY MORE THAN A DETERMINED NUMBER OF STANDARD DEVIATIONS
*       ITERATE, SMOOTHING THE MOST RECENT REJECTED IMAGE EACH TIME, 
*	UNTIL THE PROCESS CONVERGES OR A MAXIMUM ITERATION LIMIT IS 
*	REACHED.
*
*ARGUMENTS
*       IA (IN)
*       REAL(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF THE IMAGE
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF REJECTION ITERATIONS TO PERFORM
*       GAMMA (IN)
*       REAL
*               THE NUMBER OF STANDARD DEVIATIONS AT WHICH REJECTION
*               OCCURS
*       IX,IY (IN)
*       INTEGER
*               THE SIZE OF THE FILTER RECTANGLE TO BE USED
*       ILEVEL (IN)
*       INTEGER
*               THE INTERACTION LEVEL CONTROLLING THE PRINTING OF
*               RESULTS
*       SIGMA (OUT)
*       REAL
*               RETURNS AN ESTIMATE OF THE RMS NOISE PER PIXEL IN THE
*               OUTPUT IMAGE
*	CONVRG (IN)
*	REAL
*		A VALUE BETWEEN 0.0 AND 1.0 GIVING A FRACTIONAL CHANGE
*		IN VARIANCE BETWEEN ITERATIONS. ONCE THE VARIANCE
*		CHANGES BY LESS THAN THIS FACTOR, THE PROCESS IS ASSUMED
*		TO HAVE CONVERGED AND NO MORE ITERATIONS ARE DONE.
*       OUT (OUT)
*       REAL(NPIX,NLINES)
*               THE OUTPUT IMAGE
*       RINVAL (IN)
*       REAL
*               THE INVALID PIXEL FLAG FOR INPUT AND OUTPUT IMAGE
*       NGOOD (OUT)
*       INTEGER
*               RETURNS THE NUMBER OF VALID PIXELS REMAINING IN THE
*               OUTPUT IMAGE
*       RSTOR (WORKSPACE)
*       REAL(NPIX*NLINES)
*               INTERMEDIATE STORAGE
*       NSTOR (WORKSPACE)
*       INTEGER(NPIX*NLINES)
*               INTERMEDIATE STORAGE
*       RLINE (WORKSPACE)
*       REAL(NPIX)
*               INTERMEDIATE STORAGE
*       NLINE (WORKSPACE)
*       INTEGER(NPIX)
*               INTERMEDIATE STORAGE
*
*CALLS
*	THIS PACKAGE (UTILITIES.TLB):
*	        RIMBOX
*       EDRS:
*               LBGONE
*       STARLINK:
*               WRUSER
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*	(modified by DS Berry to use real input and output images
*	 and convergence test added)
*-----------------------------------------------------------------------
*
*
      real	ia(npix*nlines)
      integer	nstor(npix*nlines),nline(npix)
      real	out(npix*nlines),rstor(npix*nlines),rline(npix)
      character prbuf*80
 
*
* MAKE THE SIDES OF THE FILTER AREA ODD
*
      idx=max(0,ix/2)
      idy=max(0,iy/2)
      iix=2*idx+1
      iiy=2*idy+1
 
*
* SET THRESHOLD FOR REQUIRED NUMBER OF VALID PIXELS PER BOX TO
* 0.25* BOX AREA
*
      nmin=max((iix*iiy)/4,1)
 
*
* COPY INPUT IMAGE TO OUTPUT MATRIX, COUNTING THE NUMBER OF VALID
* PIXELS
*
      nn=npix*nlines
      nstart=0
 
      ainmax=-1.0E32
      ainmin=1.0E32
      do 1 i=1,nn
 
         ra=ia(i)
         if(ra.ne.rinval) then
            out(i)=ra
            nstart=nstart+1
            ainmax=max(ainmax,ra)
            ainmin=min(ainmin,ra)
 
         else
            out(i)=rinval
         endif
 
1     continue
 
 
*
* Q0 IS THE SMALLEST VALUE THAT THE VARIANCE OF A PIXEL CAN TAKE..
* SET IT TO BE ABOUT EQUAL TO A SMALL VALUE
*
      q0=(ainmax-ainmin)/64000.0
 
*
* IF ILEVEL IS GE 2, TELL THE USER HOW MANY GOOD PIXELS THERE ARE
*
 
      if(ilevel.ge.2) then
         write(prbuf,10)nstart
10       format('     IMAGE INITIALLY HAS',i10,' VALID PIXEL(S)')
         call lbgone(prbuf(26:))
         call wruser(' ',istat)
         call wruser(prbuf,istat)
      endif
 
 
*
* IF THERE ARE NO VALID PIXELS, EXIT WITH ERROR STATUS
*
 
      if(nstart.le.0) then
         ngood=-1
         go to 99
 
      endif
 
 
*
* PERFORM THE REJECTION ITERATIONS
* --------------------------------
*
 
      do 66 iter=1,niter
 
*
* SMOOTH THE IMAGE USING RIMBOX TO APPLY A RECTANGULAR FILTER
*
         call rimbox(out,npix,nlines,rinval,iix,iiy,nmin,rstor,nstor
     :    ,rline,nline)
 
*
* ON THE FIRST ITERATION, COMPARE THE SMOOTHED IMAGE WITH THE ORIGINAL
* AND DERIVE A NOISE ESTIMATE
*
 
         if(iter.eq.1) then
            sig=0.0
            ngood=0
 
            do 2 i=1,nn
 
*
* USE ONLY THOSE PIXELS VALID IN BOTH IMAGES
*
 
               if(ia(i).ne.rinval) then
 
                  if(out(i).ne.rinval) then
                     diff=out(i)-ia(i)
                     sig=sig+diff*diff
                     ngood=ngood+1
                  endif
 
               endif
 
2           continue
 
 
            if(ngood.ge.1) then
               var=max(sig/ngood,q0)
 
            else
               var=q0
            endif
 
 
*
* SET THRESHOLD FOR THE SQUARED DEVIATIONS AT GAMMA STANDARD DEVIATIONS
*
            thresh=gamma*gamma*var
         endif
 
 
*
* NOW COPY THE INPUT IMAGE TO THE OUTPUT MATRIX, REJECTING PIXELS
* OUTSIDE THE THRESHOLD
*
         sig=0.0
         ngood=0
 
         do 3 i=1,nn
 
*
* ACCEPT ONLY THOSE PIXELS VALID IN BOTH IMAGES
*
 
            if(ia(i).ne.rinval) then
 
               if(out(i).ne.rinval) then
                  ra=ia(i)
                  diff=out(i)-ra
                  diff2=diff*diff
 
                  if(diff2.le.thresh) then
 
*
* FORM A NEW NOISE ESTIMATE AT THE SAME TIME
*
                     out(i)=ra
                     sig=sig+diff2
                     ngood=ngood+1
 
                  else
                     out(i)=rinval
                  endif
 
               endif
 
 
            else
               out(i)=rinval
            endif
 
3        continue
 
 
*
* EVALUATE THE NOISE ESTIMATE
*
 
         if(ngood.ge.1) then
            var=max(sig/ngood,q0)
 
         else
            var=q0
         endif
 
 
*
* SET NEW REJECTION THRESHOLD
*
         thresh=gamma*gamma*var
 
*
* IF ILEVEL GE 2, SHOW THE USER THE PROGRESS OF THE ITERATIONS
*
         sigma=sqrt((var*iix*iiy)/(max(1,iix*iiy-1)))
 
         if(ilevel.ge.2) then
            write(prbuf,11)iter,ngood,sigma
11          format('     ITERATION ',i3,':',i10,
     :      ' VALID PIXEL(S) : SIGMA=',ss,g11.4)
            call wruser(prbuf,istat)
         endif
 
 
*
* IF ALL PIXELS HAVE BEEN REJECTED, ABORT
*
 
         if(ngood.le.0) then
            go to 99
 
         endif
 
*
* CALCULATE FRACTIONAL CHANGE IN VARIANCE FROM THE LAST ITERATION
*
         if(iter.ge.2) then
            change=2.0*(varlst-var)/(var+varlst)

*
* IF THE VARIANCE CHANGED BY A FACTOR LESS THAN THE VALUE OF THE 
* ARGUMENT "CONVRG", THEN THERE IS NO NEED TO DO ANY MORE ITERATIONS
*
            if(change.lt.convrg.and.change.ge.0.0) go to 67
         endif

*
* SAVE THE VARIANCE FROM THIS ITERATION
*
         varlst=var


66    continue
 
 
*
* IF ILEVEL GE 2, SHOW HOW MANY PIXELS WERE REJECTED AFTER ALL
* ITERATIONS ARE COMPLETE
*
 
67    if(ilevel.ge.2) then
         nrej=nstart-ngood
         write(prbuf,12) nrej
12       format('     ',i10,' PIXEL(S) REJECTED IN TOTAL')
         call wruser(' ',istat)
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif
 
99    return
 
      end
