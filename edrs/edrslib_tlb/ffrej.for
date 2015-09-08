      subroutine ffrej(ia,npix,nlines,invala,niter,gamma,ix,iy,ilevel
     : ,scale,sigma,ib,invalb,ngood,istor,nstor,iline,nline)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REJECT DEFECTS IN A SUBSTANTIALLY SMOOTH IMAGE
*
*METHOD
*       SMOOTH THE IMAGE USING A RECTANGULAR FILTER AND FORM A NOISE
*       ESTIMATE BASED ON A COMPARISON OF THE ORIGINAL IMAGE WITH THE
*       SMOOTHED VERSION. REJECT PIXELS WHICH DEVIATE FROM THE SMOOTHED
*       VERSION BY MORE THAN A DETERMINED NUMBER OF STANDARD DEVIATIONS
*       ITERATE, SMOOTHING THE MOST RECENT REJECTED IMAGE EACH TIME.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF THE IMAGE
*       INVALA (IN)
*       INTEGER
*               THE INVALID PIXEL FLAG FOR THE INPUT IMAGE
*       NITER (IN)
*       INTEGER
*               THE NUMBER OF REJECTION ITERATIONS REQUIRED
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
*       SCALE (IN)
*       REAL
*               THE SCALE FACTOR FOR THE INPUT IMAGE
*       SIGMA (OUT)
*       REAL
*               RETURNS AN ESTIMATE OF THE RMS NOISE PER PIXEL IN THE
*               OUTPUT IMAGE
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               THE OUTPUT IMAGE
*       INVALB (IN)
*       INTEGER
*               THE INVALID PIXEL FLAG FOR THE OUTPUT IMAGE
*       NGOOD (OUT)
*       INTEGER
*               RETURNS THE NUMBER OF VALID PIXELS REMAINING IN THE
*               OUTPUT IMAGE
*       ISTOR (WORKSPACE)
*       INTEGER(NPIX*NLINES)
*               INTERMEDIATE STORAGE
*       NSTOR (WORKSPACE)
*       INTEGER*2(NPIX*NLINES)
*               INTERMEDIATE STORAGE
*       ILINE (WORKSPACE)
*       INTEGER(NPIX)
*               INTERMEDIATE STORAGE
*       NLINE (WORKSPACE)
*       INTEGER*2(NPIX)
*               INTERMEDIATE STORAGE
*
*CALLS
*       THIS PACKAGE:
*               LBGONE,IMGBOX
*       STARLINK:
*               WRUSER
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 ia(npix*nlines),ib(npix*nlines),nstor(npix*nlines),
     :nline(npix)
      integer istor(npix*nlines),iline(npix)
      character prbuf*80
 
*
* Q0 IS THE SMALLEST VALUE THAT THE VARIANCE OF A PIXEL CAN TAKE..
* SET IT TO BE ABOUT EQUAL TO THE QUANTISATION ERROR
*
      parameter (q0=0.25)
 
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
 
      do 1 i=1,nn
 
         if(ia(i).ne.invala) then
            ib(i)=ia(i)
            nstart=nstart+1
 
         else
            ib(i)=invalb
         endif
 
1     continue
 
 
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
* SMOOTH THE IMAGE USING IMGBOX TO APPLY A RECTANGULAR FILTER
*
         call imgbox(ib,npix,nlines,invalb,iix,iiy,nmin,istor,nstor
     :    ,iline,nline)
 
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
 
               if(ia(i).ne.invala) then
 
                  if(ib(i).ne.invalb) then
                     na=ia(i)
                     nb=ib(i)
                     diff=na-nb
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
 
            if(ia(i).ne.invala) then
 
               if(ib(i).ne.invalb) then
                  na=ia(i)
                  nb=ib(i)
                  diff=na-nb
                  diff2=diff*diff
 
                  if(diff2.le.thresh) then
 
*
* FORM A NEW NOISE ESTIMATE AT THE SAME TIME
*
                     ib(i)=ia(i)
                     sig=sig+diff2
                     ngood=ngood+1
 
                  else
                     ib(i)=invalb
                  endif
 
               endif
 
 
            else
               ib(i)=invalb
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
         sigma=scale*sqrt((var*iix*iiy)/(max(1,iix*iiy-1)))
 
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
 
66    continue
 
 
*
* IF ILEVEL GE 2, SHOW HOW MANY PIXELS WERE REJECTED AFTER ALL
* ITERATIONS ARE COMPLETE
*
 
      if(ilevel.ge.2) then
         nrej=nstart-ngood
         write(prbuf,12) nrej
12       format('     ',i10,' PIXEL(S) REJECTED IN TOTAL')
         call wruser(' ',istat)
         call wruser(prbuf,istat)
         call wruser(' ',istat)
      endif
 
99    return
 
      end
 
 
 
