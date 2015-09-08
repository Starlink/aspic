      subroutine fillin(ia,npix,nlines,invala,size,ilevel,cngmax,cngrms
     : ,niter,scale,ib,nbad,dsum,wtsum,dlast,wtlast)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO REPLACE ALL THE INVALID PIXELS IN AN IMAGE WITH A SOLUTION
*       OF LAPLACE'S EQUATION WHICH MATCHES THE VALID DATA IN THE IMAGE
*       AT THE EDGES OF THE INVALID REGIONS. THIS SOLUTION HAS ZERO
*       GRADIENT NORMAL TO ANY IMAGE EDGES WHICH IT MEETS.
*
*METHOD
*       ITERATE, REPLACING EACH INVALID PIXEL WITH A WEIGHTED MEAN OF
*       ITS VALID NEIGHBOURS IN THE SAME ROW AND COLUMN.
*       THE WEIGHTS DECREASE EXPONENTIALLY WITH A SCALE LENGTH 'SIZE'
*       AND GO TO ZERO AFTER THE FIRST VALID PIXEL IS ENCOUNTERED.
*       THE LENGTH 'SIZE' IS REDUCED BY A FACTOR 2 WHENEVER THE MAX.
*       ABSOLUTE CHANGE IN AN ITERATION IS AT LEAST A FACTOR 4 LESS THAN
*       THE MAX. ABSOLUTE CHANGE OBTAINED SINCE THE CURRENT SCALE LENGTH
*       WAS FIRST USED. ITERATIONS STOP AFTER NITER HAVE BEEN PERFORMED.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVALA (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       SIZE (IN/OUT)
*       REAL
*               INITIAL SMOOTHING SIZE ON ENTRY. RETURNS THE FINAL
*               SMOOTHING SIZE
*       ILEVEL (IN)
*       INTEGER
*               INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*       CNGMAX (OUT)
*       REAL
*               MAXIMUM ABSOLUTE CHANGE IN OUTPUT VALUES WHICH OCCURRED
*               IN THE FINAL ITERATION
*       CNGRMS (OUT)
*       REAL
*               RMS CHANGE IN OUTPUT VALUES WHICH OCCURRED IN THE LAST
*               ITERATION
*       NITER (IN)
*       INTEGER
*               THE NUMBER OF ITERATIONS REQUIRED
*       SCALE (IN)
*       REAL
*               SCALE FACTOR FOR IMAGE IA
*       IB (OUT)
*       INTEGER*2(NPIX,NLINES)
*               THE OUTPUT IMAGE
*       NBAD (OUT)
*       INTEGER
*               THE NUMBER OF INVALID PIXELS REPLACED
*       DSUM,WTSUM (WORKSPACE)
*       REAL(NPIX,NLINES)
*               INTERMEDIATE STORAGE
*       DLAST,WTLAST (WORKSPACE)
*       REAL(NPIX)
*               INTERMEDIATE STORAGE
*
*CALLS
*       THIS PACKAGE:
*               LBGONE
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
      integer*2 ia(npix,nlines),ib(npix,nlines)
      real dsum(npix,nlines),wtsum(npix,nlines),dlast(npix),
     :wtlast(npix)
      character prbuf*80
      nbad=0
 
*
* SCAN THE IMAGE, COPYING INPUT PIXELS TO THE OUTPUT IMAGE
* AND COUNTING THE BAD PIXELS
*
 
      do 10 j=1,nlines
 
         do 9 i=1,npix
            ib(i,j)=ia(i,j)
 
            if(ia(i,j).eq.invala) nbad=nbad+1
9        continue
 
10    continue
 
 
*
* IF THERE ARE NO VALID PIXELS, ABORT
*
 
      if(nbad.eq.npix*nlines) go to 999
 
*
* IF THE PROGRESS OF THE ITERATIONS IS TO BE PRINTED, PRINT HEADINGS
*
 
      if(ilevel.ge.3) then
         call wruser(' ',istat)
         call wruser('      ITERATION    SMOOTHING LENGTH    MAX. CHAN'/
     :    / 'GE    RMS CHANGE',istat)
         call wruser('      ---------    ----------------    ---------'/
     :    / '--    ----------',istat)
      endif
 
 
*
* PERFORM THE REQUIRED NUMBER OF RELAXATION ITERATIONS
*
      lastmx=0
      maxcng=0
 
      do 144 iter=1,niter
 
*
* SET THE MAXIMUM ABSOLUTE CHANGE SO FAR
*
         lastmx=max(lastmx,maxcng,0)
 
*
* IF THE MAX CHANGE LAST ITERATION WAS LESS THAN 0.25 OF THE MAX CHANGE
* SO FAR, REDUCE THE SCALE SIZE BY A FACTOR 2 AND RESET THE MAX
* CHANGE SO FAR
*
 
         if(maxcng*4.le.lastmx.and.iter.ne.1) then
            size=size*0.5
            lastmx=maxcng
         endif
 
 
*
* INITIALLISE THE MAX ABSOLUTE CHANGE FOR THIS ITERATION
*
         maxcng=0
 
*
* CALCULATE THE LOGARITHMIC DECREMENT FOR THE WEIGHTS IN GOING FROM 1
* PIXEL TO THE NEXT
*
         dec=exp(-1.0/size)
 
*
* INITIALLISE STORAGE FOR FORMING WEIGHTED MEANS
*
 
         do 100 j=1,nlines
 
            do 99 i=1,npix
               dsum(i,j)=0.0
               wtsum(i,j)=0.0
99          continue
 
100      continue
 
 
*
* FIRST WORK THROUGH THE IMAGE LINES, SCANNING EACH LINE IN BOTH
* DIRECTIONS
*
 
         do 14 j=1,nlines
 
            do 15 idirn=-1,1,2
 
               if(idirn.ge.0) then
                  ifirst=1
                  ilast=npix
 
               else
                  ifirst=npix
                  ilast=1
               endif
 
 
*
* INITIALLISE STORES
*       DLAST: WEIGHTED SUM OF PREVIOUS DATA VALUES
*       WTLAST: SUM OF PREVIOUS WEIGHTS
*
               dlast(1)=0.0
               wtlast(1)=0.0
 
*
* PROCESS A LINE
*
 
               do 16 i=ifirst,ilast,idirn
 
*
* IF THE INPUT PIXEL IS VALID, RESET THE WEIGHTED SUMS
*
 
                  if(ia(i,j).ne.invala) then
                     dlast(1)=ib(i,j)
                     wtlast(1)=1.0
 
*
* FOR INVALID LOCATIONS, FORM SUMS FOR WEIGHTED MEAN
*
 
                  else
 
*
* DECREMENT THE PREVIOUS WEIGHT
*
                     wtlast(1)=wtlast(1)*dec
                     dlast(1)=dlast(1)*dec
 
*
* FORM SUMS FOR THE REPLACEMENT VALUE
*
                     dsum(i,j)=dsum(i,j)+dlast(1)
                     wtsum(i,j)=wtsum(i,j)+wtlast(1)
 
*
* IF THIS PIXEL HAS BEEN REPLACED BEFORE, ADD IT INTO THE CURRENT
* WEIGHTED SUMS FOR THIS LINE
*
 
                     if(ib(i,j).ne.invala) then
                        wtlast(1)=wtlast(1)+1.0
                        dlast(1)=dlast(1)+ib(i,j)
                     endif
 
                  endif
 
16             continue
 
15          continue
 
14       continue
 
 
*
* NOW PERFORM THE SAME PROCESS DOWN THE IMAGE COLUMNS, BUT PROCESSING
* A WHOLE LINE OF DATA AT ONCE
*
 
         do 115 jdirn=-1,1,2
 
            if(jdirn.ge.0) then
               jfirst=1
               jlast=nlines
 
            else
               jfirst=nlines
               jlast=1
            endif
 
 
*
* INITIALLISE STORES FOR A WHOLE LINE
*
 
            do 121 i=1,npix
               dlast(i)=0.0
               wtlast(i)=0.0
121         continue
 
 
*
* PROCESS COLUMNS, AS ABOVE, BUT USING A WHOLE LINE OF DATA
*
 
            do 116 j=jfirst,jlast,jdirn
 
               do 126 i=1,npix
 
                  if(ia(i,j).ne.invala) then
                     dlast(i)=ib(i,j)
                     wtlast(i)=1.0
 
                  else
                     wtlast(i)=wtlast(i)*dec
                     dlast(i)=dlast(i)*dec
                     dsum(i,j)=dsum(i,j)+dlast(i)
                     wtsum(i,j)=wtsum(i,j)+wtlast(i)
 
                     if(ib(i,j).ne.invala) then
                        wtlast(i)=wtlast(i)+1.0
                        dlast(i)=dlast(i)+ib(i,j)
                     endif
 
                  endif
 
126            continue
 
116         continue
 
115      continue
 
 
*
* SCAN THE INVALID PIXELS, REPLACING THOSE FOR WHICH A NEW WEIGHTED
* MEAN CAN BE FORMED
*
         rms=0.0
 
         do 201 j=1,nlines
 
            do 200 i=1,npix
 
*
* IF THE INPUT PIXEL WAS INVALID, AND A REPLACEMENT VALUE CAN BE
* FOUND, CALCULATE THE REPLACEMENT VALUE
*
 
               if(ia(i,j).eq.invala.and.wtsum(i,j).gt.0.0) then
                  newval=nint(dsum(i,j)/wtsum(i,j))
 
*
* FIND THE MAXIMUM ABSOLUTE CHANGE THIS ITERATION
*
                  idiff=abs(newval-ib(i,j))
                  maxcng=max(maxcng,idiff)
 
*
* FORM SUM FOR RMS CHANGE
*
                  rms=rms+real(idiff)**2
                  ib(i,j)=newval
               endif
 
200         continue
 
201      continue
 
 
*
* PRINT THE PROGRESS OF EACH ITERATION, IF REQUIRED
*
 
         if(ilevel.ge.3) then
 
*
* CALCULATE THE RMS CHANGE THIS ITERATION
*
            cngrms=scale*sqrt(rms/max(1,nbad))
            write(prbuf,33)iter,size,maxcng*scale,cngrms
33          format(6x,i6,9x,g13.6,4x,g13.6,2x,g13.6)
            call wruser(prbuf,istat)
         endif
 
144   continue
 
      cngmax=maxcng*scale
      cngrms=scale*sqrt(rms/max(nbad,1))
999   return
 
      end
 
 
 
