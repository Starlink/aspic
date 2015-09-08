      subroutine locate(x0,y0,image,npix,nlines,inval,isize,isign,
     :shftmx,maxit,toll,x,y,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO LOCATE THE CENTROIDS OF STAR LIKE IMAGE FEATURES
*
*METHOD
*       FORM MARGINAL PROFILES WITHIN A SEARCH SQUARE. SUBTRACT A
*       BACKGROUND ESTIMATE FROM EACH PROFILE AND FIND THE PROFILE
*       CENTROIDS. REPEAT FOR A SET NUMBER OF ITERATIONS , OR UNTIL
*       THE REQUIRED ACCURACY IS REACHED.
*
*ARGUMENTS
*       X0,Y0 (IN)
*       REAL
*               THE INITIAL ESTIMATE OF THE FEATURE POSITION
*       IMAGE (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IMAGE
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IMAGE
*       ISIZE (IN)
*       INTEGER
*               SIZE OF THE SEARCH SQUARE SIDE
*       ISIGN (IN)
*       INTEGER
*               POSITIVE IF THE IMAGE FEATURES ARE POSITIVE, NEGATIVE
*               IF THEY ARE NEGATIVE
*       SHFTMX (IN)
*       REAL
*               MAX SHIFT ALLOWED FROM INITIAL POSITION X0,Y0
*       MAXIT (IN)
*       INTEGER
*               NUMBER OF CENTROIDING ITERATIONS
*       TOLL (IN)
*       REAL
*               ACCURACY REQUIRED IN THE CENTROID POSITION. ITERATIONS
*               STOP WHEN THIS ACCURACY HAS BEEN MET
*       X,Y (OUT)
*       REAL
*               CENTROID POSITION FOUND. IF IERR IS NOT ZERO, X AND Y
*               RETURN THE VALUES OF X0,Y0
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG, 0:SUCCESS
*                           1:NO DATA IN SEARCH AREA
*                           2:SHFTMX EXCEEDED
*
*CALLS
*       THIS PACKAGE:
*               NTHMIN
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      integer*2 image(npix,nlines)
 
*
* SET MAXIMUM SIZE OF SEARCH AREA
*
      parameter (maxsiz=51)
      real xav(maxsiz),yav(maxsiz),stak(maxsiz/4+2)
      integer nxav(maxsiz),nyav(maxsiz)
 
*
* SET SQUARE CENTRE TO STARTING POSITION AND CHECK ARGUMENT VALIDITY
*
      x=x0
      y=y0
      ierr=0
 
      if(npix.lt.1) then
         ierr=3
 
      else if(nlines.lt.1) then
         ierr=4
 
      else
 
*
* RESTRICT SEARCH SQUARE SIZE TO LIE FROM 3 TO MAXSIZ
*
         nsamp=min(max(3,isize),maxsiz)
         ihalf=nsamp/2
 
*
* MAKE NUMBER OF ITERATIONS AT LEAST 1
*
         niter=max(1,maxit)
 
*
* START COUNTING ITERATIONS
* -------------------------
*
         iter=0
63       iter=iter+1
 
*
* FIND STARTING EDGE OF SEARCH SQUARE
*
         istart=nint(min(max(-1.0e6,x),1.0e6))-(ihalf+1)
         jstart=nint(min(max(-1.0e6,y),1.0e6))-(ihalf+1)
 
*
* REMEMBER STARTING POSITION THIS ITERATION
*
         xlast=x
         ylast=y
 
*
* INITIALLISE ARRAYS FOR FORMING MARGINAL PROFILES
*
 
         do 10 nbin=1,nsamp
            xav(nbin)=0.0
            yav(nbin)=0.0
            nxav(nbin)=0
            nyav(nbin)=0
10       continue
 
 
*
* SCAN SEARCH AREA, FORMING X AND Y PROFILES FROM ALL VALID PIXELS
*
 
         do 30 j=1,nsamp
            jposn=jstart+j
 
            if((jposn.ge.1).and.(jposn.le.nlines)) then
 
               do 20 i=1,nsamp
                  iposn=istart+i
 
                  if((iposn.ge.1).and.(iposn.le.npix)) then
 
                     if(image(iposn,jposn).ne.inval) then
                        xav(i)=xav(i)+image(iposn,jposn)
                        yav(j)=yav(j)+image(iposn,jposn)
                        nxav(i)=nxav(i)+1
                        nyav(j)=nyav(j)+1
                     endif
 
                  endif
 
20             continue
 
            endif
 
30       continue
 
 
*
* EVALUATE THOSE X PROFILE BINS WHICH CONTAIN AT LEAST 1 VALID PIXEL,
* INVERT THE RESULTS IF ISIGN.LT.0
*
 
         do 80 nbin=1,nsamp
 
            if(nxav(nbin).eq.0) then
               xav(nbin)=1.0e10
 
            else
               xav(nbin)=xav(nbin)/nxav(nbin)
 
               if(isign.lt.0) then
                  xav(nbin)=-xav(nbin)
               endif
 
            endif
 
 
*
* REPEAT FOR THE Y PROFILE
*
 
            if(nyav(nbin).eq.0) then
               yav(nbin)=1.0e10
 
            else
               yav(nbin)=yav(nbin)/nyav(nbin)
 
               if(isign.lt.0) then
                  yav(nbin)=-yav(nbin)
               endif
 
            endif
 
80       continue
 
 
*
* CALL NTHMIN TO FIND THE LOWER QUARTILE POINT IN EACH PROFILE AS A
* BACKGROUND ESTIMATE
*
         nth=max(nsamp/4,2)
         call nthmin(xav,nsamp,nth,stak,ierr)
         zlevx=stak(1)
         call nthmin(yav,nsamp,nth,stak,ierr)
         zlevy=stak(1)
 
*
* INITIALLISE SUMS FOR FORMING CENTROIDS
*
         xnumer=0.0
         xdenom=0.0
         ynumer=0.0
         ydenom=0.0
         xposn=istart
         yposn=jstart
 
*
* SCAN THE PROFILES, USING ALL DATA ABOVE THE BACKGROUND TO FORM
* SUMS FOR THE CENTROIDS
*
 
         do 110 nbin=1,nsamp
            xposn=xposn+1.0
            yposn=yposn+1.0
 
            if(nxav(nbin).ne.0) then
               data=max(xav(nbin)-zlevx,0.0)
               xnumer=xnumer+xposn*data
               xdenom=xdenom+data
            endif
 
 
            if(nyav(nbin).ne.0) then
               data=max(yav(nbin)-zlevy,0.0)
               ynumer=ynumer+yposn*data
               ydenom=ydenom+data
            endif
 
110      continue
 
 
*
* IF EITHER PROFILE CONTAINED NO DATA, ABORT WITH IERR=2
*
 
         if((xdenom.lt.1.0e-10).and.(ydenom.lt.1.0e-10)) then
            ierr=2
            x=x0
            y=y0
 
         else
 
            if(xdenom.lt.1.0e-10)then
               x=xlast
               y=ynumer/ydenom
 
            else if(ydenom.lt.1.0e-10)then
               x=xnumer/xdenom
               y=ylast
 
            else
 
*
* OTHERWISE FORM THE X AND Y CENTROIDS AND FIND THE SHIFT FROM
* THE INITIAL POSITION
*
               x=xnumer/xdenom
               y=ynumer/ydenom
            endif
 
            shift=sqrt((x-x0)**2+(y-y0)**2)
 
*
* IF MAX SHIFT IS EXCEEDED, ABORT WITH IERR=1
*
 
            if(shift.gt.shftmx) then
               ierr=1
               x=x0
               y=y0
 
            else
 
*
* OTHERWISE FIND THE POSITION SHIFT THIS ITERATION
*
               psncng=sqrt((x-xlast)**2+(y-ylast)**2)
 
*
* IF REQUIRED ACCURACY HAS BEEN MET, EXIT. OTHERWISE, IF ITERATIONS
* REMAIN, GO ROUND ITERATION LOOP AGAIN
*
 
               if(psncng.gt.toll) then
 
                  if(iter.lt.niter) go to 63
               endif
 
            endif
 
         endif
 
      endif
 
      return
 
      end
 
 
 
