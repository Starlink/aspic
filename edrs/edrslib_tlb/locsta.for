      subroutine locsta(ia,npix,nlines,inval,x0,y0,isize,x,y,sigma,
     :ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO ACCURATELY LOCATE THE CENTRE OF A STAR IMAGE OF KNOWN SIZE
*
*METHOD
*       FORM X AND Y MARGINAL PROFILES OF THE STAR IMAGE USING THE
*       DATA WITHIN A SQUARE SEARCH AREA. CALL CLNSTA TO REMOVE THE
*       BACKGROUNDS AND NEIGHBOURING STARS, DIRT, ETC. FROM THESE
*       PROFILES. FIT A GAUSSIAN OF A SPECIFIED WIDTH TO EACH PROFILE
*       USING GAUFIS TO FIND THE CENTRES IN X AND Y. PERFORM 3 SUCH
*       ITERATIONS, CENTRING THE SEARCH SQUARE ON THE PREVIOUS POSITION
*       EACH TIME. IN FORMING THE MARGINAL PROFILES, A WEIGHTING
*       FUNCTION IS USED IN THE ORTHOGONAL DIRECTION TO EXCLUDE
*       NEIGHBOUTING STARS, ETC. AS FAR AS POSSIBLE.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE IMAGE CONTAINING THE STARS
*       NPIX,NLINES (IN)
*       INTEGER
*               THE IMAGE DIMENSIONS
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IMAGE IA
*       X0,Y0 (IN)
*       REAL
*               THE INITIAL APPROXIMATE STAR POSITION
*       ISIZE (IN)
*       INTEGER
*               THE LENGTH OF THE SIDE OF THE SQUARE SEARCH AREA
*       X,Y (OUT)
*       REAL
*               THE ACCURATE CENTRE OF THE STAR
*       SIGMA (IN)
*       REAL
*               AN ESTIMATE OF THE 'SIGMA' OF THE STAR IMAGE
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               CLNSTA,GAUFIS
*
*NOTES
*       USES INTEGER*2 ARRAYS
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
*
* SET THE MAXIMUM SEARCH SQUARE SIZE
*
      parameter (maxsiz=101,niter=3,ngauit=20,toll=0.005)
      integer*2 ia(npix,nlines)
      real xdata(maxsiz),ydata(maxsiz),xn(maxsiz),yn(maxsiz),xw(maxsiz)
     : ,yw(maxsiz)
 
*
* MAKE THE SQUARE SIZE ODD AND NOT GREATER THAN MAXSIZ
*
      ix=min(isize,maxsiz)
      idx=max(1,ix/2)
      x=x0
      y=y0
      ierr=0
 
*
* PERFORM NITER ITERATIONS, EACH TIME CENTERING THE SEARCH SQUARE
* ON THE PREVIOUS ESTIMATE OF THE IMAGE CENTRE
*
 
      do 97 iter=1,niter
 
*
* CALCULATE THE EDGES OF THE SEARCH SQUARE
*
         imin=max(1,nint(min(max(-1.0e8,x),1.0e8))-idx)
         imax=min(npix,nint(min(max(-1.0e8,x),1.0e8))+idx)
         jmin=max(1,nint(min(max(-1.0e8,y),1.0e8))-idx)
         jmax=min(nlines,nint(min(max(-1.0e8,y),1.0e8))+idx)
         nx=imax-imin+1
         ny=jmax-jmin+1
 
*
* IF THE SEARCH SQUARE LIES COMPLETELY OUTSIDE THE IMAGE, SET IERR=2
* AND QUIT
*
 
         if(nx.lt.1.or.ny.lt.1) then
            ierr=2
            go to 99
 
         endif
 
 
*
* CALCULATE A WEIGHTING FUNCTION TO BE USED IN THE X DIRECTION
* WHEN FORMING THE Y MARGINAL PROFILE
*
         rsig=1.0/sigma
         dx=(imin-1)-x
 
         do 20 i=1,nx
            dx=dx+1.0
            dev=dx*rsig
            dev2=dev*dev
 
*
* THE WEIGHTING FUNCTION FALLS AT LARGE DISTANCES FROM THE CENTRE
* TO EXCLUDE NEARBY STARS, ETC.
*
 
            if(dev2.le.25.0) then
               xw(i)=exp(-0.5*dev2)
 
            else
               xw(i)=0.0
            endif
 
 
*
* INITIALLISE ARRAYS FOR FORMING THE X MARGINAL PROFILE
*
            xdata(i)=0.0
            xn(i)=0.0
20       continue
 
 
*
* NOW FORM A SIMILAR WEIGHTING FUNCTION FOR THE Y DIRECTION
*
         dy=(jmin-1)-y
 
         do 21 j=1,ny
            dy=dy+1.0
            dev=dy*rsig
            dev2=dev*dev
 
            if(dev2.le.25.0) then
               yw(j)=exp(-0.5*dev2)
 
            else
               yw(j)=0.0
            endif
 
            ydata(j)=0.0
            yn(j)=0.0
21       continue
 
 
*
* SCAN THROUGH THE SEARCH SQUARE FORMING THE WEIGHTED MARGINAL
* PROFILES OF THE STAR
*
         jj=0
 
         do 81 j=jmin,jmax
            jj=jj+1
            ii=0
 
            do 80 i=imin,imax
               ii=ii+1
 
               if(ia(i,j).ne.inval) then
                  xdata(ii)=xdata(ii)+yw(jj)*ia(i,j)
                  ydata(jj)=ydata(jj)+xw(ii)*ia(i,j)
                  xn(ii)=xn(ii)+yw(jj)
                  yn(jj)=yn(jj)+xw(ii)
               endif
 
80          continue
 
81       continue
 
 
*
* EVALUATE THE X MARGINAL
*
 
         do 70 i=1,nx
 
            if(xn(i).gt.0.0) then
               xdata(i)=xdata(i)/xn(i)
 
            else
               xdata(i)=2.0e20
            endif
 
70       continue
 
 
*
* EVALUATE THE Y MARGINAL
*
 
         do 71 j=1,ny
 
            if(yn(j).gt.0.0) then
               ydata(j)=ydata(j)/yn(j)
 
            else
               ydata(j)=2.0e20
            endif
 
71       continue
 
 
*
* CALL CLNSTA TO SUBTRACT THE BACKGROUND FROM EACH PROFILE
* AND TO REMOVE CORRUPT DATA AND NEIGHBOURING STARS
*
         call clnsta(xdata,imin,imax,x)
         call clnsta(ydata,jmin,jmax,y)
 
*
* ADJUST EACH PROFILE TO EXCLUDE DATA AT LARGE DISTANCES
*
 
         do 91 i=1,nx
            xdata(i)=max(0.0,xdata(i)*xw(i)/(0.5+xw(i)))
91       continue
 
 
         do 92 j=1,ny
            ydata(j)=max(0.0,ydata(j)*yw(j)/(0.5+yw(j)))
92       continue
 
 
*
* CALL GAUFIS TO FIT A GAUSSIAN PROFILE OF A SPECIFIED SIGMA
* TO EACH PROFILE TO FIND THE STAR CENTRE
*
         call gaufis(xdata,imin,imax,ngauit,toll,ax,x,sigma,ierr)
 
         if(ierr.ne.0) go to 99
         call gaufis(ydata,jmin,jmax,ngauit,toll,ay,y,sigma,ierr)
 
         if(ierr.ne.0) go to 99
97    continue
 
99    continue
      return
 
      end
 
 
 
