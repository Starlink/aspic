      subroutine photry(ia,npix,nlines,inval,scale,zero,x0,y0,iside
     : ,sigma,e,theta,g,range,x,y,a,b,toti,totmag,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO CALCULATE THE INTEGRATED INTENSITY OF A STAR
*
*METHOD
*       CALL LOCSTA TO LOCATE THE STAR CENTRE. PUT THE SURROUNDING
*       BINS INTO CIRCULAR OR ELLIPTICAL ISOPHOTAL ZONES, AND MAINTAIN
*       A LINKED LIST OF ALL THE VALID PIXELS IN EACH ZONE. IN EACH ZONE
*       CALL MODE TO FIND A MOST LIKELY DATA VALUE, AND FIT THE
*       SPECIFIED RADIAL STAR PROFILE TO THE RESULTING DATA POINTS USING
*       LINEAR LEAST SQUARES. FINALLY CALCULATE THE INTEGRATED INTENSITY
*       OF THE FITTED FUNCTION.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE IMAGE
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       SCALE,ZERO (IN)
*       REAL
*               SCALE AND ZERO LEVEL FOR IA
*       X0,Y0 (IN)
*       REAL
*               INITIAL APPROXIMATE STAR CENTRE
*       ISIDE (IN)
*       INTEGER
*               SIDE OF SQUARE WITHIN WHICH DATA IS USED
*       SIGMA (IN)
*       REAL
*               THE 'SIGMA' OF THE STAR PROFILE
*       E (IN)
*       REAL
*               THE AXIS RATIO OF THE ELLIPTICAL STAR IMAGE
*       THETA (IN)
*       REAL
*               THE INCLINATION OF THE MAJOR AXIS TO THE X DIRECTION
*               IN RADIANS (X THROUGH Y POSITIVE)
*       G (IN)
*       REAL
*               THE EXPONENT IN THE RADIAL STAR PROFILE
*       RANGE (IN)
*       REAL
*               THE RADIUS, IN UNITS OF THE STAR 'SIGMA', OUT TO
*               WHICH THE RADIAL PROFILE IS FITTED
*       X,Y (OUT)
*       REAL
*               THE ACCURATE STAR CENTRE
*       A (OUT)
*       REAL
*               THE STAR CENTRAL AMPLITUDE
*       B (OUT)
*       REAL
*               THE BACKGROUND LEVEL
*       TOTI (OUT)
*       REAL
*               THE INTEGRATED INTENSITY OF THE STAR
*       TOTMAG (OUT)
*       REAL
*               THE MAGNITUDE (-2.5*LOG10(TOTI))
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       THIS PACKAGE:
*               LOCSTA,MODE
*       NAG LIBRARY:
*               S14AAF
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
* SET THE MAXIMUM RADIUS TO BE USED IN THE RADIAL PROFILE
* (MAXRAD) AND THE PROBABILITY OF CORRUPT DATA (PBAD)
*
      parameter (maxrad=50,pbad=0.01,maxpts=(2*maxrad+1)**2,niter=5
     : ,twopi=6.283185)
 
*
* SET THE RESOLUTION AT WHICH THE IMAGE WILL BE BINNED INTO
* ISOPHOTAL ZONES (NO. OF BINS PER PIXEL SPACING)
*
      parameter (resol=3.0,maxbin=maxrad*resol+1.0)
 
*
* DIMENSION ARRAYS... NOTE IMAGE IS TREATED AS A 1D ARRAY
*
      integer*2 ia(npix*nlines)
      real data(maxpts),r(0:maxbin)
      integer npts(0:maxbin),lstart(0:maxbin),nxtadr(maxpts),
     :iadr(maxpts),bin
      double precision sumw,sumf,sumf2,sumdf,sumd,s14aaf
      ierr=0
 
*
* CALCULATE A MEAN SIGMA AND CALL LOCSTA TO LOCATE THE STELLAR
* IMAGE
*
      sig=sigma*(1.0+0.5*e)
      x=x0
      y=y0
 
*
* IF THE STAR WAS FOUND, CONTINUE. OTHERWISE RETURN WITH THE
* VALUE OF IERR
*
 
      if(ierr.eq.0) then
 
*
* CALCULATE THE CONSTANTS RELATING RADIAL DISTANCE TO EFFECTIVE
* DISTANCE ALONG THE MINOR AXIS FOR AN ELLIPTICAL STAR IMAGE
*
         s=sin(theta)
         c=cos(theta)
         re2=(1.0/e)**2
         consta=s*s+c*c*re2
         constb=c*c+s*s*re2
         constc=2.0*c*s*(re2-1.0)
 
*
* CALCULATE THE SCALE FACTOR TO CONVERT RADIAL DISTANCE TO BINS
*
         rsig=1.0/sigma
         rscale=resol*2.0/min(2.0,sigma)
 
*
* CALCULATE THE MAXIMUM EFFECTIVE RADIUS SET BY THE FITTING LIMIT
* 'RANGE'
*
         rlimit=range*sigma
 
*
* CALCULATE THE SEARCH SQUARE SIZE REQUIRED TO ACCOMMODATE THE MAXIMUM
* RADIUS
*
         ishift=min(max(1,nint(rlimit*e)),maxrad)
 
*
* CALCULATE THE MAXIMUM NUMBER OF RADIAL BINS REQUIRED TO HOLD THE
* RADIAL PROFILE
*
         mxbinn=min(maxbin,int(rlimit*rscale))
 
*
* CALCULATE THE EDGES OF THE SQUARE
*
         i0=nint(x)
         j0=nint(y)
         imin=max(1,i0-ishift)
         imax=min(npix,i0+ishift)
         jmin=max(1,j0-ishift)
         jmax=min(nlines,j0+ishift)
 
*
* INITIALLISE ARRAYS FOR STORING MEAN RADII OF ISOPHOTAL BINS
* AND FOR FORMING LINKED LISTS OF ALL THE DATA POINTS IN
* EACH ISOPHOTAL ZONE
*
 
         do 37 bin=0,mxbinn
            r(bin)=0.0
            npts(bin)=0
            lstart(bin)=0
37       continue
 
 
*
* SCAN THROUGH THE SQUARE AROUND THE STAR, CALCULATING THE
* X AND Y DISPLACEMENTS FROM THE CENTRE
*
         index=0
         dy=jmin-y-1.0
         dx0=imin-x-1.0
 
         do 97 j=jmin,jmax
            imlocn=(j-1)*npix+imin-1
            dy=dy+1.0
            dy2=dy*dy
            dx=dx0
 
            do 96 i=imin,imax
               imlocn=imlocn+1
               dx=dx+1.0
 
*
* IF THE PIXEL IS VALID, THEN CALCULATE THE EFFECTIVE RADIUS
*
 
               if(ia(imlocn).ne.inval) then
                  dx2=dx*dx
                  dxy=dx*dy
                  rdash=sqrt(consta*dx2+constb*dy2+constc*dxy)
 
*
* FIND THE ISOPHOTAL BIN FOR THIS PIXEL
*
                  bin=rdash*rscale
 
                  if(bin.le.mxbinn.and.rdash.le.rlimit) then
 
*
* FORM SUMS FOR THE MEAN EFFECTIVE RADIUS
*
                     npts(bin)=npts(bin)+1
                     r(bin)=r(bin)+rdash
 
*
* STORE A POINTER TO THE PIXEL IN A LINKED LIST OF ALL THE PIXELS
* IN THIS ISOPHOTAL ZONE
*
                     index=index+1
                     iadr(index)=imlocn
                     nxtadr(index)=lstart(bin)
                     lstart(bin)=index
                  endif
 
               endif
 
96          continue
 
97       continue
 
 
*
* INITIALLISE SUMS FOR FORMING THE LEAST SQUARES FIT OF THE
* SPECIFIED RADIAL PROFILE OF THE STAR
*
         sumw=0.0d0
         sumf=0.0d0
         sumf2=0.0d0
         sumdf=0.0d0
         sumd=0.0d0
 
*
* COUNT THROUGH EACH OF THE ISOPHOTAL BINS, CONSIDERING THOSE
* CONTAINING 1 OR MORE DATA POINTS
*
 
         do 100 bin=0,mxbinn
 
            if(npts(bin).ge.1) then
 
*
* OBTAIN THE LOCATION OF THE START OF THE LINKED LIST OF VALUES
* IN THE BIN, THEN READ EACH VALUE AND STORE IN THE ARRAY 'DATA'
*
               icount=0
               index=lstart(bin)
 
144            if(index.gt.0) then
                  icount=icount+1
                  data(icount)=ia(iadr(index))
                  index=nxtadr(index)
                  go to 144
 
               endif
 
 
*
* CALL MODE TO FIND THE MOST LIKELY DATA VALUE WITHIN THE BIN
*
               call mode(data,npts(bin),pbad,niter,0.1,dmode,dsig)
 
*
* FIND THE MEAN RADIUS FOR THE BIN AND CALCULATE THE STELLAR PROFILE
*
               radius=r(bin)/npts(bin)
               funct=exp(-0.5*((radius*rsig)**g))
 
*
* NOW FORM THE SUMS FOR THE LEAST SQUARES FIT, USING THE NUMBER OF
* POINTS AS A WEIGHT
*
               wt=npts(bin)
               fw=funct*wt
               sumw=sumw+wt
               sumf=sumf+fw
               sumf2=sumf2+(funct*fw)
               sumdf=sumdf+(dmode*fw)
               sumd=sumd+(dmode*wt)
            endif
 
100      continue
 
 
*
* IF THE NORMAL EQUATIONS ARE SINGULAR, RETURN WITH IERR=4
*
         det=sumf2*sumw-sumf*sumf
 
         if(det.eq.0.0) then
            ierr=4
            go to 99
 
         endif
 
 
*
* SOLVE THE NORMAL EQUATIONS FOR THE STAR CENTRAL DENSITY AND THE
* BACKGROUND, APPLYING THE SCALE AND ZERO FOR THE IMAGE
*
         a=(sumw*sumdf-sumd*sumf)/det
         b=(sumf2*sumd-sumdf*sumf)/det
         a=a*scale
         b=b*scale+zero
 
*
* CALCULATE THE INTEGRATED INTENSITY UNDER THE FITTED PROFILE
* (S14AAF EVALUATES THE GAMMA FUNCTION)
*
         ifail=1
         toti=((twopi*a*e/g)*((2.0*sigma**g)**(2.0/g)))*s14aaf(dble(2
     :    .0/g),ifail)
 
         if(toti.gt.0.0) then
            totmag=-2.5*log10(toti)
 
         else
            totmag=0.0
            ierr=5
         endif
 
      endif
 
99    return
 
      end
 
 
 
