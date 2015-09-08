      subroutine starim(ia,npix,nlines,inval,x0,y0,nxy,isize,sig0,axisr
     : ,theta,ngood,sig)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIND THE MEAN AXIS RATIO, SEEING DISK SIZE AND INCLINATION
*       OF A SET OF ELLIPTICAL STAR IMAGES.
*
*METHOD
*       FOR EACH STAR, FORM MARGINAL PROFILES IN X AND Y USING DATA
*       IN A SQUARE SEARCH AREA. ALSO FORM MARGINAL PROFILES IN
*       DIRECTIONS P AND Q, INCLINED AT 45 DEGREES TO X AND Y, USING
*       DATA IN A SEARCH SQUARE INCLINED AT 45 DEGREES. CALL CLNSTA
*       TO REMOVE BACKGROUNDS, NEIGHBOURING STARS, DIRT, ETC. FROM
*       THESE PROFILES, THEN CALL GAUFIT TO FIT A GAUSSIAN TO EACH,
*       TO DETERMINE THE CENTRE, WIDTH AND AMPLITUDE. CALCULATE A MEAN
*       CENTRE FOR EACH STAR. COMBINE THE 4 WIDTH ESTIMATES FROM EACH
*       STAR USING WMODE TO GIVE 4 MEAN WIDTHS, THEN CALL ELLIPS TO
*       CALCULATE THE MEAN STAR IMAGE ELLIPSE PARAMETERS.
*
*ARGUMENTS
*       IA (IN)
*       INTEGER*2(NPIX,NLINES)
*               THE INPUT IMAGE CONTAINING THE STAR IMAGES
*       NPIX,NLINES (IN)
*       INTEGER
*               THE DIMENSIONS OF IA
*       INVAL (IN)
*       INTEGER
*               INVALID PIXEL FLAG FOR IA
*       X0,Y0 (IN/OUT)
*       REAL(NXY)
*               INITIAL APPROXIMATE POSITIONS FOR EACH STAR IMAGE
*               IF THE STAR IS FOUND, AN ACCURATE POSITION IS RETURNED
*       NXY (IN)
*       INTEGER
*               THE NUMBER OF STAR IMAGES TO BE USED
*       ISIZE (IN)
*       INTEGER
*               THE LENGTH OF THE SIDE OF THE SEARCH SQUARE
*       SIG0 (OUT)
*       REAL
*               THE MEAN STAR 'SIGMA'
*       AXISR (OUT)
*       REAL
*               THE MEAN STAR AXIS RATIO
*       THETA (OUT)
*       REAL
*               THE MEAN INCLINATION OF THE STAR MAJOR AXES TO THE
*               X DIRECTION IN RADIANS (X THROUGH Y POSITIVE)
*       NGOOD (OUT)
*       INTEGER
*               THE NUMBER OF STARS SUCCESSFULLY FOUND AND USED IN THE
*               FIT
*       SIG (OUT)
*       REAL(NXY,5)
*               SIG(I,1), SIG(I,2), SIG(I,3) AND SIG(I,4) RETURN THE
*               GAUSSIAN WIDTHS OF STAR I IN DIRECTIONS INCLINED AT
*               0, 45, 90 AND 135 DEGREES TO THE X AXIS (X THROUGH Y
*               POSITIVE). SIG(I,5) RETURNS THE SUM OF THE AMPLITUDES
*               OF THE 4 PROFILES OF STAR I (I.E. IT IS PROPORTIONAL
*               TO THE AMPLITUDE OF STAR I). IF A STAR WAS NOT FOUND,
*               ALL ITS ENTRIES IN SIG ARE ZERO.
*
*CALLS
*       THIS PACKAGE:
*               CLNSTA,GAUFIT,WMODE,ELLIPS
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
* SET MAXIMUM RADIUS FROM CENTRE FOR FORMING THE MARGINAL PROFILES
* AND SET MAXP TO SQRT(2.0) TIMES THIS TO ALLOW BINNING IN THE 45
* DEGREE DIRECTION ALSO
*
      parameter (maxx=50,maxp=1.41421*maxx+1.0,maxsiz=2*maxx+1)
 
*
* DIMENSION ARRAYS
*
      integer*2 ia(npix,nlines)
      integer nx(-maxx:maxx),ny(-maxx:maxx),np(-maxp:maxp),nq(-
     :maxp:maxp),star,bini,binj,binp,binq
      real x0(nxy),y0(nxy),sig(nxy,5),xsum(-maxx:maxx),ysum(-maxx:maxx)
     : ,psum(-maxp:maxp),qsum(-maxp:maxp),sigma(4)
      parameter (niter=3,itmode=10,toll=0.001,ngauit=15)
 
*
* DETERMINE THE SIZE OF THE SEARCH AREA AS ODD AND NOT EXCEEDING
* MAXSIZ, THEN DETERMINE THE CORRESPONDING NUMBER OF BINS IN THE
* 45 DEGREE DIRECTIONS
*
      ix=min(isize,maxsiz)
      idx=max(1,ix/2)
      idp=nint(1.41421*idx)
 
*
* CONSIDER EACH STAR POSITION IN TURN, COUNTING SUCCESSES IN NGOOD
*
      ngood=0
 
      do 800 star=1,nxy
         x=x0(star)
         y=y0(star)
 
*
* PERFORM NITER ITERATIONS, EACH TIME CENTERING THE SEARCH AREA ON
* THE PREVIOUS ESTIMATE OF THE STAR CENTRE
*
 
         do 87 iter=1,niter
            i0=nint(min(max(-1.0e8,x),1.0e8))
            j0=nint(min(max(-1.0e8,y),1.0e8))
 
*
* INITIALLISE ARRAYS FOR FORMING THE MARGINAL PROFILES IN THE
* X AND Y DIRECTIONS AND AT 45 DEGREES (P AND Q)
*
 
            do 41 i=-idx,idx
               xsum(i)=0.0
               ysum(i)=0.0
               nx(i)=0
               ny(i)=0
41          continue
 
 
            do 42 i=-idp,idp
               psum(i)=0.0
               qsum(i)=0.0
               np(i)=0
               nq(i)=0
42          continue
 
 
*
* NOW FORM THE MARGINAL PROFILES, SCANNING A LARGE ENOUGH IMAGE
* AREA TO ACCOMMODATE THE SEARCH SQUARE TURNED THROUGH 45 DEGREES
*
 
            do 81 binj=-idp,idp
               j=j0+binj
 
*
* CHECK THAT WE ARE STILL INSIDE THE IMAGE
*
 
               if(j.ge.1.and.j.le.nlines) then
 
                  do 80 bini=-idp,idp
                     i=i0+bini
 
                     if(i.ge.1.and.i.le.npix) then
 
*
* IF THE PIXEL IS VALID, FIND THE P,Q COORDINATES
*
 
                        if(ia(i,j).ne.inval) then
                           binp=bini+binj
                           binq=binj-bini
 
*
* IF THE PIXEL LIES IN THE NORMAL SEARCH SQUARE, ADD IT TO THE
* X AND Y MARGINALS
*
 
                           if(abs(bini).le.idx.and.abs(binj).le.idx)
     :                       then
                              xsum(bini)=xsum(bini)+ia(i,j)
                              ysum(binj)=ysum(binj)+ia(i,j)
                              nx(bini)=nx(bini)+1
                              ny(binj)=ny(binj)+1
                           endif
 
 
*
* IF THE PIXEL LIES IN THE 45 DEGREE SQUARE, ADD IT TO THE P AND Q
* MARGINALS
*
 
                           if(abs(binp).le.idp.and.abs(binq).le.idp)
     :                       then
                              psum(binp)=psum(binp)+ia(i,j)
                              qsum(binq)=qsum(binq)+ia(i,j)
                              np(binp)=np(binp)+1
                              nq(binq)=nq(binq)+1
                           endif
 
                        endif
 
                     endif
 
80                continue
 
               endif
 
81          continue
 
 
*
* EVALUATE THE X AND Y MARGINALS
*
 
            do 31 i=-idx,idx
 
               if(nx(i).gt.0) then
                  xsum(i)=xsum(i)/nx(i)
 
               else
                  xsum(i)=2.0e20
               endif
 
 
               if(ny(i).gt.0) then
                  ysum(i)=ysum(i)/ny(i)
 
               else
                  ysum(i)=2.0e20
               endif
 
31          continue
 
 
*
* EVALUATE THE P AND Q MARGINALS
*
 
            do 32 i=-idp,idp
 
               if(np(i).gt.0) then
                  psum(i)=psum(i)/np(i)
 
               else
                  psum(i)=2.0e20
               endif
 
 
               if(nq(i).gt.0) then
                  qsum(i)=qsum(i)/nq(i)
 
               else
                  qsum(i)=2.0e20
               endif
 
32          continue
 
 
*
* CALL CLNSTA TO REMOVE THE BACKGROUND AND NEIGHBOURING STARS, DIRT
* ETC. FROM EACH PROFILE
*
            call clnsta(xsum(-idx),-idx,idx,0.0)
            call clnsta(ysum(-idx),-idx,idx,0.0)
            call clnsta(psum(-idp),-idp,idp,0.0)
            call clnsta(qsum(-idp),-idp,idp,0.0)
 
*
* CALL GAUFIT TO FIT A GAUSSIAN TO EACH PROFILE
*
            call gaufit(xsum(-idx),-idx,idx,ngauit,toll,ax,xcen,sigx
     :       ,back,ierrx)
            call gaufit(ysum(-idx),-idx,idx,ngauit,toll,ay,ycen,sigy
     :       ,back,ierry)
            call gaufit(psum(-idp),-idp,idp,ngauit,toll,ap,pcen,sigp
     :       ,back,ierrp)
            call gaufit(qsum(-idp),-idp,idp,ngauit,toll,aq,qcen,sigq
     :       ,back,ierrq)
 
*
* IF NO FATAL ERRORS WERE ENCOUNTERED, CALCULATE A MEAN CENTRE
* POSITION FROM THE CENTRES OF EACH PROFILE. OTHERWISE EXIT
* FROM THE ITERATION LOOP WHICH FINDS THE CENTRE
*
            ierrft=max(ierrx,ierry,ierrp,ierrq)
 
            if(ierrft.eq.0) then
               xnew=i0+(xcen+0.5*(pcen-qcen))*0.5
               ynew=j0+(ycen+0.5*(pcen+qcen))*0.5
 
*
* CALCULATE SHIFT OF CENTRE THIS ITERATION... IF IT SATISFIES THE
* ACCURACY CRITERION, EXIT FROM THE CENTRE-FINDING ITERATION LOOP
*
               shift=sqrt((x-xnew)**2+(y-ynew)**2)
               x=xnew
               y=ynew
 
               if(shift.le.toll) then
                  go to 601
 
               endif
 
 
            else
               go to 601
 
            endif
 
87       continue
 
 
*
* IF THE CENTRE WAS FOUND SUCCESSFULLY, RECORD THE WIDTHS OF EACH
* PROFILE AND THE STAR CENTRE AND FORM A WEIGHT FROM
* THE STAR AMPLITUDE
*
 
601      if(ierrft.eq.0) then
            ngood=ngood+1
 
*
* CORRECT FOR DIFFERENT BIN SPACING IN THE P,Q DIRECTIONS
*
            sigp=0.7071068*sigp
            sigq=0.7071068*sigq
            x0(star)=x
            y0(star)=y
            sig(star,1)=sigx
            sig(star,2)=sigp
            sig(star,3)=sigy
            sig(star,4)=sigq
            sig(star,5)=ap+aq+ax+ay
 
*
* IF THE CENTRE WAS NOT FOUND SUCCESSFULLY, RECORD THE WEIGHT AS ZERO
*
 
         else
            sig(star,1)=0.0
            sig(star,2)=0.0
            sig(star,3)=0.0
            sig(star,4)=0.0
            sig(star,5)=0.0
         endif
 
800   continue
 
 
*
* IF AT LEAST ONE STAR WAS SUCCESSFUL, CALL WMODE TO FIND A MEAN
* WIDTH FOR EACH PROFILE DIRECTION
*
 
      if(ngood.ge.1) then
 
         do 34 nsig=1,4
            call wmode(sig(1,nsig),sig(1,5),ngood,0.01,itmode,toll,
     :      sigma(nsig),err)
34       continue
 
 
*
* CALL ELLIPS TO FIND THE PARAMETERS OF AN ELLIPTICAL STAR IMAGE
* FROM THE MARGINAL WIDTHS
*
         call ellips(sigma,sig0,axisr,theta)
      endif
 
      return
 
      end
 
 
 
