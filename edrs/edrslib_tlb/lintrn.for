      subroutine lintrn(xa,ya,xb,yb,ok,n,c,ifit,ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO OBTAIN A LINEAR TRANSFORMATION BETWEEN 2 SETS OF X,Y
*       POSITIONS WITH LEAST SQUARED ERROR.
*
*METHOD
*       SET UP THE NORMAL EQUATIONS CORRESPONDING TO THE TYPE OF FIT
*       REQUIRED. SOLVE NORMAL EQUATIONS TO GIVE THE TRANSFORMATION.
*       IF SUCCESSFUL, EXIT. OTHERWISE REDUCE THE NUMBER OF DEGREES
*       OF FREEDOM IN THE FIT AND REPEAT.
*
*ARGUMENTS
*       XA,YA (IN)
*       REAL(N)
*               THE FIRST SET OF X,Y POSITIONS WHICH ARE TO BE
*               TRANSFORMED
*       XB,YB (IN)
*       REAL(N)
*               THE SECOND (REFERENCE) SET OF POSITIONS
*       OK (IN)
*       LOGICAL(N)
*               A SET OF LOGICAL FLAGS INDICATING WHICH POSITIONS ARE
*               TO BE USED
*       N (IN)
*       INTEGER
*               THE SIZE OF ARRAYS XA,YA,XB,YB,OK
*       C (OUT)
*       REAL(6)
*               THE SIX COEFFICIENTS DEFINING THE TRANSFORMATION
*       IFIT (IN/OUT)
*       INTEGER
*               THE TYPE OF FIT REQUIRED:
*               1:SHIFT OF ORIGIN
*               2:SHIFT AND ROTATION
*               3:SHIFT, ROTATION AND MAGNIFICATION
*               4:FULL 6 PARAMETER TRANSFORMATION
*               IFIT MAY BE REDUCED ON OUTPUT IF THE SPECIFIED FIT
*               WAS NOT OBTAINED
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*
*CALLS
*       NAG LIBRARY:
*               F04AEF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      real xa(n),ya(n),xb(n),yb(n),c(6)
      double precision a(4,4),b(4,2),ans(4,2),wkspce(4),aa(4,4),bb(4,2)
      logical ok(n)
 
*
* CHECK VALIDITY OF ARGUMENTS
*
      ierr=0
 
      if(n.lt.1) then
         ierr=1
 
      else
 
*
* IS AT LEAST 1 POSITION VALID?
*
         sw=0.0
 
         do 10 i=1,n
 
            if(ok(i)) sw=sw+1.0
10       continue
 
 
         if(sw.le.0.0) then
            ierr=2
 
         else
 
*
* OK...SET TYPE OF FIT REQUIRED BETWEEN 1 AND 4
*
            ifit=min(max(1,ifit),4)
 
*
* CHECK THAT THE FIT DOES NOT HAVE TOO MANY DEGREES OF FREEDOM FOR
* THE NUMBER OF DATA POINTS AVAILABLE
*
            npts=nint(sw)
 
            if(npts.le.2) ifit=min(ifit,3)
 
            if(npts.le.1) ifit=1
 
*
* INITIALLISE SUMS FOR NORMAL EQUATIONS
*
            swx=0.0
            swy=0.0
            swxy=0.0
            swx2=0.0
            swy2=0.0
            swxd=0.0
            swyd=0.0
            swxxd=0.0
            swyyd=0.0
            swxyd=0.0
            swyxd=0.0
 
*
* FORM SUMS, SETTING WEIGHT TO ZERO FOR INVALID POSITIONS
*
 
            do 20 i=1,n
 
               if(ok(i)) then
                  w=1.0
 
               else
                  w=0.0
               endif
 
               wx=w*xa(i)
               wy=w*ya(i)
               swx=swx+wx
               swy=swy+wy
               swxd=swxd+w*xb(i)
               swyd=swyd+w*yb(i)
 
*
* IF FIT ONLY REQUIRES A SHIFT OF ORIGIN, FURTHER SUMS ARE NOT
* REQUIRED
*
 
               if(ifit.ne.1) then
                  swxy=swxy+wx*ya(i)
                  swx2=swx2+wx*xa(i)
                  swy2=swy2+wy*ya(i)
                  swxxd=swxxd+wx*xb(i)
                  swxyd=swxyd+wx*yb(i)
                  swyxd=swyxd+wy*xb(i)
                  swyyd=swyyd+wy*yb(i)
               endif
 
20          continue
 
 
*
* ITERATE UP TO 4 TIMES, REDUCING IFIT BY 1 EACH TIME
*
            ifit=ifit+1
 
            do 60 itry=1,4
               ifit=ifit-1
 
*
* SHIFT OF ORIGIN ONLY: EQUATIONS SIMPLY SOLVED
* --------------------
*
 
               if(ifit.eq.1) then
                  c(1)=(swxd-swx)/sw
                  c(2)=1.0
                  c(3)=0.0
                  c(4)=(swyd-swy)/sw
                  c(5)=0.0
                  c(6)=1.0
                  ifail=0
 
*
* SHIFT OF ORIGIN AND ROTATION
* ----------------------------
 
               else if(ifit.eq.2) then
 
*
* CALCULATE THE CENTROIDS OF EACH SET OF POSITIONS
*
                  xd0=swxd/sw
                  yd0=swyd/sw
                  x0=swx/sw
                  y0=swy/sw
 
*
* INITIALLISE STORAGE FOR NEW SUMS
*
                  swyxd0=0.0
                  swxyd0=0.0
                  swxxd0=0.0
                  swyyd0=0.0
 
*
* FORM NEW SUMS, USING THE DEVIATIONS FROM THE CENTROIDS
*
 
                  do 146 i=1,n
 
                     if(ok(i)) then
                        swyxd0=swyxd0+(ya(i)-y0)*(xb(i)-xd0)
                        swxyd0=swxyd0+(xa(i)-x0)*(yb(i)-yd0)
                        swxxd0=swxxd0+(xa(i)-x0)*(xb(i)-xd0)
                        swyyd0=swyyd0+(ya(i)-y0)*(yb(i)-yd0)
                     endif
 
146               continue
 
 
*
* IF THE ROTATION ANGLE IS NOT DEFINED, IFAIL=1
*
                  top=swyxd0-swxyd0
                  bot=swyyd0+swxxd0
 
                  if(top.eq.0.0.and.bot.eq.0.0) then
                     ifail=1
 
                  else
 
*
* OTHERWISE CALCULATE THE ROTATION ANGLE ABOUT THE CENTROIDS
* AND ASSIGN THE RESULTS TO THE TRANSFORM COEFFICIENTS
*
                     theta=atan2(top,bot)
                     c(1)=xd0-(x0*cos(theta)+y0*sin(theta))
                     c(2)=cos(theta)
                     c(3)=sin(theta)
                     c(4)=yd0-(-x0*sin(theta)+y0*cos(theta))
                     c(5)=-sin(theta)
                     c(6)=cos(theta)
                     ifail=0
                  endif
 
 
*
* SHIFT, ROTATION AND MAGNIFICATION: SET UP NORMAL EQUATIONS
* ---------------------------------
*
 
               else if(ifit.eq.3) then
                  a(1,1)=sw
                  a(1,2)=swx
                  a(1,3)=swy
                  a(1,4)=0.0d0
                  a(2,1)=swx
                  a(2,2)=swx2+swy2
                  a(2,3)=0.0d0
                  a(2,4)=swy
                  a(3,1)=swy
                  a(3,2)=0.0d0
                  a(3,3)=swx2+swy2
                  a(3,4)=-swx
                  a(4,1)=0.0d0
                  a(4,2)=swy
                  a(4,3)=-swx
                  a(4,4)=sw
                  b(1,1)=swxd
                  b(2,1)=swxxd+swyyd
                  b(3,1)=swyxd-swxyd
                  b(4,1)=swyd
 
*
* CALL NAG ROUTINE F04AEF TO SOLVE THE LINEAR NORMAL EQUATIONS
*
                  ifail=1
                  call f04aef(a,4,b,4,4,1,ans,4,wkspce,aa,4,bb,4,ifail)
 
*
* IF SUCCESSFUL, ASSIGN RESULT TO THE TRANSFORMATION COEFFICIENTS
*
 
                  if(ifail.eq.0) then
                     c(1)=ans(1,1)
                     c(2)=ans(2,1)
                     c(3)=ans(3,1)
                     c(4)=ans(4,1)
                     c(5)=-ans(3,1)
                     c(6)=ans(2,1)
                  endif
 
 
*
* FULL FIT REQUIRED: SET UP NORMAL EQUATIONS
* -----------------
*
 
               else if(ifit.eq.4) then
                  a(1,1)=sw
                  a(1,2)=swx
                  a(1,3)=swy
                  a(2,1)=swx
                  a(2,2)=swx2
                  a(2,3)=swxy
                  a(3,1)=swy
                  a(3,2)=swxy
                  a(3,3)=swy2
                  b(1,1)=swxd
                  b(2,1)=swxxd
                  b(3,1)=swyxd
                  b(1,2)=swyd
                  b(2,2)=swxyd
                  b(3,2)=swyyd
 
*
* CALL NAG ROUTINE F04AEF TO SOLVE LINEAR NORMAL EQUATIONS
*
                  ifail=1
                  call f04aef(a,4,b,4,3,2,ans,4,wkspce,aa,4,bb,4,ifail)
 
*
* IF SUCCESSFUL, ASSIGN RESULTS TO TRANSFORMATION COEFFICIENTS
*
 
                  if(ifail.eq.0) then
                     c(1)=ans(1,1)
                     c(2)=ans(2,1)
                     c(3)=ans(3,1)
                     c(4)=ans(1,2)
                     c(5)=ans(2,2)
                     c(6)=ans(3,2)
                  endif
 
               endif
 
 
*
* IF A FIT WAS SUCCESSFULLY OBTAINED THIS TIME, EXIT FROM ITERATION
* LOOP. OTHERWISE TRY AGAIN WITH IFIT REDUCED BY 1
*
 
               if(ifail.eq.0) go to 70
60          continue
 
70          continue
         endif
 
      endif
 
      return
 
      end
 
 
 
