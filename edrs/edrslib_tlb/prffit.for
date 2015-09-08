      subroutine prffit(p,pw,pr,np,niter,toll,amp,back,sigma,gamma,
     :ierr)
 
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*       TO FIT A LEAST-SQUARES RADIAL STAR PROFILE TO APPROPRIATELY
*       BINNED DATA
*
*METHOD
*       THE ROUTINE REFINES INITIAL ESTIMATES OF THE STAR AMPLITUDE,
*       THE BACKGROUND, THE 'SIGMA' AND THE RADIAL EXPONENT G WHICH
*       ARE GIVEN ON ENTRY. THE ROUTINE REPEATEDLY FORMS AND SOLVES
*       THE LINEARISED NORMAL EQUATIONS FOR A LEAST-SQUARES FIT.
*
*ARGUMENTS
*       P (IN)
*       REAL(NP)
*               AN ARRAY OF DATA VALUES AT VARYING DISTANCES FROM THE
*               STAR CENTRE
*       PW (IN)
*       REAL(NP)
*               AN ARRAY OF WEIGHTS (INVERSELY PROPORTIONAL TO THE
*               SQUARE OF THE PROBABLE ERROR) ASSOCIATED WITH THE DATA
*               VALUES IN P
*       PR (IN)
*       REAL(NP)
*               AN ARRAY OF RADII ASSOCIATED WITH THE DATA VALUES IN P
*       NP (IN)
*       INTEGER
*               THE DIMENSION OF P,PW,PR
*       NITER (IN)
*       INTEGER
*               THE MAXIMUM NUMBER OF REFINING ITERATIONS
*       TOLL (IN)
*       REAL
*               THE FRACTIONAL ACCURACY REQUIRED IN THE RESULT
*               ACCURACY IS ASSESSED AS A FRACTION OF THE PARAMETER
*               VALUE FOR AMP,SIGMA AND GAMMA. THE ACCURACY OF BACK
*               IS ASSESSED AS A FRACTION OF AMP. ITERATIONS CEASE
*               WHEN TWO SUCCESSIVE ITERATIONS GIVE RESULTS AGREEING
*               WITHIN THE RELATIVE ACCURACY TOLL.
*       AMP (IN/OUT)
*       REAL
*               THE STAR AMPLITUDE
*       BACK (IN/OUT)
*       REAL
*               THE BACKGROUND SIGNAL
*       SIGMA (IN/OUT)
*       REAL
*               THE STAR 'SIGMA'
*       GAMMA (IN/OUT)
*       REAL
*               THE EXPONENT IN THE RADIAL PROFILE FUNCTION
*               THE FITTED FUNCTION IS:
*                   AMP*EXP(-0.5*(RADIUS/SIGMA)**GAMMA)+BACK
*       IERR (OUT)
*       INTEGER
*               ERROR FLAG: ZERO FOR SUCCESS
*                           1: ACCURACY CRITERION NOT MET
*
*CALLS
*       NAG LIBRARY:
*               F04ATF
*
*WRITTEN BY
*       R.F. WARREN-SMITH
*-----------------------------------------------------------------------
*
*
      parameter (npar=4)
      real p(np),pw(np),pr(np),x(npar)
      double precision r(npar),a(npar,npar),c(npar),dx(npar),aa(npar
     : ,npar),wks1(npar),wks2(npar)
 
*
* COPY INITIAL PARAMETER ESTIMATES TO ARRAY X
*
      ierr=1
      x(1)=back
      x(2)=amp
      x(3)=sigma
      x(4)=gamma
 
*
* BEGIN THE ITERATION LOOP:
*
 
      do 88 iter=1,niter
 
*
* INITIALLISE ARRAYS FOR FORMING THE LINEARISED NORMAL EQUATIONS
*
 
         do 6 i=1,npar
            r(i)=0.0d0
 
            do 16 j=1,npar
               a(i,j)=0.0d0
16          continue
 
6        continue
 
 
*
* CONSIDER EACH DATA POINT WITH POSITIVE WEIGHT
*
         rsig=1.0/x(3)
 
         do 44 i=1,np
 
            if(pw(i).gt.1.0e-10) then
 
*
* FORM THE REQUIRED PARAMETERS FOR THE NORMAL EQUATIONS
*
               alpha=(pr(i)*rsig)**x(4)
               ex=exp(-0.5*alpha)
               beta=x(2)*ex*alpha*0.5
               c(1)=1.0d0
               c(2)=ex
               c(3)=beta*rsig*x(4)
               c(4)=-beta*log(max(1.0e-20,pr(i))*rsig)
 
*
* FORM DEVIATION OF CURRENT PROFILE FROM DATA POINT
*
               delta=(x(1)+x(2)*ex-p(i))*pw(i)
 
*
* FORM SUMS FOR NORMAL EQUATIONS
*
 
               do 174 j=1,npar
                  r(j)=r(j)+delta*c(j)
 
                  do 173 k=1,j
                     a(k,j)=a(k,j)+c(k)*c(j)*pw(i)
173               continue
 
174            continue
 
            endif
 
44       continue
 
 
*
* FORM OTHER HALF OF SYMMETRIC MATRIX OF COEFFICIENTS
*
 
         do 164 j=1,npar-1
 
            do 163 i=j+1,npar
               a(i,j)=a(j,i)
163         continue
 
164      continue
 
 
*
* CALL NAG ROUTINE F04ATF TO SOLVE THE NORMAL EQUATIONS
*
         ifail=1
         call f04atf(a,npar,r,npar,dx,aa,npar,wks1,wks2,ifail)
 
*
* IF THE EQUATIONS WERE SINGULAR, ABORT WITH IERR=2
*
 
         if(ifail.ne.0) then
            ierr=2
            go to 99
 
         endif
 
 
*
* APPLY THE RESULTANT CORRECTIONS TO THE PROFILE PARAMETERS
* WITH DAMPING FACTORS FOR LARGE AMPLITUDE CHANGES
*
         x(1)=x(1)-dx(1)
         x(2)=x(2)-dx(2)/(1.0+2.0*abs(dx(2)/max(1.0e-20,x(2))))
         x(3)=x(3)-dx(3)/(1.0+2.0*abs(dx(3)/max(1.0e-20,x(3))))
         x(4)=x(4)-dx(4)/(1.0+2.0*abs(dx(4)/max(1.0e-20,x(4))))
 
*
* FORM RELATIVE CHANGES IN EACH PARAMETER
*
         e1=abs(dx(1)/max(1.0e-20,x(2)))
         e2=abs(dx(2)/max(1.0e-20,x(2)))
         e3=abs(dx(3)/max(1.0e-20,x(3)))
         e4=abs(dx(4)/max(1.0e-20,x(4)))
 
*
* IF RELATIVE ACCURACY CRITERION IS MET, EXIT FROM ITERATION LOOP
*
 
         if(max(e1,e2,e3,e4).le.toll) then
            back=x(1)
            amp=x(2)
            sigma=x(3)
            gamma=x(4)
            ierr=0
            go to 99
 
         endif
 
88    continue
 
99    return
 
      end
 
 
 
