      subroutine per_bells(x,y,np,xa,xb,npapr,nspl,xapr,yapr,ier)
 
*
***********************************************************************
*  CDP VERSION  1) Calculates approximation at given X array
*               2) It is all DOUBLE PRECISION
*
*
**********************************************************************
*
*        Purpose :-
*           To calculate an approximation to a discrete function
*           by the sum of the BELL spline functions in the least
*           squares sense.
*
*
*        Usage :-
*           CALL PER_BELLS(X,Y,NP,XA,XB,NPAPR,NSPL,XAPR,YAPR,IER)
*
*        DESCRIPTION OF PARAMETERS
*           X     - ARRAY OF X-VALUES (ABSCISSES)
*           Y     - ARRAY OF Y-VALUES (ORDINATES)
*           NP    - NUMBER OF EXPERIMENTAL POINTS (X(I),Y(I))
*           XA,XB - DEFINE THE INTERVAL (XA,XB) IN WHICH THE DISCRETE
*                   FUNCTION IS APPROXIMATED
*           NPAPR - NUMBER OF POINTS IN WHICH THE APPROXIMATION IS
*                   CALCULATED
*           NSPL  - NUMBER OF SPLINE FUNCTIONS IN (XA,XB)
*           YAPR  - ARRAY OF APPROXIMATED VALUES ON RETURN,THE VALUES
*                   ARE CALCULATED AT THE X VALUES CONTAINED IN XAPR
*
*           XAPR -  ARRAY CONTAINING THE X VALUES AT WHICH THE APPROX
*                   IS TO BE CALCULATED.
*
*           IER   - ERROR PARAMETER
*                   IER = 1  MEANS THAT APPROXIMATION WAS CALCULATED
*                   IER =-1  MEANS THAT XA OR XB ARE TOO FAR OUTSIDE
*                            THE INTERVAL (X(1),X(NP))
*                   IER =-2  MEANS THAT THE NUMBER OF SPLINES IS LESS
*                            THAN 2 OR GREATER THAN 100 OR GREATER
*                            THAN NUMBER OF POINTS IN (XA,XB)
*
*        Remarks :-
*           1)          The values in array X must be in ascending
*                       order.
*           2)          XA or XB or both can be outside (X(1),X(NP)),
*                       but their distances from X(1),RESP. X(NP)
*                       must be smaller than "spline step" H, where
*                       H=(XB-XA)/(NSPL-1)
*           3)          As the number of splines in (XA,XB) approaches
*                       number of points in this interval the appro-
*                       ximation function starts to oscillate. In the
*                       limit case (the number of points equal to
*                       the number of splines+2) the approximation
*                       is usually inapplicable!
*           4)          The subroutine is constructed for call up
*                       to 100 splines,if necessary,it is possible
*                       to change the NSPLMX variable and the
*                       DIMENSION declaration:
*                       ARRAY  XPART,R.....N+2
*                       ARRAY  A      .....N+5
*                       ARRAY  C      .....7*N+22
*                       for subroutine call up to N splines.
*
*        Subroutines and Function required :-
*           NONE
*
*        AUTHOR: J.BOK   INSTITUTE OF PHYSICS OF THE CHARLES UNIVERSITY
*                        KE KARLOVU 5    12116 PRAGUE 2  CZECHOSLOVAKIA
*
*
      implicit double precision (a-h,o-z)
      dimension x(1),y(1),xapr(1),yapr(1)
      dimension r(502),c(3522),xpart(502),a(505)
      nsplmx=500
 
      if(nspl.gt.1.and.nspl.lt.(nsplmx+1)) goto 10
 
*
*        Error return in case that number of splines is less than 2
*        or greater than NSPLMX
      ier=-2
      return
10    h=(xb-xa)/float(nspl-1)
 
      if((x(1)-xa).lt.h.and.(xb-x(np)).lt.h) goto 11
 
*
*        ERROR RETURN IN THE CASE THAT XA OR XB ARE TOO FAR OUTSIDE
*        (X(1),X(NP)
      ier=-1
      return
 
*
*        COMPLETE NUMBER OF SPLINES INCLUDING THE SPLINES
*        WITH THE CENTRES AT XA-H,XB+H
11    nsp=nspl+2
 
*
*        GENERATION OF THE EQUIDISTANT PARTITION IN (XA-H,XB+H)
 
      do 12 l=1,nsp
12    xpart(l)=xa+h*float(l-2)
 
 
*
*        FIND THE FIRST AND THE LAST INDEX I SUCH THAT X(I) LIES
*        IN (XA,XB) FOR ALL I FROM (IFIRST,ILAST)
 
      do 13 i=1,np
 
         if(x(i).ge.xa) goto 14
13    continue
 
14    ifirst=i
 
      do 15 i=ifirst,np
 
         if(x(i).gt.xb) goto 16
15    continue
 
16    ilast=i-1
 
*
*        NUMBER OF POINTS IN (XA,XB)
      npi=ilast-ifirst+1
 
      if(nsp.le.npi) goto 17
 
*
*        ERROR RETURN IN CASE OF TOO GREAT NUMBER OF SPLINES
      ier=-2
      return
 
*
*        SET UP COEFFICIENTS OF THE SYSTEM OF LINEAR EQUATIONS
*
*        FIND INDEXES I1,I2 SUCH THAT FOR I FROM (I1,I2)
*        THE VALUE SP1 IS NON-ZERO
17    llast=7*nsp-3
 
      do 18 l=1,llast
18    c(l)=0.
 
 
      do 19 l=1,nsp
19    r(l)=0.
 
 
      do 30 m=0,3
         i1=ifirst
         jm=nsp-m
 
         do 29 j=1,jm
            ind=7*j-3+m
 
            if(j.gt.3) goto 20
            i1=ifirst
            goto 23
 
 
20          do 21 ip=i1,ilast
 
               if(x(ip).gt.xpart(j-2)) goto 22
21          continue
 
22          i1=ip
 
23          if(j.lt.(nsp-2)) goto 24
            i2=ilast
            goto 27
 
 
24          do 25 ip=i1,ilast
 
               if(x(ip).gt.xpart(j+2)) goto 26
25          continue
 
26          i2=ip-1
 
*
*        COMPUTATION OF THE COEFFICIENTS AND THE RIGHT-HAND SIDE
*        OF THE SYSTEM OF LINEAR EQUATIONS
 
27          do 29 i=i1,i2
               sp1=per_spl(x(i),xpart(j),h)
 
               if(m.ne.0) goto 28
               sp2=sp1
               r(j)=r(j)+y(i)*sp1
               goto 29
 
28             sp2=per_spl(x(i),xpart(j+m),h)
29          c(ind)=c(ind)+sp1*sp2
 
30          continue
 
*
*        ELEMENTS OF THE MATRIX UNDER THE MAIN DIAGONAL
            lp=nsp-1
 
            do 31 l=1,lp
               ll=7*l
               c(ll+3)=c(ll-2)
               c(ll+9)=c(ll-1)
31          c(ll+15)=c(ll)
 
 
*
*        SOLVING OF THE SYSTEM OF LINEAR EQUATIONS
 
            do 33 l=1,lp
 
               do 33 m=1,3
                  ii=7*l-3
                  jj=ii+6*m
                  f=-c(jj)/c(ii)
 
                  do 32 mm=1,3
32                c(jj+mm)=c(jj+mm)+f*c(ii+mm)
 
33             r(l+m)=r(l+m)+f*r(l)
 
               lp=nsp+3
 
               do 34 l=1,lp
34             a(l)=0.
 
 
               do 35 ll=1,nsp
                  l=nsp+1-ll
                  jj=7*l
35             a(l)=(r(l)-c(jj)*a(l+3)-c(jj-1)*a(l+2)-c(jj-2)*a(l+1))
     :          /c(jj-3)
 
 
*
*        GENERATION OF THE APPROXIMATIVE VALUES
 
               do 36 i=1,npapr
                  xx = xapr(i)
                  l1=(xx-xpart(1))/h
                  l2=l1+3
 
                  if(l1.eq.0) l1=1
 
                  if(l2.gt.nsp) l2=nsp
                  yapr(i)=0.
 
*
*        THE VALUE PER_SPL(X,XL,H) IS NON-ZERO ONLY FOR L FROM (L1,L2)
 
                  do 36 l=l1,l2
                     xl=xpart(1)+h*float(l-1)
36                yapr(i)=yapr(i)+a(l)*per_spl(xx,xl,h)
 
 
*
*        NORMAL RETURN
                  ier=1
                  return
 
      end
 
 
 
