      subroutine imgmth(img,scale,zero,inval,npix,nlines,oper,noper
     : ,imp,var,varp,con,iout,inval2,scale2,zero2,opcode,work,ierr)
 
*
      parameter (nsymb=36,minint=-32767,maxint=32767)
 
*
      character opsymb(nsymb)*6,oper(noper)*6
      integer*2 img(npix,nlines,*),iout(npix,nlines)
      integer inval(*),imp(*),varp(*),opcode(noper),tos
      real s(0:80),scale(*),zero(*),var(*),con(*),work(npix,nlines)
      double precision g05ddf
 
*
* DEFINE THE RECOGNISED OPERATIONS
*
      data ((opsymb(i)),i=1,nsymb)/'LDCON ','LDVAR ','LDIM  ','=     '
     :,'-     ','+     ','**    ','*     ','/     ','NEG   ','SQRT  '
     :,'EXP   ','LOG10 ','LOG   ','SIN   ','COS   ','TAN   ','ASIN  '
     :,'ACOS  ','ATAN  ','ATAN2 ','SINH  ','COSH  ','TANH  ','ABS   '
     :,'AINT  ','ANINT ','MOD   ','SIGN  ','DIM   ','MIN   ','MAX   '
     :,'XX    ','YY    ','CLIP  ','GAUSS '/
 
*
* SCAN THE SYMBOL TABLE FOR EACH INPUT OPERATOR TO FIND ITS
* INTEGER OPCODE
*
      ierr=0
 
      do 1 i=1,noper
 
         do 2 j=1,nsymb
 
            if(oper(i).eq.opsymb(j)) then
               opcode(i)=j
               go to 1
 
            endif
 
2        continue
 
 
*
* ARRIVE HERE IF AN OPERATOR IS NOT RECOGNISED..QUIT WITH ERROR
*
         ierr=1
         go to 99
 
1     continue
 
 
*
* INITIALLISE THE MINIMUM AND MAXIMUM OUTPUT VALUES
*
      rmin=1.0e20
      rmax=-1.0e20
 
*
* SCAN THE IMAGE
*
 
      do 89 j=1,nlines
 
         do 88 i=1,npix
 
*
* FOR EACH PIXEL, INITIALLISE THE ARITHMETIC,IMAGE,VARIABLE AND
* CONSTANT STACK POINTERS
*
            tos=0
            nimg=0
            nvar=0
            ncon=0
 
*
* PERFORM THE REQUIRED OPERATIONS ON THE ARITHMETIC STACK
*
 
            do 100 nop=1,noper
               go to(101,102,103,104,105,106,107,108,109,110,111,112
     :          ,113,114,115,116,117,118,119,120,121,122,123,124,125
     :           ,126,127,128,129,130,131,132,133,134,135,136),
     :           opcode(nop)
 
 
*
* LDCON : LOAD CONSTANT ON TO STACK
* -----
*
101            tos=tos+1
               ncon=ncon+1
               s(tos)=con(ncon)
               go to 100
 
 
*
* LDVAR : LOAD VARIABLE ON TO STACK
*
102            tos=tos+1
               nvar=nvar+1
               npoint=varp(nvar)
               s(tos)=var(npoint)
               go to 100
 
 
*
* LDIM : LOAD IMAGE ON TO STACK
* ----
*
103            nimg=nimg+1
               ipoint=imp(nimg)
               ival=img(i,j,ipoint)
 
*
* PUT ON STACK IF PIXEL IS VALID
*
 
               if(ival.ne.inval(ipoint)) then
                  tos=tos+1
                  s(tos)=ival*scale(ipoint)+zero(ipoint)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* = : END OF CALCULATION
* -
*
104            result=s(tos)
               rmin=min(result,rmin)
               rmax=max(result,rmax)
               iout(i,j)=1
               work(i,j)=result
               go to 88
 
 
*
* - : SUBTRACT TOS
* -
*
105            s(tos-1)=s(tos-1)-s(tos)
               tos=tos-1
               go to 100
 
 
*
* + : ADD TOS
* -
*
106            s(tos-1)=s(tos-1)+s(tos)
               tos=tos-1
               go to 100
 
 
*
* ** : RAISE TO POWER
* --
*
107            a=s(tos-1)
               b=s(tos)
 
*
* POSITIVE NUMBER MAY BE RAISED TO ANY POWER
*
 
               if(a.ge.0.0) then
                  c=a**b
 
               else
 
*
* OTHERWISE INDEX MUST BE AN INTEGER
*
                  ib=nint(b)
 
                  if(abs(ib-b).le.1.0e-7) then
                     c=a**ib
 
                  else
                     go to 199
 
                  endif
 
               endif
 
               tos=tos-1
               s(tos)=c
               go to 100
 
 
*
* * : MULTIPLY TOS
* -
*
108            s(tos-1)=s(tos-1)*s(tos)
               tos=tos-1
               go to 100
 
 
*
* / : DIVIDE TOS
* -
*
109            a=s(tos)
 
*
* AVOID DIVISION BY ZERO
*
 
               if(a.ne.0)then
                  s(tos-1)=s(tos-1)/a
                  tos=tos-1
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* NEG : NEGATE TOS
* ---
*
110            s(tos)=-s(tos)
               go to 100
 
 
*
* SQRT : SQUARE ROOT TOS
* ----
*
111            a=s(tos)
 
               if(a.ge.0.0) then
                  s(tos)=sqrt(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* EXP : E TO POWER TOS
* ---
*
112            a=s(tos)
 
               if(a.le.100.0) then
                  s(tos)=exp(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* LOG10 : LOG TOS BASE 10
* -----
*
113            a=s(tos)
 
               if(a.gt.0.0) then
                  s(tos)=log10(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* LOG : NATURAL LOG OF TOS
* ---
*
114            a=s(tos)
 
               if(a.gt.0.0) then
                  s(tos)=log(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* SIN : SINE OF TOS
* ---
*
115            s(tos)=sin(s(tos))
               go to 100
 
 
*
* COS : COSINE OF TOS
* ---
*
116            s(tos)=cos(s(tos))
               go to 100
 
 
*
* TAN : TANGENT OF TOS
* ---
*
117            a=s(tos)
               s(tos)=tan(a)
               go to 100
 
 
*
* ASIN : ARCSINE OF TOS
* ----
*
118            a=s(tos)
 
               if(abs(a).le.1.0) then
                  s(tos)=asin(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* ACOS : ARCCOSINE OF TOS
* ----
*
119            a=s(tos)
 
               if(abs(a).le.1.0) then
                  s(tos)=acos(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* ATAN : ARCTANGENT OF TOS
* ----
*
120            s(tos)=atan(s(tos))
               go to 100
 
 
*
* ATAN2 : ARCTANGENT OF TOS RATIO
* -----
*
121            a=s(tos-1)
               b=s(tos)
 
               if(a.ne.0.0.or.b.ne.0.0) then
                  tos=tos-1
                  s(tos)=atan2(a,b)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* SINH : HYPERBOLIC SINE OF TOS
* ----
*
122            a=s(tos)
 
               if(abs(a).le.100.0) then
                  s(tos)=sinh(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* COSH : HYPERBOLIC COSINE OF TOS
* ----
*
123            a=s(tos)
 
               if(abs(a).le.100.0) then
                  s(tos)=cosh(a)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* TANH : HYPERBOLIC TANGENT OF TOS
* ----
*
124            s(tos)=tanh(s(tos))
               go to 100
 
 
*
* ABS : ABSOLUTE VALUE OF TOS
* ---
*
125            s(tos)=abs(s(tos))
               go to 100
 
 
*
* AINT : TRUNCATE TOS
* ----
*
126            s(tos)=aint(s(tos))
               go to 100
 
 
*
* ANINT : NEAREST INTEGER TO TOS
* -----
*
127            s(tos)=anint(s(tos))
               go to 100
 
 
*
* MOD : MODULO ON TWO TOS ENTRIES
* ---
*
128            a=s(tos-1)
               b=s(tos)
 
               if(b.ne.0.0) then
                  tos=tos-1
                  s(tos)=mod(a,b)
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* SIGN : TRANSFER OF SIGN BETWEEN TWO TOS ENTRIES
* ----
*
129            s(tos-1)=sign(s(tos-1),s(tos))
               tos=tos-1
               go to 100
 
 
*
* DIM : POSITIVE DIFFERENCE OF TWO TOS ENTRIES
* ---
*
130            s(tos-1)=dim(s(tos-1),s(tos))
               tos=tos-1
               go to 100
 
 
*
* MIN : MINIMUM OF TWO TOS ENTRIES
* ---
*
131            s(tos-1)=min(s(tos-1),s(tos))
               tos=tos-1
               go to 100
 
 
*
* MAX : MAXIMUM OF TWO TOS ENTRIES
* ---
*
132            s(tos-1)=max(s(tos-1),s(tos))
               tos=tos-1
               go to 100
 
 
*
* XX : LOAD IMAGE X COORDINATE ON TO STACK
* --
*
133            tos=tos+1
               s(tos)=i
               go to 100
 
 
*
* YY : LOAD IMAGE Y COORDINATE ON TO STACK
* --
*
134            tos=tos+1
               s(tos)=j
               go to 100
 
 
*
* CLIP : ABORT IF TOS LIES OUTSIDE WINDOW
* ----
*
135            a=s(tos-2)
               b=s(tos-1)
               c=s(tos)
 
               if(a.le.b.and.b.le.c) then
                  tos=tos-2
                  s(tos)=b
 
               else
                  go to 199
 
               endif
 
               go to 100
 
 
*
* GAUSS : PUT GAUSSIAN NOISE ON TOS
* -----
*
136            s(tos)=g05ddf(0.0d0,dble(abs(s(tos))))
               go to 100
 
 
*
* ARRIVE HERE IF AN INVALID ARITHMETIC OPERATION HAS BEEN TRIED
*
199            iout(i,j)=0
               go to 88
 
100         continue
 
88       continue
 
89    continue
 
 
*
* CALCULATE OUTPUT SCALING
*
 
      if((rmax-rmin).gt.1.0e-20) then
         scale2=(rmax-rmin)/(0.75*(maxint-minint))
         zero2=((rmax+rmin)-(maxint+minint))*0.5
         rscl=1.0/scale2
 
      else if(rmax.ge.rmin) then
         scale2=1.0
         zero2=rmin
         rscl=1.0
 
      else
         scale2=1.0
         zero2=0.0
         rscl=1.0
      endif
 
 
*
* CONVERT THE RESULTS TO INTEGERS, TAKING ACCOUNT OF INVALID RESULTS
*
 
      do 302 j=1,nlines
 
         do 301 i=1,npix
 
            if(iout(i,j).eq.1) then
               iout(i,j)=nint((work(i,j)-zero2)*rscl)
 
            else
               iout(i,j)=inval2
            endif
 
301      continue
 
302   continue
 
99    return
 
      end
 
 
 
