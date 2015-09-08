      SUBROUTINE RECFILSUB (XO,YO,XT,YT,NEWVAL,IXEXT,IYEXT,OUTPICT)
C+
C  RECFILSUB
C
C	called from program RECFIL
C	changes pixels inside rectangle with opposite corners XO,YO XT,YT to NEWVAL
C
C  Given (arguments)
C    XO,YO  (R)   corner of rectangle
C    XT,YT  (R)   corner of rectangle
C    NEWVAL (R)   new values for inside rectangle
C    IXEXT,IYEXT (I)   dims of OUTPICT
C    OUTPICT (RA)   image to be modified
C
C  Returned (arguments)
C    OUTPICT  (RA)  modified image
C
C  D. Brownrigg, D. Tudhope  ROE  1981
C-

      INTEGER IXEXT,IYEXT
      REAL OUTPICT(IXEXT,IYEXT)
      REAL XO,YO,XT,YT,NEWVAL
      INTEGER I,J,IL,IR,IB,IT,IXO,IYO,IXT,IYT

C  arrays start at 1, not 0
      IXO=NINT(XO+1)
      IYO=NINT(YO+1)
      IXT=NINT(XT+1)
      IYT=NINT(YT+1)
C  get correct values for left,right etc
      IL=MIN(IXO,IXT)
      IR=MAX(IXO,IXT)
      IB=MIN(IYO,IYT)
      IT=MAX(IYO,IYT)
      DO J=IB,IT
        DO I=IL,IR
          OUTPICT(I,J)=NEWVAL
        ENDDO
      ENDDO
      END
