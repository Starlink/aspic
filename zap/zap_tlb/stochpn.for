      SUBROUTINE STOCHPN(DAT,FLO,HI,NSAMP,TYP,WIDE,RATIO,UNIT)
C
C     WRITTEN BY KEN HARTLEY, RGO
C     MODIFIED FOR PRINTRONIX PRINTER BY W. PENCE, AAO
C
C ..THE COMMON BLOCK LINKSTOCH LINKS WITH INIT...
C
      PARAMETER NL=264
      DOUBLE PRECISION XXX
      LOGICAL*1 XMASK(6),FIRST,LAST,IPOW(8)
      BYTE BYTE1
      INTEGER UNIT
      COMMON /LINKSTOCH/RAND(2000)
      LOGICAL*1 IOUT(264)
      DIMENSION DAT(2112)
      DIMENSION IBIT(2112),DATA(2112)
      DATA XMASK/'01'X,'02'X,'04'X,'08'X,'10'X,'20'X/
      DATA FIRST,LAST /'05'X,'0A'X/
      DATA BYTE1/4/
      DATA IPOW/128,64,32,16,8,4,2,1/
C
      IF (UNIT .LT. 0)THEN
        NPWIDE=786
        NBPI=60
      ELSE
        NPWIDE=2112
        NBPI=200
      END IF
C ..TESTS ARE CARRIED OUT TO CHECK THAT FLO AND HI...
C ..ARE IN THE VALID RANGE PRESCRIBED...
C ..AND THAT TYP IS A VALID NUMBER CODE...
C
      IF(ABS(TYP).EQ.2.0.AND.FLO.LT.0.0.OR.HI.LT.0.0)RETURN
      IF(HI.EQ.FLO)RETURN
      IF(TYP.EQ.999.0)GOTO 999
      IF(ABS(TYP).GT.2.0)RETURN
      IF(TYP.EQ.0.0)RETURN
      IF(FLOAT(INT(TYP)).NE.TYP)RETURN
C
C ..THE VALUES OF FLO AND HI ARE SWAPPED IF...
C ..GIVEN IN REVERSE...
C
  999 IF (HI.GE.FLO) GO TO 50
      TEM=HI
      HI=FLO
      FLO=TEM
   50 CONTINUE
C
C ..IF FLO IS 0.0,FLO IS SET TO A VERY SMALL NUMBER...
C ..TO ALLOW LOGS TO BE TAKEN IF NECCESSARY...
C
      IF(FLO.EQ.0.0)FLO=1E-6
C
C      DATA ARE FIXED IF TYPE= + 2 OR - 2 AND VALUE IS NEGATIVE
C
      IF (ABS(TYP).EQ.2.0) THEN
            DO 60 I=1,NSAMP
               IF (DAT(I).LT.0.0) DAT(I)=-DAT(I)
               IF (DAT(I).EQ.0.0) DAT(I)=1.0E-6
   60       CONTINUE
                           END IF
C
C ..THE WIDTH OF THE IMAGE IS DECIDED...
C
      IWIDE=NPWIDE
      IF(WIDE.LE. FLOAT(NPWIDE)/NBPI
     1  .AND.WIDE.GE.1.0)IWIDE=WIDE*NBPI
      IF(NSAMP.LE.0.OR.NSAMP.GT.NPWIDE)RETURN
C
C ..IPIX IS THE NUMBER OF BITS PER PIXEL...
C
      IPIX=IWIDE/NSAMP
      ILENGTH=FLOAT(IPIX)*RATIO+.01
      ILENGTH=MAX(1,ILENGTH)
      ISTART=(NPWIDE-IWIDE)/2
      IF (ABS(TYP).EQ.2.0) THEN
         DIV1=ALOG(HI)-ALOG(FLO)
         DIV2=-DIV1
      ELSE
         DIV1=1
         DIV2=-DIV1
      END IF
      IF(TYP.NE.999.0)GOTO 1000
      DO 123 L=1,NSAMP
        DATA(L)=DAT(L)
  123 CONTINUE
      GOTO 321
C
C ..THIS LOOP SCALES THE DATA OF DAT INTO ARRAY DATA...
C
 1000 DO 100 I=1,NSAMP
C
C ..THE CORRECT TYP OF SCALING IS CHOSEN...
C
        IF (TYP.EQ.1.0) DATA(I)=(DAT(I)-FLO)/(HI-FLO)
        IF (TYP.EQ.-1.0) DATA(I)=(DAT(I)-HI)/(FLO-HI)
        IF (TYP.EQ.2.0) DATA(I)=(ALOG(DAT(I))-ALOG(FLO))/DIV1
        IF (TYP.EQ.-2.0) DATA(I)=(ALOG(DAT(I))-ALOG(HI))/DIV2
  100 CONTINUE
C
C ..THIS LOOP OUTPUTS THE ROW ILENGTH TIMES TO PRODUCE...
C ..PIXELS OF THE DESIRED SHAPE...
C
  321 DO 400 IROW=1,ILENGTH
C
C ..THE POSITION AT WHICH THE POINTER IS SET IS RANDOM...
C
        XXX=G05CAF(XXX)
        X=XXX
        IPOS=INT(X*1999.0+1.0)
        DO 250 ISET=1,NPWIDE
          IBIT(ISET)=0
  250   CONTINUE
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF ELEMENTS OF DAT...
C
        DO 300 ISAMP=1,NSAMP
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF BITS IN EACH PIXEL...
C
          DO 200 IBITS=1,IPIX
C
C ..THE POSITION OF THE BIT UNDER CONSIDERATION IS CALCULATED...
C
            K=(ISAMP-1)*IPIX+IBITS+ISTART
C
C ..THE VALUE OF IBIT(K) IS SET ACCORDING...
C ..TO THE PROBABILITY OF THE VALUE OF DATA BEING...
C ..GREATER THAN A RANDOM NUMBER...
C
            IF (DATA(ISAMP).GT.RAND(IPOS)) IBIT(K)=1
C
C ..THE POINTER IS INCREMENTED,AND IF THE END OF THE...
C ..RAND IS REACHED,IT IS SET TO THE START OF RAND...
C
            IPOS=IPOS+1
            IF (IPOS.GT.2000) IPOS=1
  200     CONTINUE
  300   CONTINUE
C
C ..THE WORDS CONTAINING THE BITS TO WRITE TO THE TAPE...
C ..ARE THEN ASSEMBLED FROM THE BITS ALREADY FORMED...
C
      IF (UNIT .LT. 0)THEN
      DO 255 ISET = 1,140
255   IOUT(ISET)='C0'X
      DO 370 I=1,NPWIDE
        IF (IBIT(I) .EQ. 1)THEN
          KWORD=1+((I-1)/6)
          KBIT=1+MOD(I-1,6)
          IOUT(KWORD)=IOUT(KWORD).OR.XMASK(KBIT)
        END IF
370   CONTINUE
C
C ..THE LINE IS NOW WRITTEN TO THE FILE...
C
      WRITE(-UNIT,99)FIRST,(IOUT(I),I=1,131),LAST
99    FORMAT(133A1)
C
      ELSE
C
      DO 390 IIN1=1,NL
        IOUT(IIN1)=0
        DO 380 JBIT=1,8
          K=(IIN1-1)*8+JBIT
          IF (IBIT(K) .EQ. 1)IOUT(IIN1)=IOUT(IIN1)+IPOW(JBIT)
380     CONTINUE
390   CONTINUE
C
C    THE LINE IS NOW WRITTEN OF THE FILE
C
      WRITE(UNIT)BYTE1,(IOUT(J),J=1,NL)
C
      END IF
C
  400 CONTINUE
  500 RETURN
      END
 
*+
*
*  - - - - - - - - - - -
*  :  L U T R O T      :
*  - - - - - - - - - - -
*
*
*  ROTATE ENTRIES 1-255 IN THE LUT
*  BY MEANS OF THE TRACKER BALL
*  PRESS BUTTON 1 TO RESET LUT
*  PRESS BUTTON 4 TO EXIT
*
*  THE TABLE OFFSET IS RETURNED TO DSCL
*
*
*  DLT/SEP 81
*  DJK/OCT 81
*
*-
