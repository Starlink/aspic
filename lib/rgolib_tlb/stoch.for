      SUBROUTINE STOCH(DAT,FLO,HI,NSAMP,IWORD,NL,TYP,WIDE,RATIO)
C 
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C
C         SUBROUTINE STOCH 
C
C
C         This is the key routine in the Greyscale package described in  LUN 
C         14.  It  takes  a  1-dimensional array of numbers and creates the 
C         output representation  of  them  and  writes it  to  a  file  for 
C         subsequent plotting. 
C
C         DAT         Real  In    The data array to be plotted. (1D) 
C
C         FLO         Real  In    The lower cutoff of  data  values  to  be 
C                                 plotted. 
C
C         HI          Real  in    The upper cutoff of  data  values  to  be 
C                                 plotted. 
C
C         NSAMP       I*4   In    The dimesion  of  DAT  -  the  number  of 
C                                 samples to be output. 
C
C         IWORD       I*4   In    The word length of this implementation  - 
C                                 in this case it must be 8. 
C
C         NL          I*4   In    The number of words used to  represent  a 
C                                 full  row  of  output to the Versatec. In 
C                                 the case of the Vax it must be 264. 
C
C         TYP         Real  In    Specifies the type of scaling to be used. 
C                                 It   must   have   one   of   the  values 
C                                 -2.0,-1.0,0.0,1.0,2.0 . Basically 1 means 
C                                 linear,  2  logarithmic, + means from low 
C                                 to high and - from high to low. 
C
C         WIDE        Real  In    The width of the plot in inches (as nearly 
C                                 as  possible).  It  must be betwe 1.0 and 
C                                 10.56. 
C
C         RATIO       Real  In    The ratio of the width (x  size)  of  the 
C                                 pixels output to their length (y-size). 
C
C
C         K F Hartley              RGO                            10-AUG-82 
C
C-------------------------------------------------------------------------- 
C                                                               
                                                                                
                                                                                
                                                                                
C ..THE COMMON BLOCK LINKSTOCH LINKS WITH INIT...
C ..THE COMMON BLOCK LINKF LINKS WITH WRITBLOCK...
C
      LOGICAL*1 IPOW(8)
      INTEGER*4 LO
      COMMON /LINKSTOCH/IPOW,RAND(2000)/LINKF/IOUT
      LOGICAL*1 IOUT(264)
      DIMENSION DAT(NSAMP)
      DIMENSION IBIT(2112),DATA(2112)
C
C ..TESTS ARE CARRIED OUT TO CHECK THAT FLO AND HI...
C ..ARE IN THE VALID RANGE PRESCRIBED...
C ..AND THAT TYP IS A VALID NUMBER CODE...
C
      IF ((ABS(TYP).EQ.2.0).AND.(FLO.LT.0.0.OR.HI.LT.0.0)) RETURN
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
      IWIDE=2112
      IF(WIDE.LE.10.56.AND.WIDE.GE.1.0)IWIDE=WIDE*200.0
      IF(NSAMP.LE.0.OR.NSAMP.GT.2112)RETURN
C
C ..IPIX IS THE NUMBER OF BITS PER PIXEL...
C
      IPIX=IWIDE/NSAMP
      ILENGTH=FLOAT(IPIX)*RATIO
      ISTART=(2112-IWIDE)/2
      IF (ABS(TYP).EQ.2.0) THEN
         DIV1=ALOG(HI)-ALOG(FLO)
         DIV2=-DIV1
      ELSE
         DIV1=1.0
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
        IPOS=INT(RAN(LO)*1999.0+1.0)
        DO 250 ISET=1,2112
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
        DO 370 IINT=1,NL
          IOUT(IINT)=0
          DO 350 JBIT=1,IWORD
            K=(IINT-1)*IWORD+JBIT
            IF (IBIT(K).EQ.1) IOUT(IINT)=IOUT(IINT)+IPOW(JBIT)
  350     CONTINUE
  370   CONTINUE
C
C ..THE LINE IS NOW WRITTEN TO THE FILE...
C
        CALL WRITBLOK(90,NL)
  400 CONTINUE
  500 RETURN
      END
