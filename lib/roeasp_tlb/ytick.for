      SUBROUTINE YTICK (XMIN,XMAX,YMIN,YMAX)
C+
C      Subroutine to plot a set of tick marks &
C      corresponding numbers along a Y axis.
C
C  Given;
C  XMIN  (R) Min. X coord.
C  XMAX  (R) Max. X   "  .
C  YMIN  (R) Min. Y   "  .
C  YMAX  (R) Max. Y   "  .
C
C  Returned;
C
C  Subroutines called;
C  MOVTO2, LINTO2, CHAFIX, CHAINT, CHAESC.
C  Function called; AXINCR.
C
C  Structure:-
C  As for XTICK.
C
C  A C Davenhall./ROE/              1/2/82.
C-
      REAL XMIN,XMAX,YMIN,YMAX
      REAL RANGE,INCR,TIKPOS,TIKBAS,TIKLGT
      REAL TIKX1,TIKX2,TIKX3,TIKX4,NUMPOS
      INTEGER NN,SCALE
      REAL REM,NUM,YPOS,XPOS
      SCALE=INT(ALOG10(ABS(YMAX)))
      RANGE=YMAX-YMIN
      INCR=AXINCR (RANGE)
C
C      Find initial tick position.
C
      NN=INT((YMIN+INCR)/INCR)
      TIKBAS=INCR*FLOAT(NN)
      TIKPOS=TIKBAS
C
C      Find tick length & X coords. for ticks.
C
      TIKLGT=(XMAX-XMIN)*0.02
      TIKX1=XMIN
      TIKX2=XMIN+TIKLGT
      TIKX3=XMAX-TIKLGT
      TIKX4=XMAX
      NUMPOS=XMIN-((XMAX-XMIN)*1.25E-1)
C
C      Plot ticks.
C
      DO WHILE (TIKPOS.LT.(YMAX))
        CALL MOVTO2 (TIKX4,TIKPOS)
        CALL LINTO2 (TIKX3,TIKPOS)
        CALL MOVTO2 (TIKX2,TIKPOS)
        CALL LINTO2 (TIKX1,TIKPOS)
        REM=(TIKPOS-TIKBAS)/(INCR*2.0E0)
        REM=REM+1.0E-5
        REM=REM-FLOAT(INT(REM))
        IF (ABS(REM).LT.1.0E-4) THEN
          CALL MOVTO2 (NUMPOS,TIKPOS)
          NUM=TIKPOS/(1.0E1**SCALE)
          CALL CHAFIX (NUM,6,2)
        END IF
        TIKPOS=TIKPOS+INCR
      END DO
      XPOS=XMIN-((XMAX-XMIN)*1.0E-1)
      YPOS=YMAX+((YMAX-YMIN)*0.05)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL ('X10**$.')
      CALL CHAINT (SCALE,2)
      END
