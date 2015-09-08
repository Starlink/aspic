      SUBROUTINE XTICK (XMIN,XMAX,YMIN,YMAX)
C+
C      Subroutine to put tick marks & corrsponding
C      numbers along an X axis.
C
C  Given;
C  XMIN (R)  Min. X coord.
C  XMAX (R)  Max. X   "  .
C  Structure:-
C  YMIN (R)  Min. Y   "  .
C  YMAX (R)  Max. Y   "  .
C

C  Returned; -.
C
C  Subroutines called;
C  MOVTO2, LINTO2, CHAFIX, CHAINT, CHAESC.
C  Function called; AXINCR.
C
C
C  Structure:-
C
C     Compute range of axis.
C     divide by 10.
C     multiply by 10 & fix to integer
C     float & divide by 10.
C     find position of first tick;
C      = fix(((min. of range + increment)/increment)*increment)
C     do while (more ticks needed
C       plot tick.
C
C  A C Davenhall./ROE/                         1/2/82.
C-
      REAL XMIN,XMAX,YMIN,YMAX
      REAL RANGE,INCR,TIKPOS,TIKBAS,TIKHT
      REAL TIKY1,TIKY2,TIKY3,TIKY4,NUMPOS
      REAL NUM,XPOS,YPOS,REM
      INTEGER SCALE
      INTEGER NN
      SCALE=INT(ALOG10(ABS(XMAX)))
      RANGE=XMAX-XMIN
      INCR=AXINCR (RANGE)
C
C      Find initial tick position.
C
      NN=INT((XMIN+INCR)/INCR)
      TIKBAS=INCR*FLOAT(NN)
      TIKPOS=TIKBAS
C
C      Find tick height & y coords. for ticks.
C
      TIKHT=(YMAX-YMIN)*0.02
      TIKY1=YMIN
      TIKY2=YMIN+TIKHT
      TIKY3=YMAX-TIKHT
      TIKY4=YMAX
      NUMPOS=YMIN-((YMAX-YMIN)*0.05)
C
C      Plot ticks.
C
      DO WHILE (TIKPOS.LT.(XMAX))
        CALL MOVTO2 (TIKPOS,TIKY4)
        CALL LINTO2 (TIKPOS,TIKY3)
        CALL MOVTO2 (TIKPOS,TIKY2)
        CALL LINTO2 (TIKPOS,TIKY1)
        REM=(TIKPOS-TIKBAS)/(INCR*2.0E0)
        REM=REM+1.0E-5
        REM=REM-FLOAT(INT(REM))
        IF (ABS(REM).LT.1.0E-4) THEN
          XPOS=TIKPOS-(0.3*INCR)
          CALL MOVTO2 (XPOS,NUMPOS)
          NUM=TIKPOS/(1.0E1**SCALE)
          CALL CHAFIX (NUM,4,1)
        END IF
        TIKPOS=TIKPOS+INCR
      END DO
      YPOS=YMIN-((YMAX-YMIN)*0.09)
      XPOS=XMAX-((XMAX-XMIN)*0.15)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL ('X10**$.')
      CALL CHAINT (SCALE,2)
      END
