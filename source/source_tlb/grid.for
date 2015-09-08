C+
C
C      STARLINK PROGRAM *** GRID ***
C
C      WRITTEN BY K F HARTLEY AT RGO ON 25/3/81
C
C      SUPERIMPOSES A RECTANGULAR GRID ON A 2-D IMAGE
C      AT REGULAR SPACES (SAME IN X AND Y)
C      AND STORES THE NEW IMAGE ( USEFUL FOR DISPLAY
C      AND VERSATEC OUTPUT).
C
C      IF THE DATA VALUE IS GREATER THAN 128 THE GRID HAS ZERO VALUE
C      "   "   "     "   "  LESS      "   "   "   "    "  VALUE 255
C
C      PARAMETERS ARE:
C
C         INPUT:  THE IMAGE TO BE GRIDDED
C
C         OUTPUT: THE OUTPUT IMAGE (= INPUT + GRID)
C
C         SPACING: THE PIXEL SPACING IN BOTH X & Y OF THE GRID
C
C
C-
      INTEGER PIN,POUT,AX(2),STATUS,SP
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
C
C      FIRST READ THE INPUT FRAME
C      N.B. IT MUST BE A 2D IMAGE
C
      CALL RDIMAG('INPUT',FMT_SW,2,AX,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL.AND.I.NE.2) THEN
         CALL WRERR('HELLIN')
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF
C
C      AND GET AN OUTPUT FRAME
C
      CALL WRIMAG('OUTPUT',FMT_SW,AX,2,POUT,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
          CALL WRERR('HELLOUT')
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF
C
C      NOW REQUEST THE GRID SPACING
C      THIS VERSION ONLY WORKS IN TERMS OF PIXEL VALUES
C      NOT A REAL CO-ORDINATE SYSTEM
C
  100 CALL RDKEYI('SPACING',.FALSE.,1,SP,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL.OR.SP.LE.0) THEN
         CALL WRERR('HELLSP')
         CALL CNPAR('SPACING',STATUS)
         GO TO 100
      END IF
C
C      NOW DO THE WORK
C
      CALL GRID(%VAL(PIN),%VAL(POUT),AX(1),AX(2),SP)
C
C      AND EXIT
C
      CALL FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE GRID(IN,OUT,N,M,ISP)
C
C      PUTS A GRID ON TOP OF THE DATA WHICH SHOULD BE
C      VISIBLE FOR ALL DATA SAMPLES , ON THE ASSUMPTION
C      THAT 0 IS BLACK AND 255 IS WHITE
C      THE SWITCH OVER IS AT A VALUE OF 128
C
      INTEGER*2 IN(N,M),OUT(N,M)
C
C       FIRST COPY OVER THE DATA
C
      DO 200 J=1,M
         DO 100 I=1,N
            OUT(I,J)=IN(I,J)
  100    CONTINUE
  200 CONTINUE
C
C     NOW DO THE SUBSTITUTION
C
      DO 400 J=1,M
C
C      LOOP THROUGH THE ROWS
C
         IF (MOD(J,ISP).EQ.0) THEN
C
C      IF IT IS A ROW THAT NEEDS A HORIZONTAL LINE , THEN SET IT UP
C
          DO 300 I=1,N
            IF (IN(I,J).GT.128) THEN
               OUT(I,J)=0
            ELSE
               OUT(I,J)=255
            END IF
  300     CONTINUE
         ELSE
C
C      IF NOT , JUST CHANGE THE PLACES WHERE THE VERTICAL LINES
C      CROSS IT
            DO 350 I=ISP,N,ISP
               IF (IN(I,J).GT.128) THEN
                   OUT(I,J)=0
               ELSE
                  OUT(I,J)=255
               END IF
  350       CONTINUE
         END IF
  400 CONTINUE
      RETURN
      END