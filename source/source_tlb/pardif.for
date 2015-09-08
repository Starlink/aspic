      PROGRAM PARDIF
C+
C
C      STARLINK ENVIRONMENT PROGRAM      *** PARDIF ***
C
C      WRITTEN BY K F HARTLEY AT RGO ON 2/9/81
C
C      VERSION #1
C
C      CREATES AN OUTPUT IMAGE BY SUBTRACTING A SHIFTED VERSION
C      OF THE ORIGINAL FROM THE UNSHIFTED VERSION.
C      THE SHIFT MAY BE DELTA X PIXELS IN X AND DELTA Y IN Y (+ OR -)
C      NO SCALING IS DONE, AND ALL THE IMAGES ARE REAL.
C
C      THIS MAY BE OF VALUE AS AN EDGE DETECTOR .
C
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
      INTEGER PIN,POUT,AX(2),DXY(2),STATUS
C
C      FIRST GET INPUT AND OUTPUT FRAMES
C
      CALL RDIMAG('INPUT',FMT_R,2,AX,I,PIN,STATUS)
      IF (STATUS.NE.ERR_NORMAL.OR.I.NE.2) THEN
         CALL WRERR('HELLIN')
         CALL FRDATA('INPUT',STATUS)
         CALL EXIT
      END IF
      CALL WRIMAG('OUTPUT',FMT_R,AX,2,POUT,STATUS)
      IF (STATUS.NE.ERR_NORMAL) THEN
         CALL WRERR('HELLOUT')
         CALL FRDATA(' ',STATUS)
         CALL EXIT
      END IF
C
C      NOW GET THE PIXEL SHIFT
C
  100 CALL RDKEYI('DELTAXY',.FALSE.,2,DXY,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL.OR.I.NE.2) THEN
         CALL WRERR('HELLXY')
         CALL CNPAR('DELTAXY',STATUS)
         GO TO 100
      END IF
C
C      NOW DO THE WORK
C
      CALL XYSHIFT(%VAL(PIN),%VAL(POUT),AX(1),AX(2),DXY(1),DXY(2))
C
C      NOW TIDY UP AND GO HOME
C
      CALL FRDATA(' ' ,STATUS)
      CALL EXIT
      END
      SUBROUTINE XYSHIFT(IN,OUT,N,M,DX,DY)
C
C      THIS ROUTINE CREATES OUT FROM IN BY SHIFTING EACH
C      PIXEL VALUE BY DX AND DY PLACES IN X AND Y
C
      INTEGER DX,DY
      REAL IN(N,M),OUT(N,M)
      LOGICAL*1 INSIDE
C
C      WE LOOP THROUGH ALL THE OUTPUT PIXELS , PUTTING IN A
C      SHIFTED VALUE IF AVAILABLE , OR XERO IF NOT
C
C      THE LOGIACL VARIABLE "INSIDE" IS SET TO TRUE FOR INTERIOR POINTS
C
      DO 200 J=1,M
         JJ=J-DY
         DO 100 I=1,N
            II=I-DX
C
C      N.B.   WE SUBTRACT DX AND DY BECAUSE THEY ARE SHIFTS
C             FROM II,JJ TO I,J
C             I.E. FROM OUT TO IN
C
             INSIDE=II.GT.1.AND.II.LE.N.AND.JJ.GT.1.AND.JJ.LE.M
             IF (INSIDE) THEN
               OUT(I,J)=IN(I,J)-IN(II,JJ)
             ELSE
                OUT(I,J)=0
             END IF
  100    CONTINUE
  200 CONTINUE
      END
