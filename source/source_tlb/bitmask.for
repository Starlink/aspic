C+
C
C      STARLINK ENVIRONMENT PROGRAM      *** BITMASK ***
C
C      WRITTEN BY K F HARTLEY AT RGO ON 16/1/81
C
C      VERSION #1
C
C      THIS PROGRAM PERFORMS A BITWISE-AND OF A 1,2 OR 3 D
C      STARLINK IMAGE WITH A USER DEFINED VALUE
C
C      THIS IS OF (PERHAPS) MOST USE WITH -(2**N)
C      GIVING A LAYERED CONTOUR-LIKE APPEARANCE
C      BUT ANY INTEGER VALUE MAY BE USED.
C
C      THERE ARE THREE PARAMETERS : INPUT , OUTPUT AND MASK VALUE
C
C-
      INTEGER*4 PIN,POUT,AX(3),STATUS
      CALL RDIMAG('INPUT',104,3,AX,I,PIN,STATUS)
      IF (STATUS.NE.0.OR.I.LE.0) THEN
         CALL WRERR('HELLIN',STATUS)
         CALL EXIT
      END IF
      CALL WRIMAG('OUTPUT',104,AX,I,POUT,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRERR('HELLO',STATUS)
         CALL EXIT
      END IF
      NTOT=1
      DO 100 K=1,I
      NTOT=NTOT*AX(K)
  100 CONTINUE
C
C      ALLOWS A 1 OR 2 OR 3 D IMAGE TO BE HANDLED
C
  200 CALL RDKEYI('MASK',.FALSE.,1,MASKVAL,I,STATUS)
      IF (STATUS.NE.0.OR.I.NE.1) THEN
         CALL WRERR('HELL',STATUS)
         CALL CNPAR('MASK',STATUS)
         GO TO 200
      END IF
C
C      HAVING GOT THE PARAMETERS
C
      CALL BITMASK(%VAL(PIN),%VAL(POUT),NTOT,BITMASKVAL)
C
C      TIDY UP AND GO HOME
C
      CALL  FRDATA(' ',STATUS)
      CALL EXIT
      END
      SUBROUTINE BITMASK(IN,OUT,N,MV)
C
C      ANDS "IN" WITH MV TO GIVE "OUT"
C
      INTEGER*4 IN(N),OUT(N)
      DO 100 I=1,N
         OUT(I)=IAND(IN(I),MV)
  100 CONTINUE
      RETURN
      END
