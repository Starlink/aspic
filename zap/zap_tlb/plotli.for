      SUBROUTINE PLOTLI(LINE,NX,KMIN,ALO,AHI,STEP,BSCALE,BZERO,
     +                  SIZE)
C
C
C
      INTEGER LINE(NX)
      REAL AX(2),AY(2),SIZE(2)
      CHARACTER TEXT*72,DEVICE*4
C
C
C
      KSTEP = STEP/BSCALE
      IF (KSTEP.LT.1) KSTEP = 1
      ASTEP = BSCALE*REAL(KSTEP)
C
C  Get min and max of values
C
      LMIN = INT((ALO-BZERO)/BSCALE) - KMIN + 1
      LMAX = INT((AHI-BZERO)/BSCALE) - KMIN + 1
      AMIN = LINE(LMIN)
      AMAX = LINE(LMIN)
      DO K = LMIN,LMAX,KSTEP
         S = 0.0
         JA = K
         JB = JA + KSTEP - 1
         DO J = JA,JB
            IF (J.LE.LMAX) THEN
               S = S + FLOAT(LINE(J))
            ENDIF
         ENDDO
         IF (S.GT.AMAX) AMAX = S
         IF (S.LT.AMIN) AMIN = S
      ENDDO
      MIN = AMIN
      MAX = AMAX
      AY(1) = AMIN
      IF (MAX.GT.MIN) THEN
         AY(2) = AMAX + 0.2*(AMAX-AMIN)
      ELSE
         AY(2) = AY(1) + 1.0
      ENDIF
      AX(1) = ALO - ASTEP
      AX(2) = AHI + ASTEP
C
C  Draw Simpleplot axes
C
      CALL JBAXES(AX,2,SIZE(1),'VALUE',5,AY,2,SIZE(2),'NUMBER',6)
C
C  Plot the data
C
      AV = (REAL(LMIN+KMIN-1)+REAL(KSTEP)/2.0)*BSCALE + BZERO
      AV = AV - ASTEP
      CALL JOIN PT(AV,AY(1))
      DO K = LMIN,LMAX,KSTEP
         JA = K
         JB = JA + KSTEP - 1
         S = 0.0
         DO J = JA,JB
            IF (J.LE.LMAX) THEN
               S = S + FLOAT(LINE(J))
            ENDIF
         ENDDO
         CALL JOIN PT(AV,S)
         AV = AV + ASTEP
         CALL JOIN PT(AV,S)
      ENDDO
      CALL JOIN PT(AV,AY(1))
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PRAREA *
C      *            *
C      **************
C
C THIS S/R PUTS AN AREA OF THE ARRAY OUT ON THE TERMINAL
C
C ------------------------------------------------------
C
C
C
