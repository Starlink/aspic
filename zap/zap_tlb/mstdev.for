      SUBROUTINE MSTDEV(LINE,NX,NRAN,JN,AM,STD,SNUM)
      INTEGER LINE(NX)
      INTEGER NRAN(2)
      DOUBLE PRECISION S,SS,SN
C
C
C
      KL = NRAN(1)
      KH = NRAN(2)
      DO J = 1,JN
         S = 0.0
         SN = 0.0
         SS = 0.0
         IF (KH.GE.KL) THEN
            DO K = KL,KH
               AN = DBLE(LINE(K))
               V = DBLE(K)
               SN = SN + AN
               S = S + AN*V
               SS = SS + AN*V*V
            ENDDO
         ENDIF
         IF (SN.GT.0.0) THEN
            AM = S/SN
         ELSE
            AM = 0.0
         ENDIF
         IF (SN.GT.1.0) THEN
            SS = (SS-S*S/SN)/(SN-1.0)
            IF (SS.GT.1.0E-20) THEN
               STD = SQRT(SS)
            ELSE
               STD = 0.0
            ENDIF
         ELSE
            STD = 0.0
         ENDIF
         KLA = IFIX(AM-3.0*STD)
         KHA = IFIX(AM+3.0*STD)
         IF(KLA.GT.NRAN(1)) KL = KLA
         IF(KHA.LT.NRAN(2)) KH = KHA
      ENDDO
      SNUM = SN
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R HGRAM *
C      *           *
C      *************
C
C SUBROUTINE TO GET HISTOGRAM
C
C ---------------------------------------------------------
C
C
