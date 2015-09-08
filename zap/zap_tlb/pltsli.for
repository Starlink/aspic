      SUBROUTINE PLTSLI(KDATA,KX,KY,LX,LY,BS,BZ,SIZE,IERR)
C
C
C
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER LX(2),LY(2)
      INTEGER*2 KDATA(KX,KY)
      REAL SIZE(2),DEVLIM(2)
      CHARACTER TEXT*72
C
C
C
      IERR = 0
C
C
C
      NN = (LX(2)-LX(1)+1)*(LY(2)-LY(1)+1)
      CALL GETDYN('DIS',FMT_R,NN,IPDIS,ISTAT)
      IF (ISTAT.NE.ERR_NORMAL) THEN
         CALL WRUSER('CANT GET DISPLAY SPACE',ISTATA)
      ELSE
         MAX = KDATA(LX(1),LY(1))
         MIN = MAX
         DO K = LY(1),LY(2)
            DO J = LX(1),LX(2)
               IF (KDATA(J,K).LT.MIN) MIN = KDATA(J,K)
               IF (KDATA(J,K).GT.MAX) MAX = KDATA(J,K)
            ENDDO
         ENDDO
         AMIN = REAL(MIN)*BS + BZ
         AMAX = REAL(MAX)*BS + BZ
         DEVLIM(1) = AMIN
         DEVLIM(2) = AMAX
         CALL RDKEYR('DEVLIM',.TRUE.,2,DEVLIM,NVAL,ISTATA)
         BOT = DEVLIM(1)
         TOP = DEVLIM(2)
         CALL PAGE(SIZE(1),SIZE(2))
         CALL LIM3D(BOT,TOP)
         ASIZE = 0.9*SIZE(1)
         KXA = LX(2) - LX(1) - 1
         KYA = LY(2) - LY(1) - 1
         CALL TRANS(KDATA,KX,KY,LX(1),LY(1),%VAL(IPDIS),KXA,
     +              KYA,BS,BZ)
         CALL SOLID(%VAL(IPDIS),KXA,KYA,ASIZE,15.0)
         CALL CNPAR('DEVLIM',ISTATA)
         CALL FRDATA('DIS',ISTATA)
      ENDIF
C
C
C
      END
 
 
 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      *************
C      *           *
C      * S/R TRANS *
C      *           *
C      *************
C
C
C --------------------------------------------------------
C
C
C
