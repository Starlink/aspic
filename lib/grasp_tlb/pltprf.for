C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R PLTPRF *
C      *            *
C      **************
C
C
C       This s/r plots out the Lorentzian profile described by CC
C       and the residuals from the fit given by RESID
C
C
C      A J PENNY                  RGO                  82-OCT-25
C -----------------------------------------------------------
C
C
C
      SUBROUTINE PLTPRF(RESID,LX,LY,CC,SIZE)
C
C
C
      REAL RESID(LX,LY),CC(32),SIZE(2),AX(2),AY(2)
      REAL DIST(200),PDATA(200),PDIFF(200),PFIT(200),E(200)
      INTEGER NUM(200)
C
C
C
      X = CC(9)
      Y = CC(10)
      AH = CC(11)
      RX = CC(4)
      RY = CC(5)
      P = CC(6)
      PRX = CC(7)
      PRY = CC(8)
      AVRAD = (RX+RY)/2.0
C
C
C
      XMAX = (X-1.0)/RX
      XMAXA = (REAL(LX) - X)/RX
      YMAX = (Y - 1.0)/RY
      YMAXA = (REAL(LY) - Y)/RY
      IF (XMAXA.GT.XMAX) XMAX = XMAXA
      IF (YMAXA.GT.YMAX) YMAX = YMAXA
      DRMAX = SQRT(XMAX*XMAX+YMAX*YMAX)
      ANUMPT = 15.0*DRMAX
      IF (ANUMPT.GT.200.0) ANUMPT = 200.0
C
C
C
      DO K = 1,200
         NUM(K) = 0
         DIST(K) = 0.0
         PDATA(K) = 0.0
         PDIFF(K) = 0.0
         PFIT(K) = 0.0
         E(K) = 1.0E-10
      ENDDO
C
C
C
      DO K = 1,LY
         DO J = 1,LX
            DX = REAL(J) - X
            DY = REAL(K) - Y
            DR = SQRT((DX/RX)**2.0+(DY/RY)**2.0)
            IF (DR.LE.DRMAX) THEN
               KD = 1 + INT((ANUMPT-1.0)*(DR/DRMAX))
               DIST(KD) = DIST(KD) + DR*AVRAD
               NUM(KD) = NUM(KD) + 1
               D1 = SQRT((DX/RX)**2.0+(DY/RY)**2.0)
               D2 = SQRT((DX/PRX)**2.0+(DY/PRY)**2.0)
               PD = AH/(1.0+D1**(P*(1.0+D2)))
               PFIT(KD) = PFIT(KD) + PD
               PDIFF(KD) = PDIFF(KD) + RESID(J,K)
               PDATA(KD) = PDATA(KD) + PD + RESID(J,K)
            ENDIF
         ENDDO
      ENDDO
C
C  Bunch up if any points in the radial profile have no data
C
      DO K = 1,200-1
         IF (NUM(K).EQ.0) THEN
            NEXT = K
            KFOUND = 0
            DO J = K+1,200
               IF (KFOUND.EQ.0.AND.NUM(J).NE.0) THEN
                  KFOUND = 1
                  NEXT = J
               ENDIF
            ENDDO
            KGAP = NEXT - K
            NUM(K) = NUM(K+KGAP)
            DIST(K) = DIST(K+KGAP)
            PFIT(K) = PFIT(K+KGAP)
            PDATA(K) = PDATA(K+KGAP)
            PDIFF(K) = PDIFF(K+KGAP)
            NUM(K+KGAP) = 0
         ENDIF
      ENDDO
      NTOT = 0
      DO K = 1,200
         IF (NUM(K).NE.0) NTOT = NTOT + 1
      ENDDO
C
C
C
      DO K = 1,NTOT
         ANUM = REAL(NUM(K))
         PDATA(K) = PDATA(K)/ANUM
         DIST(K) = DIST(K)/ANUM
         PFIT(K) = PFIT(K)/ANUM
         PDIFF(K) = (PDIFF(K)/ANUM)/PFIT(K)
      ENDDO
      IF (NTOT.NE.200) THEN
         DO K = NTOT+1,200
            DIST(K) = DIST(NTOT)
            PDIFF(K) = PDIFF(NTOT)
            PFIT(K) = PFIT(NTOT)
            PDATA(K) = PDATA(NTOT)
         ENDDO
      ENDIF
C
C  Plot out the points
C
      AX(1) = 0.0
      AX(2) = 0.0
      AY(1) = 0.0
      AY(2) = 0.0
      DO K = 1,200
         IF (DIST(K).GT.AX(2)) AX(2) = DIST(K)
         IF (PDATA(K).GT.AY(2)) AY(2) = PDATA(K)
         IF (PDATA(K).LT.AY(1)) AY(1) = PDATA(K)
         IF (PFIT(K).GT.AY(2)) AY(2) = PFIT(K)
         IF (PFIT(K).LT.AY(1)) AY(1) = PFIT(K)
      ENDDO
      CALL JBAXES(AX,2,SIZE(1),' ',1,AY,2,SIZE(2),' ',1)
      DO K = 1,200
         X = DIST(K)
         Y = PDATA(K)
         CALL MARK PT(X,Y,3)
      ENDDO
      CALL BREAK
      DO K = 1,200
         X = DIST(K)
         Y = PFIT(K)
         CALL JOIN PT(X,Y)
      ENDDO
C
C
C
      END


