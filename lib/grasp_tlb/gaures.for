C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R GAURES *
C      *            *
C      **************
C
C   PURPOSE
C  This s/r updates the estimates by the just performed Gaussian
C  iteration and sees if the fit has converged. Used by AGAUSS and
C  BGAUSS (qv).
C  It makes new estimates of the height and base and corrects the
C  position and radii
C
C
C   ARGUMENTS
C    IN
C       E(6)   Real     Input update factors
C       M      Integer  X size of Data array
C       N      Integer  Y size of Data array
C    IN/OUT
C       XO     Real     X position of Gauss centre
C       YO     Real     Y position of Gauss centre
C       RX     Real     X Gaussian radius
C       RY     Real     Y Gaussian radius
C    OUT
C       A      Real     Gaussian height
C       B      Real     Gaussian base
C       ITERA  Integer  Convergence flag  =0 for not, =1 for yes
C
C   CALLS
C     None
C
C
C   A.J.PENNY                   RGO                    83-2-20
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE GAURES(E,A,B,XO,YO,RX,RY,ITERA,M,N)
C
C
C
      REAL E(6)
C
C
C
      RXO = ABS(RX)
      RYO = ABS(RY)
      HRX = RXO/2.0
      HRY = RYO/2.0
      A = E(2)
      SMALL = 1.0E-6
      IF(ABS(A).LT.SMALL) A = SIGN(SMALL,A)
      DX = E(3)/A
      DY = E(4)/A
      IF(A.GT.0.0) THEN
         DRX = E(5)/A
         DRY = E(6)/A
      ELSE
         DX = -1.0*DX
         DY = -1.0*DY
         DRX = 0.0
         DRY = 0.0
      ENDIF
      B = E(1)
C
      IF(ABS(DX).GT.HRX) DX = SIGN(HRX,DX)
      IF(ABS(DY).GT.HRY) DY = SIGN(HRY,DY)
      XM = M
      XO = XO + DX
      IF(XO.LT.3.0) XO = 3.0
      IF(XO.GT.XM-2.0) XO = XM - 2.0
      YN = N
      YO = YO + DY
      IF(YO.LT.3.0) YO = 3.0
      IF(YO.GT.YN-2.0) YO = YN - 2.0
      RX = RXO + DRX
      RY = RYO + DRY
      IF(RX.LT.0.5) RX = 0.5
      IF(RY.LT.0.5) RY = 0.5
C
      ITERA = 1
      IF(ABS(DX).GT.0.1.OR.ABS(DY).GT.0.1.OR.ABS(DRX).GT.0.005.
     +   OR.ABS(DRY).GT.0.005) THEN
         ITERA = 0
      ENDIF
C
C
      END



