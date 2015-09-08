C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R HEIGHT *
C      *            *
C      **************
C
C
C   PURPOSE
C   This computes the height of the generalized 2-D Lorentzian
C   profile, as defined by the parameters stored in CC at the point
C   X,Y (where the center of the profile is at 0,0 )
C
C
C   ARGUMENTS
C  IN
C     X       Real     X position relative to centre of profile
C     Y       Real     Y psoition relative to centre of profile
C     CC(32)  Real     Array containing profile parameters RX,RY,P,PRX,PRY
C                      in CC(4),CC(5),CC(6),CC(7),CC(8)
C  OUT
C     AH      Real     Height of profile at the X,Y point
C
C   CALLS
C      None
C
C
C
C   A.J.PENNY                   RGO                    83-2-28
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE HEIGHT(AH,X,Y,CC)
C
C
C
      REAL CC(32)
C
C
C
      IF (X.EQ.0.0.AND.Y.EQ.0.0) THEN
C
C      To avoid any possible overflow at the origin use the
C      analytical value of 1.0
C
         AH=1.0
      ELSE
C
C      Otherwise use the analytical expression for the profile
C      function.
C
         RX=CC(4)
         RY=CC(5)
         P=CC(6)
         PRX=CC(7)
         PRY=CC(8)
         GG=X*X/(RX*RX) + Y*Y/(RY*RY)
         HH=X*X/(PRX*PRX) + Y*Y/(PRY*PRY)
         PA=P*(1.0 + SQRT(HH))
         R = SQRT(GG)
         AH=1.0/(1.0+R**PA)
      END IF
C
C
C
      END



