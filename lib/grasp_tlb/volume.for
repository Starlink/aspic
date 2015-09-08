C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      FUNCTION VOLUME(CC)
C
C   This function performs an integration under a 2-D curve
C   defined by some of the parameters in the array CC
C
C   The method used is to add on cotributions around square contours
C   starting at the origin.
C
C   Parameters
C      INPUT
C         CC     This is the array which defines the paramereters
C                of the function to be fitted.
C
C      OUTPUT
C         VOLUME This is the returned value of the function - the
C                volume under the curve, effectively over the whole
C                doubly infinite plane.
C
C   Written by K F Hartley at RGO on 12/4/82
      REAL CC(32)
C
C   The integration is performed at points in between the original
C   data samples - in this case 100 times for each pixel.
C
      SEP=0.1
C
C   AREA is the area DXDY
C   RADIUS is derived from the profile parameters RX and RY
C   MAXN cuts off the integration at 10xRADIUS from the center.
C
      AREA=SEP*SEP
      RADIUS = (CC(4)+CC(5))/2.0
      MAXN=INT(10.*RADIUS/SEP)
      VOL=1.0*AREA
      OLDVOL=VOL
      DO N=1,MAXN
C
C      Integrate round the 4 sides of the current square.
C
         DO I=-N,N
            X=REAL(I)*SEP
            Y=REAL(N)*SEP
            AY = -1.0*Y
            CALL HGHT(AH,X,AY,CC)
            CALL HGHT(BH,X,Y,CC)
            VOL = VOL + (AH+BH)*AREA
         END DO
         DO J=-(N-1),(N+1)
            X=REAL(N)*SEP
            Y=REAL(J)*SEP
            AY = -1.0*Y
            CALL HGHT(AH,X,AY,CC)
            CALL HGHT(BH,X,Y,CC)
            VOL = VOL + (AH+BH)*AREA
         END DO
C
C      Then compare the current value with the one from the previous
C      integration.
C
         CHANGE = ABS((OLDVOL-VOL)/VOL)
         IF (CHANGE.LE.0.0001) GO TO 100
         OLDVOL=VOL
      END DO
  100 CONTINUE
C
C   Set the function value VOLUME to the computed value VOL
C
      VOLUME=VOL
C
C
C
      END



C
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C
      SUBROUTINE HGHT(AH,X,Y,CC)
C
C   This computes the height of the generalized 2-D Lorentzian
C   profile, as defined by the parameters stored in CC at the point
C   X,Y (where the center of the profile is at 0,0 )
C
C   Written by K F Hartley at RGO on 12/4/82
C
      REAL CC(32)
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



