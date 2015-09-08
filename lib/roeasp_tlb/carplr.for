      SUBROUTINE CARPLR (XPOS,YPOS,H,K,PHI,RADIUS,SINTHA,COSTHA)
C+
C     CARPLR.
C
C     Subroutine to convert a data point in a cartesian
C     reference frame into polar coordinates in a frame
C     rotated and translated with respect to the original
C     one.
C
C  Given;
C   XPOS   (R) X coord. of original pt. in cartesian frame.
C   YPOS   (R) Y   "  . "     "     " . "     "        "  .
C   H      (R) Translation in the X coord.
C   K      (R)     "       "   "  Y   "  .
C   PHI    (R) Angle of rotation between the frames (radians).
C
C  Returned;
C   RADIUS (R) Radial polar coord.
C   SINTHA (R) Sine of angular polar coord.
C   COSTHA (R) Cosine of angular polar coord.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   17/9/82.
C  A C Davenhall./ROE/    {Modified}                     21/9/82.
C-
      REAL XPOS,YPOS,H,K,PHI,RADIUS,SINTHA,COSTHA
C
      REAL XP,YP
C
C
C    Compute the coords. in a Cartesian frame rotated and
C    translated to be conincident with the required polar frame.
C
      XP=((XPOS+H)*COS(PHI))-((YPOS+K)*SIN(PHI))
      YP=((XPOS+H)*SIN(PHI))+((YPOS+K)*COS(PHI))
C
C    convert these cartesian coords. to polar coords.
C
      RADIUS=SQRT((XP**2)+(YP**2))
      IF (RADIUS.GT.1.0E-4) THEN
        SINTHA=YP/RADIUS
        COSTHA=XP/RADIUS
      ELSE
        SINTHA=0.0E0
        COSTHA=1.0E0
      END IF
      END
