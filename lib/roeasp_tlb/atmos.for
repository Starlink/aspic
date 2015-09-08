      SUBROUTINE ATMOS (ATMABS,XEXT,YEXT,IMAGE)
C+
C     ATMOS.
C 
C     Subroutine to dim an image frame held as intensity
C     by atmosperic absorption. The amount of absorption
C     is specified in magnitudes.
C
C  Given;
C   ATMABS (R)  Atmospheric absorption to be applied to the image
C               (magnitudes).
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Intput image array, in intensity.
C
C  Returned;
C   IMAGE  (RA) Image array dimmed by the specified amount.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                  30/8/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
      REAL ATMABS
C
      REAL RATIO
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
C
C    Calculate the ratio in intensity corresponding to the 
C    specified absorption in magnitudes.
C
      RATIO=1.0E1**(-ATMABS/POGSON)
C
C    Dim the image frame.
C
      DO J=1,YEXT
        DO I=1,XEXT
          IMAGE(I,J)=IMAGE(I,J)*RATIO
        END DO
      END DO
      END 
