      SUBROUTINE PSEUMG (XEXT,YEXT,IMAGE)
C+
C      PSEUMG.
C
C      Subroutine to convert a 2D image normalised relative to a
C      sky of 1.0 (the sky not having been subtracted) into
C      pseudo-magnitudes (2.5log(intensity above sky)). Points
C      below or equal to the sky will be fudged by setting them
C      equal to an arbitary small number. Thus the image 
C      returned by this routine is unsuitable for many
C      photometric purposes and its primary use will probably
C      for pre-prossecing images prior to plotting in mag./sq.arcsec.
C
C  Given;
C   XEXT  (I)  X extent of the image.
C   YEXT  (I)  Y   "    "   "    "  .
C   IMAGE (RA) Input image; intensity normalised relative to the sky.
C
C  Returned;
C   IMAGE (RA) Output image in pseudo-magnitudes.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   9/9/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
C
C    
      DO J=1,YEXT
        DO I=1,XEXT
          IF (IMAGE(I,J)-1.0E0.GE.1.0E-7) THEN
C
C   Pixel appreciably above the sky. Compute the pseudo magnitude
C   correctly.
C
            IMAGE(I,J)=POGSON*ALOG10(IMAGE(I,J)-1.0E0)
          ELSE
C
C    Pixel is not appreciably above the sky.
C    Frig by setting equal to the pseudo magnitude corresponding
C    to the smallest intesnity to scrape through the test (above).
C
            IMAGE(I,J)=-1.75E1
          END IF
        END DO
      END DO
      END
