      SUBROUTINE INSTEF (EFFIC,XEXT,YEXT,IMAGE)
C+
C     INSTEF.
C
C     Subroutine to dim an image frame by an amount corresponding
C     to the fraction of light transmitted through the
C     combination of telescope and detector.
C
C  Given;
C   EFFIC  (R)  Fraction of incident light transmitted through
C               and detected by the combination of telescope and
C               detector.
C   XEXT   (I)  X extent of the image.
C   YEXT   (I)  Y   "    "   "    "  .
C   IMAGE  (RA) Input image array.
C
C  Returned;
C   IMAGE  (RA) Image array dimmed by the specified fraction.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                       30/8/82.
C-
      INTEGER XEXT,YEXT
      REAL IMAGE(XEXT,YEXT)
      REAL EFFIC
C
C
      DO J=1,YEXT
        DO I=1,XEXT
          IMAGE(I,J)=IMAGE(I,J)*EFFIC
        END DO
      END DO
      END
