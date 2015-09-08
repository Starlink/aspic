      SUBROUTINE MEDBOX (IMAGE,XEXT,YEXT,IBASE,ITOP,JBASE,JTOP,
     :                   MEDIAN)
C+
C     MEDBOX.
C
C     Subroutine to compute the median from a patch of an image 
C     array. Depending on the size of the patch the median will
C     either be computed exactly or extracted from the histogram
C     of the patch.
C
C  Given;
C   IMAGE   (RA) Image array.
C   XEXT    (I)  X extent of the image.
C   YEXT    (I)  Y   "    "   "    "  .
C   IBASE   (I)  Lower X coord. of the patch.
C   ITOP    (I)  Upper "   "  . "   "    "  .
C   JBASE   (I)  Lower Y   "  . "   "    "  .
C   JTOP    (I)  Upper "   "  . "   "    "  .
C
C  Returned;
C   MEDIAN  (R)  Median computed for the patch.
C
C  Subroutines called;
C   E2D:-   GENHIS, HISTPROP, BUBMED.
C
C  A C Davenhall./ROE/                                       14/8/82.
C  Based on BACK1 by B. D. Kelly/ROE/                        31/3/82.
C-
      INTEGER XEXT,YEXT,IBASE,ITOP,JBASE,JTOP
      REAL IMAGE(XEXT,YEXT)
      REAL MEDIAN
C
      REAL DUM1,DUM2,DUM3
      REAL MAXVAL,MINVAL
C
      INTEGER HISPTS
      PARAMETER (HISPTS=100)
      INTEGER JHIST(HISPTS)
C
      INTEGER NPIX,THRPIX
      PARAMETER (THRPIX=25)
C
C
C    Compute the number of pixels in the patch.
C
      NPIX=(JTOP-JBASE+1)*(ITOP-IBASE+1)
C
C    Depending on the number of pixels in the patch either compute
C    the median exactly or extract it from the histogram of the
C    patch.
C
      IF (NPIX.GT.THRPIX) THEN
C
C    Extract from histogram.
C
C
C    Find the values of the maximum and minimum pixels in the
C    patch for use by the histograming routines (they set
C    the range of the histogram).
C
        MAXVAL=IMAGE(IBASE,JBASE)
        MINVAL=IMAGE(IBASE,JBASE)
        DO J=JBASE,JTOP
          DO I=IBASE,ITOP
            IF (IMAGE(I,J).GT.MAXVAL) MAXVAL=IMAGE(I,J)
            IF (IMAGE(I,J).LT.MINVAL) MINVAL=IMAGE(I,J)
          END DO
        END DO
        CALL GENHIS (IMAGE,XEXT,YEXT,IBASE,ITOP,JBASE,JTOP,
     :               MAXVAL,MINVAL,JHIST,HISPTS)
        CALL HISTPROP (JHIST,HISPTS,MAXVAL,MINVAL,DUM1,DUM2,MEDIAN,
     :                 DUM3)
      ELSE
C
C    Compute exactly.
C
        CALL BUBMED (IMAGE,XEXT,YEXT,IBASE,ITOP,JBASE,JTOP,MEDIAN)
      END IF
      END
