      SUBROUTINE VERPLT (IMAGE,XEXT,YEXT,CONT,TITLE)
C+
C     VERPLT.
C
C     Subroutine to Plot a contour map of an image
C     array on the Versatec, using Fings.
C
C  Given;
C   IMAGE  (RA)  Image array.
C   XEXT   (I)   X extent of image.
C   YEXT   (I)   Y   "    "    "  .
C   CONT   (I)   No. of contours to appear in the image.
C   TITLE  (C)   Title to appear on the plot.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Graphics:-   GDEVI1, DRAXIS, STOPLOT.
C   E2D:-        CONMAP.
C   Fings:-      MOVTO2, CHAESC, CHAHOL.
C
C  Structure:-
C   find the largest and smallest pixels in the image.
C   Increment between contours = range/no. of contours.
C   Set up for plotting to the Versatec.
C   Draw the axes.
C   Plot the contour map.
C   Stop plotting.
C   Send to the Versatec.
C
C  A C Davenhall./ROE/                                    15/8/82.
C-
      INTEGER XEXT,YEXT,CONT
      REAL IMAGE(XEXT,YEXT)
      CHARACTER TITLE*(*)
C
      REAL MINVAL,MAXVAL,INCR
      REAL XPOS,YPOS
      LOGICAL CTRLC
C
C
C    Find the largest and smallest pixel values in the image.
C
      MINVAL=IMAGE(1,1)
      MAXVAL=IMAGE(1,1)
      DO J=1,YEXT
        DO I=1,XEXT
          IF (IMAGE(I,J).GT.MAXVAL) MAXVAL=IMAGE(I,J)
          IF (IMAGE(I,J).LT.MINVAL) MINVAL=IMAGE(I,J)
        END DO
      END DO
C
C    Determine the increment between contours.
C
      INCR=(MAXVAL-MINVAL)/FLOAT(CONT)
C
C    Set up for plotting to the versatec using Fings.
C
      CALL GDEVI1 (3,.TRUE.)
C
C    Define the plotting space and draw a set of axes.
C
      CALL DRAXIS (0.0E0,FLOAT(XEXT),0.0E0,FLOAT(YEXT),
     :             'X (PIXELS)','Y')
C
C    Plot the contours.
C
      CTRLC=.FALSE.
      CALL CONMAP (IMAGE,XEXT,YEXT,1,XEXT,1,YEXT,MINVAL,
     :             INCR,CONT,CTRLC)
C
C    Put up the title.
C
      XPOS=2.0E-1*FLOAT(XEXT)
      YPOS=1.05E0*FLOAT(YEXT)
      CALL MOVTO2 (XPOS,YPOS)
      CALL CHAESC ('$')
      CALL CHAHOL (%REF(TITLE)//'$.')
C
C    Terminate Fings.
C
      CALL STOPLOT
      END
