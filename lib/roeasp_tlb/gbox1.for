      SUBROUTINE GBOX1 (CORMIN,CORMAX,MAXBOX,MAXPAR,NBOX,
     :                  DIFF,INCR,CORBOX)
C+
C     GBOX1.
C
C     Subroutine to define a set of square apertures (boxes)
C     for CPB's equivalent profile routines.
C
C  Given;
C   CORMIN (IA) Coords. for smallest box.
C   CORMAX (IA) Coords. for largest box.
C   MAXBOX (I)  Max. permitted no. of boxes.
C   MAXPAR (I)  No. of parameters for each box, must be at 
C               least 4.
C   NBOX   (I)  No. of ditinct boxes requested.
C
C  Used;
C   DIFF   (IA) Work array, size = MAXBOX.
C   INCR   (IA)  "     "  ,  "   =   "   .
C   
C  Returned;
C   NBOX   (I)  No. of boxes generated (may in some cases
C               be different to the no. of boxes requested).
C   CORBOX (IA) Coords. for boxes generated.
C
C  Subroutines called;
C   None.
C
C  Structure:-
C   Compute differences between min. and max. boxes.
C   If (min. difference less than the no. of boxes)
C     No. of boxes = min difference
C   end if
C   for all coords
C     increment = (difference / no. of boxes)
C   end for
C   Compute coords. of boxes.
C
C  A C Davenhall./ROE/                                      24/8/82.
C-
      INTEGER MAXBOX,MAXPAR,NBOX
      INTEGER CORMIN(MAXPAR),CORMAX(MAXPAR),CORBOX(MAXBOX,MAXPAR),
     :        DIFF(MAXPAR),INCR(MAXPAR)
C
      INTEGER MINDIF
C
C
C    Compute the differences between the min. and max. coords
C
      DO I=1,MAXPAR
        DIFF(I)=ABS(CORMAX(I)-CORMIN(I))
      END DO
C
C    Compute the minimum difference.
C
      MINDIF=DIFF(1)
      DO I=2,MAXPAR
        IF (DIFF(I).LT.MINDIF) MINDIF=DIFF(I)
      END DO
C
C    Reset the number of boxes if the minimum difference is less
C    than the number of boxes.
C
      IF (MINDIF.LT.NBOX) NBOX=MINDIF
C
C    Compute the appropriate increment for each coordinate.
C
      DO I=1,MAXPAR
        INCR(I)=DIFF(I)/NBOX
      END DO
C
C    Set up coords. of boxes.
C
      DO I=1,MAXPAR
C
C    Smallest box.
C
        CORBOX(1,I)=CORMIN(I)
C
C    Intermediate boxes.
C
        DO K=2,NBOX-1
          IF (I.LE.2) THEN
            CORBOX(K,I)=CORMIN(I)-(INCR(I)*(K-1))
          ELSE
            CORBOX(K,I)=CORMIN(I)+(INCR(I)*(K-1))
          END IF
        END DO
C
C    Largest box.
C
        CORBOX(NBOX,I)=CORMAX(I)
      END DO
      END
